{ compiler, isJS }: self: super: with super.lib; let


  util = import ./util.nix { inherit compiler isJS; };


  chrome-rev = "71336116f3f78d3bb1f499bf4b88efcd8738a9cf";


  jsaddle-src = super.fetchFromGitHub
    { owner  = "ghcjs";
      repo   = "jsaddle";
      rev    = "d569be43f92b9b8c01dc3ee4c41401ab406a2076";
      sha256 = "1m1xxy4l9ii91k1k504qkxh9k1ybprm1m66mkb9dqlwcpyhcccmv";
    };


  servant-src = super.fetchFromGitHub
    { owner  = "haskell-servant";
      repo   = "servant";
      rev    = "b4e5aa0deff238e117137be68a4345bb02b7a80b";
      sha256 = "0x27bgrasbxzp045rqj4ldrfnm2k832ch7vfkl9s7xj0afrcy6pg";
    };


  continuations-src = super.fetchFromGitHub
    { owner  = "morganthomas";
      repo   = "Shpadoinkle-continuations";
      rev    = "820066602122eca8b681057555186bb73c1e39f0";
      sha256 = "009cr2aqbdazlgpkv4z87ws2g22pjh2dpdbb5ddk80rf8bk4sxax";
    };


  snabbdom-src = super.fetchFromGitHub
    { owner  = "snabbdom";
      repo   = "snabbdom";
      rev    = "4c86aff05a34ebbd067b8430afd40542b3f728c0";
      sha256 = "10cvraxmblpwi30pib9zmz86dschpp89p95d8mhvdidv1hynscqj";
    };


  gitignore = util.gitignore
      [ ".git"
        "*.ghc*"
        "*.cabal"
        "*result*"
        "*dist*"
        "*.nix"
        "*.md"
        ".*.yml"
      ];


  addWError = x: super.haskell.lib.overrideCabal x (drv: {
    configureFlags = ["--ghc-option=-Werror"];
  });


in {


  google-chrome = (import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${chrome-rev}.tar.gz";
    }) {}).google-chrome;

  haskell = super.haskell //
    { packages = super.haskell.packages //
      { "${util.compilerjs}" = with super.haskell.lib;
      let dontJS = if isJS then x: dontHaddock (dontCheck x) else id;
      in super.haskell.packages.${util.compilerjs}.override (old: {
        overrides = super.lib.composeExtensions (old.overrides or (_:_: {})) (hself: hsuper:
        let call = n: p: addWError (hself.callCabal2nix n (gitignore p) {});
        in {

          Shpadoinkle                  = call "Shpadoinkle"                  ../core;
          Shpadoinkle-backend-snabbdom = call "Shpadoinkle-backend-snabbdom" ../backends/snabbdom;
          Shpadoinkle-backend-static   = call "Shpadoinkle-backend-static"   ../backends/static;
          Shpadoinkle-backend-pardiff  = call "Shpadoinkle-backend-pardiff"  ../backends/pardiff;
          Shpadoinkle-debug            = call "Shpadoinkle-debug"            ../debug;
          Shpadoinkle-lens             = call "Shpadoinkle-lens"             ../lens;
          Shpadoinkle-html             = call "Shpadoinkle-html"             ../html;
          Shpadoinkle-router           = call "Shpadoinkle-router"           ../router;
          Shpadoinkle-widgets          = call "Shpadoinkle-widgets"          ../widgets;
          Shpadoinkle-examples         = call "Shpadoinkle-examples"         ../examples;
          Shpadoinkle-experiments      = call "Shpadoinkle-experiments"      ../experiments;
          Shpadoinkle-tests            = super.haskell.packages.${compiler}.callCabal2nix "tests" (gitignore ../tests)       {};

          Shpadoinkle-continuations    = dontJS (hself.callCabal2nix "Shpadoinkle-continuations" "${continuations-src}/" {});

          hashable             = dontJS hsuper.hashable;
          comonad              = dontJS hsuper.comonad;

          compactable          = doJailbreak hsuper.compactable;
          beam-core            = doJailbreak hsuper.beam-core;
          beam-migrate         = doJailbreak hsuper.beam-migrate;
          ghcid                = doJailbreak hsuper.ghcid;

          cryptohash-sha1      = dontJS hsuper.cryptohash-sha1;
          cryptohash-md5       = dontJS hsuper.cryptohash-md5;
          extra                = dontJS hsuper.extra;
          email-validate       = dontJS hsuper.email-validate;
          SHA                  = dontJS hsuper.SHA;
          pureMD5              = dontJS hsuper.pureMD5;
          hex                  = dontJS hsuper.hex;
          unliftio             = dontJS hsuper.unliftio;
          semigroupoids        = dontJS hsuper.semigroupoids;
          megaparsec           = dontJS hsuper.megaparsec;
          lens                 = dontJS hsuper.lens;
          hpack                = if isJS then super.haskell.packages.${compiler}.hpack else hsuper.hpack;
          http-types           = dontJS hsuper.http-types;
          silently             = dontJS hsuper.silently;
          QuickCheck           = dontJS hsuper.QuickCheck;
          tasty-quickcheck     = dontJS hsuper.tasty-quickcheck;
          temporary            = dontJS hsuper.temporary;
          hspec                = dontJS hsuper.hspec;
          time-compat          = dontJS hsuper.time-compat;
          scientific           = dontJS hsuper.scientific;
          servant              = dontJS    (hself.callCabal2nix "servant"              "${servant-src}/servant" {});
          servant-server       = dontCheck (hself.callCabal2nix "servant-server"       "${servant-src}/servant-server" {});
          servant-client       = dontCheck (hself.callCabal2nix "servant-client"       "${servant-src}/servant-client" {});
          servant-client-ghcjs = doJailbreak (dontJS (hself.callCabal2nix "servant-client-ghcjs" "${servant-src}/servant-client-ghcjs" {}));
          snabbdom             = hself.callCabal2nix "snabbdom" snabbdom-src {};
          jsaddle-warp         = doJailbreak (hself.callCabal2nix "jsaddle-warp"         "${jsaddle-src}/jsaddle-warp" {});
          jsaddle              = doJailbreak (hself.callCabal2nix "jsaddle"              "${jsaddle-src}/jsaddle" {});

          # Diff = dontJS (if compiler == "ghc844" then appendPatch hsuper.Diff ./Diff-Test.patch else hsuper.diff);
        });
      });
    };
  };
}
