{ compiler, isJS }: self: super: with super.lib; let


  nixpkgs-unstable = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/84d74ae9c9cbed73274b8e4e00be14688ffc93fe.tar.gz";
  };

  util = import ./util.nix { inherit compiler isJS; pkgs = import nixpkgs-unstable {}; };

  chrome-rev = "71336116f3f78d3bb1f499bf4b88efcd8738a9cf";


  jsaddle-src = builtins.fetchGit
    { url = https://github.com/ghcjs/jsaddle.git;
      rev = "97273656e28790ab6e35c827f8086cf47bfbedca";
      ref = "master";
    };


  servant-src = super.fetchFromGitHub
    { owner  = "haskell-servant";
      repo   = "servant";
      rev    = "b4e5aa0deff238e117137be68a4345bb02b7a80b";
      sha256 = "0x27bgrasbxzp045rqj4ldrfnm2k832ch7vfkl9s7xj0afrcy6pg";
    };

  servant-rawm-src = super.fetchFromGitHub
    { owner  = "cdepillabout";
      repo   = "servant-rawm";
      rev    = "e6b7dd6d8bbc9610ed2b1d974c5813034b1c3da7";
      sha256 = "12qxmq4i3vdcfddbq5wnyyfl0l68qccrf911hg3dk8i9iz02wgjm";
    };


  snabbdom-src = super.fetchFromGitHub
    { owner  = "snabbdom";
      repo   = "snabbdom";
      rev    = "4c86aff05a34ebbd067b8430afd40542b3f728c0";
      sha256 = "10cvraxmblpwi30pib9zmz86dschpp89p95d8mhvdidv1hynscqj";
    };


  servant-client-js-src = super.fetchFromGitHub
    { owner = "morganthomas";
      repo = "servant-client-js";
      rev = "9dcec8521ec18226b404d18504ddd28cd118b2d0";
      sha256 = "10vah3qv3q3hiwidynbq272x6cg3wcan68rlxsy0i0f238vn7g96";
   };


  servant-jsaddle-src = super.fetchFromGitHub
    { owner  = "haskell-servant";
      repo   = "servant-jsaddle";
      rev    = "6ef0204b651b259ae165c3ad94e5814213515e44";
      sha256 = "1193iplbbqvxd0nxbzikr6x8ilc6qqi4rp8yr34zlrzi0d32lqjn";
    };


  quickcheck-classes-src = super.fetchFromGitHub
    { owner  = "andrewthad";
      repo   = "quickcheck-classes";
      rev    = "ba37536b2fce9afe745d8bee7dc2bf9c37ea1246";
      sha256 = "14m56kidwq35r37vm47kj33lvhc1sm05n8vv5hfrm4k8bh5d59qf";
    };


  ghcjs-base-stub-src = super.fetchFromGitHub
    { owner = "louispan";
      repo = "ghcjs-base-stub";
      rev = "8eaee240c9af1a2290f4572a87528f3ddb3e9f12";
      sha256 = "07cp0nlm0ab6z69pdsbb2rdp3gw5c874v3j3pj051041vxcbbb54";
    };


  ease = builtins.fetchGit {
    url = https://gitlab.com/fresheyeball/Ease.git;
    rev = "86c5b8696186f4f0a6b47b7556f50f9f4083cfa9";
    ref = "master";
  };


  stylish-haskell-src = self.fetchFromGitHub {
    owner = "jaspervdj";
    repo = "stylish-haskell";
    rev = "v0.12.2.0";
    sha256 = "1jc844x8v93xgnl6fjrk31gb2zr96nxbqmgmnc4hdfhfyajh5y7w";
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


  addFlags = x: super.haskell.lib.overrideCabal (super.haskell.lib.appendConfigureFlags x
      ["--ghc-option=-Werror"]) (drv: {
    haddockFlags   = ["--css=${../etc/linuwial.css}"];
  });


  addDev  = x: super.haskell.lib.appendConfigureFlags x [ "-f" "development" ];

  addTest = x: hpkgs: if isJS then super.haskell.lib.dontCheck x else
    (super.haskell.lib.appendConfigureFlags (super.haskell.lib.addBuildDepends x
      (with hpkgs; [hspec QuickCheck quickcheck-classes quickcheck-classes-base ])
    ) [ "-f" "testing" ]);


in {
  # stylish haskell binary is outside of package set becase we're interested only in the binary,
  inherit ((import stylish-haskell-src {}).stylish-haskell.components.exes)
    stylish-haskell;

  hlint = with (import nixpkgs-unstable {});
    haskell.lib.justStaticExecutables haskellPackages.hlint;

  google-chrome = (import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${chrome-rev}.tar.gz";
    }) {}).google-chrome;

  haskell = super.haskell //
    { packages = super.haskell.packages //
      { "${util.compilerjs}" = with super.haskell.lib;
      super.haskell.packages.${util.compilerjs}.override (old: {

        overrides = super.lib.composeExtensions (old.overrides or (_:_: {})) (hself: hsuper:

        let

          call     = n: p: addFlags (hself.callCabal2nix n (gitignore p) {});
          forThese = f: builtins.foldl' (acc: x: acc // { ${x} = f hsuper.${x}; }) {};
          dontJS   = if isJS then x: dontHaddock (dontCheck x) else id;

          patchLicense = x: x.overrideAttrs (old: {
            postPatch = ''
              ${old.postPatch}
              rm LICENSE
              cp ${servant-rawm-src}/LICENSE .
            '';
          });

          hpkgs    = {

          Shpadoinkle                  = call "Shpadoinkle"                  ../core;

          Shpadoinkle-backend-snabbdom = call "Shpadoinkle-backend-snabbdom" ../backends/snabbdom;
          Shpadoinkle-backend-static   = call "Shpadoinkle-backend-static"   ../backends/static;
          Shpadoinkle-backend-pardiff  = call "Shpadoinkle-backend-pardiff"  ../backends/pardiff;
          Shpadoinkle-console          = call "Shpadoinkle-console"          ../console;
          Shpadoinkle-developer-tools  = addDev (call "Shpadoinkle-developer-tools"  ../developer-tools);
          Shpadoinkle-disembodied      = call "Shpadoinkle-disembodied"      ../disembodied;
          Shpadoinkle-lens             = call "Shpadoinkle-lens"             ../lens;
          Shpadoinkle-marketing        = call "Shpadoinkle-marketing"        ../marketing;
          Shpadoinkle-html             = call "Shpadoinkle-html"             ../html;
          Shpadoinkle-router           = call "Shpadoinkle-router"           ../router;
          Shpadoinkle-widgets          = addTest (call "Shpadoinkle-widgets" ../widgets) hpkgs;

          Shpadoinkle-tests            = super.haskell.packages.${compiler}.callCabal2nix "tests" (gitignore ../tests) {};
          Shpadoinkle-examples         = call "Shpadoinkle-examples"         ../examples;

          Shpadoinkle-isreal           = call "Shpadoinkle-isreal"           ../isreal;

          ease                    = hself.callCabal2nix "ease" ease {};
          ghcjs-base-stub         = hself.callCabal2nix "ghcjs-base-stub" ghcjs-base-stub-src {};
          hpack                   = if isJS then super.haskell.packages.${compiler}.hpack else hsuper.hpack;
          servant                 = dontJS    (hself.callCabal2nix "servant"             "${servant-src}/servant"                  {});
          servant-server          = dontCheck (hself.callCabal2nix "servant-server"      "${servant-src}/servant-server"           {});
          servant-client          = dontCheck (hself.callCabal2nix "servant-client"      "${servant-src}/servant-client"           {});
          servant-rawm            = dontJS    (patchLicense (hself.callCabal2nix "servant-rawm"        "${servant-rawm-src}/servant-rawm"        {}));
          servant-rawm-server     = dontCheck (patchLicense (hself.callCabal2nix "servant-rawm-server" "${servant-rawm-src}/servant-rawm-server" {}));
          servant-rawm-client     = dontCheck (patchLicense (hself.callCabal2nix "servant-rawm-client" "${servant-rawm-src}/servant-rawm-client" {}));
          servant-client-js       = hself.callCabal2nix "servant-client-js" servant-client-js-src {};
          servant-jsaddle         = dontCheck (hself.callCabal2nix "servant-jsaddle" "${servant-jsaddle-src}"        {});
          snabbdom                = hself.callCabal2nix "snabbdom" snabbdom-src {};
          jsaddle-warp            = dontCheck (hself.callCabal2nix "jsaddle-warp"    "${jsaddle-src}/jsaddle-warp"   {});
          jsaddle                 = dontCheck (hself.callCabal2nix "jsaddle"         "${jsaddle-src}/jsaddle"        {});
          quickcheck-classes      = doJailbreak (hself.callCabal2nix "quickcheck-classes"      "${quickcheck-classes-src}/quickcheck-classes"      {});
          quickcheck-classes-base = doJailbreak (hself.callCabal2nix "quickcheck-classes-base" "${quickcheck-classes-src}/quickcheck-classes-base" {});

          # Diff = dontJS (if compiler == "ghc844" then appendPatch hsuper.Diff ./Diff-Test.patch else hsuper.diff);
        } // forThese dontJS [
          "hashable"
          "comonad"
          "cryptohash-sha1"
          "cryptohash-md5"
          "extra"
          "email-validate"
          "SHA"
          "pureMD5"
          "hex"
          "unliftio"
          "semigroupoids"
          "criterion"
          "megaparsec"
          "lens"
          "http-types"
          "text-short"
          "silently"
          "QuickCheck"
          "tasty-quickcheck"
          "temporary"
          "hspec"
          "time-compat"
          "scientific"
          "base-compat-batteries"
          "Glob"
        ] // forThese doJailbreak [
          "compactable"
          "beam-core"
          "beam-migrate"
          "ghcid"
          "generic-lens-labels"
        ] // forThese unmarkBroken [
          "alg"
          "dual"
          "category"
        ];
      in hpkgs);
      });
    };
  };
}
