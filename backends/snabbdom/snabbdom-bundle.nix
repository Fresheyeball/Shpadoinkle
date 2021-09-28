let
  pkgs = import ../../nix/base-pkgs.nix { chan = import ../../nix/chan.nix; } {};

  snabbdom =
    builtins.fetchGit
      { url = "https://github.com/athanclark/snabbdom.git";
        rev = "27db501b76c7c1a4e1bc1bfd4e9b0dc6903c7cb8";
      };
in
with pkgs;
stdenv.mkDerivation
  { name = "snabbdom-bundle";
    src = snabbdom;

    buildInputs =
      [ nodejs
        nodePackages.parcel-bundler
      ];

    buildPhase =
      let
        node-modules =
          (import snabbdom { inherit pkgs; }).nodeDependencies
          + /lib/node_modules;
      in
      ''
      mkdir $out
      ln -s ${node-modules} .
      npm run compile
      ln -s ${./Shpadoinkle/Backend/Snabbdom/Setup_src.js} build/package/Setup_src.js
      parcel build build/package/Setup_src.js
      '';

    installPhase =
      ''
      mv dist/Setup_src.js $out/Setup.js
      '';
  }
