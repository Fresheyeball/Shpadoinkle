{ compiler, isJS,
  useFastWeak ? false,
  useReflexOptimizer ? false,
  enableLibraryProfiling ? false,
  enableTraceReflexEvents ? false,
  useTextJSString ? true,
  enableExposeAllUnfoldings ? true,
  __useTemplateHaskell ? true,
  haskellOverlaysPre ? [],
  haskellOverlaysPost ? []
}: self: super: let

  util = import ./util.nix { inherit compiler isJS; };

  # Makes sure that old `overrides` from a previous call to `override` are not
  # forgotten, but composed. Do this by overriding `override` and passing a
  # function which takes the old argument set and combining it. What a tongue
  # twister!
  makeRecursivelyOverridable = x: x // {
    override = new: makeRecursivelyOverridable (x.override (old: (combineOverrides old new)));
  };

  nixpkgs = super;
  lib = nixpkgs.lib;
  haskellLib = nixpkgs.haskell.lib;

  combineOverrides = old: new: old // new // lib.optionalAttrs (old ? overrides && new ? overrides) {
    overrides = lib.composeExtensions old.overrides new.overrides;
  };
  ghcSavedSplices = ghcSavedSplices-8_6;
  ghcSavedSplices-8_6 = (makeRecursivelyOverridable nixpkgs.haskell.packages.integer-simple.ghcSplices-8_6).override {
    overrides = lib.foldr lib.composeExtensions (_: _: {}) (let
      haskellOverlays = nixpkgs.haskell.overlays;
    in [
      reflex-overlays.combined
      reflex-overlays.saveSplices
      (self: super: with haskellLib; {
        blaze-textual = haskellLib.enableCabalFlag super.blaze-textual "integer-simple";
        cryptonite = disableCabalFlag super.cryptonite "integer-gmp";
        integer-logarithms = disableCabalFlag super.integer-logarithms "integer-gmp";
        scientific = enableCabalFlag super.scientific "integer-simple";
        dependent-sum-template = dontCheck super.dependent-sum-template;
        generic-deriving = dontCheck super.generic-deriving;
      })
    ]);
  };

  reflex-platform = super.fetchFromGitHub
    { owner  = "reflex-frp";
      repo   = "reflex-platform";
      rev    = "92a06ffaea035fb6aee5ebd8b282081c2126c8d4";
      sha256 = "0gz02nzsj7jghw08dl3gp00d1211i6218453v2pxrjxb92d5501z";
    };

  selfy = self //
    import (reflex-platform + "/nixpkgs-overlays/hack-get/default.nix")
      { inherit lib; } selfy //
      { buildPackages = self.buildPackages // { inherit (selfy) thunkSet; };
    };

  reflex-overlays = import (reflex-platform + "/haskell-overlays/default.nix") {
      nixpkgs = selfy;
      inherit (selfy) lib;
      haskellLib = selfy.haskell.lib;
      inherit
        useFastWeak useReflexOptimizer enableLibraryProfiling enableTraceReflexEvents
        useTextJSString enableExposeAllUnfoldings __useTemplateHaskell
        haskellOverlaysPre
        haskellOverlaysPost;
      inherit ghcSavedSplices;
    };


in {
  haskell = super.haskell //
  { packages = super.haskell.packages //
    { ${util.compilerjs} = super.haskell.packages.${util.compilerjs}.override (old:
      { overrides = super.lib.composeExtensions (old.overrides or (_:_:{})) reflex-overlays.combined;
      });
    };
  };
}
