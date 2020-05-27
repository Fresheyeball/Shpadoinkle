{ isJS ? true }: let
  isDarwin = builtins.currentSystem == "x86_64-darwin";
in import ../default.nix
{
  inherit isJS;
  pack = "Shpadoinkle-tests";
  extra = pkgs: base: with pkgs;
  if isDarwin then base // {
      shellHook   = ''
        ${base.shellHook}
        echo ""
        cat ${../etc/darwin}
      '';
  } else base // {
      CHROME      = "${google-chrome}/bin/google-chrome-stable";
      EXAMPLES    = "../result";
      HEADLESS    = false;
      buildInputs = base.buildInputs ++
        [ selenium-server-standalone chromedriver google-chrome ];
  };
}
