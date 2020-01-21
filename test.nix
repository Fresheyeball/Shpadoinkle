{ compiler ? "ghc864", isJS ? true }:
let pkgs = import ./pkgs.nix compiler isJS "19.09"; in with pkgs; with lib; let

  packages = import ./default.nix { inherit compiler isJS; };
  util = import ./util.nix { inherit compiler isJS; };

in runCommand "${util.compilerjs}-test" {

    # chrome needs fonts to live
    FONTCONFIG_FILE = makeFontsConf {
      inherit fontconfig;
      fontDirectories = [ "${corefonts}" ];
    };
    COMPILER = util.compilerjs;
    EXAMPLES = "${packages.Shpadoinkle-examples}";
    CHROME = "${google-chrome}/bin/google-chrome-stable";
    HEADLESS = true;
    buildInputs =
    [
      tigervnc
      socat
      selenium-server-standalone
      chromedriver
      google-chrome
    ];

  } ''
    # set virtual display
    export DISPLAY=:10

    # start vnc server to host xorg in memory
    # x server (including vnc) wont create a unix socket if permissions don't line up
    # which they wont because we are inside a nix build
    # so instead we use tcp port 8999
    Xvnc :10 -listen tcp -rfbport 8999 -nolisten unix -ac -auth $PWD/auth &

    # make the folder the the unix socket where we will proxy vnc server
    mkdir -p /tmp/.X11-unix

    # socat doesn't have the same restriction as x server in regards to permissions
    # so we make the unix socket with socat, and proxy it to xvnc
    socat UNIX-LISTEN:/tmp/.X11-unix/X10 TCP:localhost:8999 &

    # start selenium in the background
    selenium-server &> selenium.log &
    # wait for selenium to start
    sleep 3

    mkdir -p $out/userDataDir
    export DATADIR=$out/userDataDir

    # run the end to end tests
    ${packages.Shpadoinkle-tests}/bin/tests
    rm -r $out
    echo SUCCESS > $out

  ''
