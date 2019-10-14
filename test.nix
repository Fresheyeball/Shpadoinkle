let pkgs = import ./pkgs.nix; in with pkgs; with lib; let

  chrome-rev = "9619debe3d8b99bc56342ec4e0ee818aaa5eb985";
  chrome = (import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${chrome-rev}.tar.gz";
    }) {}).google-chrome;

  test = compiler: packages: runCommand "${compiler}-test" {

    # chrome needs fonts to live
    FONTCONFIG_FILE = makeFontsConf {
      inherit fontconfig;
      fontDirectories = [ "${corefonts}" ];
    };
    COMPILER = compiler;
    EXAMPLES = "${packages.Shpadoinkle-examples}";
    CHROME = "${chrome}/bin/google-chrome-stable";
    HEADLESS = true;
    buildInputs =
    [
      tigervnc
      socat
      selenium-server-standalone
      chromedriver
      chrome
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

  '';


  constituents = foldl (xs: compiler: xs ++
    (let built = import ./default.nix { inherit compiler; };
    in [ (attrValues built) (test compiler built) ])) []
    [
      "ghc843"
      "ghcjs84"
    ];

in releaseTools.aggregate {
  name = "shapdoinkle_release";
  constituents = constituents;
}

