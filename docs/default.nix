{ chan ? "20.03" }: with import ../nix/pkgs.nix { inherit chan; };
let

  theme = fetchurl {
    url    = https://gitlab.com/antora/antora-ui-default/-/jobs/artifacts/master/raw/build/ui-bundle.zip?job=bundle-stable;
    sha256 = "0jiis08zw1g263fnpsac928p2vz91cm53iklh8l0xy1fmn8szbxp";
  };


  util = import ../nix/util.nix {};

in
stdenv.mkDerivation {

  name         = "documentation";

  buildInputs  = [ antora ];

  src          = util.gitignore [
    "*.md"
    "*.nix"
    "*/**.hs"
    "*.ghc*"
    "**/package.yml"
    "*.cabal"
    "*dist*"
    "*result*"
    "*.md"
  ] ../.;


  shellHook    = ''
    rm -rf theme
    ln -s ${theme} theme
    cat ${../etc/figlet}
    function serve-docs(){
      echo "Building initial docs..."
      antora antora-playbook
      echo "Serving on port 8080..."
      ${haskellPackages.wai-app-static}/bin/warp -d public -p 8080 &
      echo "Watching for changes..."
      while inotifywait -e modify -r docs; do antora antora-playbook; done
    }
    echo ""
    echo "Build and serve docs by running"
    echo "serve-docs"
  '';


  buildCommand = ''
    mkdir $out
    export HOME=$PWD

    ln -s $src/docs docs
    ln -s $src/.git .git
    ln -s $src/antora-playbook.yml antora-playbook.yml
    ln -s ${theme} theme

    echo Building docs...

    antora antora-playbook
    cp -r public/* $out
  '';


}

