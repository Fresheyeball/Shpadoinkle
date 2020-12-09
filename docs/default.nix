{ chan ? "20.03" }: with import ../nix/pkgs.nix { inherit chan; };
let

  theme = fetchurl {
    url    = https://gitlab.com/robinbb/antora-ui-default/-/jobs/artifacts/Shpadoinkle/raw/build/ui-bundle.zip?job=bundle-stable;
    sha256 = "1g2xkll0brjlwsyxgvsb6ff9dihmdaf4hix4mc2b7bmaf9fgay19";
  };


  util = import ../nix/util.nix {};

  src = util.gitignore [
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

in
stdenv.mkDerivation {

  name         = "Shpadoinkle-documentation";

  buildInputs  = [ antora inotify-tools git ];

  shellHook    = ''
    SHPADOINKLE_TOP="$(git rev-parse --show-toplevel)"
    SHPADOINKLE_DOCS="$SHPADOINKLE_TOP"/docs
    cat "$SHPADOINKLE_TOP"/etc/figlet
    rm -rf "$SHPADOINKLE_DOCS"/theme
    ln -s ${theme} "$SHPADOINKLE_DOCS"/theme
    function serve-docs() (
      set -euo pipefail
      echo "Building initial docs..."
      cd "$SHPADOINKLE_DOCS"
      antora antora-playbook
      echo "Serving on port 8080..."
      ${haskellPackages.wai-app-static}/bin/warp -d public -p 8080 &
      echo "Watching for changes..."
      while inotifywait -e modify -r .; do antora antora-playbook; done
    )
    echo ""
    echo "Build and serve docs by running"
    echo "serve-docs"
  '';

  buildCommand = ''
    set -euo pipefail
    mkdir "$out"
    HOME="$PWD"
    export HOME
    cp -r "${src}"/. .
    chmod +w ./docs
    ln -s "${theme}" ./docs/theme
    cd docs
    antora antora-playbook
    cp -r public/. "$out"
  '';

}
