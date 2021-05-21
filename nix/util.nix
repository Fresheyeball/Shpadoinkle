{ pkgs, compiler ? "", isJS ? false }:
rec
{ compilerjs = if isJS then "ghcjs${builtins.substring 3 2 compiler}" else compiler;
  gitignore = (pkgs.callPackage (pkgs.fetchFromGitHub
    { owner = "siers";
      repo = "nix-gitignore";
      rev = "4f2d85f2f1aa4c6bff2d9fcfd3caad443f35476e";
      sha256 = "1vzfi3i3fpl8wqs1yq95jzdi6cpaby80n8xwnwa8h2jvcw3j7kdz";
    }) {}).gitignoreSource;

  cannibalize = pkgs.writeScriptBin "cannibalize" ''
    #!${pkgs.bash}/bin/bash

    input=$1
    output=$2
    echo ""
    echo "╓──────────────────────────────────────────────────────────────────────"
    echo "║ Optimizing file $input ..."
    echo "╙──────────────────────────────────────────────────────────────────────"
    echo ""

    ${pkgs.closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
        --jscomp_warning=checkVars \
        --js_output_file=$output \
        --isolation_mode IIFE \
        --language_in=ECMASCRIPT_2018 \
        --create_source_map $output.map \
        --source_map_format=V3 \
        --source_map_include_content \
        --compilation_level ADVANCED_OPTIMIZATIONS \
        --warning_level=QUIET \
        $input
  '';

  doCannibalize = deriv:
    pkgs.haskell.lib.overrideCabal deriv (old: {
      postInstall = ''
        for dir in $out/bin/*/;
        do
            input="''${dir}all.js"
            output="''${dir}all.min.js"
            echo "input is $input"
            ${cannibalize}/bin/cannibalize ''${input} ''${output} || true
            rm $dir/index.html
            cp ${../etc/index.html} $dir/index.html

        done
        '';
    });
}
