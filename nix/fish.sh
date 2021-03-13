nix-build -A Shpadoinkle.doc
cp -r result-doc/share/doc/Shpadoinkle*/html public/core

nix-build -A Shpadoinkle-backend-pardiff.doc
cp -r result-doc/share/doc/Shpadoinkle-backend-pardiff-*/html public/backend-pardiff

nix-build -A Shpadoinkle-backend-snabbdom.doc
cp -r result-doc/share/doc/Shpadoinkle-backend-snabbdom-*/html public/backend-snabbdom

nix-build -A Shpadoinkle-backend-static.doc
cp -r result-doc/share/doc/Shpadoinkle-backend-static-*/html public/backend-static

nix-build -A Shpadoinkle-console.doc
cp -r result-doc/share/doc/Shpadoinkle-console-*/html public/console

nix-build -A Shpadoinkle-disembodied.doc
cp -r result-doc/share/doc/Shpadoinkle-disembodied-*/html public/disembodied

nix-build -A Shpadoinkle-lens.doc
cp -r result-doc/share/doc/Shpadoinkle-lens*/html public/lens

nix-build -A Shpadoinkle-html.doc
cp -r result-doc/share/doc/Shpadoinkle-html-*/html public/html

nix-build -A Shpadoinkle-router.doc
cp -r result-doc/share/doc/Shpadoinkle-router-*/html public/router

nix-build -A Shpadoinkle-widgets.doc
cp -r result-doc/share/doc/Shpadoinkle-widgets-*/html public/widgets

nix-build -A Shpadoinkle-streaming.doc
cp -r result-doc/share/doc/Shpadoinkle-streaming-*/html public/streaming

nix-build -A Shpadoinkle-template.doc
cp -r result-doc/share/doc/Shpadoinkle-template-*/html public/template

nix-build -A Shpadoinkle-isreal.doc
cp -r result-doc/share/doc/Shpadoinkle-isreal-*/html public/isreal
