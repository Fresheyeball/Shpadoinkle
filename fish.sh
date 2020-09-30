mkdir -p public/haddock

nix-build -A Shpadoinkle.doc
cp -r result-doc/share/doc/Shpadoinkle*/html public/core

nix-build -A Shpadoinkle-lens.doc
cp -r result-doc/share/doc/Shpadoinkle-lens*/html public/lens

nix-build -A Shpadoinkle-backend-pardiff.doc
cp -r result-doc/share/doc/Shpadoinkle-backend-pardiff-*/html public/backend-pardiff

nix-build -A Shpadoinkle-backend-snabbdom.doc
cp -r result-doc/share/doc/shpadoinkle-backend-snabbdom-*/html public/backend-snabbdom

nix-build -A Shpadoinkle-backend-static.doc
cp -r result-doc/share/doc/shpadoinkle-backend-static-*/html public/backend-static

nix-build -A Shpadoinkle-console.doc
cp -r result-doc/share/doc/shpadoinkle-console-*/html public/console

nix-build -A Shpadoinkle-html.doc
cp -r result-doc/share/doc/shpadoinkle-html-*/html public/html

nix-build -A Shpadoinkle-router.doc
cp -r result-doc/share/doc/shpadoinkle-router-*/html public/router

nix-build -A Shpadoinkle-widgets.doc
cp -r result-doc/share/doc/shpadoinkle-widgets-*/html public/widgets
