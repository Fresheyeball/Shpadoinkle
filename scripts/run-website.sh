# run from repo root!
nix-build website/ --arg isJS true --arg optimize false -o client &&
( cd client/ && python3 -m http.server )
