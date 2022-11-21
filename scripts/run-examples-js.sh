# run from repo root!
nix-build -A Shpadoinkle-examples --arg isJS true -o client --arg optimize false &&
( cd client/bin && python3 -m http.server )
