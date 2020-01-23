echo "Building Client..."
nix-build --arg isJS true  -A Shpadoinkle-examples -o client --fallback
echo "Building Server..."
nix-build --arg isJS false -A Shpadoinkle-examples -o server --fallback
echo "Running Server with Client..."
./server/bin/servant-crud-server --assets ./client/bin/servant-crud-client.jsexe
