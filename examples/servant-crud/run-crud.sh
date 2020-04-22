echo "Building Client..."
nix-build --arg isJS true  -A Shpadoinkle-examples -o client --fallback &
C=$!
echo "Building Server..."
nix-build --arg isJS false -A Shpadoinkle-examples -o server --fallback &
S=$!
wait $C $S
echo "Running Server with Client..."
./server/bin/servant-crud-server --assets ./client/bin/servant-crud-client.jsexe
