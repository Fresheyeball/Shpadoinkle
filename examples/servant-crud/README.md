# See the example

```bash
nix-build --arg isJS true  -A Shpadoinkle-examples -o client
nix-build --arg isJS false -A Shpadoinkle-examples -o server
./server/bin/server-crud-server --assets ./client/bin/servant-crud-client.jsexe
```
