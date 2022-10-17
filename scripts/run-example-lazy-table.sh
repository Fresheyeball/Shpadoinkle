# run from repo root!
./scripts/build-all.sh -A Shpadoinkle-examples
( cd client/bin/lazy-loading-table-client.jsexe && python3 -m http.server ) &
./server/bin/lazy-loading-table-server
