# run from repo root!
./scripts/build-all.sh -A Shpadoinkle-examples
mkdir clientw &&
trap 'rm -rf ./clientw' SIGINT SIGTERM &&
cp -r client/. clientw &&
chmod -R +w clientw &&
cp clientw/bin/servant-crud-client.jsexe/{all.js,all.min.js} &&
echo running
./server/bin/servant-crud-server -p 8000 -a ./clientw/bin/servant-crud-client.jsexe/
