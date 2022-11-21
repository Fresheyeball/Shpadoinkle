# run from repo root!
echo building
[ -f server ] && rm server
[ -f client ] && rm client
nix-build $@ --arg isJS false -o server --arg optimize false & S=$!
nix-build $@ --arg isJS true -o client --arg optimize false & C=$!

abort() { kill $S $C 2>/dev/null && exit 1; }
wait -n $S $C || abort
wait -n $S $C || abort
echo built
