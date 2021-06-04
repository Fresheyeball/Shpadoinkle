# We want cache as a singular step. Because we do not wish to cache parts of the
# project if the set is broken.

set -eu

cmds=(
  "nix-build --arg isJS true"
  "nix-build --arg isJS false"
  "nix-build --arg isJS true  -A Shpadoinkle-examples.env"
  "nix-build --arg isJS false -A Shpadoinkle-examples.env"
  "nix-build --argstr system x86_64-darwin --arg isJS true"
  "nix-build --argstr system x86_64-darwin --arg isJS false"
  "nix-build --argstr system x86_64-darwin --arg isJS true  -A Shpadoinkle-examples.env"
  "nix-build --argstr system x86_64-darwin --arg isJS false -A Shpadoinkle-examples.env"
  "nix-build ./snowman/template --arg isJS false --arg asShell true                       --no-out-link"
  "nix-build ./snowman/template --arg isJS true  --arg asShell true                       --no-out-link"
  "nix-build ./snowman/template --arg isJS false --arg asShell true --arg withHoogle true --no-out-link"
  "nix-build ./snowman/template --argstr system x86_64-darwin --arg isJS false --arg asShell true                       --no-out-link"
  "nix-build ./snowman/template --argstr system x86_64-darwin --arg isJS true  --arg asShell true                       --no-out-link"
  "nix-build ./snowman/template --argstr system x86_64-darwin --arg isJS false --arg asShell true --arg withHoogle true --no-out-link"
)

pids=()

for cmd in "${cmds[@]}"; do
  $cmd | cachix push shpadoinkle &
  pids+=($!)
done

for pid in "${pids[@]}"; do
  wait "$pid"
done

echo "Cachix up to date"
