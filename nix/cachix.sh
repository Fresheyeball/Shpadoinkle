# We want cache as a singular step. Because we do not wish to cache parts of the
# project if the set is broken.

nix-build --arg isJS true                              | cachix push shpadoinkle &
nix-build --arg isJS false                             | cachix push shpadoinkle &
nix-build --arg isJS true  -A Shpadoinkle-examples.env | cachix push shpadoinkle &
nix-build --arg isJS false -A Shpadoinkle-examples.env | cachix push shpadoinkle &
nix-build --arg isJS true  -A Shpadoinkle-tests.env    | cachix push shpadoinkle &
nix-build --arg isJS false -A Shpadoinkle-tests.env    | cachix push shpadoinkle &

nix-build --argstr system x86_64-darwin --arg isJS true                              | cachix push shpadoinkle &
nix-build --argstr system x86_64-darwin --arg isJS false                             | cachix push shpadoinkle &
nix-build --argstr system x86_64-darwin --arg isJS true  -A Shpadoinkle-examples.env | cachix push shpadoinkle &
nix-build --argstr system x86_64-darwin --arg isJS false -A Shpadoinkle-examples.env | cachix push shpadoinkle &
nix-build --argstr system x86_64-darwin --arg isJS true  -A Shpadoinkle-tests.env    | cachix push shpadoinkle &
nix-build --argstr system x86_64-darwin --arg isJS false -A Shpadoinkle-tests.env    | cachix push shpadoinkle &

nix-build ./snowman/template --arg isJS false --arg asShell true                       --no-out-link | cachix push shpadoinkle &
nix-build ./snowman/template --arg isJS true  --arg asShell true                       --no-out-link | cachix push shpadoinkle &
nix-build ./snowman/template --arg isJS false --arg asShell true --arg withHoogle true --no-out-link | cachix push shpadoinkle &

nix-build ./snowman/template --argstr system x86_64-darwin --arg isJS false --arg asShell true                       --no-out-link | cachix push shpadoinkle &
nix-build ./snowman/template --argstr system x86_64-darwin --arg isJS true  --arg asShell true                       --no-out-link | cachix push shpadoinkle &
nix-build ./snowman/template --argstr system x86_64-darwin --arg isJS false --arg asShell true --arg withHoogle true --no-out-link | cachix push shpadoinkle &
wait
echo "Cachix up to date"
