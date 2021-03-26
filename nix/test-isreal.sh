nixops destroy --all && nixops delete --all
which nixops; nixops --version
nixops create -d tmp isreal/deploy.nix
hash=$(cat ./nix/chan.nix | tr -d '"')
nixops deploy -d tmp --allow-reboot --fallback -I nixpkgs="https://github.com/NixOS/nixpkgs/archive/$hash.tar.gz"
nixops destroy --all && nixops delete --all
