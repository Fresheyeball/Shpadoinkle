until ! [[ -e /tmp/nixops-mutex ]]; do
  wait 1
done
touch /tmp/nixops-mutex
nixops destroy --all && nixops delete --all
which nixops; nixops --version
nixops create -d tmp isreal/deploy.nix
hash=$(cat ./nix/chan.nix | tr -d '"')
nixops deploy -d tmp --allow-reboot --dry-run --fallback -I nixpkgs="https://github.com/NixOS/nixpkgs/archive/$hash.tar.gz"
res=$?
nixops destroy --all && nixops delete --all
rm /tmp/nixops-mutex
exit $res

