echo "Checking for nixops mutex"
until ! [[ -e /tmp/nixops-mutex ]]; do
  echo "Mutex found, waiting for release"
  wait 1
done
echo "Mutex is open, locking"
touch /tmp/nixops-mutex
echo "Destroying any stray deployments"
nixops destroy --all && nixops delete --all
echo "Deploying Isreal Swan"
which nixops; nixops --version
nixops create -d colorado isreal/deploy.nix
hash=$(cat ./nix/chan.nix | tr -d '"')
nixops deploy -d colorado --allow-reboot --fallback -I nixpkgs="https://github.com/NixOS/nixpkgs/archive/$hash.tar.gz"
res=$?
echo "Destroying deployment"
nixops destroy --all && nixops delete --all
echo "Releasing lock"
rm /tmp/nixops-mutex
exit $res
