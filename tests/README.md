# Run E2E tests

```bash
nix-build -A Shpadoinkle-examples
cd tests
nix-shell
cabal v1-run
```
