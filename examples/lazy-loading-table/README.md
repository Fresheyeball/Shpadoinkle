# Lazy loading table example

To run this example:

```
[shpadoinkle]$ nix-shell
[nix-shell]$ cabal repl examples:lazy-loading-table-client
*Types> import Main
*Types Main> main
```

And in another shell:

```
[shpadoinkle]$ nix-shell
[nix-shell]$ cabal repl examples:lazy-loading-table-server
*Types> import Main
*Types Main> main
```

And in a browser with the CORS restriction disabled navigate to http://localhost:8080/. For example, if you have Chromium on Linux, you can run it like this to disable the CORS restriction:

chromium --disable-web-security --user-data-dir=~/.config/chromium-nosec
