# Isreal Swan

Snowman as a service

## Installation

```nix
imports = [
 "${(builtins.fetchGit {
    url    = https://gitlab.com/fresheyeball/Shpadoinkle.git;
    rev    = "c7807d5bcd31563a1fbc8df24832ae10c34a3268";
    ref    = "master";
  })}/isreal/module.nix"
];

services.shpadoinkle-isreal.enable = true;
```

This will setup a web server where you can post code. You may also customize the port and workspace location.

## Usage

### Compile and View

We will run a build with token `foo`.

```bash
echo "module Main where\nmain = putStrLn \"Howdy\"" > Test.hs
curl -X POST -H "Content-Type:application/octet-stream" --data-binary @Test.hs http://localhost:8080/compile/foo
```

If there was an error it will be returned. Otherwise you can view the compiled code at

`http://localhost:8080/serve/foo/index.html`

### Clean up

Clean up one token

```bash
curl -X DELETE http://localhost:8080/clean/foo
```

Clean up everything

```bash
curl -X DELETE http://localhost:8080/clean-all
```