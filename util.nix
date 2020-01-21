{ compiler, isJS }:
{ compilerjs = if isJS then "ghcjs${builtins.substring 3 2 compiler}" else compiler;
}
