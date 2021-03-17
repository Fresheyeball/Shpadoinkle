{ chan ? (import ../nix/chan.nix) }:
let pkgs = import ../nix/pkgs.nix { inherit chan; };
in pkgs.writeShellScriptBin "swan" ''
set -eu

echo
echo "let’s build a snowman!"
echo
echo "what should we call your snowman (e.g. tom, george, beowulf)?"

read -r name

if [[ -z "$name" ]]; then
  name="beowulf"
fi

echo 'your snowman will be named "'"$name"'"'
echo "where should $name live (e.g.  ./$name)?"
echo

read -r path

if [[ -z "$path" ]]; then
  path="./$name"
fi

mkdir -p "$path"
cp -r ${./template}/* "$path" || exit 1
cp ${./figlet} "$path/figlet"
cp ${./intro} "$path/intro"

echo "naming snowman $name …"
echo

chmod -R u+w "$path"
shopt -s extglob
for i in $(find "$path" -type f ! -name README.md); do
  ${pkgs.gnused}/bin/sed -i "$i" -e "s/snowman/$name/g" > .snmtmp
  ${pkgs.gnused}/bin/sed -i "$i" -e "s/CHAN/${chan}/g" > .snmtmp
  ${pkgs.gnused}/bin/sed -i "$i" -e "s/REV/${pkgs.lib.commitIdFromGitRepo ../.git}/g" > .snmtmp
  rm .snmtmp
done

mv "$path/snowman.cabal" "$path/$name.cabal"

chown -R $USER "$path"

cat ${./success}

cd "$path"

''
