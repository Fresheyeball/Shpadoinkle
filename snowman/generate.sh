#!/usr/bin/env bash



#  This file is part of Shpadoinkle Snowman.
#
#  Shpadoinkle Snowman is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  Shpadoinkle Snowman is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with Shpadoinkle Snowman.  If not, see <https://www.gnu.org/licenses/>.


set -eu

wd=$(pwd)

tmpdir=$(mktemp -d "${TMPDIR:-/tmp}"/tmp.XXXXXXXX)/Shpadoinkle
git clone https://gitlab.com/platonic/shpadoinkle.git $tmpdir
cd $tmpdir
rev=$(git rev-parse HEAD)
echo $rev

cd $wd
chan=$(cat $tmpdir/nix/chan.nix | sed -e s/\"//g)
echo $chan

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
cp -r $tmpdir/snowman/template/* "$path" || exit 1
cp $tmpdir/snowman/figlet "$path/figlet"
cp $tmpdir/snowman/intro "$path/intro"

echo "naming snowman $name …"
echo

chmod -R u+w "$path"
shopt -s extglob
for i in $(find "$path" -type f ! -name README.md); do
  sed -i "$i" -e "s/snowman/$name/g" > .snmtmp
  sed -i "$i" -e "s/CHAN/$chan/g" > .snmtmp
  sed -i "$i" -e "s/REV/$rev/g" > .snmtmp
  rm .snmtmp
done

mv "$path/snowman.cabal" "$path/$name.cabal"

chown -R $USER "$path"

cat $tmpdir/snowman/success

cd "$path"

rm -rf $tmpdir
