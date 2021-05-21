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

if ! command -v nix-build &> /dev/null
then
  echo "Nix is required for this to work"
fi

tmpdir=$(mktemp -d "${TMPDIR:-/tmp}"/tmp.XXXXXXXX)
git clone https://gitlab.com/platonic/shpadoinkle.git $tmpdir
$(nix-build $tmpdir -A swan --no-out-link --show-trace)/bin/swan
rm -rf $tmpdir
