{ chan ? (import ../nix/chan.nix)
}:


#  This file is part of Shpadoinkle Website.
#
#  Shpadoinkle Website is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  Shpadoinkle Website is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with Shpadoinkle Website.  If not, see <https://www.gnu.org/licenses/>.


let
  brand = import ./brand.nix {};
  pkgs  = import ../nix/pkgs.nix { inherit chan; };
in pkgs.writeScriptBin "generate-assets.sh" ''
  rm -f ./website/favicon.html ./website/assets/landing_logo.svg
  cp ${brand.icons.faviconHTML} ./website/favicon.html
  cp ${brand.logo.full.svg { color = "bright_yellow"; }} ./website/assets/landing_logo.svg
''

