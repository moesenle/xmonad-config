{ mkDerivation, stdenv, xmonad, xmonad-contrib, xmonad-extras
}:
mkDerivation {
  pname = "xmonad-dev";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    xmonad xmonad-contrib xmonad-extras
  ];
  description = "Utilities for HTTP Communication";
  license = stdenv.lib.licenses.unfree;
}
