{ mkDerivation, base, containers, lens, ListZipper, monad-extras
, mtl, sdl2, stdenv
}:
mkDerivation {
  pname = "tile-rider";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers lens ListZipper monad-extras mtl sdl2
  ];
  homepage = "https://github.com/ublubu/tile-rider";
  description = "a simple puzzle game in Haskell with SDL2";
  license = stdenv.lib.licenses.unfree;
}
