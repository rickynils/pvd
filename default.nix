{ haskellPackages }:

with haskellPackages;

cabal.mkDerivation (self: {
  pname = "pvd";
  version = "1.1.1";
  isLibrary = false;
  isExecutable = true;
  src = ./.;
  buildDepends = [
    CodecImageDevIL
    stm
    mtl
    network
    X11
  ];
  meta = {
    homepage = https://github.com/rickynils/pvd;
    description = "Photo Viewer Daemin";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
    maintainers = [ self.stdenv.lib.maintainers.rickynils ];
  };
})
