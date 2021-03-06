{ mkDerivation, base, pure-core, pure-default, stdenv }:
mkDerivation {
  pname = "excelsior";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-core pure-default ];
  license = stdenv.lib.licenses.bsd3;
}
