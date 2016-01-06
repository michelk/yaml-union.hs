{ mkDerivation, base, stdenv, yaml }:
mkDerivation {
  pname = "yaml-union";
  version = "0.0.0";
  src = ./.;
  buildDepends = [ base yaml ];
  homepage = "https://github.com/michelk/yaml-union.hs";
  description = "Read multiple yaml-files and override fields";
  license = stdenv.lib.licenses.bsd3;
}
