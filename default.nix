{ mkDerivation, base, stdenv, yaml }:
mkDerivation {
  pname = "yaml-overrides";
  version = "0.0.0";
  src = ./.;
  buildDepends = [ base yaml ];
  homepage = "https://github.com/michelk/yaml-overrides.hs";
  description = "Read multiple yaml-files and override fields";
  license = stdenv.lib.licenses.bsd3;
}
