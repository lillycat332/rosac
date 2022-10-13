{ mkDerivation
, pkgs
, base
, lib
, megaparsec
, optparse-applicative
, parser-combinators
, string-conversions
, text
, ...
}:
mkDerivation {
  pname = "rosalia";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    megaparsec
    parser-combinators
    string-conversions
    text
  ];
  # buildDepends = [
  #   pkgs.llvmPackages_13.llvm
  # ];
  executableHaskellDepends = [ base optparse-applicative text ];
  homepage = "https://github.com/lillycat332/rosac";
  description = "Rosalia Compiler";
  license = lib.licenses.bsd2;
  mainProgram = "rosac";
}
