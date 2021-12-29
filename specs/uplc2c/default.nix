{ chan ? "7e9b0dff974c89e070da1ad85713ff3c20b0ca97"
, pkgs ? import (builtins.fetchTarball { url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz"; }) {}
}:
with pkgs;
let deps = [ (texlive.combine { inherit (texlive) scheme-basic amsmath graphics hyperref; }) ];
in
  stdenv.mkDerivation {
    name = "uplc2c-architecture";
    src = ./src;
    buildInputs = deps;
    buildPhase = ''
      mkdir -p $out
      HOME=./. pdflatex uplc2c-architecture.tex
      HOME=./. pdflatex uplc2c-architecture.tex
      cp uplc2c-architecture.pdf "$out/UPLC2C Architecture.pdf"
    '';
    installPhase = ''
      echo done
    '';
  }
