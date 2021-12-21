{ chan ? "7e9b0dff974c89e070da1ad85713ff3c20b0ca97"
, pkgs ? import (builtins.fetchTarball { url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz"; }) {}
}:
with pkgs;
let deps = [ (texlive.combine { inherit (texlive) scheme-basic amsmath graphics hyperref; }) ];
in
  stdenv.mkDerivation {
    name = "ardana-rollups-on-chain-pricing";
    src = ./src;
    buildInputs = deps;
    buildPhase = ''
      mkdir -p $out
      HOME=./. pdflatex on-chain-pricing.tex
      HOME=./. pdflatex on-chain-pricing.tex
      cp on-chain-pricing.pdf "$out/Ardana Rollups On-Chain Tx Pricing.pdf"
    '';
    installPhase = ''
      echo done
    '';
  }
