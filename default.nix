{ stdenv, haskellPackages }:
let
  ghc = haskellPackages.ghcWithPackages (p: with p; [
    texmath
  ]);
in
stdenv.mkDerivation {
  name = "tex2typst";
  src = ./.;
  buildInputs = [ ghc ];
  buildPhase = ''
    ghc ./Main.hs -o ./Main -Wall -Werror
  '';
  installPhase = ''
    install -Dm555 ./Main $out/bin/tex2typst
  '';
}
