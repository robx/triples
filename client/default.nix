{ pkgs ? import <nixpkgs> {},
  foo ? "hello",
  old ? import (builtins.fetchTarball "https://nixos.org/channels/nixos-18.03/nixexprs.tar.xz") {}
}:

pkgs.stdenv.mkDerivation rec {
  name    = "triples-client-${version}";
  version = "xxx";

  src = ./.;

  buildInputs = [ old.elmPackages.elm ];

  buildPhase = ''
    elm-make src/Main.elm --yes --output main.js
  '';

  installPhase = ''
    mkdir -p $out
    cp main.js $out/main.js
  '';
}
