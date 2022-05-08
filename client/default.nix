{ pkgs ? import <nixpkgs> {},
  version ? "dev",
  elm18 ? (
    import (builtins.fetchTarball "https://nixos.org/channels/nixos-18.03/nixexprs.tar.xz") {}
  ).elmPackages.elm
}:

pkgs.stdenv.mkDerivation {
  name    = "triples-client-${version}";

  src = ./.;

  buildInputs = [ elm18 ];

  buildPhase = ''
    elm-make src/Main.elm --yes --output main.js
  '';

  installPhase = ''
    mkdir -p $out/share
    cp index.html main.js $out/share/
  '';
}
