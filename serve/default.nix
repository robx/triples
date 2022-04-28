{ pkgs, version }:
pkgs.buildGoModule {
  pname = "triples-serve";
  inherit version;
  src = ./.;
  vendorSha256 = "sha256-gP92Prsu1ZFUmYEcf99LuPMxFoJ24Wi+m2u6W+FrIX8=";
}
