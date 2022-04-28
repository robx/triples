{ pkgs, version }:
pkgs.buildGoModule {
  pname = "triples-serve";
  inherit version;

  # In 'nix develop', we don't need a copy of the source tree
  # in the Nix store.
  src = ./.;

  # This hash locks the dependencies of this package. It is
  # necessary because of how Go requires network access to resolve
  # VCS.  See https://www.tweag.io/blog/2021-03-04-gomod2nix/ for
  # details. Normally one can build with a fake sha256 and rely on native Go
  # mechanisms to tell you what the hash should be or determine what
  # it should be "out-of-band" with other tooling (eg. gomod2nix).
  # To begin with it is recommended to set this, but one must
  # remember to bump this hash when your dependencies change.
  vendorSha256 = "sha256-gP92Prsu1ZFUmYEcf99LuPMxFoJ24Wi+m2u6W+FrIX8=";
}
