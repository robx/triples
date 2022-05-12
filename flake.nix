{
  description = "Triples card game";

  # Nixpkgs / NixOS version to use.
  inputs.nixpkgs.url = "nixpkgs/nixos-21.11";
  inputs.elm18nixpkgs = {
    url = "https://nixos.org/channels/nixos-18.03/nixexprs.tar.xz";
    flake = false;
  };

  outputs = { self, nixpkgs, elm18nixpkgs }:
    let

      # to work with older version of flakes
      lastModifiedDate = self.lastModifiedDate or self.lastModified or "19700101";

      # Generate a user-friendly version number.
      version = builtins.substring 0 8 lastModifiedDate;

      # System types to support.
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
      elm18nixpkgsFor = forAllSystems (system: import elm18nixpkgs { inherit system; });

    in
    {
      # Provide some binary packages for selected system types.
      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          elm18pkgs = elm18nixpkgsFor.${system};
        in
        rec {
          triples-serve = pkgs.callPackage ./serve/default.nix {
            inherit version;
          };
          triples-client = pkgs.callPackage ./client/default.nix {
            elm18 = elm18pkgs.elmPackages.elm;
            inherit version;
          };
          triples-static = pkgs.stdenv.mkDerivation {
            name = "triples-static";
            src = self;
            buildInputs = [ triples-client ];
            phases = [ "unpackPhase" "installPhase" ];
            installPhase = ''
              mkdir -p $out/
              cp static/* $out/
              cp $buildInputs/main.js $out/ # fixme
            '';
          };
        });
    };
}
