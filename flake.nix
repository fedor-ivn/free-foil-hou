{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in with pkgs; {
        devShells.default = mkShell {
          buildInputs = pkgs.lib.optional stdenv.isDarwin (
            [darwin.Libsystem]
          );
          packages = [
            haskell.compiler.ghc982
            stack
            ghcid
            (haskell-language-server.override {
              supportedGhcVersions = [ "98" ];
            })
            haskellPackages.fourmolu
            haskellPackages.BNFC
          ];
        };
      }
    );
}
