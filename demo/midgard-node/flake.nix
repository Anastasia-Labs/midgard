{
  description = "A Nix-flake-based Typescript development environment";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nodeToolchain.url = "github:NixOS/nixpkgs/eef00dfd8a712b34af845f9350bac681b1228bd1";
  };

  outputs = { self, nixpkgs, nodeToolchain, }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
        toolchainPkgs = import nodeToolchain { inherit system; };
      });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs, toolchainPkgs }: {
        default = pkgs.mkShell {
          packages = [
            pkgs.bun
            toolchainPkgs.pnpm_9
            toolchainPkgs.nodejs_22
          ];
        };
      });
    };
}
