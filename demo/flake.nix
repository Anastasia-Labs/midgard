{
  description = "Nix developer environment for the Midgard demo apps";
  nixConfig = {
    bash-prompt = "\\[\\e[0;92m\\][\\[\\e[0;92m\\]nix develop:\\[\\e[0;92m\\]\\w\\[\\e[0;92m\\]]\\[\\e[0;92m\\]$ \\[\\e[0m\\]";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nodeToolchain.url = "github:NixOS/nixpkgs/eef00dfd8a712b34af845f9350bac681b1228bd1";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, nodeToolchain, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        toolchainPkgs = import nodeToolchain { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            toolchainPkgs.nodejs_22
            toolchainPkgs.pnpm_9
          ];
          shellHook = ''
            echo "node `${toolchainPkgs.nodejs_22}/bin/node --version`"
            echo "pnpm `${toolchainPkgs.pnpm_9}/bin/pnpm --version`"
          '';
        };
      }
    );
}
