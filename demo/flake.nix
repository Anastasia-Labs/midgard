{
  description = "Nix developer environment for the Midgard demo apps";
  nixConfig = {
    bash-prompt = "\\[\\e[0;92m\\][\\[\\e[0;92m\\]nix develop:\\[\\e[0;92m\\]\\w\\[\\e[0;92m\\]]\\[\\e[0;92m\\]$ \\[\\e[0m\\]";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nodeToolchain.url = "github:NixOS/nixpkgs/eef00dfd8a712b34af845f9350bac681b1228bd1";
    goToolchain.url = "github:NixOS/nixpkgs/466bc97ad91ffa340bfd137eff2e56452119b42d";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, nodeToolchain, goToolchain, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        toolchainPkgs = import nodeToolchain { inherit system; };
        goToolchainPkgs = import goToolchain { inherit system; };
        pinnedGo = goToolchainPkgs.go_1_25;
      in
      {
        devShells.default = assert pinnedGo.version == "1.25.7"; pkgs.mkShell {
          # Native watcher chain-sync is a release-identity boundary. Refuse a
          # nixpkgs advance until the repository's exact Go pin is reviewed.
          packages = [
            toolchainPkgs.nodejs_22
            toolchainPkgs.pnpm_9
            pinnedGo
          ];
          shellHook = ''
            echo "node `${toolchainPkgs.nodejs_22}/bin/node --version`"
            echo "pnpm `${toolchainPkgs.pnpm_9}/bin/pnpm --version`"
            test "$(go version | awk '{print $3}')" = "go1.25.7"
            echo "go `go version`"
          '';
        };
      }
    );
}
