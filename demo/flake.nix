{
  description = "Nix developer environment for the Midgard demo apps";
  nixConfig = {
    bash-prompt = "\\[\\e[0;92m\\][\\[\\e[0;92m\\]nix develop:\\[\\e[0;92m\\]\\w\\[\\e[0;92m\\]]\\[\\e[0;92m\\]$ \\[\\e[0m\\]";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nodeToolchain.url = "github:NixOS/nixpkgs/eef00dfd8a712b34af845f9350bac681b1228bd1";
    # Pin nixpkgs 26.05 for Go 1.26.5; nixpkgs 26.11 dropped Intel Mac support.
    goToolchain.url = "github:NixOS/nixpkgs/2f108b9d935fcdfa7aed10e4055dc36765379d8a";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, nodeToolchain, goToolchain, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        toolchainPkgs = import nodeToolchain { inherit system; };
        goToolchainPkgs = import goToolchain { inherit system; };
        pinnedGo = goToolchainPkgs.go_1_26;
      in
      {
        devShells.default = assert pinnedGo.version == "1.26.5"; pkgs.mkShell {
          # Native watcher chain-sync is a release-identity boundary. Refuse a
          # nixpkgs advance until the repository's exact Go pin is reviewed.
          packages = [
            toolchainPkgs.nodejs_22
            toolchainPkgs.pnpm_9
            toolchainPkgs.cargo
            toolchainPkgs.rustc
            pinnedGo
          ];
          shellHook = ''
            echo "node `${toolchainPkgs.nodejs_22}/bin/node --version`"
            echo "pnpm `${toolchainPkgs.pnpm_9}/bin/pnpm --version`"
            ${toolchainPkgs.cargo}/bin/cargo --version
            ${toolchainPkgs.rustc}/bin/rustc --version
            test "$(go version | awk '{print $3}')" = "go1.26.5"
            echo "go `go version`"
          '';
        };
      }
    );
}
