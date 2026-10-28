{
  # One of the two ways to get everything Midgard needs; the other is the
  # Dockerfiles and compose files. Both take their versions from
  # toolchain.json, pnpm from package.json's packageManager and aiken from
  # flake.lock. This flake builds natively and makes no images.
  description = "Midgard's toolchain, runtime services and native builds, with Nix";
  nixConfig = {
    bash-prompt = "\\[\\e[0;92m\\][\\[\\e[0;92m\\]nix develop:\\[\\e[0;92m\\]\\w\\[\\e[0;92m\\]]\\[\\e[0;92m\\]$ \\[\\e[0m\\]";
  };
  inputs = {
    # Pinned by flake.lock but unused by the outputs below.
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # nixpkgs 26.05 with Node 22.22.2; every package not pinned elsewhere
    # comes from here.
    nodeToolchain.url = "github:NixOS/nixpkgs/eef00dfd8a712b34af845f9350bac681b1228bd1";
    # Pin nixpkgs 26.05 for Go 1.26.5; nixpkgs 26.11 dropped Intel Mac support.
    goToolchain.url = "github:NixOS/nixpkgs/2f108b9d935fcdfa7aed10e4055dc36765379d8a";
    flake-utils.url = "github:numtide/flake-utils";
    # The one Aiken compiler this repository compiles, formats and tests with.
    # flake.lock pins its exact commit; everything else reads the pin from there.
    aiken.url = "github:colll78/aiken/midgard-c08fcece";
    aiken.inputs.flake-utils.follows = "flake-utils";
  };
  outputs = { self, nixpkgs, nodeToolchain, goToolchain, flake-utils, aiken }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        versions = builtins.fromJSON (builtins.readFile ./toolchain.json);
        # Rust comes from the rust-overlay aiken's lock already pins, so the
        # exact toolchain.json release needs no input of its own.
        pkgs = import nodeToolchain {
          inherit system;
          overlays = [ (import aiken.inputs.rust-overlay) ];
        };
        inherit (pkgs) lib;
        goToolchainPkgs = import goToolchain { inherit system; };

        pinned = name: package: version:
          assert lib.assertMsg (package.version == version)
            "${name} is ${package.version}; toolchain.json pins ${version}";
          package;
        nodejs = pinned "node" pkgs.nodejs_22 versions.node;
        go = pinned "go" goToolchainPkgs.go_1_26 versions.go;
        rust = pkgs.rust-bin.stable.${versions.rust}.minimal;
        # Postgres keeps its on-disk format across a major version, so each
        # route takes its own platform's latest patch release.
        postgresql = assert lib.assertMsg
          (lib.versions.major pkgs.postgresql_15.version == versions.postgres)
          "postgresql is ${pkgs.postgresql_15.version}; toolchain.json pins ${versions.postgres}";
          pkgs.postgresql_15;
        aikenPackage = aiken.packages.${system}.aiken;

        # The exact pnpm package.json's packageManager names (corepack's
        # `pnpm@<version>+sha512.<hex>` is the npm tarball's digest).
        packageManager = lib.match "pnpm@([^+]+)\\+sha512\\.([0-9a-f]+)"
          (builtins.fromJSON (builtins.readFile ./package.json)).packageManager;
        pnpm = (pkgs.pnpm_11.override { inherit nodejs; }).overrideAttrs (previous: {
          version = builtins.elemAt packageManager 0;
          src = pkgs.fetchurl {
            url = "https://registry.npmjs.org/pnpm/-/pnpm-${builtins.elemAt packageManager 0}.tgz";
            sha512 = builtins.elemAt packageManager 1;
          };
        });
        # Contributors run pnpm through scripts/pnpm.mjs, as CI does.
        pnpmWrapper = pkgs.writeShellScriptBin "pnpm" ''
          exec ${nodejs}/bin/node ${../scripts/pnpm.mjs} "$@"
        '';

        # The node transport sidecar, built as the Dockerfiles build it.
        l1NodeTransport = (pkgs.buildGoModule.override { inherit go; }) {
          pname = "midgard-l1-node-transport";
          version = "0";
          src = ./l1-node-transport/native;
          # The digest of the module downloads go.sum names; a go.sum change
          # prints the new one.
          vendorHash = "sha256-4oS0CTBXH4mtRil2hRY3LLZw7oqHXhDYdfWO78DA+cc=";
          subPackages = [ "." ];
          env.CGO_ENABLED = "0";
          ldflags = [ "-s" "-w" ];
        };

        # The Architecture G MPF owner the node runs as a child process.
        architectureGOwner = (pkgs.makeRustPlatform { cargo = rust; rustc = rust; }).buildRustPackage {
          pname = "architecture-g-owner";
          version = "0";
          src = ./midgard-node/native/mpf-event-flat-wasm;
          cargoLock.lockFile = ./midgard-node/native/mpf-event-flat-wasm/Cargo.lock;
          cargoBuildFlags = [ "--bin" "architecture-g-owner" ];
          doCheck = false;
        };

        runtimeServices = import ./nix/runtime-services.nix { inherit pkgs system versions; };

        workspace = import ./nix/workspace.nix {
          inherit pkgs nodejs pnpm l1NodeTransport architectureGOwner;
          # The digest of the packages pnpm-lock.yaml names; a lockfile change
          # prints the new one.
          pnpmDepsHash = "sha256-KlYBhbimwEgGEBnDQFvHRlCtnh/nkebJTMSJUrwx5r8=";
          # Flakes copy only tracked files, so dist/ and node_modules/ stay out.
          src = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.difference ./. (lib.fileset.unions [
              ./flake.nix
              ./flake.lock
              ./nix
            ]);
          };
        };
      in
      {
        packages = runtimeServices // workspace // {
          aiken = aikenPackage;
          inherit postgresql;
          midgard-l1-node-transport = l1NodeTransport;
          architecture-g-owner = architectureGOwner;
        };
        devShells.default = pkgs.mkShell {
          packages = [
            nodejs
            pnpmWrapper
            rust
            go
            aikenPackage
            postgresql
          ] ++ builtins.attrValues runtimeServices;
          shellHook = ''
            echo "node `${nodejs}/bin/node --version`"
            echo "pnpm `${pnpmWrapper}/bin/pnpm --version`"
            ${rust}/bin/cargo --version
            ${go}/bin/go version
            ${aikenPackage}/bin/aiken --version
          '';
        };
      }
    );
}
