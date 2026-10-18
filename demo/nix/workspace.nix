# The three Midgard processes, built from the pnpm workspace with the recipes
# their Dockerfiles use: a frozen-lockfile install and each package's
# `build:contrib-raw` over the production dependency closure. Docker then runs
# `pnpm deploy`, whose forced legacy mode re-resolves package metadata that an
# offline store does not hold, so here the built workspace is reinstalled
# from the store with only the app's production closure and installed whole.
# Each output wraps its entry points with the pinned Node and defaults the
# native binary paths to their Nix builds.
{ pkgs, nodejs, pnpm, l1NodeTransport, architectureGOwner, src, pnpmDepsHash }:
let
  inherit (pkgs) lib;

  pnpmDeps = pkgs.fetchPnpmDeps {
    pname = "midgard-workspace";
    version = "0";
    inherit src pnpm;
    fetcherVersion = 3;
    hash = pnpmDepsHash;
  };

  app = { name, entry, binaries, env }:
    pkgs.stdenv.mkDerivation {
      pname = name;
      version = "0";
      inherit src pnpmDeps;
      nativeBuildInputs = [
        nodejs
        pnpm
        pkgs.pnpmConfigHook
        pkgs.makeWrapper
      ] ++ lib.optional pkgs.stdenv.isLinux pkgs.autoPatchelfHook;
      # The release-age policy looks publish times up online; the lockfile's
      # entries passed it when they were added, and the sandbox is offline.
      prePnpmInstall = ''
        pnpm config set minimum-release-age 0
      '';
      # Prebuilt native addons (classic-level, msgpackr-extract) link libstdc++.
      buildInputs = lib.optional pkgs.stdenv.isLinux pkgs.stdenv.cc.cc.lib;
      # Their Alpine prebuilds ship alongside and are never loaded on glibc.
      autoPatchelfIgnoreMissingDeps = [ "libc.musl-*.so.1" ];
      buildPhase = ''
        runHook preBuild
        pnpm --filter-prod "${name}..." --workspace-concurrency=1 run build:contrib-raw
        runHook postBuild
      '';
      installPhase = ''
        runHook preInstall
        rm -rf node_modules */node_modules
        pnpm install --offline --frozen-lockfile --prod --ignore-scripts \
          --filter "${name}..."
        mkdir -p $out/lib $out/bin
        cp -r . $out/lib/midgard
        ${lib.concatMapStrings (bin: ''
          makeWrapper ${nodejs}/bin/node $out/bin/${bin.name} \
            --add-flags $out/lib/midgard/${name}/${bin.path} \
            ${lib.concatStrings (lib.mapAttrsToList (key: value: "--set-default ${key} ${value} ") env)}
        '') binaries}
        runHook postInstall
      '';
      meta.mainProgram = entry;
    };

  transport = { L1_NODE_TRANSPORT_BINARY_PATH = "${l1NodeTransport}/bin/midgard-l1-node-transport"; };
in
{
  midgard-node = app {
    name = "midgard-node";
    entry = "midgard-node";
    binaries = [ { name = "midgard-node"; path = "dist/index.js"; } ];
    env = transport // {
      MPF_NATIVE_OWNER_BINARY_PATH = "${architectureGOwner}/bin/architecture-g-owner";
    };
  };
  midgard-watcher = app {
    name = "midgard-watcher";
    entry = "midgard-watcher";
    binaries = [ { name = "midgard-watcher"; path = "dist/cli.js"; } ];
    env = transport;
  };
  da-committee-node = app {
    name = "da-committee-node";
    entry = "da-committee-node";
    binaries = [
      { name = "da-committee-node"; path = "dist/index.js"; }
      { name = "midgard-public-retained-da"; path = "dist/public-retained-da.js"; }
    ];
    env = transport;
  };
}
