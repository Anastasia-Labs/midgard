# The three Midgard processes, built from the pnpm workspace with the recipes
# their Dockerfiles use: a frozen-lockfile install and each package's
# `build:contrib-raw` over the production dependency closure. Docker then runs
# `pnpm deploy --prod`, whose forced legacy mode re-resolves package metadata
# that an offline store does not hold. Here the built workspace is reinstalled
# from the store with only the app's production closure, each selected
# package keeping its dev dependencies: workspace packages satisfy their peers
# (lucid, effect) through them, so a `--prod` install leaves those imports
# unresolved. As in the images, each output carries its native binaries under
# `native/` and sets only the environment the Dockerfile sets; operators name
# the binary paths in their own configuration.
{ pkgs, nodejs, pnpm, l1NodeTransport, architectureGOwner, src, pnpmDepsHash }:
let
  inherit (pkgs) lib;

  pnpmDeps = pkgs.fetchPnpmDeps {
    pname = "midgard-workspace";
    version = "0";
    inherit src pnpm;
    fetcherVersion = 3;
    hash = pnpmDepsHash;
    # Runs before the fetcher's own fixup, which then sorts the index.
    preFixup = ''
      if [ -f "$storePath/v11/index.db" ]; then
        ${nodejs}/bin/node ${./pnpm-store-side-effect-times.mjs} \
          ${pkgs.pnpm-fixup-state-db}/lib/node_modules/pnpm-fixup-state-db/node_modules/@pnpm/store.index/lib/index.js \
          "$storePath/v11"
      fi
    '';
  };

  app = { name, entry, binaries, native, env ? { } }:
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
        pnpm install --offline --frozen-lockfile --ignore-scripts \
          --filter-prod "${name}..."
        mkdir -p $out/lib $out/bin $out/native
        cp -r . $out/lib/midgard
        ${lib.concatMapStrings (binary: ''
          ln -s ${binary} $out/native/
          sha256sum ${binary} | cut -d ' ' -f 1 > $out/native/$(basename ${binary}).sha256
        '') native}
        ${lib.concatMapStrings (bin: ''
          makeWrapper ${nodejs}/bin/node $out/bin/${bin.name} \
            --add-flags $out/lib/midgard/${name}/${bin.path} \
            ${lib.concatStrings (lib.mapAttrsToList (key: value: "--set-default ${key} ${value} ") ({ NODE_ENV = "production"; } // env))}
        '') binaries}
        runHook postInstall
      '';
      meta.mainProgram = entry;
    };

  transport = "${l1NodeTransport}/bin/midgard-l1-node-transport";
in
{
  midgard-node = app {
    name = "midgard-node";
    entry = "midgard-node";
    binaries = [ { name = "midgard-node"; path = "dist/index.js"; } ];
    native = [ transport "${architectureGOwner}/bin/architecture-g-owner" ];
  };
  midgard-watcher = app {
    name = "midgard-watcher";
    entry = "midgard-watcher";
    binaries = [ { name = "midgard-watcher"; path = "dist/cli.js"; } ];
    native = [ transport ];
    # glibc must see this before Node starts; see the watcher README.
    env.MALLOC_MMAP_THRESHOLD_ = "131072";
  };
  da-committee-node = app {
    name = "da-committee-node";
    entry = "da-committee-node";
    binaries = [
      { name = "da-committee-node"; path = "dist/index.js"; }
      { name = "midgard-public-retained-da"; path = "dist/public-retained-da.js"; }
    ];
    native = [ transport ];
  };
}
