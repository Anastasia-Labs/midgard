import { defineConfig } from "tsup";

export default defineConfig({
  // Named entries keep the output flat: `resolveWorkerEntry` looks for the
  // corpus worker next to the CLI bundle, exactly as midgard-node lays out
  // its own workers.
  entry: {
    index: "src/index.ts",
    "corpus-chain-builder": "src/workers/corpus-chain-builder.ts",
  },
  format: ["esm"],
  // Self-contained bundles, one per entry: the worker is spawned by path and
  // must not depend on a sibling chunk (midgard-node builds its workers the
  // same way, one tsup invocation per entry set).
  splitting: false,
  minify: true,
  sourcemap: true,
  clean: true,
  // midgard-node is consumed from source: its exports map carries only the
  // `midgard-source` condition, so the tooling bundle inlines the operator
  // modules it drives and the operator package never grows a per-module dist
  // for anyone to resolve. Every other dependency stays external and resolves
  // from this package's own node_modules at runtime.
  noExternal: [/^midgard-node(\/|$)/],
  esbuildOptions(options) {
    options.conditions = [...(options.conditions ?? []), "midgard-source"];
    options.loader = {
      ...(options.loader ?? {}),
      ".sql": "text",
    };
  },
});
