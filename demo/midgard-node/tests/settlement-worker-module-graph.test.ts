/**
 * The settlement worker runs under a fixed old-generation limit
 * (SETTLEMENT_WORKER_HEAP_MB), so everything its module graph loads at startup
 * is paid for out of that heap. The fault-proofs root barrel once re-exported a
 * lint-only source scanner that imports the TypeScript compiler, and the
 * settlement worker reached it through
 * commands/event-settlement-proof.ts -> mpf/event-window.* -> the barrel.
 *
 * esbuild's metafile lists every file a bundle parses, including the ones
 * `export *` pulls in and tree-shaking later drops, so it is the whole static
 * module graph each entry loads.
 */
import { join } from "node:path";

import { build } from "esbuild";
import { describe, expect, it } from "vitest";

const packageRoot = join(import.meta.dirname, "..");
const faultProofsBarrel = join(
  packageRoot,
  "../midgard-fault-proofs/src/index.ts",
);

const moduleGraph = async (entry: string) => {
  const { metafile } = await build({
    entryPoints: [entry],
    absWorkingDir: packageRoot,
    bundle: true,
    write: false,
    metafile: true,
    outdir: join(packageRoot, ".module-graph"),
    platform: "node",
    format: "esm",
    target: "node22",
    logLevel: "silent",
    conditions: ["midgard-source"],
    loader: { ".sql": "text" },
  });
  return Object.keys(metafile.inputs).map((input) => join(packageRoot, input));
};

const typescriptCompiler = (graph: readonly string[]) =>
  graph.filter((path) => /\/node_modules\/typescript\//u.test(path));

describe("runtime module graphs do not load the TypeScript compiler", () => {
  it("the fault-proofs root barrel", async () => {
    const graph = await moduleGraph(faultProofsBarrel);
    expect(graph).toContain(faultProofsBarrel);
    expect(typescriptCompiler(graph)).toEqual([]);
  }, 60_000);

  it("the settlement worker, which reaches that barrel", async () => {
    const graph = await moduleGraph(
      join(packageRoot, "src/workers/settlement.ts"),
    );
    // The source condition stands in for the fault-proofs dist the shipped
    // worker imports; both are built from this barrel.
    expect(graph).toContain(faultProofsBarrel);
    expect(typescriptCompiler(graph)).toEqual([]);
  }, 60_000);
});
