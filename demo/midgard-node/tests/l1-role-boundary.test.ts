/**
 * Roles read L1 only through their follower (option E): no role process has
 * a Kupmios, Blockfrost or Ogmios client in its module graph. The role
 * entries are `listen`'s runtime, the worker threads it starts, the DA
 * committee node and the watcher; their whole graph (dynamic imports
 * included) holds none of the tool-side L1 modules and constructs no
 * external provider. The CLI binary (`src/index.ts`) also carries the tools,
 * so only its static graph is held to this: a tool reaches its external
 * adapter by a dynamic import, which a `listen` process never runs.
 *
 * esbuild's metafile lists every file a bundle parses, with each import's
 * kind, so it is the module graph each entry loads. The two fixtures prove
 * the check can fail (one statically reaches the Kupmios adapter, one builds
 * a Blockfrost client itself).
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";

import { build } from "esbuild";
import { describe, expect, it } from "vitest";

const packageRoot = join(import.meta.dirname, "..");
const demo = join(packageRoot, "..");

/** Tool-side L1 modules no role graph may hold. */
const TOOL_L1_MODULES = [
  /\/midgard-node\/src\/l1-external\//u,
  /\/midgard-core\/src\/native-ledger-kupmios\.ts$/u,
  /\/midgard-core\/src\/ogmios-slot-query\.ts$/u,
  /\/midgard-node\/src\/commands\/(?:l1-command-access|l1-tool-adapter|cli-runtime)\.ts$/u,
];

/** Modules the CLI binary's static graph may not hold (tools load them
 * dynamically). */
const EXTERNAL_L1_MODULES = TOOL_L1_MODULES.slice(0, 3);

const PROVIDER_CONSTRUCTION =
  /new\s+(?:[A-Za-z_$][\w$]*\.)?(?:Kupmios|Blockfrost|Koios|Maestro)\s*\(/u;

/**
 * The one pinned exception: the module holding only the fault-proofs prover
 * CLI's submit provider (owner ruling: the prover CLI keeps Kupmios and
 * Blockfrost). Roles reach it through the fault-proofs runtime barrel, but
 * only the prover CLI's submit commands call it.
 */
const PINNED_PROVIDER_CONSTRUCTION = [
  join(demo, "midgard-fault-proofs/src/runtime.make-lucid-for-submit.ts"),
];

type Graph = Readonly<{ all: readonly string[]; static: readonly string[] }>;

const moduleGraph = async (entry: string): Promise<Graph> => {
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
  const inputs = metafile.inputs;
  const start = Object.keys(inputs).find(
    (input) => join(packageRoot, input) === entry,
  );
  if (start === undefined) throw new Error(`${entry} is not in its own graph`);
  const reached = new Set([start]);
  const pending = [start];
  while (pending.length > 0) {
    for (const imported of inputs[pending.pop()!]?.imports ?? []) {
      if (imported.kind === "dynamic-import" || imported.external) continue;
      if (reached.has(imported.path)) continue;
      reached.add(imported.path);
      pending.push(imported.path);
    }
  }
  const absolute = (input: string) => join(packageRoot, input);
  return {
    all: Object.keys(inputs).map(absolute),
    static: [...reached].map(absolute),
  };
};

const toolModules = (files: readonly string[], modules = TOOL_L1_MODULES) =>
  files.filter((file) => modules.some((pattern) => pattern.test(file)));

const providerConstructions = (files: readonly string[]) =>
  files.filter(
    (file) =>
      !file.includes("/node_modules/") &&
      /\.[cm]?[jt]s$/u.test(file) &&
      !PINNED_PROVIDER_CONSTRUCTION.includes(file) &&
      PROVIDER_CONSTRUCTION.test(readFileSync(file, "utf8")),
  );

const roleViolations = async (entry: string) => {
  const graph = await moduleGraph(entry);
  return [...toolModules(graph.all), ...providerConstructions(graph.all)].map(
    (file) => file.slice(demo.length + 1),
  );
};

const ROLE_ENTRIES = [
  "midgard-node/src/commands/listen.cli-runtime.ts",
  "midgard-node/src/workers/commit-block-header.ts",
  "midgard-node/src/workers/confirm-block-commitments.ts",
  "midgard-node/src/workers/settlement.ts",
  "midgard-node/src/workers/validation.ts",
  "midgard-node/src/workers/mpf-root-builder.ts",
  "da-committee-node/src/index.ts",
  "da-committee-node/src/public-retained-da.ts",
  "midgard-watcher/src/cli.ts",
];

describe("role processes hold no external L1 client", () => {
  it.each(ROLE_ENTRIES)(
    "%s",
    async (entry) => {
      expect(await roleViolations(join(demo, entry))).toEqual([]);
    },
    120_000,
  );

  it("the CLI binary loads external adapters only dynamically", async () => {
    const graph = await moduleGraph(join(packageRoot, "src/index.ts"));
    expect(toolModules(graph.static, EXTERNAL_L1_MODULES)).toEqual([]);
    // The tools do reach them, through their dynamic imports.
    expect(toolModules(graph.all, EXTERNAL_L1_MODULES)).not.toEqual([]);
  }, 120_000);

  it("pins only the module that constructs the prover CLI's provider", () => {
    // The exemption is that one module: it does construct a provider, and the
    // signer module beside it no longer does.
    for (const file of PINNED_PROVIDER_CONSTRUCTION)
      expect(PROVIDER_CONSTRUCTION.test(readFileSync(file, "utf8"))).toBe(true);
    expect(
      PROVIDER_CONSTRUCTION.test(
        readFileSync(
          join(
            demo,
            "midgard-fault-proofs/src/runtime.resolve-prover-signer.ts",
          ),
          "utf8",
        ),
      ),
    ).toBe(false);
  });

  it("flags a role that reaches an external adapter or builds a provider", async () => {
    const fixtures = join(packageRoot, "tests/fixtures/l1-role-boundary");
    expect(
      await roleViolations(join(fixtures, "role-reaching-kupmios-access.ts")),
    ).toContain("midgard-node/src/l1-external/kupmios-access.ts");
    expect(
      await roleViolations(join(fixtures, "role-constructing-blockfrost.ts")),
    ).toEqual([
      "midgard-node/tests/fixtures/l1-role-boundary/role-constructing-blockfrost.ts",
    ]);
  }, 120_000);
});
