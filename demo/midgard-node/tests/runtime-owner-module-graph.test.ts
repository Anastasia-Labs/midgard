/**
 * The follower-change driver owns every runtime duty the event-history owner
 * had (plan §15, N1H): the producer gate, the landed-block rebase, orphan
 * repair, startup preparation and the settlement fence. No runtime entry
 * may load an owner module, so deleting them changes no node behaviour.
 *
 * esbuild's metafile lists every file a bundle parses (type-only imports are
 * erased), so it is the whole static module graph each entry loads. The
 * operator's `history-genesis-pin` command is left out: it derives the
 * owner's source pin and goes with the owner.
 */
import { existsSync, readdirSync } from "node:fs";
import { join, relative } from "node:path";

import { build, type Plugin } from "esbuild";
import { describe, expect, it } from "vitest";

const packageRoot = join(import.meta.dirname, "..");
const src = join(packageRoot, "src");

const OWNER_MODULES = [
  /^services\/event-history-owner/u,
  /^services\/event-history-(runtime|producer|recovery)\.ts$/u,
  /^database\/eventHistoryAuthority/u,
  /^database\/eventHistoryLedger(Repair|Receipts)\.ts$/u,
  /^l1-event-history-/u,
  /^l1-ledger-snapshot\.ts$/u,
];

const GENESIS_PIN_COMMAND = join(src, "commands/history-genesis-pin.ts");

const withoutGenesisPinCommand: Plugin = {
  name: "without-history-genesis-pin",
  setup: (build) => {
    build.onResolve({ filter: /\/history-genesis-pin\.js$/u }, (args) => ({
      path: args.path,
      external: true,
    }));
  },
};

const ownerModulesReached = async (entry: string) => {
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
    packages: "external",
    loader: { ".sql": "text" },
    plugins: [withoutGenesisPinCommand],
  });
  const graph = Object.keys(metafile.inputs).map((input) =>
    relative(src, join(packageRoot, input)),
  );
  expect(graph).toContain(relative(src, entry));
  return graph.filter((path) => OWNER_MODULES.some((re) => re.test(path)));
};

const workers = readdirSync(join(src, "workers"))
  .filter((name) => name.endsWith(".ts"))
  .map((name) => join(src, "workers", name));

describe("no runtime entry loads an event-history owner module", () => {
  it("names the one command it leaves out", () => {
    expect(existsSync(GENESIS_PIN_COMMAND)).toBe(true);
    expect(workers.length).toBeGreaterThan(0);
  });

  it("the node CLI, which runs the node", async () => {
    expect(await ownerModulesReached(join(src, "index.ts"))).toEqual([]);
  }, 120_000);

  it.each(workers.map((path) => [relative(src, path), path]))(
    "%s",
    async (_name, path) => {
      expect(await ownerModulesReached(path)).toEqual([]);
    },
    120_000,
  );
});
