/**
 * The node reads and submits L1 through the local node transport only: no
 * node source names the removed provider choice (`L1_PROVIDER`), builds a
 * Lucid Kupmios provider, or reads the Ogmios and Kupo endpoints outside the
 * event-history owner's own inputs.
 */
import { readdirSync, readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

const PACKAGE_ROOT = fileURLToPath(new URL("..", import.meta.url));
const SCANNED = ["src", "scripts"];
const SOURCE = /\.(?:ts|mts|js|mjs)$/u;

/**
 * The files that may still name `L1_OGMIOS_KEY` or `L1_KUPO_KEY`: the
 * event-history owner's transport inputs, read once into the config and handed
 * to the owner, and the `history-genesis-pin` command's Ogmios URL default.
 */
const OWNER_ENDPOINT_READERS = new Set([
  "src/services/config.make-config.ts",
  "src/services/config.node-config-dep.ts",
  "src/commands/listen.run-node.ts",
  "src/commands/history-genesis-pin.ts",
  "src/index.registration-2.ts",
  "scripts/capture-node-slot-config.mjs",
]);

const FORBIDDEN: readonly Readonly<{ name: string; pattern: RegExp }>[] = [
  { name: "the L1_PROVIDER key", pattern: /\bL1_PROVIDER\b/u },
  {
    name: "a Lucid Kupmios provider",
    pattern:
      /\bnew\s+Kupmios\b|import[^;]*\bKupmios\b[^;]*from\s*["']@lucid-evolution\//u,
  },
  {
    name: "the node's Kupmios ledger",
    pattern: /\bmakeNodeKupmios\b|\bNativeLedgerKupmios\b/u,
  },
  {
    name: "a remote L1 provider",
    pattern: /\bkoios\b|blockfrost\.io|blockfrost-fallback-key/iu,
  },
];

const OWNER_ENDPOINT = /\bL1_(?:OGMIOS|KUPO)_KEY\b/u;

const sourceFiles = (root: string): string[] =>
  SCANNED.flatMap((directory) =>
    (
      readdirSync(join(root, directory), {
        recursive: true,
        withFileTypes: true,
      }) as import("node:fs").Dirent[]
    )
      .filter((entry) => entry.isFile() && SOURCE.test(entry.name))
      .map((entry) =>
        relative(root, join(entry.parentPath, entry.name)).replaceAll(
          "\\",
          "/",
        ),
      ),
  ).sort();

/** Every forbidden L1 provider use in the package's sources, as `file: what`. */
const nodeL1ProviderViolations = (root: string): string[] =>
  sourceFiles(root).flatMap((file) => {
    const text = readFileSync(join(root, file), "utf8");
    const found = FORBIDDEN.filter(({ pattern }) => pattern.test(text)).map(
      ({ name }) => `${file}: ${name}`,
    );
    if (OWNER_ENDPOINT.test(text) && !OWNER_ENDPOINT_READERS.has(file))
      found.push(`${file}: an Ogmios or Kupo endpoint outside the owner`);
    return found;
  });

describe("the node's L1 access gate", () => {
  it("finds no removed provider key, Kupmios provider or stray Ogmios/Kupo endpoint reader", () => {
    expect(nodeL1ProviderViolations(PACKAGE_ROOT)).toEqual([]);
  });

  it("scans the sources it guards", () => {
    const files = sourceFiles(PACKAGE_ROOT);
    expect(files).toContain("src/services/lucid.ts");
    expect(files).toContain("src/services/l1-provider.ts");
    for (const allowed of OWNER_ENDPOINT_READERS)
      expect(files).toContain(allowed);
  });
});
