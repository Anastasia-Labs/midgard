/**
 * The node reads and submits L1 through the local node transport only: no
 * node source names the removed provider choice (`L1_PROVIDER`), builds a
 * Lucid Kupmios provider, or reads an Ogmios or Kupo endpoint key. The one
 * exempt directory is `src/l1-external/`, the tools' external adapters
 * (option E), which only a tool reaches, by a dynamic import; the role
 * boundary test (`l1-role-boundary.test.ts`) holds every role graph clear
 * of it.
 */
import { readdirSync, readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

const PACKAGE_ROOT = fileURLToPath(new URL("..", import.meta.url));
const SCANNED = ["src", "scripts"];
const SOURCE = /\.(?:ts|mts|js|mjs)$/u;
const TOOL_EXTERNAL_ADAPTERS = "src/l1-external/";

const FORBIDDEN: readonly Readonly<{ name: string; pattern: RegExp }>[] = [
  { name: "the L1_PROVIDER key", pattern: /\bL1_PROVIDER\b/u },
  {
    name: "a Lucid Kupmios provider",
    pattern:
      /\bnew\s+(?:[A-Za-z_$][\w$]*\.)?Kupmios\b|import[^;]*\bKupmios\b[^;]*from\s*["']@lucid-evolution\//u,
  },
  {
    name: "the node's Kupmios ledger",
    pattern: /\bmakeNodeKupmios\b|\bNativeLedgerKupmios\b/u,
  },
  {
    name: "a remote L1 provider",
    pattern: /\bkoios\b|blockfrost\.io|blockfrost-fallback-key/iu,
  },
  {
    name: "an Ogmios or Kupo endpoint key",
    pattern: /\bL1_(?:OGMIOS|KUPO)_KEY\b/u,
  },
];

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
      )
      .filter((file) => !file.startsWith(TOOL_EXTERNAL_ADAPTERS)),
  ).sort();

/** Every forbidden L1 provider use in the package's sources, as `file: what`. */
const nodeL1ProviderViolations = (root: string): string[] =>
  sourceFiles(root).flatMap((file) => {
    const text = readFileSync(join(root, file), "utf8");
    return FORBIDDEN.filter(({ pattern }) => pattern.test(text)).map(
      ({ name }) => `${file}: ${name}`,
    );
  });

describe("the node's L1 access gate", () => {
  it("finds no removed provider key, Kupmios provider or Ogmios/Kupo endpoint key", () => {
    expect(nodeL1ProviderViolations(PACKAGE_ROOT)).toEqual([]);
  });

  it("scans the sources it guards", () => {
    const files = sourceFiles(PACKAGE_ROOT);
    expect(files).toContain("src/services/lucid.ts");
    expect(files).toContain("src/services/l1-provider.ts");
    expect(files).toContain("src/services/config.node-config-dep.ts");
    expect(files).toContain("scripts/capture-node-slot-config.mjs");
    expect(files).not.toContain("src/l1-external/kupmios-access.ts");
  });
});
