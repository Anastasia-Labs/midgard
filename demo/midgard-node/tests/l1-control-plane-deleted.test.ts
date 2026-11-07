/**
 * The node's event-history control plane is deleted (plan §13.1, N1-close).
 * This gate reads every tracked file of the node and node-tools packages
 * other than Markdown and finds none that names a deleted module, table or
 * key (`scripts/lib/l1-control-plane-deleted.mjs` lists them and the text
 * that may still name them). Markdown, in these packages and everywhere
 * else, is read by `scripts/ci/l1-control-plane-deleted-docs.test.mjs`,
 * which runs on every pull request.
 */
import { relative } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  CODE_ROOTS,
  deletedNameReaders,
  DOC,
  exempt,
  HISTORICAL_MARKER,
  namesDeleted,
  trackedFiles,
} from "../../../scripts/lib/l1-control-plane-deleted.mjs";

const REPO_ROOT = fileURLToPath(new URL("../../..", import.meta.url));
const THIS_FILE = relative(REPO_ROOT, fileURLToPath(import.meta.url));

const scanned = (path: string): boolean =>
  path !== THIS_FILE &&
  !DOC.test(path) &&
  CODE_ROOTS.some((root) => path.startsWith(root));

describe("the deleted event-history control plane", () => {
  const files = trackedFiles(REPO_ROOT).filter(scanned);

  it("has no reader left in the node or node-tools sources", () => {
    expect(deletedNameReaders(REPO_ROOT, files)).toEqual([]);
  });

  it("scans the files it guards", () => {
    const checked = files.filter((path) => !exempt(path));
    for (const path of [
      "demo/midgard-node/src/index.ts",
      "demo/midgard-node/.env.example",
      "demo/midgard-node/docker-compose.kupmios.yaml",
      "demo/midgard-node-tools/src/index.ts",
    ])
      expect(checked).toContain(path);
  });

  it("refuses a deleted name, in code or in prose", () => {
    expect(namesDeleted("src/a.ts", "names `L1_KUPO_KEY`")).toBe(true);
    expect(namesDeleted("src/a.ts", "// left to the history owner")).toBe(true);
    expect(namesDeleted("src/a.ts", "// no event-history owner here")).toBe(
      true,
    );
    expect(
      namesDeleted("src/a.ts", `// the history owner // ${HISTORICAL_MARKER}`),
    ).toBe(true);
  });
});
