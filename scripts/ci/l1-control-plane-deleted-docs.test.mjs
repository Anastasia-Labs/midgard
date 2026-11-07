// No Markdown document names the node's deleted event-history control plane
// outside a historical record or a `doc-links:historical` block. The node's
// own test reads its sources; this one reads the docs on every pull request,
// docs-only ones included. The names and the exemptions are in
// `scripts/lib/l1-control-plane-deleted.mjs`.

import assert from "node:assert/strict";
import { dirname, resolve } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

import {
  DOC,
  HISTORICAL_MARKER,
  deletedNameReaders,
  exempt,
  namesDeleted,
  trackedFiles,
} from "../lib/l1-control-plane-deleted.mjs";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const docs = trackedFiles(ROOT).filter((path) => DOC.test(path));

test("no document names the deleted event-history control plane", () => {
  assert.deepEqual(deletedNameReaders(ROOT, docs), []);
});

test("the scan reads the documents it guards", () => {
  const checked = docs.filter((path) => !exempt(path));
  for (const path of [
    "demo/midgard-node/README.md",
    "docs-site/content/docs/getting-started/l1-backend.mdx",
    "docs-site/content/docs/watchers/concept.mdx",
    ".agents/skills/midgard-e2e-acceptance/references/release-readiness.md",
  ])
    assert.ok(checked.includes(path), path);
});

test("a deleted name fails unless its block is marked historical", () => {
  for (const line of [
    "names `L1_KUPO_KEY`",
    "The event-history owner reads outputs.",
    "the history owner (event-history runtime)",
    "History owner<br/>runtime",
  ]) {
    assert.equal(namesDeleted("docs/a.md", line), true, line);
    assert.equal(
      namesDeleted("docs/a.md", `${line}\n<!-- ${HISTORICAL_MARKER} -->`),
      false,
      line,
    );
    assert.equal(
      namesDeleted("docs/a.md", `${line}\n\n<!-- ${HISTORICAL_MARKER} -->`),
      true,
      line,
    );
  }
});
