import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import { readSourceFacets, sourceFacetPaths } from "./source-facets.mjs";

test("source guards include extracted implementation and exclude other modules", (t) => {
  const root = mkdtempSync(join(tmpdir(), "midgard-source-facets-"));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  const entry = join(root, "router.ts");
  writeFileSync(entry, 'export { route } from "./router.route.js";');
  writeFileSync(join(root, "router.route.ts"), 'const forbidden = "HTTP";');
  writeFileSync(join(root, "other.ts"), "unrelated");
  writeFileSync(join(root, "router.route.js"), "compiled");
  assert.deepEqual(sourceFacetPaths(entry), [
    entry,
    join(root, "router.route.ts"),
  ]);
  const source = readSourceFacets(entry);
  assert.match(source, /HTTP/u);
  assert.doesNotMatch(source, /unrelated|compiled/u);
});

test("test helpers are read with their retained test entrypoint", (t) => {
  const root = mkdtempSync(join(tmpdir(), "midgard-test-facets-"));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  const entry = join(root, "journal.test.ts");
  writeFileSync(entry, "test registration");
  writeFileSync(join(root, "journal.fixture.ts"), "test setup");
  assert.match(readSourceFacets(entry), /test registration\ntest setup/u);
});

test("a missing retained entrypoint remains an error", () => {
  assert.throws(() =>
    readSourceFacets(join(tmpdir(), "absent-midgard-entry.ts")),
  );
});

test("directory source guards include nested modules and exclude non-source files", (t) => {
  const root = mkdtempSync(join(tmpdir(), "midgard-source-directory-"));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  mkdirSync(join(root, "handlers"));
  const route = join(root, "router.ts");
  const handler = join(root, "handlers", "transaction.ts");
  writeFileSync(route, 'const route = "healthz";');
  writeFileSync(handler, 'const forbidden = "/da/payload";');
  writeFileSync(join(root, "README.md"), "non-source");
  assert.deepEqual(sourceFacetPaths(root), [handler, route]);
  const source = readSourceFacets(root);
  assert.match(source, /healthz/u);
  assert.match(source, /da\/payload/u);
  assert.doesNotMatch(source, /non-source/u);
});
