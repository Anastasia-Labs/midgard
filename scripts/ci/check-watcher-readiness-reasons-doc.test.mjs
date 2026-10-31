// Every reason the watcher names in `readinessReasons`, as its runtime
// constant WATCHER_OPERATIONS_READINESS_REASONS lists them, has an entry in
// the watcher `/readyz` section of the reasons doc. The constant is loaded
// from the watcher source at runtime (Node strips its types); its module has
// no imports, so no build is needed.

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { test } from "node:test";

import { DOC, ROOT } from "./check-readiness-reasons-doc.mjs";

const SECTION = "## Watcher `/readyz` reasons";
const REASONS_MODULE =
  "demo/midgard-watcher/src/runtime/operations-observability.watcher-operations-readiness-reasons.ts";

const { WATCHER_OPERATIONS_READINESS_REASONS } = await import(
  join(ROOT, REASONS_MODULE)
);

/** The names each bullet of the section opens with, before its first `:`. */
const documentedReasons = (docText) => {
  const start = docText.indexOf(`${SECTION}\n`);
  assert.ok(start >= 0, `${DOC} lacks "${SECTION}"`);
  const next = docText.indexOf("\n## ", start + SECTION.length);
  const section = docText.slice(start, next < 0 ? undefined : next);
  const bullets = [];
  for (const line of section.split("\n"))
    if (line.startsWith("- ")) bullets.push(line);
    else if (line.startsWith("  ") && bullets.length > 0)
      bullets[bullets.length - 1] += ` ${line.trim()}`;
  const names = new Set();
  for (const bullet of bullets) {
    const head = bullet.indexOf("`:");
    if (head < 0) continue;
    for (const [, name] of bullet.slice(0, head + 1).matchAll(/`([^`]+)`/gu))
      names.add(name);
  }
  return names;
};

const undocumented = (docText) => {
  const documented = documentedReasons(docText);
  return WATCHER_OPERATIONS_READINESS_REASONS.filter(
    (reason) => !documented.has(reason),
  );
};

test("every watcher readiness reason has an entry in the doc", () => {
  assert.ok(WATCHER_OPERATIONS_READINESS_REASONS.length > 10);
  assert.deepEqual(undocumented(readFileSync(join(ROOT, DOC), "utf8")), []);
});

test("the check names a reason whose entry is removed", () => {
  const docText = readFileSync(join(ROOT, DOC), "utf8");
  const removed = docText
    .replace(/^- `active_alert`:.*\n(?: {2}.*\n)*/mu, "")
    .replace("`journal_capacity`, ", "");
  assert.deepEqual(undocumented(removed), ["active_alert", "journal_capacity"]);
});
