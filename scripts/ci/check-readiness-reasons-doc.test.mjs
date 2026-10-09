import assert from "node:assert/strict";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { test } from "node:test";

import {
  DOC,
  DOC_SECTION,
  NODE_SRC,
  READINESS_MODULES,
  ROOT,
  nodeReadinessReasons,
  reasonsSection,
  undocumentedReasons,
} from "./check-readiness-reasons-doc.mjs";

const fixtureRoot = (modules) => {
  const root = mkdtempSync(join(tmpdir(), "readiness-reasons-"));
  for (const rel of READINESS_MODULES) {
    const path = join(root, NODE_SRC, rel);
    mkdirSync(dirname(path), { recursive: true });
    writeFileSync(path, "");
  }
  for (const [rel, text] of Object.entries(modules)) {
    const path = join(root, NODE_SRC, rel);
    mkdirSync(dirname(path), { recursive: true });
    writeFileSync(path, text);
  }
  return root;
};

test("reasons come from readiness modules only, without non-reason names", () => {
  const root = fixtureRoot({
    "commands/readiness.ts":
      'reasons.push("db_unhealthy");\nreasons.push(`stale_heartbeat:${w}`);\n',
    "fibers/commit.ts": [
      'export const COMMIT_SOURCE = "commit_worker";',
      'export const COMMIT_FAILED = "commit_worker_failed";',
      "yield* raiseLivenessIncident(globals, COMMIT_SOURCE, COMMIT_FAILED, d);",
    ].join("\n"),
    "fibers/watchdog.ts":
      'raiseLivenessIncident(g, s, r, d);\nreturn { reason: "manifest_mismatch" };\n',
    "unrelated.ts": 'export const NOT_A_REASON = "some_label";\n',
  });
  try {
    assert.deepEqual([...nodeReadinessReasons(root).keys()].sort(), [
      "commit_worker_failed",
      "db_unhealthy",
      "stale_heartbeat",
    ]);
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
});

test("a reason is documented by its name or its parameterised form", () => {
  const section = "- `db_unhealthy`: x.\n- `stale_heartbeat:<worker>`: y.\n";
  assert.deepEqual(
    undocumentedReasons(
      ["db_unhealthy", "stale_heartbeat", "queue_depth_exceeded"],
      section,
    ),
    ["queue_depth_exceeded"],
  );
  // A longer name that merely starts with the reason does not document it.
  assert.deepEqual(
    undocumentedReasons(["db_unhealthy"], "`db_unhealthy_extra`"),
    ["db_unhealthy"],
  );
});

test("the reasons section ends at the next level-two heading", () => {
  const doc = `# T\n\n${DOC_SECTION}\n\n- \`a_b\`\n\n## Logs\n\n- \`c_d\`\n`;
  const section = reasonsSection(doc);
  assert.match(section, /`a_b`/u);
  assert.doesNotMatch(section, /`c_d`/u);
  assert.equal(reasonsSection("# T\n"), undefined);
});

test("every node readiness reason has operator text in the doc", () => {
  const section = reasonsSection(readFileSync(join(ROOT, DOC), "utf8"));
  assert.notEqual(section, undefined, `${DOC} lacks "${DOC_SECTION}"`);
  const reasons = nodeReadinessReasons();
  assert.ok(reasons.size > 50, `only ${reasons.size} reasons derived`);
  assert.deepEqual(undocumentedReasons(reasons.keys(), section), []);
});

test("the doc check fails once one reason's text is removed", () => {
  const section = reasonsSection(readFileSync(join(ROOT, DOC), "utf8"));
  const removed = section.replaceAll(/`commit_window_pending`[^\n]*/gu, "");
  assert.deepEqual(
    undocumentedReasons(nodeReadinessReasons().keys(), removed),
    ["commit_window_pending"],
  );
});
