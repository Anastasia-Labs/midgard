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
  FOLLOWER_READINESS_TYPE,
  FOLLOWER_SRC,
  NODE_SRC,
  READINESS_MODULES,
  ROOT,
  followerReadinessReasons,
  nodeReadinessReasons,
  reasonsSection,
  undocumentedReasons,
} from "./check-readiness-reasons-doc.mjs";

const write = (path, text) => {
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, text);
};

const fixtureRoot = (modules, followerModules = {}) => {
  const root = mkdtempSync(join(tmpdir(), "readiness-reasons-"));
  for (const rel of READINESS_MODULES) write(join(root, NODE_SRC, rel), "");
  write(
    join(root, FOLLOWER_SRC, "follow/status.ts"),
    `export type ${FOLLOWER_READINESS_TYPE} = never;\n`,
  );
  for (const [rel, text] of Object.entries(modules))
    write(join(root, NODE_SRC, rel), text);
  for (const [rel, text] of Object.entries(followerModules))
    write(join(root, FOLLOWER_SRC, rel), text);
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

test("the follower's readiness type and imported constants yield reasons", () => {
  const root = fixtureRoot(
    {
      "services/l1-follower.readiness.ts": [
        'import { SEED_PENDING, type Other } from "@al-ft/midgard-l1-follower";',
        'import { LOCAL_HOLD } from "./holds.js";',
        'import { DEFAULT_MS } from "@al-ft/midgard-l1-follower/heads";',
      ].join("\n"),
      "services/holds.ts": 'export const LOCAL_HOLD = "local_hold_named";\n',
    },
    {
      "follow/status.ts": [
        'export const CATCHING_UP = "follower_catching_up";',
        `export type ${FOLLOWER_READINESS_TYPE} =`,
        '  | Intervention | typeof CATCHING_UP | "follower_literal";',
      ].join("\n"),
      "types.ts": 'export type Intervention = "beyond_k" | "origin_late";\n',
      "follow/wallet-seed.ts": [
        'export const SEED_PENDING = "seed_pending";',
        "export const DEFAULT_MS = 300_000;",
        'export const UNUSED = "never_imported";',
      ].join("\n"),
    },
  );
  try {
    assert.deepEqual([...followerReadinessReasons(root).keys()].sort(), [
      "beyond_k",
      "follower_catching_up",
      "follower_literal",
      "origin_late",
    ]);
    assert.deepEqual([...nodeReadinessReasons(root).keys()].sort(), [
      "beyond_k",
      "follower_catching_up",
      "follower_literal",
      "local_hold_named",
      "origin_late",
      "seed_pending",
    ]);
    // A follower-only reason the doc lacks fails the check.
    const section = [
      "beyond_k",
      "follower_catching_up",
      "follower_literal",
      "local_hold_named",
      "seed_pending",
    ]
      .map((reason) => `- \`${reason}\`: text.`)
      .join("\n");
    assert.deepEqual(
      undocumentedReasons(nodeReadinessReasons(root).keys(), section),
      ["origin_late"],
    );
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
});

test("an unresolvable follower readiness member fails by name", () => {
  const root = fixtureRoot(
    {},
    {
      "follow/status.ts": `export type ${FOLLOWER_READINESS_TYPE} = typeof GONE;\n`,
    },
  );
  try {
    assert.throws(() => followerReadinessReasons(root), /typeof GONE/u);
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

test("the doc check fails once a follower-only reason's text is removed", () => {
  const section = reasonsSection(readFileSync(join(ROOT, DOC), "utf8"));
  const reasons = [...nodeReadinessReasons().keys()];
  // From the follower's readiness type, and from an imported constant.
  for (const reason of ["l1_follower_prune_failing", "wallet_seed_pending"])
    assert.deepEqual(
      undocumentedReasons(reasons, section.replaceAll(`\`${reason}\``, "")),
      [reason],
    );
});
