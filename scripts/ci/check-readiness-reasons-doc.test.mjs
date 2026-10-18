import assert from "node:assert/strict";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { spawnSync } from "node:child_process";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { after, test } from "node:test";
import { fileURLToPath } from "node:url";

import {
  DOC,
  DOC_SECTION,
  FOLLOWER_READINESS_TYPE,
  FOLLOWER_SRC,
  NODE_SRC,
  PRESENTATION,
  PRESENTATION_BRANCH,
  READINESS_ASSEMBLERS,
  READINESS_MODULES,
  ROOT,
  followerReadinessReasons,
  nodeReadinessReasons,
  presentedReasons,
  reasonsSection,
  undocumentedReasons,
  unpresentedReasons,
} from "./check-readiness-reasons-doc.mjs";

const CHECKER = fileURLToPath(
  new URL("./check-readiness-reasons-doc.mjs", import.meta.url),
);
const [HANDLER] = READINESS_ASSEMBLERS;

const write = (path, text) => {
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, text);
};

const fixtureRoot = (modules, followerModules = {}) => {
  const root = mkdtempSync(join(tmpdir(), "readiness-reasons-"));
  for (const rel of [...READINESS_ASSEMBLERS, ...READINESS_MODULES])
    write(join(root, NODE_SRC, rel), "");
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
    [HANDLER]:
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
      [HANDLER]: [
        'import { SEED_PENDING, type Other } from "@al-ft/midgard-l1-follower";',
        'import { LOCAL_HOLD } from "../services/holds.js";',
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

test("every node readiness reason has operator text in the doc and a presentation entry", () => {
  const section = reasonsSection(readFileSync(join(ROOT, DOC), "utf8"));
  assert.notEqual(section, undefined, `${DOC} lacks "${DOC_SECTION}"`);
  const presented = presentedReasons(
    readFileSync(join(ROOT, PRESENTATION), "utf8"),
  );
  assert.notEqual(presented, undefined, `${PRESENTATION} lacks its branch`);
  const reasons = nodeReadinessReasons();
  assert.ok(reasons.size > 50, `only ${reasons.size} reasons derived`);
  assert.deepEqual(undocumentedReasons(reasons.keys(), section), []);
  assert.deepEqual(unpresentedReasons(reasons.keys(), presented), []);
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

// The checker run whole against a fixture tree: the node's source, the doc
// and the presentation all come from it.
const trees = [];
after(() => {
  for (const root of trees) rmSync(root, { recursive: true, force: true });
});
const checkerTree = ({ modules = {}, documented = [], presentation }) => {
  const root = fixtureRoot(modules);
  trees.push(root);
  write(
    join(root, DOC),
    `# T\n\n${DOC_SECTION}\n\n${documented.map((r) => `- \`${r}\`: text.`).join("\n")}\n\n## Logs\n`,
  );
  write(join(root, PRESENTATION), presentation);
  return root;
};

const presentationNaming = (names, elsewhere = "") =>
  [
    `const shown: ReadonlySet<string> = new Set([${names.map((n) => `"${n}"`).join(", ")}]);`,
    `const unrelated = new Set(["${elsewhere}"]);`,
    "const render = (key: string, safe: string) => {",
    `  if (${PRESENTATION_BRANCH} || key === "reason") {`,
    '    return shown.has(safe) ? safe : "E_REDACTED";',
    "  }",
    "  return unrelated.has(safe) ? safe : null;",
    "};",
    "",
  ].join("\n");

const runChecker = (root) =>
  spawnSync(process.execPath, [CHECKER, "--root", root], { encoding: "utf8" });

// A held build is named as `<constant>:<stage>` by a function the readiness
// handler calls and spreads into `reasons`; the reason is in neither the
// handler nor a module with a hold marker.
const buildHold = {
  [HANDLER]: [
    'import { buildHoldReadiness } from "../services/build-holds.js";',
    "const holds = buildHoldReadiness(alarms);",
    "reasons.push(...holds.reasons);",
  ].join("\n"),
  "services/build-holds.ts": [
    'export const BUILD_HELD = "build_held";',
    "export const buildHoldReadiness = (held: string | null) => ({",
    "  reasons: held === null ? [] : [`${BUILD_HELD}:${held}`],",
    "});",
  ].join("\n"),
};

test("a build-hold reason without operator text fails the check", () => {
  const root = checkerTree({
    modules: buildHold,
    presentation: presentationNaming(["build_held"]),
  });
  assert.deepEqual(nodeReadinessReasons(root).get("build_held"), [
    "services/build-holds.ts",
  ]);
  const run = runChecker(root);
  assert.equal(run.status, 1, run.stdout);
  assert.match(
    run.stderr,
    new RegExp(`${DOC} has no operator text for:\\n  build_held `, "u"),
  );
});

test("a build-hold reason with operator text and a presentation entry passes", () => {
  const run = runChecker(
    checkerTree({
      modules: buildHold,
      documented: ["build_held"],
      presentation: presentationNaming(["build_held"]),
    }),
  );
  assert.equal(run.status, 0, run.stderr);
  assert.match(run.stdout, /^1 node readiness reasons, all documented/u);
});

test("a reason reached through a re-export and a for-of loop is found", () => {
  const modules = {
    [HANDLER]: [
      'import { derivedReasons } from "../services/index.js";',
      "for (const reason of derivedReasons(now)) reasons.push(reason);",
    ].join("\n"),
    "services/index.ts": 'export * from "./plane.js";\n',
    "services/plane.ts": [
      'const PLANE_WEDGED = "plane_wedged";',
      "export function derivedReasons() {",
      "  return [PLANE_WEDGED];",
      "}",
    ].join("\n"),
  };
  const run = runChecker(
    checkerTree({
      modules,
      presentation: presentationNaming(["plane_wedged"]),
    }),
  );
  assert.equal(run.status, 1, run.stdout);
  assert.match(run.stderr, /^ {2}plane_wedged \(services\/plane\.ts\)$/mu);
});

test("a reason with operator text but no presentation entry fails the check", () => {
  const run = runChecker(
    checkerTree({
      modules: buildHold,
      documented: ["build_held"],
      // Named outside the branch that renders a reason: not an entry.
      presentation: presentationNaming([], "build_held"),
    }),
  );
  assert.equal(run.status, 1, run.stdout);
  assert.match(
    run.stderr,
    new RegExp(`${PRESENTATION} has no entry for:\\n  build_held `, "u"),
  );
  assert.doesNotMatch(run.stderr, /has no operator text/u);
});

test("a reason with a presentation entry but no operator text fails the check", () => {
  const run = runChecker(
    checkerTree({
      modules: { [HANDLER]: 'reasons.push("plain_reason");\n' },
      presentation: presentationNaming(["plain_reason"]),
    }),
  );
  assert.equal(run.status, 1, run.stdout);
  assert.match(run.stderr, /has no operator text for:\n {2}plain_reason /u);
  assert.doesNotMatch(run.stderr, /has no entry/u);
});

test("a bounded format in the presentation names each reason it spells", () => {
  const presented = presentedReasons(
    [
      "const bounded = /^(?:(?:a_one|b_two)_floor:[0-9]+|c_(?:x|y):[0-9]+)$/u;",
      `if (${PRESENTATION_BRANCH}) {`,
      "  return bounded.test(safe);",
      "}",
      "",
    ].join("\n"),
  );
  assert.deepEqual([...presented].sort(), [
    "a_one_floor",
    "b_two_floor",
    "c_x",
    "c_y",
  ]);
  assert.equal(presentedReasons("const x = 1;\n"), undefined);
});
