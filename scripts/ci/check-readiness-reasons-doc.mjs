#!/usr/bin/env node
// Every reason the node can put in `/readyz` must have operator text in the
// reasons doc.
//
// The reasons are derived from the node's source rather than kept in a list
// here. A readiness module is one that declares or reports a hold
// (`DriverHold`, `IntentPredicateWait`), raises a liveness reason (`raiseLivenessIncident(`), or is
// one of the modules that assemble `/readyz` (READINESS_MODULES). In those
// modules a reason is:
//   - a constant whose value is a snake_case name, `const NAME = "a_b"`, unless
//     the constant names something other than a reason (NON_REASON_CONSTANT:
//     a liveness source, a due-work kind, a table, a key);
//   - the leading snake_case name of a string pushed onto `reasons`, returned
//     or set as `reason:` (`reasons.push(\`queue_depth_exceeded:${n}\`)`).
// NOT_READINESS lists the names this finds that are not reasons.
// A reason is documented when the reasons section of the doc names it in
// backticks, alone or as the prefix of a parameterised form (`name:<n>`).
//
// Run from anywhere: `node scripts/ci/check-readiness-reasons-doc.mjs`.
// Exit 0 when every reason is documented, 1 otherwise.

import { readdirSync, readFileSync, statSync } from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

export const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
export const NODE_SRC = "demo/midgard-node/src";
export const DOC =
  ".agents/skills/running-the-devnet/references/endpoints-and-logs.md";
export const DOC_SECTION = "## Node `/readyz` reasons";

/** Modules that assemble `/readyz` without declaring a hold themselves. */
export const READINESS_MODULES = [
  "commands/readiness.ts",
  "commands/listen-router.get-readiness-handler.ts",
  "commands/listen-router.get-readiness-handler.inputs.ts",
  "commands/listen-startup.await-landed-state-queue.ts",
  "services/settlement-readiness.ts",
  "services/globals.l1-control-plane.activity.ts",
  "services/follower-write-gate.local.ts",
  "services/l1-follower.readiness.ts",
  "services/liveness-halt.ts",
  "services/intent-journal.refusals.ts",
  "l1-operator-set/snapshot.ts",
];

/** Constant names whose value is not a readiness reason. */
const NON_REASON_CONSTANT =
  /(?:^|_)(?:SOURCE|KIND|KEY|TABLE|NAMESPACE|SCOPE|HOLDER|METRIC)$/u;

/** Names a readiness module uses that never reach `/readyz`, with why. */
export const NOT_READINESS = new Map([
  [
    "manifest_mismatch",
    "the operator watchdog's strike-gate verdict; readiness carries operator_watchdog_manifest_mismatch",
  ],
]);

const SNAKE = "[a-z][a-z0-9]*(?:_[a-z0-9]+)+";
const CONSTANT = new RegExp(
  `\\bconst\\s+([A-Z][A-Z0-9_]*)\\s*(?::[^=\\n]+)?=\\s*\\n?\\s*"(${SNAKE})"`,
  "gu",
);
const REPORTED = new RegExp(
  `(?:reasons\\.push\\(|\\breturn|\\breason:)\\s*["\`](${SNAKE})(?=[:"\`])`,
  "gu",
);

const tsFiles = (dir) =>
  readdirSync(dir).flatMap((name) => {
    const path = join(dir, name);
    if (statSync(path).isDirectory()) return tsFiles(path);
    return path.endsWith(".ts") && !path.endsWith(".d.ts") ? [path] : [];
  });

const isReadinessModule = (rel, text) =>
  READINESS_MODULES.includes(rel) ||
  /\bDriverHold\b|\bIntentPredicateWait\b|raiseLivenessIncident\(/u.test(text);

/** The reasons the node source can report, each with the modules naming it. */
export const nodeReadinessReasons = (root = ROOT) => {
  const src = join(root, NODE_SRC);
  for (const rel of READINESS_MODULES) statSync(join(src, rel)); // a moved readiness module fails here, by name
  const reasons = new Map();
  for (const path of tsFiles(src)) {
    const rel = relative(src, path);
    const text = readFileSync(path, "utf8");
    if (!isReadinessModule(rel, text)) continue;
    const add = (reason) =>
      NOT_READINESS.has(reason) ||
      reasons.set(reason, [...(reasons.get(reason) ?? []), rel]);
    for (const [, name, value] of text.matchAll(CONSTANT))
      if (!NON_REASON_CONSTANT.test(name)) add(value);
    for (const [, value] of text.matchAll(REPORTED)) add(value);
  }
  return reasons;
};

/** The reasons section of the doc: from DOC_SECTION to the next `## `. */
export const reasonsSection = (docText) => {
  const start = docText.indexOf(`${DOC_SECTION}\n`);
  if (start < 0) return undefined;
  const next = docText.indexOf("\n## ", start + DOC_SECTION.length);
  return next < 0 ? docText.slice(start) : docText.slice(start, next);
};

/** The reasons the section does not name in backticks. */
export const undocumentedReasons = (reasons, section) =>
  [...reasons]
    .filter((reason) => !new RegExp(`\`${reason}(?:\`|:)`, "u").test(section))
    .sort();

const main = () => {
  const docText = readFileSync(join(ROOT, DOC), "utf8");
  const section = reasonsSection(docText);
  if (section === undefined) {
    console.error(`${DOC}: no "${DOC_SECTION}" section`);
    return 1;
  }
  const reasons = nodeReadinessReasons();
  const missing = undocumentedReasons(reasons.keys(), section);
  if (missing.length === 0) {
    console.log(
      `${reasons.size} node readiness reasons, all documented in ${DOC}`,
    );
    return 0;
  }
  console.error(`${DOC} has no operator text for:`);
  for (const reason of missing)
    console.error(`  ${reason} (${reasons.get(reason).join(", ")})`);
  return 1;
};

if (process.argv[1] === fileURLToPath(import.meta.url)) process.exit(main());
