#!/usr/bin/env node
// Every reason the node can put in `/readyz` must have operator text in the
// reasons doc.
//
// The reasons are derived from the node's and the L1 follower package's
// source rather than kept in a list here. A readiness module is one that
// declares or reports a hold (`DriverHold`, `IntentPredicateWait`), raises a
// liveness reason (`raiseLivenessIncident(`), or is one of the modules that
// assemble `/readyz` (READINESS_MODULES). In those modules a reason is:
//   - a constant whose value is a snake_case name, `const NAME = "a_b"`, unless
//     the constant names something other than a reason (NON_REASON_CONSTANT:
//     a liveness source, a due-work kind, a table, a key);
//   - the leading snake_case name of a string pushed onto `reasons`, returned
//     or set as `reason:` (`reasons.push(\`queue_depth_exceeded:${n}\`)`);
//   - an imported constant whose value is a snake_case name, resolved in the
//     node module or, for `@al-ft/midgard-l1-follower` imports, in the
//     follower package (`wallet_seed_pending` reaches a hold this way).
// The node also forwards the follower's `FollowStatus.readiness`: every member
// of the follower's FOLLOWER_READINESS_TYPE union is a reason, whether a
// `typeof NAME` constant, a string literal or another string-literal union
// (the interventions).
// NOT_READINESS lists the names this finds that are not reasons.
// A reason is documented when the reasons section of the doc names it in
// backticks, alone or as the prefix of a parameterised form (`name:<n>`).
//
// Run from anywhere: `node scripts/ci/check-readiness-reasons-doc.mjs`.
// Exit 0 when every reason is documented, 1 otherwise.

import { existsSync, readdirSync, readFileSync, statSync } from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

export const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
export const NODE_SRC = "demo/midgard-node/src";
export const FOLLOWER_SRC = "demo/midgard-l1-follower/src";
/** The follower's type of every reason `FollowStatus.readiness` can carry. */
export const FOLLOWER_READINESS_TYPE = "FollowReadinessReason";
const FOLLOWER_PACKAGE = /^@al-ft\/midgard-l1-follower(?:\/|$)/u;
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
  "services/startup-waiting.ts",
  "services/node-instance-lock.ts",
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

const IMPORT = /\bimport\s+(?:type\s+)?\{([^}]*)\}\s*from\s*"([^"]+)"/gu;
const IMPORTED_CONSTANT = /^(?:type\s+)?([A-Z][A-Z0-9_]*)(?:\s+as\s+\w+)?$/u;

const tsFiles = (dir) =>
  readdirSync(dir).flatMap((name) => {
    const path = join(dir, name);
    if (statSync(path).isDirectory()) return tsFiles(path);
    return path.endsWith(".ts") && !path.endsWith(".d.ts") ? [path] : [];
  });

const isReadinessModule = (rel, text) =>
  READINESS_MODULES.includes(rel) ||
  /\bDriverHold\b|\bIntentPredicateWait\b|raiseLivenessIncident\(/u.test(text);

/** The snake_case constants a module declares, by name. */
const constantsOf = (text) =>
  new Map([...text.matchAll(CONSTANT)].map(([, name, value]) => [name, value]));

/** The snake_case constants of every module under `dir`, by name. */
const constantsUnder = (dir) => {
  const constants = new Map();
  for (const path of tsFiles(dir))
    for (const [name, value] of constantsOf(readFileSync(path, "utf8"))) {
      if (constants.has(name) && constants.get(name) !== value)
        throw new Error(`${dir}: constant ${name} has two values`);
      constants.set(name, value);
    }
  return constants;
};

/** The members of `type NAME = A | B | ...;` in `text`, or undefined. */
const unionMembers = (text, name) => {
  const match = new RegExp(`\\btype\\s+${name}\\s*=([^;]*);`, "u").exec(text);
  if (match === null) return undefined;
  return match[1]
    .split("|")
    .map((member) => member.trim())
    .filter((member) => member !== "" && member !== "never");
};

/**
 * The reasons the follower's FOLLOWER_READINESS_TYPE admits: each member is
 * a `typeof NAME` constant, a string literal, or another union declared in
 * the follower package. A member that resolves to none of these fails, by
 * name.
 */
export const followerReadinessReasons = (root = ROOT) => {
  const src = join(root, FOLLOWER_SRC);
  const texts = tsFiles(src).map((path) => [
    relative(src, path),
    readFileSync(path, "utf8"),
  ]);
  const constants = constantsUnder(src);
  const reasons = new Map();
  const add = (reason, rel) =>
    reasons.set(reason, [
      ...(reasons.get(reason) ?? []),
      `${FOLLOWER_SRC}/${rel}`,
    ]);
  const resolve = (type, seen) => {
    if (seen.has(type)) return;
    seen.add(type);
    for (const [rel, text] of texts) {
      const members = unionMembers(text, type);
      if (members === undefined) continue;
      for (const member of members) {
        const literal = /^"([^"]*)"$/u.exec(member);
        const typeOf = /^typeof\s+([A-Z][A-Z0-9_]*)$/u.exec(member);
        if (literal !== null) add(literal[1], rel);
        else if (typeOf !== null && constants.has(typeOf[1]))
          add(constants.get(typeOf[1]), rel);
        else if (/^[A-Z]\w*$/u.test(member)) resolve(member, seen);
        else throw new Error(`${FOLLOWER_SRC}: ${type} member ${member}`);
      }
      return;
    }
    throw new Error(`${FOLLOWER_SRC}: no type ${type}`);
  };
  resolve(FOLLOWER_READINESS_TYPE, new Set()); // a renamed type fails here, by name
  return reasons;
};

/** The reasons the node can report, each with the modules naming it. */
export const nodeReadinessReasons = (root = ROOT) => {
  const src = join(root, NODE_SRC);
  for (const rel of READINESS_MODULES) statSync(join(src, rel)); // a moved readiness module fails here, by name
  const followerConstants = constantsUnder(join(root, FOLLOWER_SRC));
  const importedConstant = (rel, spec, name) => {
    if (FOLLOWER_PACKAGE.test(spec)) return followerConstants.get(name);
    if (!spec.startsWith(".")) return undefined;
    const target = join(dirname(join(src, rel)), spec.replace(/\.js$/u, ".ts"));
    return existsSync(target)
      ? constantsOf(readFileSync(target, "utf8")).get(name)
      : undefined;
  };
  const reasons = followerReadinessReasons(root);
  for (const path of tsFiles(src)) {
    const rel = relative(src, path);
    const text = readFileSync(path, "utf8");
    if (!isReadinessModule(rel, text)) continue;
    const add = (reason) =>
      NOT_READINESS.has(reason) ||
      reasons.set(reason, [...(reasons.get(reason) ?? []), rel]);
    for (const [name, value] of constantsOf(text))
      if (!NON_REASON_CONSTANT.test(name)) add(value);
    for (const [, value] of text.matchAll(REPORTED)) add(value);
    for (const [, names, spec] of text.matchAll(IMPORT))
      for (const entry of names.split(",")) {
        const name = IMPORTED_CONSTANT.exec(entry.trim())?.[1];
        if (name === undefined || NON_REASON_CONSTANT.test(name)) continue;
        const value = importedConstant(rel, spec, name);
        if (value !== undefined) add(value);
      }
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
