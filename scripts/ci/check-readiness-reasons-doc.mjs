#!/usr/bin/env node
// Every reason the node can put in `/readyz` must have operator text in the
// reasons doc and an entry in the public presentation.
//
// The reasons are derived from the node's and the L1 follower package's
// source rather than kept in a list here. A readiness module is one through
// which a reason reaches `/readyz`:
//   - an assembler (READINESS_ASSEMBLERS, the modules that answer `/readyz`);
//   - a module whose function a readiness module calls into `reasons`: the
//     callee of a value pushed onto, spread into or iterated into `reasons`
//     (`reasons.push(...holds.reasons)` after `const holds = f(...)`), in
//     the module that declares it, through re-exports (`f` reaches the build
//     holds and the liveness reasons the L1 control plane derives this way);
//   - a module that raises a liveness incident (`raiseLivenessIncident(`),
//     declares or reports a follower-driver hold or intent wait
//     (`DriverHold`, `IntentPredicateWait`), runs or fails a startup step
//     under a reason (`retryStartupStep(`, `reportStartupWaiting(`,
//     `startupStepFailed(`), or exits once a transient budget is spent
//     (`signalTransientExhausted(`);
//   - a module READINESS_MODULES names, one that hands its reasons on some
//     other way.
// In a readiness module a reason is:
//   - a constant whose value is a snake_case name, `const NAME = "a_b"`, unless
//     the constant names something other than a reason (NON_REASON_CONSTANT:
//     a liveness source, a startup step, a due-work kind, a table, a key);
//   - the leading snake_case name of a string put in `reasons`, returned or
//     set as `reason:` (also as its `??` fallback);
//   - an imported constant whose value is a snake_case name, resolved in the
//     node module or, for `@al-ft/midgard-l1-follower` imports, in the
//     follower package (`wallet_seed_pending` reaches a hold this way).
// The node also forwards the follower's `FollowStatus.readiness`: every member
// of the follower's FOLLOWER_READINESS_TYPE union is a reason, whether a
// `typeof NAME` constant, a string literal or another string-literal union
// (the interventions).
// NOT_READINESS lists the names this finds that are not reasons.
// A reason is documented when the reasons section of the doc names it in
// backticks, alone or as the prefix of a parameterised form (`name:<n>`). It
// is presented when the branch of the public presentation that renders a
// reason names it, shown or redacted, there or in a constant the branch reads
// (the shown set, the bounded formats).
//
// Run from anywhere: `node scripts/ci/check-readiness-reasons-doc.mjs
// [--root <tree>]`. Exit 0 when every reason is documented and presented, 1
// otherwise.

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
export const PRESENTATION = `${NODE_SRC}/commands/listen-readiness-public-presentation.ts`;
/** The guard of the presentation's branch that renders a reason. */
export const PRESENTATION_BRANCH = 'key === "reasons"';

/** The modules that answer `/readyz`: serving, and while the node starts. */
export const READINESS_ASSEMBLERS = [
  "commands/listen-router.get-readiness-handler.ts",
  "commands/listen.startup-http.ts",
];

/** Readiness modules no assembler call or reason channel reaches. */
export const READINESS_MODULES = [
  "services/intent-journal.refusals.ts",
  "l1-operator-set/snapshot.ts",
];

/** A module that raises, holds, waits or fails under a named reason. */
const REASON_CHANNEL =
  /\bDriverHold\b|\bIntentPredicateWait\b|\b(?:raiseLivenessIncident|retryStartupStep|reportStartupWaiting|startupStepFailed|signalTransientExhausted)\(/u;

/** Constant names whose value is not a readiness reason. */
const NON_REASON_CONSTANT =
  /(?:^|_)(?:SOURCE|STEP|KIND|KEY|TABLE|NAMESPACE|SCOPE|HOLDER|METRIC)$/u;

/** Names a readiness module uses that never reach `/readyz`, with why. */
export const NOT_READINESS = new Map([
  [
    "manifest_mismatch",
    "the operator watchdog's strike-gate verdict; readiness carries operator_watchdog_manifest_mismatch",
  ],
  [
    "commit_stage_breaker",
    "a commit build alarm the node reports under details only; it leaves the node ready",
  ],
]);

const SNAKE = "[a-z][a-z0-9]*(?:_[a-z0-9]+)+";
const CONSTANT = new RegExp(
  `\\bconst\\s+([A-Z][A-Z0-9_]*)\\s*(?::[^=\\n]+)?=\\s*\\n?\\s*"(${SNAKE})"`,
  "gu",
);
const REPORTED = new RegExp(
  `(?:\\breturn|\\breason:(?:[^,\\n]*\\?\\?)?)\\s*["\`](${SNAKE})(?=[:"\`])`,
  "gu",
);
const LITERAL = new RegExp(`["\`](${SNAKE})(?=[:"\`])`, "gu");
/** Where a value enters `reasons`: the bracket after the match holds it. */
const SINK =
  /\breasons\.push\(|\breasons\s*[:=]\s*\[|\bfor\s*(?=\(\s*const\s+(?:reason|\{\s*reason\s*\}|\[[^\]]*\breason\s*\])\s+of\b)/gu;
const IDENTIFIER = /[A-Za-z_$][\w$]*/gu;

const IMPORT = /\bimport\s+(type\s+)?\{([^}]*)\}\s*from\s*"([^"]+)"/gu;
const IMPORTED = /^(type\s+)?([\w$]+)(?:\s+as\s+([\w$]+))?$/u;

const tsFiles = (dir) =>
  readdirSync(dir).flatMap((name) => {
    const path = join(dir, name);
    if (statSync(path).isDirectory()) return tsFiles(path);
    return path.endsWith(".ts") && !path.endsWith(".d.ts") ? [path] : [];
  });

/** The text from the bracket at `open` to the one that closes it. */
const bracketed = (text, open) => {
  const close = { "(": ")", "[": "]", "{": "}" };
  const pending = [];
  for (let at = open; at < text.length; at += 1) {
    if (close[text[at]] !== undefined) pending.push(close[text[at]]);
    else if (text[at] === pending.at(-1)) {
      pending.pop();
      if (pending.length === 0) return text.slice(open, at + 1);
    }
  }
  return text.slice(open);
};

/** Each `import { ... }` binding of a module: local name, exported name,
 * specifier, and whether it imports a type only. */
const importsOf = (text) =>
  [...text.matchAll(IMPORT)].flatMap(([, typeOnly, names, spec]) =>
    names.split(",").flatMap((entry) => {
      const match = IMPORTED.exec(entry.trim());
      if (match === null) return [];
      const [, typeEntry, name, local] = match;
      const type = typeOnly !== undefined || typeEntry !== undefined;
      return [{ local: local ?? name, name, spec, type }];
    }),
  );

/** The `.ts` file a relative specifier from `path` names, or undefined. */
const moduleFile = (path, spec) => {
  if (!spec.startsWith(".")) return undefined;
  const target = join(dirname(path), spec.replace(/\.js$/u, ".ts"));
  return existsSync(target) ? target : undefined;
};

/** The module that declares `name` exported by `path`, through re-exports. */
const declaringModule = (path, name, seen = new Set()) => {
  if (path === undefined || seen.has(path)) return undefined;
  seen.add(path);
  const text = readFileSync(path, "utf8");
  if (new RegExp(`\\b(?:const|function|class)\\s+${name}\\b`, "u").test(text))
    return path;
  for (const [, names, spec] of text.matchAll(
    /\bexport\s*(?:\{([^}]*)\}|\*)\s*from\s*"([^"]+)"/gu,
  )) {
    const entry = names
      ?.split(",")
      .map((each) => each.trim().split(/\s+as\s+/u))
      .find((each) => (each[1] ?? each[0]) === name);
    if (names !== undefined && entry === undefined) continue;
    const found = declaringModule(
      moduleFile(path, spec),
      entry?.[0] ?? name,
      seen,
    );
    if (found !== undefined) return found;
  }
  return undefined;
};

/** The text of every place `text` puts a value in `reasons`. */
const sinksOf = (text) =>
  [...text.matchAll(SINK)].map(({ index }) =>
    bracketed(text, index + text.slice(index).search(/[([]/u)),
  );

/**
 * The modules `path` calls into `reasons`: each imported value a sink names,
 * directly or as the callee of the local constant it names.
 */
const calledProducers = (path, text, sinks) => {
  const imports = new Map(
    importsOf(text)
      .filter(({ type }) => !type)
      .map((binding) => [binding.local, binding]),
  );
  const callee = new Map();
  for (const [, name, root] of text.matchAll(
    /\bconst\s+([\w$]+)\s*(?::[^=\n]+)?=\s*(?:yield\*\s*|await\s+)?([\w$]+)\s*\(/gu,
  ))
    callee.set(name, [...(callee.get(name) ?? []), root]);
  const producers = new Set();
  for (const sink of sinks)
    for (const name of sink.match(IDENTIFIER) ?? [])
      for (const each of [name, ...(callee.get(name) ?? [])]) {
        const binding = imports.get(each);
        if (binding === undefined) continue;
        const found = declaringModule(
          moduleFile(path, binding.spec),
          binding.name,
        );
        if (found !== undefined) producers.add(found);
      }
  return producers;
};

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
  const listed = [...READINESS_ASSEMBLERS, ...READINESS_MODULES];
  for (const rel of listed) statSync(join(src, rel)); // a moved readiness module fails here, by name
  const followerConstants = constantsUnder(join(root, FOLLOWER_SRC));
  const importedConstant = (path, spec, name) => {
    if (FOLLOWER_PACKAGE.test(spec)) return followerConstants.get(name);
    const target = moduleFile(path, spec);
    return target === undefined
      ? undefined
      : constantsOf(readFileSync(target, "utf8")).get(name);
  };
  const reasons = followerReadinessReasons(root);
  const pending = [
    ...listed.map((rel) => join(src, rel)),
    ...tsFiles(src).filter((path) =>
      REASON_CHANNEL.test(readFileSync(path, "utf8")),
    ),
  ];
  for (const seen = new Set(); pending.length > 0; ) {
    const path = pending.shift();
    if (seen.has(path)) continue;
    seen.add(path);
    const rel = relative(src, path);
    const text = readFileSync(path, "utf8");
    const add = (reason) =>
      NOT_READINESS.has(reason) ||
      reasons.set(reason, [...(reasons.get(reason) ?? []), rel]);
    const sinks = sinksOf(text);
    for (const [name, value] of constantsOf(text))
      if (!NON_REASON_CONSTANT.test(name)) add(value);
    for (const [, value] of text.matchAll(REPORTED)) add(value);
    for (const sink of sinks)
      for (const [, value] of sink.matchAll(LITERAL)) add(value);
    for (const { name, spec } of importsOf(text)) {
      if (!/^[A-Z][A-Z0-9_]*$/u.test(name) || NON_REASON_CONSTANT.test(name))
        continue;
      const value = importedConstant(path, spec, name);
      if (value !== undefined) add(value);
    }
    pending.push(...calledProducers(path, text, sinks));
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

/** The snake_case names in `text`, a regex's `a_(?:b|c)` spelled out. */
const snakeNames = (text) => {
  const group = /([a-z0-9_]*)\(\?:([a-z0-9_|]+)\)([a-z0-9_]*)/u;
  let spelled = text;
  for (let match; (match = group.exec(spelled)) !== null; ) {
    const [whole, before, alternatives, after] = match;
    const each = alternatives.split("|").map((alt) => before + alt + after);
    spelled = `${spelled.slice(0, match.index)}${each.join("|")}${spelled.slice(match.index + whole.length)}`;
  }
  return spelled.match(new RegExp(`(?<![\\w])${SNAKE}(?![\\w])`, "gu")) ?? [];
};

/**
 * The reasons the public presentation names, shown or redacted: those the
 * branch PRESENTATION_BRANCH guards names, and those of the constants it
 * reads. Undefined when the presentation has no such branch.
 */
export const presentedReasons = (text) => {
  const guard = text.indexOf(PRESENTATION_BRANCH);
  const open = guard < 0 ? -1 : text.indexOf("{", guard);
  if (open < 0) return undefined;
  const branch = bracketed(text, open);
  const read = new Set(branch.match(IDENTIFIER));
  const constants = [
    ...text.matchAll(/\bconst\s+([\w$]+)[^=]*=([\s\S]*?);\n/gu),
  ].flatMap(([, name, value]) => (read.has(name) ? [value] : []));
  return new Set(snakeNames([branch, ...constants].join("\n")));
};

/** The reasons the presentation does not name. */
export const unpresentedReasons = (reasons, presented) =>
  [...reasons].filter((reason) => !presented.has(reason)).sort();

const main = (root) => {
  const docText = readFileSync(join(root, DOC), "utf8");
  const section = reasonsSection(docText);
  if (section === undefined) {
    console.error(`${DOC}: no "${DOC_SECTION}" section`);
    return 1;
  }
  const presented = presentedReasons(
    readFileSync(join(root, PRESENTATION), "utf8"),
  );
  if (presented === undefined) {
    console.error(`${PRESENTATION}: no \`${PRESENTATION_BRANCH}\` branch`);
    return 1;
  }
  const reasons = nodeReadinessReasons(root);
  const missing = [
    [
      DOC,
      "has no operator text for",
      undocumentedReasons(reasons.keys(), section),
    ],
    [
      PRESENTATION,
      "has no entry for",
      unpresentedReasons(reasons.keys(), presented),
    ],
  ].filter(([, , names]) => names.length > 0);
  if (missing.length === 0) {
    console.log(
      `${reasons.size} node readiness reasons, all documented in ${DOC} and presented in ${PRESENTATION}`,
    );
    return 0;
  }
  for (const [file, lacks, names] of missing) {
    console.error(`${file} ${lacks}:`);
    for (const reason of names)
      console.error(`  ${reason} (${reasons.get(reason).join(", ")})`);
  }
  return 1;
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const at = process.argv.indexOf("--root");
  process.exit(main(at < 0 ? ROOT : resolve(process.argv[at + 1])));
}
