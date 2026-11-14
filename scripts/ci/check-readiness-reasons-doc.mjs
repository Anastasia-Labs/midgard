#!/usr/bin/env node
// Every reason the node can put in `/readyz` must have a row in the readiness
// catalogue (`services/readiness-catalogue*.ts`), and every code the
// catalogue holds operator text in the reasons doc.
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
// The catalogue's codes are read from its row calls: `reason(`, `detail(`,
// `both(` or `field(` of a string literal or of a constant, resolved where
// the catalogue module imports it from, and each such call mapped over a
// constant list (`...NAMES.map((name) => reason(name))`), whose members are
// its string literals or the `name: null` keys of a `members` record. A row
// this cannot resolve fails, by name. A code is documented when the reasons
// section of the doc names it in backticks, alone or as the prefix of a
// parameterised form (`name:<n>`); a `field` row, a code shown only inside a
// nested report, needs no text of its own.
//
// Run from anywhere: `node scripts/ci/check-readiness-reasons-doc.mjs
// [--root <tree>]`. Exit 0 when every reason is catalogued and every code
// documented, 1 otherwise.

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
/** The directory and file-name prefix of the catalogue's modules. */
export const CATALOGUE = `${NODE_SRC}/services/readiness-catalogue`;

/** The modules that answer `/readyz`: serving, and while the node starts. */
export const READINESS_ASSEMBLERS = [
  "commands/http/readiness.ts",
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

const ROW_CALL = /\b(reason|detail|both|field)\(\s*(?:"([^"]+)"|([\w$]+))/gu;
const ROW_MAP =
  /\.\.\.([A-Z][A-Z0-9_]*)\.map\(\s*\(\s*([\w$]+)\s*\)\s*=>\s*(reason|detail|both|field)\(\s*\2\b/gu;

/** The string value of `const name = "..."` in `text`, or undefined. */
const stringConstant = (text, name) =>
  new RegExp(
    `\\bconst\\s+${name}\\s*(?::[^=\\n]+)?=\\s*\\n?\\s*"([^"]+)"`,
    "u",
  ).exec(text)?.[1];

/** The members of the list `const name = [...]` or `members<...>({...})`. */
const listMembers = (text, name) => {
  const at = new RegExp(`\\bconst\\s+${name}\\b[^=]*=\\s*`, "u").exec(text);
  if (at === null) return undefined;
  const rest = text.slice(at.index + at[0].length);
  if (rest.startsWith("["))
    return [...bracketed(rest, 0).matchAll(/"([^"]+)"/gu)].map(([, v]) => v);
  const record = rest.startsWith("members<") ? rest.indexOf(">({") : -1;
  if (record < 0) return undefined;
  return [
    ...bracketed(rest, record + 2).matchAll(/([\w$]+)\s*:\s*null\b/gu),
  ].map(([, key]) => key);
};

/**
 * The codes the readiness catalogue holds, each with its kind. A row whose
 * code this cannot resolve to a string fails, by name.
 */
export const cataloguedCodes = (root = ROOT) => {
  const dir = dirname(join(root, CATALOGUE));
  const prefix = `${CATALOGUE.split("/").at(-1)}.`;
  const followerConstants = constantsUnder(join(root, FOLLOWER_SRC));
  const codes = new Map();
  for (const name of readdirSync(dir).filter(
    (each) => each.startsWith(prefix) && each.endsWith(".ts"),
  )) {
    const path = join(dir, name);
    const text = readFileSync(path, "utf8").replace(
      /\/\*[\s\S]*?\*\/|\/\/[^\n]*/gu,
      "",
    );
    const imports = new Map(
      importsOf(text).map((binding) => [binding.local, binding]),
    );
    /** Where `name` is declared: this module, or the one it imports it from. */
    const resolved = (constant, read) => {
      const local = read(text, constant);
      if (local !== undefined) return local;
      const binding = imports.get(constant);
      if (binding === undefined) return undefined;
      if (FOLLOWER_PACKAGE.test(binding.spec))
        return read === stringConstant
          ? followerConstants.get(binding.name)
          : undefined;
      const source = declaringModule(
        moduleFile(path, binding.spec),
        binding.name,
      );
      return source === undefined
        ? undefined
        : read(readFileSync(source, "utf8"), binding.name);
    };
    const add = (code, kind) => {
      if (codes.has(code)) throw new Error(`${name}: ${code} catalogued twice`);
      codes.set(code, kind);
    };
    const mapped = [...text.matchAll(ROW_MAP)];
    for (const [, list, , kind] of mapped) {
      const members = resolved(list, listMembers);
      if (members === undefined || members.length === 0)
        throw new Error(`${name}: no members for ${list}`);
      for (const code of members) add(code, kind);
    }
    for (const call of text.matchAll(ROW_CALL)) {
      const [whole, kind, literal, constant] = call;
      if (
        mapped.some(
          ({ index, 0: spread }) =>
            call.index > index && call.index < index + spread.length,
        )
      )
        continue;
      const code = literal ?? resolved(constant, stringConstant);
      if (code === undefined) throw new Error(`${name}: row ${whole}`);
      add(code, kind);
    }
  }
  if (codes.size === 0) throw new Error(`${CATALOGUE}*.ts: no rows`);
  return codes;
};

/** The reasons the catalogue does not hold. */
export const uncataloguedReasons = (reasons, catalogued) =>
  [...reasons].filter((reason) => !catalogued.has(reason)).sort();

const main = (root) => {
  const docText = readFileSync(join(root, DOC), "utf8");
  const section = reasonsSection(docText);
  if (section === undefined) {
    console.error(`${DOC}: no "${DOC_SECTION}" section`);
    return 1;
  }
  const catalogued = cataloguedCodes(root);
  const reasons = nodeReadinessReasons(root);
  const lines = [...catalogued].flatMap(([code, kind]) =>
    kind === "field" ? [] : [code],
  );
  const missing = [
    [
      `${CATALOGUE}*.ts`,
      "has no row for",
      uncataloguedReasons(reasons.keys(), catalogued),
    ],
    [
      DOC,
      "has no operator text for",
      undocumentedReasons(new Set([...reasons.keys(), ...lines]), section),
    ],
  ].filter(([, , names]) => names.length > 0);
  if (missing.length === 0) {
    console.log(
      `${reasons.size} node readiness reasons, all in the ${catalogued.size}-code catalogue; every code documented in ${DOC}`,
    );
    return 0;
  }
  for (const [file, lacks, names] of missing) {
    console.error(`${file} ${lacks}:`);
    for (const reason of names)
      console.error(
        `  ${reason} (${reasons.get(reason)?.join(", ") ?? "catalogued"})`,
      );
  }
  return 1;
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const at = process.argv.indexOf("--root");
  process.exit(main(at < 0 ? ROOT : resolve(process.argv[at + 1])));
}
