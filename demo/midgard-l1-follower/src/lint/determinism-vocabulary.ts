import ts from "typescript";

/**
 * What the determinism lint (determinism.ts) refuses: the banned modules and
 * globals, and the syntax tests that tell a run-time read of a global from a
 * name or a type.
 */
export type DeterminismRule =
  | "clock"
  | "randomness"
  | "network_import"
  | "network_global"
  | "host_import"
  | "environment"
  | "implicit_locale"
  | "dynamic_code"
  | "global_alias"
  | "unresolved_import"
  | "excluded_import"
  | "stale_allowance"
  | "duplicate_allowance";

const NETWORK_MODULES: readonly string[] = [
  "http",
  "https",
  "http2",
  "net",
  "tls",
  "dgram",
  "dns",
  "child_process",
  "undici",
  "axios",
  "node-fetch",
  "ws",
  "pg",
  "@lucid-evolution/provider",
  "@cardano-ogmios/client",
  "@al-ft/l1-node-transport",
];

/** `module` loads code from the host's files (`createRequire`). */
const HOST_MODULES: readonly string[] = [
  "fs",
  "os",
  "worker_threads",
  "v8",
  "module",
];
/** Modules that are the clock: their exports read it or wait on it. */
const CLOCK_MODULES: readonly string[] = ["perf_hooks", "timers"];
/** The `process` module: its exports (`env`, `hrtime`, `argv`) read the host. */
const ENVIRONMENT_MODULES: readonly string[] = ["process"];

/** Names the global object goes by: `globalThis.Date` is `Date`. */
export const GLOBAL_OBJECTS = new Set([
  "globalThis",
  "global",
  "window",
  "self",
]);

export const CLOCK_GLOBALS = new Set([
  "Date",
  "performance",
  "setTimeout",
  "setInterval",
  "setImmediate",
]);
export const NETWORK_GLOBALS = new Set([
  "fetch",
  "WebSocket",
  "XMLHttpRequest",
  "EventSource",
]);
/** Globals that run code built from a string at run time. */
export const DYNAMIC_CODE_GLOBALS = new Set(["eval", "Function"]);

/**
 * The `crypto` exports that draw randomness: the random draws, key and prime
 * generation, and the Web Crypto object (`webcrypto`, `subtle`) whose members
 * generate keys and random values.
 */
export const RANDOM_IMPORTS = new Set([
  "randomBytes",
  "randomUUID",
  "randomInt",
  "randomFill",
  "randomFillSync",
  "getRandomValues",
  "generateKey",
  "generateKeySync",
  "generateKeyPair",
  "generateKeyPairSync",
  "generatePrime",
  "generatePrimeSync",
  "webcrypto",
  "subtle",
]);

/** Members of a global that read the clock or draw randomness, by global. */
export const MEMBER_RULES: ReadonlyMap<
  string,
  ReadonlyMap<string, DeterminismRule>
> = new Map<string, ReadonlyMap<string, DeterminismRule>>([
  ["Math", new Map([["random", "randomness"]])],
  [
    "crypto",
    new Map([...RANDOM_IMPORTS].map((member) => [member, "randomness"])),
  ],
  [
    "process",
    new Map([
      ["hrtime", "clock"],
      ["uptime", "clock"],
      ["cpuUsage", "clock"],
    ]),
  ],
  ["AbortSignal", new Map([["timeout", "clock"]])],
]);

/**
 * The `Intl` constructors that format or compare by a locale: called with no
 * locale they read the host's (and `DateTimeFormat` with no `timeZone` reads
 * the host's time zone).
 */
export const INTL_LOCALE_CONSTRUCTORS = new Set([
  "Collator",
  "DateTimeFormat",
  "DisplayNames",
  "DurationFormat",
  "ListFormat",
  "NumberFormat",
  "PluralRules",
  "RelativeTimeFormat",
  "Segmenter",
]);

/**
 * Methods that format or compare by a locale, with the index of their
 * locale argument: left out, they read the host's locale. The date ones also
 * read the host's time zone unless their options name one.
 */
export const LOCALE_METHODS: ReadonlyMap<string, number> = new Map([
  ["localeCompare", 1],
  ["toLocaleString", 0],
  ["toLocaleDateString", 0],
  ["toLocaleTimeString", 0],
  ["toLocaleUpperCase", 0],
  ["toLocaleLowerCase", 0],
]);
export const ZONED_LOCALE_METHODS = new Set([
  "toLocaleDateString",
  "toLocaleTimeString",
]);

/**
 * The rule a member read `base.member` of a global breaks, if any. Every
 * member of `process` reads the host (its environment, arguments, platform,
 * working directory, memory) except the clock ones, which are `clock`.
 */
export const memberRule = (
  base: string,
  member: string,
): DeterminismRule | null =>
  MEMBER_RULES.get(base)?.get(member) ??
  (base === "process" ? "environment" : null);

/**
 * Globals whose members the lint checks one by one. Read whole (`const p =
 * process`, `f(Math)`, `globalThis[name]`), a member read through the alias
 * escapes the check, so the whole read is itself a problem.
 */
export const INSPECTED_GLOBALS = new Set([
  ...GLOBAL_OBJECTS,
  ...MEMBER_RULES.keys(),
  "Intl",
]);

/**
 * Whether a whole-global reference is used where none of its members can
 * escape: a literal member read, the object of a checked destructuring, an
 * equality or `instanceof` operand, or a `typeof` operand.
 */
export const isContainedGlobalRead = (node: ts.Expression): boolean => {
  const parent = node.parent;
  if (ts.isPropertyAccessExpression(parent) && parent.expression === node)
    return true;
  if (ts.isElementAccessExpression(parent) && parent.expression === node)
    return ts.isStringLiteralLike(parent.argumentExpression);
  if (
    ts.isVariableDeclaration(parent) &&
    parent.initializer === node &&
    ts.isObjectBindingPattern(parent.name)
  )
    return true;
  if (ts.isTypeOfExpression(parent)) return true;
  return (
    ts.isBinaryExpression(parent) &&
    [
      ts.SyntaxKind.EqualsEqualsEqualsToken,
      ts.SyntaxKind.ExclamationEqualsEqualsToken,
      ts.SyntaxKind.EqualsEqualsToken,
      ts.SyntaxKind.ExclamationEqualsToken,
      ts.SyntaxKind.InstanceOfKeyword,
    ].includes(parent.operatorToken.kind)
  );
};

export const normalise = (specifier: string): string =>
  specifier.replace(/^node:/u, "");

const matchesModule = (name: string, banned: string): boolean =>
  banned.endsWith("/")
    ? name.startsWith(banned)
    : name === banned || name.startsWith(`${banned}/`);

export const bannedModuleRule = (
  specifier: string,
  extra: readonly string[],
): DeterminismRule | null => {
  const name = normalise(specifier);
  if (HOST_MODULES.some((banned) => matchesModule(name, banned)))
    return "host_import";
  if (CLOCK_MODULES.some((banned) => matchesModule(name, banned)))
    return "clock";
  if (ENVIRONMENT_MODULES.some((banned) => matchesModule(name, banned)))
    return "environment";
  if (/l1-node-transport/u.test(name)) return "network_import";
  return [...NETWORK_MODULES, ...extra].some((banned) =>
    matchesModule(name, banned),
  )
    ? "network_import"
    : null;
};

/** True where an identifier is a value reference, not a name or a type. */
export const isValueReference = (node: ts.Identifier): boolean => {
  const parent = node.parent;
  if (ts.isPropertyAccessExpression(parent) && parent.name === node)
    return false;
  // `typeof setTimeout` in a type reads nothing at run time.
  if (
    ts.isQualifiedName(parent) ||
    ts.isTypeReferenceNode(parent) ||
    ts.isTypeQueryNode(parent)
  )
    return false;
  if (
    ts.isExpressionWithTypeArguments(parent) &&
    ts.isHeritageClause(parent.parent)
  )
    return (
      parent.parent.token === ts.SyntaxKind.ExtendsKeyword &&
      ts.isClassLike(parent.parent.parent)
    );
  if (
    (ts.isPropertyAssignment(parent) ||
      ts.isPropertyDeclaration(parent) ||
      ts.isPropertySignature(parent) ||
      ts.isMethodDeclaration(parent) ||
      ts.isMethodSignature(parent)) &&
    parent.name === node
  )
    return false;
  if (
    ts.isImportSpecifier(parent) ||
    ts.isExportSpecifier(parent) ||
    ts.isImportClause(parent) ||
    ts.isNamespaceImport(parent)
  )
    return false;
  if (ts.isVariableDeclaration(parent) && parent.name === node) return false;
  if (ts.isParameter(parent) && parent.name === node) return false;
  return true;
};
