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
  | "global_alias"
  | "unresolved_import"
  | "excluded_import"
  | "stale_allowance";

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

const HOST_MODULES: readonly string[] = ["fs", "os", "worker_threads", "v8"];
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
export const RANDOM_MEMBERS: ReadonlyMap<string, ReadonlySet<string>> = new Map(
  [
    ["Math", new Set(["random"])],
    [
      "crypto",
      new Set(["randomUUID", "getRandomValues", "randomBytes", "randomInt"]),
    ],
    ["process", new Set(["hrtime", "uptime", "cpuUsage"])],
  ],
);
/**
 * Globals whose members the lint checks one by one. Read whole (`const p =
 * process`, `f(Math)`, `globalThis[name]`), a member read through the alias
 * escapes the check, so the whole read is itself a problem.
 */
export const INSPECTED_GLOBALS = new Set([
  ...GLOBAL_OBJECTS,
  ...RANDOM_MEMBERS.keys(),
]);

/**
 * Whether a whole-global reference is used where none of its members can
 * escape: a literal member read, the object of a checked destructuring, an
 * equality operand or a `typeof` operand.
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
    ].includes(parent.operatorToken.kind)
  );
};

export const RANDOM_IMPORTS = new Set([
  "randomBytes",
  "randomUUID",
  "randomInt",
  "randomFill",
  "randomFillSync",
  "getRandomValues",
]);

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
