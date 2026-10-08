import { existsSync, readFileSync } from "node:fs";
import { dirname, relative, resolve } from "node:path";

import ts from "typescript";

/**
 * The §7.2 determinism lint for S3 derivation modules: a derivation is a pure
 * function of facts, class B/C content and the manifest, so it may not read
 * a clock, draw randomness, reach the network or the sidecar, or read the
 * host (its files, its OS, its environment).
 */
export type DeterminismRule =
  | "clock"
  | "randomness"
  | "network_import"
  | "network_global"
  | "host_import"
  | "environment"
  | "unresolved_import"
  | "stale_allowance";

export type DeterminismProblem = Readonly<{
  path: string;
  line: number;
  rule: DeterminismRule;
  text: string;
}>;

export type DeterminismLintOptions = Readonly<{
  /** Extra module specifiers (exact, or a prefix ending in `/`) to refuse. */
  bannedModules?: readonly string[];
}>;

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

const HOST_MODULES: readonly string[] = ["fs", "os"];

/** Names the global object goes by: `globalThis.Date` is `Date`. */
const GLOBAL_OBJECTS = new Set(["globalThis", "global", "window", "self"]);

const CLOCK_GLOBALS = new Set([
  "Date",
  "performance",
  "setTimeout",
  "setInterval",
  "setImmediate",
]);
const NETWORK_GLOBALS = new Set([
  "fetch",
  "WebSocket",
  "XMLHttpRequest",
  "EventSource",
]);
const RANDOM_MEMBERS: ReadonlyMap<string, ReadonlySet<string>> = new Map([
  ["Math", new Set(["random"])],
  [
    "crypto",
    new Set(["randomUUID", "getRandomValues", "randomBytes", "randomInt"]),
  ],
  ["process", new Set(["hrtime", "uptime", "cpuUsage"])],
]);
const RANDOM_IMPORTS = new Set([
  "randomBytes",
  "randomUUID",
  "randomInt",
  "randomFill",
  "randomFillSync",
  "getRandomValues",
]);

const normalise = (specifier: string): string =>
  specifier.replace(/^node:/u, "");

const matchesModule = (name: string, banned: string): boolean =>
  banned.endsWith("/")
    ? name.startsWith(banned)
    : name === banned || name.startsWith(`${banned}/`);

const bannedModuleRule = (
  specifier: string,
  extra: readonly string[],
): DeterminismRule | null => {
  const name = normalise(specifier);
  if (HOST_MODULES.some((banned) => matchesModule(name, banned)))
    return "host_import";
  if (/l1-node-transport/u.test(name)) return "network_import";
  return [...NETWORK_MODULES, ...extra].some((banned) =>
    matchesModule(name, banned),
  )
    ? "network_import"
    : null;
};

/** True where an identifier is a value reference, not a name or a type. */
const isValueReference = (node: ts.Identifier): boolean => {
  const parent = node.parent;
  if (ts.isPropertyAccessExpression(parent) && parent.name === node)
    return false;
  if (ts.isQualifiedName(parent) || ts.isTypeReferenceNode(parent))
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
  if (ts.isImportSpecifier(parent) || ts.isExportSpecifier(parent))
    return false;
  if (ts.isVariableDeclaration(parent) && parent.name === node) return false;
  if (ts.isParameter(parent) && parent.name === node) return false;
  return true;
};

type Scan = Readonly<{
  problems: DeterminismProblem[];
  /** Relative module specifiers the file loads at run time. */
  imports: readonly string[];
}>;

const scanSource = (
  path: string,
  source: string,
  options: DeterminismLintOptions,
): Scan => {
  const file = ts.createSourceFile(path, source, ts.ScriptTarget.Latest, true);
  const extra = options.bannedModules ?? [];
  const problems: DeterminismProblem[] = [];
  const imports: string[] = [];
  /** Local names bound to the `crypto` module (`import * as c`, `import c`). */
  const cryptoAliases = new Set<string>();
  const report = (node: ts.Node, rule: DeterminismRule): void => {
    problems.push({
      path,
      line: file.getLineAndCharacterOfPosition(node.getStart(file)).line + 1,
      rule,
      text: node.getText(file).slice(0, 120),
    });
  };
  const checkSpecifier = (
    node: ts.Node,
    specifier: ts.Expression | undefined,
    typeOnly = false,
  ): void => {
    if (specifier === undefined || !ts.isStringLiteralLike(specifier)) return;
    const rule = bannedModuleRule(specifier.text, extra);
    if (rule !== null) report(node, rule);
    if (!typeOnly && specifier.text.startsWith("."))
      imports.push(specifier.text);
  };
  /** The global a reference names: `Date`, `globalThis.Date` and `globalThis["Date"]` alike. */
  const globalName = (node: ts.Expression): string | null => {
    if (ts.isIdentifier(node))
      return cryptoAliases.has(node.text) ? "crypto" : node.text;
    if (
      ts.isPropertyAccessExpression(node) &&
      ts.isIdentifier(node.expression) &&
      GLOBAL_OBJECTS.has(node.expression.text)
    )
      return node.name.text;
    if (
      ts.isElementAccessExpression(node) &&
      ts.isIdentifier(node.expression) &&
      GLOBAL_OBJECTS.has(node.expression.text) &&
      ts.isStringLiteralLike(node.argumentExpression)
    )
      return node.argumentExpression.text;
    return null;
  };
  /** The rule a member read `base.member` breaks, if any. */
  const memberRule = (base: string, member: string): DeterminismRule | null => {
    if (base === "process" && member === "env") return "environment";
    if (RANDOM_MEMBERS.get(base)?.has(member) === true)
      return base === "process" ? "clock" : "randomness";
    return null;
  };
  const globalRule = (name: string): DeterminismRule | null =>
    CLOCK_GLOBALS.has(name)
      ? "clock"
      : NETWORK_GLOBALS.has(name)
        ? "network_global"
        : null;
  const visit = (node: ts.Node): void => {
    if (ts.isImportDeclaration(node)) {
      const clause = node.importClause;
      const bindings = clause?.namedBindings;
      const typeOnly =
        clause !== undefined &&
        (clause.isTypeOnly ||
          (clause.name === undefined &&
            bindings !== undefined &&
            ts.isNamedImports(bindings) &&
            bindings.elements.length > 0 &&
            bindings.elements.every((element) => element.isTypeOnly)));
      checkSpecifier(node, node.moduleSpecifier, typeOnly);
      const module = ts.isStringLiteralLike(node.moduleSpecifier)
        ? normalise(node.moduleSpecifier.text)
        : "";
      if (module === "crypto" && clause !== undefined && !clause.isTypeOnly) {
        if (clause.name !== undefined) cryptoAliases.add(clause.name.text);
        if (bindings !== undefined && ts.isNamespaceImport(bindings))
          cryptoAliases.add(bindings.name.text);
        if (bindings !== undefined && ts.isNamedImports(bindings))
          for (const element of bindings.elements)
            if (RANDOM_IMPORTS.has((element.propertyName ?? element.name).text))
              report(element, "randomness");
      }
    } else if (ts.isExportDeclaration(node)) {
      checkSpecifier(node, node.moduleSpecifier, node.isTypeOnly);
    } else if (ts.isCallExpression(node)) {
      const callee = node.expression;
      if (
        callee.kind === ts.SyntaxKind.ImportKeyword ||
        (ts.isIdentifier(callee) && callee.text === "require")
      )
        checkSpecifier(node, node.arguments[0]);
    } else if (ts.isPropertyAccessExpression(node)) {
      const base = globalName(node.expression);
      const rule =
        (base === null ? null : memberRule(base, node.name.text)) ??
        (ts.isIdentifier(node.expression) &&
        GLOBAL_OBJECTS.has(node.expression.text)
          ? globalRule(node.name.text)
          : null);
      if (rule !== null) report(node, rule);
    } else if (
      ts.isElementAccessExpression(node) &&
      ts.isStringLiteralLike(node.argumentExpression)
    ) {
      const base = globalName(node.expression);
      const member = node.argumentExpression.text;
      const rule =
        (base === null ? null : memberRule(base, member)) ??
        (ts.isIdentifier(node.expression) &&
        GLOBAL_OBJECTS.has(node.expression.text)
          ? globalRule(member)
          : null);
      if (rule !== null) report(node, rule);
    } else if (
      ts.isVariableDeclaration(node) &&
      ts.isObjectBindingPattern(node.name) &&
      node.initializer !== undefined
    ) {
      // `const { env } = process`, `const { random } = Math`.
      const base = globalName(node.initializer);
      if (base !== null)
        for (const element of node.name.elements) {
          const member = element.propertyName ?? element.name;
          if (!ts.isIdentifier(member)) continue;
          const rule =
            memberRule(base, member.text) ??
            (GLOBAL_OBJECTS.has(base) ? globalRule(member.text) : null);
          if (rule !== null) report(element, rule);
        }
    } else if (ts.isIdentifier(node) && isValueReference(node)) {
      const rule = globalRule(node.text);
      if (rule !== null) report(node, rule);
    }
    ts.forEachChild(node, visit);
  };
  visit(file);
  return { problems, imports };
};

/** Lints one source file. */
export const lintDeterminismSource = (
  path: string,
  source: string,
  options: DeterminismLintOptions = {},
): DeterminismProblem[] => scanSource(path, source, options).problems;

/** Lints several S3 modules; empty means every module is deterministic. */
export const lintDeterminism = (
  files: readonly Readonly<{ path: string; source: string }>[],
  options: DeterminismLintOptions = {},
): DeterminismProblem[] =>
  files.flatMap((file) =>
    lintDeterminismSource(file.path, file.source, options),
  );

export type DeterminismModulesOptions = DeterminismLintOptions &
  Readonly<{
    /** The directory the globs are relative to (a package root). */
    root: string;
    /**
     * Globs naming the projection's entry modules, relative to `root`
     * (for example `src/l1-events/*.ts`).
     */
    include: readonly string[];
    /** Globs never linted or followed, relative to `root`. */
    exclude?: readonly string[];
    /**
     * Problems the role accepts, each with its reason: every problem with
     * this path, rule and text. An allowance no problem matches is itself
     * a `stale_allowance` problem, so the list cannot outlive the code.
     */
    allow?: readonly DeterminismAllowance[];
  }>;

export type DeterminismAllowance = Readonly<{
  path: string;
  rule: DeterminismRule;
  text: string;
  reason: string;
}>;

export type DeterminismModulesReport = Readonly<{
  /** Every module linted, relative to `root`, sorted. */
  files: readonly string[];
  problems: readonly DeterminismProblem[];
}>;

/** Test files inject clocks on purpose; they are never linted or followed. */
const TEST_FILES: readonly string[] = [
  "**/*.test.ts",
  "**/*.spec.ts",
  "**/tests/**",
  "**/*.d.ts",
];

/** A data file a raw import loads (`./x.sql`, `./x.sql?raw`): never code. */
const DATA_IMPORT = /\.(?!(?:[cm]?[jt]sx?)$)[a-z0-9]+(?:\?[a-z]+)?$/iu;

/** The source file a relative `.js`-style specifier names, or null. */
const resolveRelative = (from: string, specifier: string): string | null => {
  const base = resolve(dirname(from), specifier);
  const candidates = /\.[cm]?js$/u.test(base)
    ? [base.replace(/js$/u, "ts"), base.replace(/\.[cm]?js$/u, ".tsx")]
    : /\.[cm]?tsx?$/u.test(base)
      ? [base]
      : [`${base}.ts`, `${base}.tsx`, resolve(base, "index.ts")];
  return candidates.find((candidate) => existsSync(candidate)) ?? null;
};

/**
 * Lints every module the `include` globs match under `root`, and every
 * module they load through relative imports, transitively (type-only
 * imports excluded): a helper a derivation calls is held to the same rules
 * as the derivation. Package imports are checked against the banned
 * modules, not followed. A relative import of a data file that exists
 * (`.json`, `.sql`) is not code; any other relative import that resolves to
 * no source file is a problem, so the walk cannot skip a module silently.
 */
export const lintDeterminismModules = (
  options: DeterminismModulesOptions,
): DeterminismModulesReport => {
  const root = resolve(options.root);
  const exclude = [...TEST_FILES, ...(options.exclude ?? [])];
  const excluded = new Set(
    ts.sys
      .readDirectory(root, [".ts", ".tsx", ".mts"], undefined, exclude)
      .map((path) => resolve(path)),
  );
  const pending = ts.sys
    .readDirectory(root, [".ts", ".tsx", ".mts"], exclude, options.include)
    .map((path) => resolve(path));
  const seen = new Set<string>();
  const problems: DeterminismProblem[] = [];
  for (let path = pending.pop(); path !== undefined; path = pending.pop()) {
    if (seen.has(path) || excluded.has(path) || path.includes("/node_modules/"))
      continue;
    seen.add(path);
    const shown = relative(root, path);
    const scan = scanSource(shown, readFileSync(path, "utf8"), options);
    problems.push(...scan.problems);
    for (const specifier of scan.imports) {
      const data = DATA_IMPORT.exec(specifier);
      if (
        data !== null &&
        existsSync(resolve(dirname(path), specifier.replace(/\?[a-z]+$/iu, "")))
      )
        continue;
      const target = resolveRelative(path, specifier);
      if (target === null)
        problems.push({
          path: shown,
          line: 0,
          rule: "unresolved_import",
          text: specifier,
        });
      else pending.push(target);
    }
  }
  const allow = options.allow ?? [];
  const matches = (
    entry: DeterminismAllowance,
    problem: DeterminismProblem,
  ): boolean =>
    entry.path === problem.path &&
    entry.rule === problem.rule &&
    entry.text === problem.text;
  const kept = problems.filter(
    (problem) => !allow.some((entry) => matches(entry, problem)),
  );
  for (const entry of allow)
    if (!problems.some((problem) => matches(entry, problem)))
      kept.push({
        path: entry.path,
        line: 0,
        rule: "stale_allowance",
        text: entry.text,
      });
  return {
    files: [...seen].map((path) => relative(root, path)).sort(),
    problems: kept.sort(
      (a, b) => a.path.localeCompare(b.path, "en") || a.line - b.line,
    ),
  };
};
