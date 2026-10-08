import { existsSync, readFileSync } from "node:fs";
import { dirname, relative, resolve } from "node:path";

import ts from "typescript";

import {
  bannedModuleRule,
  CLOCK_GLOBALS,
  type DeterminismRule,
  GLOBAL_OBJECTS,
  INSPECTED_GLOBALS,
  isContainedGlobalRead,
  isValueReference,
  NETWORK_GLOBALS,
  normalise,
  RANDOM_IMPORTS,
  RANDOM_MEMBERS,
} from "./determinism-vocabulary.js";

export type { DeterminismRule } from "./determinism-vocabulary.js";

/**
 * The §7.2 determinism lint for S3 derivation modules: a derivation is a pure
 * function of facts, class B/C content and the manifest, so it may not read
 * a clock, draw randomness, reach the network or the sidecar, or read the
 * host (its files, its OS, its environment).
 */
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
    if (specifier === undefined) return;
    if (!ts.isStringLiteralLike(specifier)) {
      // A module named at run time cannot be followed or checked.
      report(node, "unresolved_import");
      return;
    }
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
  /** `globalThis.process` read whole is an alias of `process`. */
  const wholeGlobalRule = (
    node: ts.Expression,
    name: string,
  ): DeterminismRule | null =>
    INSPECTED_GLOBALS.has(name) && !isContainedGlobalRead(node)
      ? "global_alias"
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
          ? (globalRule(node.name.text) ??
            wholeGlobalRule(node, node.name.text))
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
          ? (globalRule(member) ?? wholeGlobalRule(node, member))
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
      else if (
        (INSPECTED_GLOBALS.has(node.text) || cryptoAliases.has(node.text)) &&
        !isContainedGlobalRead(node)
      )
        report(node, "global_alias");
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
    /**
     * Globs never linted, relative to `root`. A linted module that imports
     * one is an `excluded_import` problem: the walk never skips a module
     * silently.
     */
    exclude?: readonly string[];
    /**
     * Problems the role accepts, each with its reason: exactly `count`
     * problems with this path, rule and text. A different number is a
     * problem: more keeps every one of them, and fewer (none included) is a
     * `stale_allowance`, so the list cannot outlive or outgrow the code.
     */
    allow?: readonly DeterminismAllowance[];
  }>;

export type DeterminismAllowance = Readonly<{
  path: string;
  rule: DeterminismRule;
  text: string;
  /** How many problems it accepts (default 1). */
  count?: number;
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
 * no source file, a dynamic import of a computed specifier, and an import
 * of an excluded module are problems, so the walk cannot skip a module
 * silently.
 */
export const lintDeterminismModules = (
  options: DeterminismModulesOptions,
): DeterminismModulesReport => {
  const root = resolve(options.root);
  const extensions = [".ts", ".tsx", ".mts"];
  const exclude = [...TEST_FILES, ...(options.exclude ?? [])];
  const list = (
    excludes: readonly string[],
    includes: readonly string[],
  ): string[] =>
    ts.sys
      .readDirectory(root, extensions, excludes, includes)
      .map((path) => resolve(path));
  // The modules under `root` the exclude globs remove, read with the same
  // (exclude) semantics the include walk uses.
  const linted = new Set(list([...exclude, "**/node_modules/**"], ["**/*"]));
  const excluded = new Set(
    list(["**/node_modules/**"], ["**/*"]).filter((path) => !linted.has(path)),
  );
  const pending = list(exclude, options.include);
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
      else if (excluded.has(target))
        problems.push({
          path: shown,
          line: 0,
          rule: "excluded_import",
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
  const allowed = new Set<DeterminismProblem>();
  const kept: DeterminismProblem[] = [];
  for (const entry of allow) {
    const matched = problems.filter((problem) => matches(entry, problem));
    const count = entry.count ?? 1;
    if (matched.length > count) continue;
    for (const problem of matched) allowed.add(problem);
    if (matched.length < count)
      kept.push({
        path: entry.path,
        line: 0,
        rule: "stale_allowance",
        text: `${entry.text} (${String(matched.length)} of ${String(count)})`,
      });
  }
  kept.push(...problems.filter((problem) => !allowed.has(problem)));
  return {
    files: [...seen].map((path) => relative(root, path)).sort(),
    problems: kept.sort(
      (a, b) => a.path.localeCompare(b.path, "en") || a.line - b.line,
    ),
  };
};
