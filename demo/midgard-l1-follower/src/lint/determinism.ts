import ts from "typescript";

/**
 * The §7.2 determinism lint for S3 derivation modules: a derivation is a pure
 * function of facts, class B/C content and the manifest, so it may not read
 * a clock, draw randomness, or reach the network or the sidecar.
 */
export type DeterminismRule =
  | "clock"
  | "randomness"
  | "network_import"
  | "network_global";

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

const isBannedModule = (
  specifier: string,
  extra: readonly string[],
): boolean => {
  const name = normalise(specifier);
  if (/l1-node-transport/u.test(name)) return true;
  return [...NETWORK_MODULES, ...extra].some((banned) =>
    banned.endsWith("/")
      ? name.startsWith(banned)
      : name === banned || name.startsWith(`${banned}/`),
  );
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

/** Lints one source file. */
export const lintDeterminismSource = (
  path: string,
  source: string,
  options: DeterminismLintOptions = {},
): DeterminismProblem[] => {
  const file = ts.createSourceFile(path, source, ts.ScriptTarget.Latest, true);
  const extra = options.bannedModules ?? [];
  const problems: DeterminismProblem[] = [];
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
  ): void => {
    if (
      specifier !== undefined &&
      ts.isStringLiteralLike(specifier) &&
      isBannedModule(specifier.text, extra)
    )
      report(node, "network_import");
  };
  const visit = (node: ts.Node): void => {
    if (ts.isImportDeclaration(node)) {
      checkSpecifier(node, node.moduleSpecifier);
      const bindings = node.importClause?.namedBindings;
      const module = ts.isStringLiteralLike(node.moduleSpecifier)
        ? normalise(node.moduleSpecifier.text)
        : "";
      if (
        module === "crypto" &&
        bindings !== undefined &&
        ts.isNamedImports(bindings)
      )
        for (const element of bindings.elements)
          if (RANDOM_IMPORTS.has((element.propertyName ?? element.name).text))
            report(element, "randomness");
    } else if (ts.isExportDeclaration(node)) {
      checkSpecifier(node, node.moduleSpecifier);
    } else if (ts.isCallExpression(node)) {
      const callee = node.expression;
      if (
        callee.kind === ts.SyntaxKind.ImportKeyword ||
        (ts.isIdentifier(callee) && callee.text === "require")
      )
        checkSpecifier(node, node.arguments[0]);
    } else if (
      ts.isPropertyAccessExpression(node) &&
      ts.isIdentifier(node.expression)
    ) {
      const members = RANDOM_MEMBERS.get(node.expression.text);
      if (members?.has(node.name.text) === true)
        report(
          node,
          node.expression.text === "process" ? "clock" : "randomness",
        );
    } else if (ts.isIdentifier(node) && isValueReference(node)) {
      if (CLOCK_GLOBALS.has(node.text)) report(node, "clock");
      else if (NETWORK_GLOBALS.has(node.text)) report(node, "network_global");
    }
    ts.forEachChild(node, visit);
  };
  visit(file);
  return problems;
};

/** Lints several S3 modules; empty means every module is deterministic. */
export const lintDeterminism = (
  files: readonly Readonly<{ path: string; source: string }>[],
  options: DeterminismLintOptions = {},
): DeterminismProblem[] =>
  files.flatMap((file) =>
    lintDeterminismSource(file.path, file.source, options),
  );
