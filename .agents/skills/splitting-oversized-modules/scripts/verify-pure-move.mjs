#!/usr/bin/env node
// Proves that splitting TypeScript modules moved declarations without
// changing them.
//
// The before side is always read from git (`<ref>:<path>`), never from a file
// on disk, so a before side cannot be a copy you edited or an output you wrote
// earlier. The after side is the files in the working tree.
//
// Every top-level statement of every file is parsed with the TypeScript
// compiler and reduced to a canonical form: its syntax tree, with comments,
// formatting, quote style, numeric spelling, trailing commas and the
// `export` / `default` modifiers ignored. Import and export statements
// (`import ...`, `export { ... }`, `export * from ...`) are skipped. The
// before and after multisets must be equal.
//
// It does not prove that an import in an after file resolves to the same
// binding the before file saw, that the export surface is the same, or that
// module evaluation order is the same. Typecheck and the tests carry those.
//
// Usage:
//   node verify-pure-move.mjs --before <ref>:<path> [--before ...]
//                             --after <path> [--after ...]
//                             [--repo <dir>] [--typescript-root <dir>]
//
// An --after directory expands to every .ts/.tsx/.mts/.cts file beneath it.
// --repo is the git repository the before side is read from (default: the
// repository containing the current directory). --typescript-root is the
// repository whose demo workspace provides `typescript` (default: the
// repository containing this script).
//
// Exit codes: 0 pure move confirmed; 1 NOT a pure move; 2 usage error;
// 3 could not look (a ref or path git cannot show, an unreadable or
// unparsable file, or `typescript` not resolvable).

import { spawnSync } from "node:child_process";
import { existsSync, readdirSync, readFileSync, statSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const EXIT_PURE = 0;
const EXIT_NOT_PURE = 1;
const EXIT_USAGE = 2;
const EXIT_COULD_NOT_LOOK = 3;

const SCRIPT_REPO = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../../../..",
);
const TS_EXTENSIONS = /\.(?:[cm]?ts|tsx)$/u;

class UsageError extends Error {}
class CouldNotLook extends Error {}

const USAGE =
  "Usage: verify-pure-move.mjs --before <ref>:<path> [--before ...] --after <path> [--after ...] [--repo <dir>] [--typescript-root <dir>]";

export const parseArgs = (argv) => {
  const options = {
    before: [],
    after: [],
    repo: undefined,
    typescriptRoot: undefined,
  };
  for (let index = 0; index < argv.length; index += 1) {
    const flag = argv[index];
    const value = argv[index + 1];
    const needValue = () => {
      if (value === undefined || value.startsWith("--")) {
        throw new UsageError(`${flag} needs a value`);
      }
      index += 1;
      return value;
    };
    if (flag === "--before") options.before.push(needValue());
    else if (flag === "--after") options.after.push(needValue());
    else if (flag === "--repo") options.repo = needValue();
    else if (flag === "--typescript-root") options.typescriptRoot = needValue();
    else if (flag === "-h" || flag === "--help") throw new UsageError("");
    else throw new UsageError(`unknown argument ${flag}`);
  }
  if (options.before.length === 0)
    throw new UsageError("give at least one --before <ref>:<path>");
  if (options.after.length === 0)
    throw new UsageError("give at least one --after <path>");
  for (const spec of options.before) {
    const colon = spec.indexOf(":");
    if (colon <= 0 || colon === spec.length - 1) {
      throw new UsageError(
        `--before ${spec} is not <ref>:<path>. The before side is always read from git, never from a file on disk; commit or name the ref that holds the original (for example HEAD:demo/pkg/src/big.ts).`,
      );
    }
  }
  return options;
};

export const loadTypeScript = (root) => {
  // The demo root does not declare typescript itself; pnpm installs it for the
  // workspace packages, so fall back to midgard-core, which pins it.
  const anchors = [
    join(root, "demo/package.json"),
    join(root, "demo/midgard-core/package.json"),
  ];
  for (const anchor of anchors) {
    if (!existsSync(anchor)) continue;
    try {
      const require = createRequire(anchor);
      const path = require.resolve("typescript");
      return { ts: require("typescript"), path };
    } catch {
      // try the next anchor
    }
  }
  throw new CouldNotLook(
    `could not resolve typescript from ${anchors.join(" or ")}; install the demo workspace (pnpm --dir demo install) or pass --typescript-root`,
  );
};

const git = (repo, args) => {
  const result = spawnSync("git", ["-C", repo, ...args], {
    encoding: "utf8",
    maxBuffer: 512 * 1024 * 1024,
  });
  if (result.error)
    throw new CouldNotLook(`could not run git: ${result.error.message}`);
  return result;
};

const repositoryRoot = (directory) => {
  const result = git(directory, ["rev-parse", "--show-toplevel"]);
  if (result.status !== 0) {
    throw new CouldNotLook(
      `${directory} is not inside a git repository: ${result.stderr.trim()}`,
    );
  }
  return result.stdout.trim();
};

const readBefore = (repo, spec) => {
  const colon = spec.indexOf(":");
  const ref = spec.slice(0, colon);
  const path = spec.slice(colon + 1);
  const commit = git(repo, [
    "rev-parse",
    "--verify",
    "--quiet",
    `${ref}^{commit}`,
  ]);
  if (commit.status !== 0)
    throw new CouldNotLook(`git cannot resolve ${ref} to a commit in ${repo}`);
  const sha = commit.stdout.trim();
  const shown = git(repo, ["show", `${sha}:${path}`]);
  if (shown.status !== 0) {
    throw new CouldNotLook(
      `git cannot show ${spec} (${sha.slice(0, 12)}): ${shown.stderr.trim()}`,
    );
  }
  return {
    label: `${ref}:${path}`,
    fileName: path,
    text: shown.stdout,
    commit: sha,
  };
};

const expandAfter = (path) => {
  if (!existsSync(path))
    throw new CouldNotLook(`--after ${path} does not exist`);
  if (statSync(path).isFile()) return [path];
  const files = [];
  const walk = (directory) => {
    for (const entry of readdirSync(directory, { withFileTypes: true })) {
      if (entry.name === "node_modules") continue;
      const full = join(directory, entry.name);
      if (entry.isDirectory()) walk(full);
      else if (TS_EXTENSIONS.test(entry.name)) files.push(full);
    }
  };
  walk(path);
  return files.sort();
};

const scriptKind = (ts, fileName) => {
  if (fileName.endsWith(".tsx")) return ts.ScriptKind.TSX;
  if (/\.(?:[cm]?js|jsx)$/u.test(fileName)) return ts.ScriptKind.JS;
  return ts.ScriptKind.TS;
};

// Node flags that change meaning and are not visible as child nodes.
const semanticFlags = (ts) =>
  ts.NodeFlags.Let |
  ts.NodeFlags.Const |
  ts.NodeFlags.Using |
  ts.NodeFlags.NestedNamespace |
  ts.NodeFlags.Namespace |
  ts.NodeFlags.GlobalAugmentation |
  ts.NodeFlags.OptionalChain;

// Serializes a subtree. Everything that is not a child node but carries
// meaning (identifier text, literal values, operators, let/const) is written
// explicitly; comments, whitespace and trailing commas never reach the tree.
const serialize = (ts, root, dropModifiers) => {
  const mask = semanticFlags(ts);
  const out = [];
  const visit = (node, isRoot) => {
    out.push("(", String(node.kind));
    const flags = node.flags & mask;
    if (flags) out.push(`f${flags}`);
    // Identifiers, literals, template parts and JSX text carry `text`: the
    // cooked value, so quote style and numeric spelling (0x10, 1_000) drop out.
    if (typeof node.text === "string") out.push(JSON.stringify(node.text));
    if (typeof node.rawText === "string")
      out.push(JSON.stringify(node.rawText));
    if (node.operator !== undefined) out.push(`o${node.operator}`);
    if (node.token !== undefined && ts.isHeritageClause(node))
      out.push(`t${node.token}`);
    if (node.keywordToken !== undefined) out.push(`k${node.keywordToken}`);
    if (node.isTypeOf === true) out.push("typeof");
    ts.forEachChild(
      node,
      (child) => visit(child, false),
      (children) => {
        let list = [...children];
        if (isRoot && children === root.modifiers) {
          list = list.filter((modifier) => !dropModifiers.has(modifier.kind));
        }
        if (list.length === 0) return;
        out.push("[");
        for (const child of list) visit(child, false);
        out.push("]");
      },
    );
    out.push(")");
  };
  visit(root, true);
  return out.join(" ");
};

const bindingNames = (ts, name) => {
  if (ts.isIdentifier(name)) return [name.text];
  return name.elements.flatMap((element) =>
    ts.isOmittedExpression(element) ? [] : bindingNames(ts, element.name),
  );
};

const describe = (ts, statement, sourceFile) => {
  const text = (node) => (node ? node.getText(sourceFile) : "default");
  if (ts.isFunctionDeclaration(statement))
    return ["function", statement.name?.text ?? "default"];
  if (ts.isClassDeclaration(statement))
    return ["class", statement.name?.text ?? "default"];
  if (ts.isInterfaceDeclaration(statement))
    return ["interface", statement.name.text];
  if (ts.isTypeAliasDeclaration(statement))
    return ["type", statement.name.text];
  if (ts.isEnumDeclaration(statement)) return ["enum", statement.name.text];
  if (ts.isModuleDeclaration(statement))
    return ["namespace", text(statement.name)];
  if (ts.isVariableStatement(statement)) {
    const flags = statement.declarationList.flags;
    const kind =
      (flags & ts.NodeFlags.Using) === ts.NodeFlags.Using
        ? "using"
        : flags & ts.NodeFlags.Const
          ? "const"
          : flags & ts.NodeFlags.Let
            ? "let"
            : "var";
    const names = statement.declarationList.declarations.flatMap(
      (declaration) => bindingNames(ts, declaration.name),
    );
    return [kind, names.join(", ")];
  }
  if (ts.isExportAssignment(statement)) {
    return statement.isExportEquals
      ? ["export =", "export="]
      : ["export default", "default"];
  }
  const oneLine = statement.getText(sourceFile).replace(/\s+/gu, " ");
  return [
    "statement",
    oneLine.length > 60 ? `${oneLine.slice(0, 57)}...` : oneLine,
  ];
};

const isImportOrExport = (ts, statement) =>
  ts.isImportDeclaration(statement) ||
  ts.isImportEqualsDeclaration(statement) ||
  ts.isExportDeclaration(statement) ||
  ts.isNamespaceExportDeclaration(statement);

export const declarationsOf = (ts, { label, fileName, text }) => {
  const sourceFile = ts.createSourceFile(
    fileName,
    text,
    ts.ScriptTarget.Latest,
    true,
    scriptKind(ts, fileName),
  );
  const diagnostics = sourceFile.parseDiagnostics ?? [];
  if (diagnostics.length > 0) {
    const first = diagnostics[0];
    const { line } = sourceFile.getLineAndCharacterOfPosition(first.start ?? 0);
    const message = ts.flattenDiagnosticMessageText(first.messageText, " ");
    throw new CouldNotLook(`could not parse ${label}:${line + 1}: ${message}`);
  }
  const dropModifiers = new Set([
    ts.SyntaxKind.ExportKeyword,
    ts.SyntaxKind.DefaultKeyword,
  ]);
  const declarations = [];
  for (const statement of sourceFile.statements) {
    if (isImportOrExport(ts, statement)) continue;
    const [kind, name] = describe(ts, statement, sourceFile);
    const { line } = sourceFile.getLineAndCharacterOfPosition(
      statement.getStart(sourceFile),
    );
    declarations.push({
      kind,
      name,
      key: `${kind} ${name}`,
      canonical: serialize(ts, statement, dropModifiers),
      where: `${label}:${line + 1}`,
    });
  }
  return declarations;
};

const group = (declarations) => {
  const byKey = new Map();
  for (const declaration of declarations) {
    const list = byKey.get(declaration.key) ?? [];
    list.push(declaration);
    byKey.set(declaration.key, list);
  }
  return byKey;
};

// Returns one finding per declaration name whose before and after copies are
// not the same multiset of canonical forms.
export const compareDeclarations = (before, after) => {
  const beforeByKey = group(before);
  const afterByKey = group(after);
  const keys = [
    ...new Set([...beforeByKey.keys(), ...afterByKey.keys()]),
  ].sort();
  const findings = [];
  for (const key of keys) {
    const was = beforeByKey.get(key) ?? [];
    const now = afterByKey.get(key) ?? [];
    const wasForms = was.map((d) => d.canonical).sort();
    const nowForms = now.map((d) => d.canonical).sort();
    if (
      wasForms.length === nowForms.length &&
      wasForms.every((form, i) => form === nowForms[i])
    ) {
      continue;
    }
    let problem;
    if (now.length === 0) problem = "missing";
    else if (was.length === 0) problem = "unexpected";
    else if (
      was.length !== now.length &&
      new Set([...wasForms, ...nowForms]).size === 1
    ) {
      problem = "copies";
    } else problem = "changed";
    findings.push({
      problem,
      key,
      before: was.map((d) => d.where),
      after: now.map((d) => d.where),
    });
  }
  return findings;
};

const describeFinding = ({ problem, key, before, after }) => {
  const where = (list) => (list.length === 0 ? "none" : list.join(", "));
  const text = {
    missing: `missing     ${key}: before ${where(before)}; absent after`,
    unexpected: `unexpected  ${key}: absent before; after ${where(after)}`,
    copies: `copies      ${key}: ${before.length} before, ${after.length} after (${where(after)})`,
    changed: `changed     ${key}: before ${where(before)}; after ${where(after)}`,
  };
  return text[problem];
};

export const main = (
  argv,
  {
    cwd = process.cwd(),
    stdout = process.stdout,
    stderr = process.stderr,
  } = {},
) => {
  let options;
  try {
    options = parseArgs(argv);
  } catch (error) {
    if (!(error instanceof UsageError)) throw error;
    if (error.message) stderr.write(`verify-pure-move: ${error.message}\n`);
    stderr.write(`${USAGE}\n`);
    return EXIT_USAGE;
  }
  try {
    const { ts, path: tsPath } = loadTypeScript(
      resolve(cwd, options.typescriptRoot ?? SCRIPT_REPO),
    );
    const repo = repositoryRoot(resolve(cwd, options.repo ?? "."));
    const beforeFiles = options.before.map((spec) => readBefore(repo, spec));
    const afterPaths = [
      ...new Set(options.after.flatMap((p) => expandAfter(resolve(cwd, p)))),
    ];
    if (afterPaths.length === 0) {
      throw new CouldNotLook(
        `no TypeScript files under ${options.after.join(", ")}`,
      );
    }
    const afterFiles = afterPaths.map((path) => {
      let text;
      try {
        text = readFileSync(path, "utf8");
      } catch (error) {
        throw new CouldNotLook(`could not read ${path}: ${error.message}`);
      }
      const label = relative(cwd, path) || path;
      return { label, fileName: path, text };
    });
    const before = beforeFiles.flatMap((file) => declarationsOf(ts, file));
    const after = afterFiles.flatMap((file) => declarationsOf(ts, file));
    const commits = [
      ...new Set(beforeFiles.map((f) => f.commit.slice(0, 12))),
    ].join(", ");
    const findings = compareDeclarations(before, after);
    const scope = `${before.length} top-level declarations in ${beforeFiles.length} before file(s) at ${commits}; ${after.length} in ${afterFiles.length} after file(s); typescript ${ts.version} from ${tsPath}`;
    if (findings.length === 0) {
      stdout.write(`Pure move confirmed: ${scope}\n`);
      stdout.write(
        "Not proven by this check: import targets, the export surface, module evaluation order, and files outside the lists above.\n",
      );
      return EXIT_PURE;
    }
    stdout.write(
      `NOT a pure move: ${findings.length} declaration name(s) differ (${scope})\n`,
    );
    for (const finding of findings)
      stdout.write(`  ${describeFinding(finding)}\n`);
    return EXIT_NOT_PURE;
  } catch (error) {
    if (!(error instanceof CouldNotLook)) throw error;
    stderr.write(`verify-pure-move: could not look: ${error.message}\n`);
    return EXIT_COULD_NOT_LOOK;
  }
};

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  process.exitCode = main(process.argv.slice(2));
}
