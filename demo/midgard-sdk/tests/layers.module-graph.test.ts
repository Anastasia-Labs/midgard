/**
 * Static contract, not runtime behaviour: the SDK's layer subpaths depend in
 * one direction. A module belongs to the lowest layer whose run-time graph
 * loads it, and `layers.module-owners.json` pins that owner for every module
 * in `src`, under `"none"` when no layer loads it at run time. The run-time
 * graph of a layer, followed from the source file its export declares,
 * contains no module owned by a layer outside its `LAYERS` row and never the
 * root, so loading a layer never loads a layer above it. A lower layer that
 * starts to load a higher layer's module, its entry's re-export or a private
 * helper behind it, moves that module's owner and fails the pin. The root
 * re-exports exactly the layer entries, lowest first, so `@al-ft/midgard-sdk`
 * keeps every name.
 *
 * A name, value or type, is exported by one layer only: the one that owns the
 * module declaring it. A test runner mocks a module specifier, so a stub on
 * one layer would not reach code that reads the same name through another;
 * with one exporter there is one specifier to mock (or the root).
 *
 * The graph follows static imports and re-exports and every `import()`: one
 * whose specifier is not a string literal fails here. Type-only imports and
 * re-exports are not followed: they are erased before run time. The type-only
 * imports that point at a higher layer are pinned in `TYPE_ONLY_UPWARD`, so a
 * new one, or one of them turned into a value import, fails here. A module
 * that is added, removed or changes owner fails the pin with the edit to make
 * in `layers.module-owners.json`.
 */
import { existsSync, readdirSync, readFileSync } from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import ts from "typescript";
import { describe, expect, it } from "vitest";

const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");

/** Lowest layer first; each layer with the layers it may import. */
const LAYERS: readonly (readonly [string, readonly string[]])[] = [
  ["./core", []],
  ["./state-queue", ["./core"]],
  ["./da", ["./core", "./state-queue"]],
  ["./operators", ["./core", "./state-queue"]],
  ["./users", ["./core", "./state-queue"]],
  ["./deployment", ["./core", "./state-queue", "./da"]],
  ["./fraud-proof", ["./core", "./state-queue", "./da", "./deployment"]],
];

/**
 * `core`'s validator-set types name the fault-proof contract chains and the
 * user-event contract pair; their modules sit in higher layers.
 */
const TYPE_ONLY_UPWARD = [
  "src/common.fraud-proofs.ts -> src/fraud-proof/contracts/index.ts (./fraud-proof)",
  "src/common.fraud-proofs.ts -> src/fraud-proof/contracts/script-decode.ts (./fraud-proof)",
  "src/common.fraud-proofs.ts -> src/user-events/history-deployment.ts (./state-queue)",
];

const manifest = JSON.parse(
  readFileSync(resolve(packageRoot, "package.json"), "utf8"),
) as { readonly exports: Record<string, Record<string, string>> };

const PIN = "tests/layers.module-owners.json";

/** The pin's key for the modules no layer loads at run time. */
const NONE = "none";

/** Each layer, then `NONE`, with the package-relative modules it owns. */
const OWNERS = JSON.parse(
  readFileSync(resolve(packageRoot, PIN), "utf8"),
) as Record<string, readonly string[]>;

const declaredEntry = (subpath: string): string => {
  const source = manifest.exports[subpath]?.["midgard-source"];
  if (source === undefined) {
    throw new Error(`package.json declares no ${subpath} source export`);
  }
  return resolve(packageRoot, source);
};

const typeOnly = (statement: ts.ImportDeclaration | ts.ExportDeclaration) => {
  if (ts.isExportDeclaration(statement)) {
    const clause = statement.exportClause;
    return (
      statement.isTypeOnly ||
      (clause !== undefined &&
        ts.isNamedExports(clause) &&
        clause.elements.length > 0 &&
        clause.elements.every((element) => element.isTypeOnly))
    );
  }
  const clause = statement.importClause;
  const bindings = clause?.namedBindings;
  return (
    clause?.isTypeOnly === true ||
    (clause?.name === undefined &&
      bindings !== undefined &&
      ts.isNamedImports(bindings) &&
      bindings.elements.length > 0 &&
      bindings.elements.every((element) => element.isTypeOnly))
  );
};

/** The `import()` calls anywhere in `source`. */
const dynamicImports = (source: ts.SourceFile): ts.CallExpression[] => {
  const calls: ts.CallExpression[] = [];
  const visit = (node: ts.Node): void => {
    if (
      ts.isCallExpression(node) &&
      node.expression.kind === ts.SyntaxKind.ImportKeyword
    ) {
      calls.push(node);
    }
    ts.forEachChild(node, visit);
  };
  visit(source);
  return calls;
};

const resolveLocal = (file: string, specifier: string): string => {
  const target = resolve(dirname(file), specifier.replace(/\.js$/u, ".ts"));
  if (!existsSync(target)) {
    throw new Error(`${file} imports unresolvable ${specifier}`);
  }
  return target;
};

/**
 * The local modules `file` imports, re-exports or loads with `import()`, in
 * source order. An SDK module that names the SDK package, or an `import()`
 * whose specifier is not a string literal, throws.
 */
const localImports = (
  file: string,
): { readonly target: string; readonly typeOnly: boolean }[] => {
  const source = ts.createSourceFile(
    file,
    readFileSync(file, "utf8"),
    ts.ScriptTarget.Latest,
    true,
  );
  const named = (specifier: string): boolean => {
    if (/^@al-ft\/midgard-sdk(?:\/|$)/u.test(specifier)) {
      throw new Error(`${file} imports its own package as ${specifier}`);
    }
    return specifier.startsWith(".");
  };
  const declared = source.statements
    .filter(
      (statement): statement is ts.ImportDeclaration | ts.ExportDeclaration =>
        (ts.isImportDeclaration(statement) ||
          ts.isExportDeclaration(statement)) &&
        statement.moduleSpecifier !== undefined &&
        ts.isStringLiteral(statement.moduleSpecifier) &&
        named(statement.moduleSpecifier.text),
    )
    .map((statement) => ({
      target: resolveLocal(
        file,
        (statement.moduleSpecifier as ts.StringLiteral).text,
      ),
      typeOnly: typeOnly(statement),
    }));
  const loaded = dynamicImports(source).flatMap((call) => {
    const [specifier] = call.arguments;
    if (
      specifier === undefined ||
      !ts.isStringLiteralLike(specifier) ||
      call.arguments.length > 2
    ) {
      throw new Error(
        `${file} has an import() the graph cannot follow: ${call.getText(source)}`,
      );
    }
    return named(specifier.text)
      ? [{ target: resolveLocal(file, specifier.text), typeOnly: false }]
      : [];
  });
  return [...declared, ...loaded];
};

/** The local modules `file` imports, re-exports or loads at run time. */
const runtimeImports = (file: string): string[] =>
  localImports(file)
    .filter((edge) => !edge.typeOnly)
    .map((edge) => edge.target);

/** Each module the entry loads at run time, with the module that loads it. */
const runtimeGraph = (entry: string): Map<string, string | undefined> => {
  const files = new Map<string, string | undefined>([[entry, undefined]]);
  const pending = [entry];
  for (let file = pending.shift(); file !== undefined; file = pending.shift()) {
    for (const target of runtimeImports(file)) {
      if (files.has(target)) continue;
      files.set(target, file);
      pending.push(target);
    }
  }
  return files;
};

const graphs = new Map(
  LAYERS.map(([subpath]) => [subpath, runtimeGraph(declaredEntry(subpath))]),
);

/** The lowest layer whose run-time graph loads `file`. */
const layerOf = (file: string): string | undefined =>
  LAYERS.find(([subpath]) => graphs.get(subpath)?.has(file))?.[0];

const sourceFiles = (directory: string): string[] =>
  readdirSync(directory, { withFileTypes: true }).flatMap((entry) =>
    entry.isDirectory()
      ? sourceFiles(join(directory, entry.name))
      : entry.name.endsWith(".ts")
        ? [join(directory, entry.name)]
        : [],
  );

/** The import chain from `subpath`'s entry to `file`. */
const chain = (subpath: string, file: string): string => {
  const graph = graphs.get(subpath);
  const path: string[] = [];
  for (
    let step: string | undefined = file;
    step !== undefined;
    step = graph?.get(step)
  ) {
    path.unshift(relative(packageRoot, step));
  }
  return path.join(" -> ");
};

const order = (key: string): number =>
  key === NONE
    ? LAYERS.length
    : LAYERS.findIndex(([subpath]) => subpath === key);

/** What to change in the pin for this tree, one line per edit. */
const pinEdits = (): string[] => {
  const edits: string[] = [];
  const keys = [...LAYERS.map(([subpath]) => subpath), NONE];
  if (JSON.stringify(Object.keys(OWNERS)) !== JSON.stringify(keys)) {
    edits.push(
      `${PIN}: use the keys ${keys.map((key) => `"${key}"`).join(", ")}, in that order`,
    );
  }
  const pinned = new Map<string, string[]>();
  for (const [key, files] of Object.entries(OWNERS)) {
    if (JSON.stringify(files) !== JSON.stringify([...files].sort())) {
      edits.push(`${PIN}: sort the "${key}" list`);
    }
    for (const file of files) {
      pinned.set(file, [...(pinned.get(file) ?? []), key]);
    }
  }
  const tree = new Map(
    sourceFiles(resolve(packageRoot, "src")).map((file) => [
      relative(packageRoot, file),
      layerOf(file) ?? NONE,
    ]),
  );
  const why = (file: string, owner: string): string =>
    owner === NONE
      ? "no layer loads it at run time"
      : `the lowest layer that loads it is ${owner}: ${chain(owner, resolve(packageRoot, file))}`;
  for (const [file, keys] of pinned) {
    const owner = tree.get(file);
    if (owner === undefined) {
      edits.push(
        `${PIN}: remove "${file}" from ${keys.map((key) => `"${key}"`).join(" and ")}; the file is gone`,
      );
    } else if (keys.length > 1) {
      edits.push(
        `${PIN}: list "${file}" once, under "${owner}"; it is under ${keys.map((key) => `"${key}"`).join(" and ")}`,
      );
    } else if (keys[0] !== owner) {
      const lower = order(owner) < order(keys[0] ?? NONE);
      edits.push(
        `${PIN}: move "${file}" from "${keys[0]}" to "${owner}"; ${why(file, owner)}` +
          (lower
            ? `. If ${owner} must not load it, drop that import instead`
            : ""),
      );
    }
  }
  for (const [file, owner] of tree) {
    if (!pinned.has(file)) {
      edits.push(`${PIN}: add "${file}" to "${owner}"; ${why(file, owner)}`);
    }
  }
  return edits.sort();
};

/** The pinned owner of each module, else the tree's. */
const pinnedOwner = new Map(
  Object.entries(OWNERS)
    .filter(([key]) => key !== NONE)
    .flatMap(([subpath, files]) =>
      files.map((file) => [resolve(packageRoot, file), subpath] as const),
    ),
);
const ownerOf = (file: string): string | undefined =>
  pinnedOwner.get(file) ?? layerOf(file);

const exportCache = new Map<string, Map<string, string>>();

/**
 * Each name `file` exports, value or type, with the declaration it resolves
 * to: `<module>#<name>`, the module package-relative or a package specifier.
 * A local export shadows an `export *`, as in TypeScript.
 */
const exportsOf = (file: string): Map<string, string> => {
  const cached = exportCache.get(file);
  if (cached !== undefined) return cached;
  const names = new Map<string, string>();
  exportCache.set(file, names);
  const source = ts.createSourceFile(
    file,
    readFileSync(file, "utf8"),
    ts.ScriptTarget.Latest,
    true,
  );
  const from = (specifier: string, name: string): string => {
    if (!specifier.startsWith(".")) return `${specifier}#${name}`;
    const origin = exportsOf(resolveLocal(file, specifier)).get(name);
    if (origin === undefined) {
      throw new Error(`${file}: ${specifier} exports no ${name}`);
    }
    return origin;
  };
  const imported = new Map<string, readonly [string, string]>();
  const stars: string[] = [];
  for (const statement of source.statements) {
    if (
      ts.isImportDeclaration(statement) &&
      ts.isStringLiteral(statement.moduleSpecifier) &&
      statement.importClause?.namedBindings !== undefined &&
      ts.isNamedImports(statement.importClause.namedBindings)
    ) {
      for (const element of statement.importClause.namedBindings.elements) {
        imported.set(element.name.text, [
          statement.moduleSpecifier.text,
          (element.propertyName ?? element.name).text,
        ]);
      }
    } else if (ts.isExportDeclaration(statement)) {
      const specifier =
        statement.moduleSpecifier !== undefined &&
        ts.isStringLiteral(statement.moduleSpecifier)
          ? statement.moduleSpecifier.text
          : undefined;
      const clause = statement.exportClause;
      if (clause === undefined && specifier !== undefined) {
        stars.push(specifier);
      } else if (clause !== undefined && ts.isNamedExports(clause)) {
        for (const element of clause.elements) {
          const name = (element.propertyName ?? element.name).text;
          const via = specifier === undefined ? imported.get(name) : undefined;
          names.set(
            element.name.text,
            specifier !== undefined
              ? from(specifier, name)
              : via !== undefined
                ? from(...via)
                : `${relative(packageRoot, file)}#${name}`,
          );
        }
      }
    } else if (
      ts.canHaveModifiers(statement) &&
      ts
        .getModifiers(statement)
        ?.some((modifier) => modifier.kind === ts.SyntaxKind.ExportKeyword)
    ) {
      const declared = ts.isVariableStatement(statement)
        ? statement.declarationList.declarations.map(
            (declaration) => declaration.name,
          )
        : [(statement as ts.DeclarationStatement).name];
      for (const name of declared) {
        if (name === undefined || !ts.isIdentifier(name)) {
          throw new Error(`${file}: unreadable export ${statement.getText()}`);
        }
        names.set(name.text, `${relative(packageRoot, file)}#${name.text}`);
      }
    }
  }
  for (const specifier of stars) {
    if (!specifier.startsWith(".")) {
      throw new Error(
        `${file}: cannot list the names of export * from ${specifier}`,
      );
    }
    for (const [name, origin] of exportsOf(resolveLocal(file, specifier))) {
      if (!names.has(name)) names.set(name, origin);
    }
  }
  return names;
};

/** Each name two layers export, with what to change. */
const sharedNames = (): string[] => {
  const exporters = new Map<string, { origin: string; layers: string[] }>();
  for (const [subpath] of LAYERS) {
    for (const [name, origin] of exportsOf(declaredEntry(subpath))) {
      const entry = exporters.get(name) ?? { origin, layers: [] };
      entry.layers.push(subpath);
      exporters.set(name, entry);
    }
  }
  return [...exporters]
    .filter(([, { layers }]) => layers.length > 1)
    .map(([name, { origin, layers }]) => {
      const declaring = origin.slice(0, origin.lastIndexOf("#"));
      const owner = declaring.startsWith("src/")
        ? (ownerOf(resolve(packageRoot, declaring)) ?? NONE)
        : "another package";
      return (
        `${name}: exported by ${layers.join(" and ")}; it is declared in ` +
        `${declaring}, owned by ${owner}. Keep the export in the owning layer ` +
        `and drop the re-export that brings it into the others`
      );
    })
    .sort();
};

describe("SDK layer subpaths", () => {
  it("root re-exports exactly the layer entries, lowest first", () => {
    expect(runtimeImports(declaredEntry("."))).toEqual(
      LAYERS.map(([subpath]) => declaredEntry(subpath)),
    );
  });

  it("every import() in the SDK has a literal specifier", () => {
    for (const file of sourceFiles(resolve(packageRoot, "src"))) {
      expect(() => localImports(file)).not.toThrow();
    }
  });

  it("every module keeps the layer that owns it", () => {
    expect(pinEdits()).toEqual([]);
  });

  it("no name, value or type, is exported by two layers", () => {
    expect(sharedNames()).toEqual([]);
  });

  it.each(LAYERS)("%s loads only the layers below it", (subpath, below) => {
    const graph = graphs.get(subpath) ?? new Map<string, string | undefined>();
    expect(graph.has(declaredEntry("."))).toBe(false);
    const higher = [...graph.keys()]
      .map((file) => ({ file, owner: ownerOf(file) }))
      .filter(
        ({ owner }) =>
          owner !== undefined && owner !== subpath && !below.includes(owner),
      )
      .map(({ file, owner }) => `${owner}: ${relative(packageRoot, file)}`)
      .sort();
    expect(higher).toEqual([]);
    expect(OWNERS[subpath]?.length).toBeGreaterThan(0);
  });

  it("type-only imports reach a higher layer only where pinned", () => {
    const upward = LAYERS.flatMap(([subpath, below]) =>
      [...(graphs.get(subpath)?.keys() ?? [])]
        .filter((file) => layerOf(file) === subpath)
        .flatMap((file) =>
          localImports(file)
            .filter((edge) => edge.typeOnly)
            .map((edge) => ({ file, target: edge.target }))
            .filter(({ target }) => {
              const layer = layerOf(target);
              return (
                layer !== undefined &&
                layer !== subpath &&
                !below.includes(layer)
              );
            })
            .map(
              ({ target }) =>
                `${relative(packageRoot, file)} -> ${relative(packageRoot, target)} (${layerOf(target)})`,
            ),
        ),
    );
    expect(upward.sort()).toEqual(TYPE_ONLY_UPWARD);
  });
});
