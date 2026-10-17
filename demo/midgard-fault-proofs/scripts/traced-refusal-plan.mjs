/**
 * Which Vitest cases run each `refusedBy` pin, read from the test sources.
 *
 * A pin is a `refusedBy: "<module>"` literal anywhere under tests/, in a test
 * file or in a support module. A pin in an `it` case of a test file runs in
 * that case. A pin anywhere else sits in a top-level declaration, and runs in
 * every case that reaches that declaration: through other declarations of
 * the same file, through the files that import it (named, aliased or
 * namespace imports, and `export ... from` re-exports), until each path ends
 * in an `it` case of a test file. A pin that no case reaches is an error, so a
 * pin moved into a new support module is planned by default or refused.
 *
 * The reading is lexical and over-approximates: a case that merely mentions
 * a declaration that holds a pin is planned too. A pin is counted as checked
 * only when the traced run records it from the file that declares it, so a
 * wrongly planned case can cost time, never let a pin pass unchecked.
 */
import { readdirSync, readFileSync } from "node:fs";
import { dirname, relative, resolve } from "node:path";

/** The source files a pin may sit in; the same set the CI inputs test reads. */
const isSourceFile = (file) => /\.(?:ts|mts|js|mjs)$/u.test(file);

/** The files Vitest collects as tests (vitest.config.ts `include`). */
export const isTestFile = (file) => /\.test\.tsx?$/u.test(file);

export const pinPattern = () => /\brefusedBy:\s*"([^"]+)"/gu;

const stringLiteral = String.raw`"((?:[^"\\]|\\.)*)"`;

/**
 * The `it` cases a source declares, by where each starts: `it("name", ...)`,
 * and `it.each(table)("name", ...)` whose name is a template.
 */
const caseStarts = (source) => {
  const cases = [];
  for (const match of source.matchAll(
    new RegExp(String.raw`\bit\(\s*${stringLiteral}`, "gu"),
  )) {
    cases.push({
      index: match.index,
      name: JSON.parse(`"${match[1]}"`),
      template: false,
    });
  }
  for (const match of source.matchAll(/\bit\.each\(/gu)) {
    const name = new RegExp(String.raw`\)\s*\(\s*${stringLiteral}`, "gu");
    name.lastIndex = match.index;
    const found = name.exec(source);
    if (found === null) continue;
    cases.push({
      index: match.index,
      name: JSON.parse(`"${found[1]}"`),
      template: true,
    });
  }
  return cases.sort((left, right) => left.index - right.index);
};

/** Top-level declarations: those that start a line, by where each starts. */
const declarationStarts = (source) =>
  [
    ...source.matchAll(
      /^(?:export\s+)?(?:const|let|var|(?:async\s+)?function\*?|class)\s+([A-Za-z_$][\w$]*)/gmu,
    ),
  ].map((match) => ({ index: match.index, name: match[1] }));

const lastBefore = (starts, index) =>
  starts.filter((start) => start.index < index).at(-1);

const escapeIdentifier = (name) => name.replaceAll("$", "\\$");

/** Where `name` is used as an identifier (not a property) in `source`. */
const uses = (source, name) =>
  [
    ...source.matchAll(
      new RegExp(`(?<![\\w$.])${escapeIdentifier(name)}(?![\\w$])`, "gu"),
    ),
  ].map((match) => match.index);

/**
 * Reads the sources under one tests directory, each once, and answers which
 * cases reach a position in one of them.
 */
const testTree = (testsRoot) => {
  const files = readdirSync(testsRoot, { recursive: true })
    .map(String)
    .filter(isSourceFile)
    .map((file) => resolve(testsRoot, file))
    .sort();
  const sources = new Map(
    files.map((file) => [file, readFileSync(file, "utf8")]),
  );

  /** The file a relative specifier written in `from` names, if any. */
  const resolveSpecifier = (from, specifier) => {
    if (!specifier.startsWith(".")) return undefined;
    const target = resolve(dirname(from), specifier);
    return [
      target,
      target.replace(/\.js$/u, ".ts"),
      target.replace(/\.mjs$/u, ".mts"),
      `${target}.ts`,
      resolve(target, "index.ts"),
    ].find((candidate) => sources.has(candidate));
  };

  /** `{ a, b as c, type d }` as [imported, local] pairs. */
  const namedBindings = (clause) =>
    clause
      .split(",")
      .map((part) => part.trim().replace(/^type\s+/u, ""))
      .filter((part) => part.length > 0)
      .map((part) => {
        const [imported, local = imported] = part.split(/\s+as\s+/u);
        return [imported.trim(), local.trim()];
      });

  /**
   * Every way `file` exposes `name` to another file: the local names it is
   * imported under, and the names re-exports pass it on under.
   */
  const importersOf = (file, name) => {
    const found = [];
    for (const [importer, source] of sources) {
      if (importer === file) continue;
      for (const match of source.matchAll(
        /\bimport\s+(?:type\s+)?(?:[\w$]+\s*,\s*)?\{([^}]*)\}\s*from\s*["']([^"']+)["']/gu,
      )) {
        if (resolveSpecifier(importer, match[2]) !== file) continue;
        for (const [imported, local] of namedBindings(match[1])) {
          if (imported === name) {
            found.push({ kind: "local", file: importer, name: local });
          }
        }
      }
      for (const match of source.matchAll(
        /\bimport\s+\*\s+as\s+([\w$]+)\s+from\s*["']([^"']+)["']/gu,
      )) {
        if (resolveSpecifier(importer, match[2]) !== file) continue;
        found.push({ kind: "namespace", file: importer, name, via: match[1] });
      }
      for (const match of source.matchAll(
        /\bexport\s+(?:type\s+)?\{([^}]*)\}\s*from\s*["']([^"']+)["']/gu,
      )) {
        if (resolveSpecifier(importer, match[2]) !== file) continue;
        for (const [imported, exported] of namedBindings(match[1])) {
          if (imported === name) {
            found.push({ kind: "export", file: importer, name: exported });
          }
        }
      }
      for (const match of source.matchAll(
        /\bexport\s+\*\s+from\s*["']([^"']+)["']/gu,
      )) {
        if (resolveSpecifier(importer, match[1]) !== file) continue;
        found.push({ kind: "export", file: importer, name });
      }
    }
    return found;
  };

  /** Where a statement that names a module (an import or re-export) sits. */
  const moduleStatementSpans = (source) =>
    [
      ...source.matchAll(
        /^(?:import|export)\s+(?:type\s+)?(?:[\w$]+\s*,\s*)?(?:\{[^}]*\}|\*(?:\s+as\s+[\w$]+)?|[\w$]+)\s*from\s*["'][^"']+["']/gmu,
      ),
    ].map((match) => [match.index, match.index + match[0].length]);

  const outside = (spans, index) =>
    spans.every(([start, end]) => index < start || index >= end);

  /**
   * The cases that reach position `index` of `file`, collected into `sites`;
   * `visited` holds the declarations already followed.
   */
  const reach = (file, index, sites, visited) => {
    const source = sources.get(file);
    const enclosingCase = lastBefore(caseStarts(source), index);
    const declarations = declarationStarts(source);
    const enclosing = lastBefore(declarations, index);
    if (
      isTestFile(file) &&
      enclosingCase !== undefined &&
      (enclosing === undefined || enclosingCase.index > enclosing.index)
    ) {
      sites.set(`${file}\0${enclosingCase.name}`, {
        file,
        name: enclosingCase.name,
        template: enclosingCase.template,
      });
      return;
    }
    if (enclosing === undefined) return;
    const end =
      declarations.find((start) => start.index > enclosing.index)?.index ??
      source.length;
    reachDeclaration(
      file,
      enclosing.name,
      [enclosing.index, end],
      sites,
      visited,
    );
  };

  /** The cases that reach a name `file` declares or re-exports. */
  const reachDeclaration = (file, name, span, sites, visited) => {
    const key = `${file}\0${name}`;
    if (visited.has(key)) return;
    visited.add(key);
    const source = sources.get(file);
    const statements = moduleStatementSpans(source);
    for (const at of uses(source, name)) {
      if (span !== undefined && at >= span[0] && at < span[1]) continue;
      if (!outside(statements, at)) continue;
      reach(file, at, sites, visited);
    }
    for (const importer of importersOf(file, name)) {
      if (importer.kind === "export") {
        reachDeclaration(
          importer.file,
          importer.name,
          undefined,
          sites,
          visited,
        );
        continue;
      }
      const importerSource = sources.get(importer.file);
      const importerStatements = moduleStatementSpans(importerSource);
      const positions =
        importer.kind === "local"
          ? uses(importerSource, importer.name)
          : [
              ...importerSource.matchAll(
                new RegExp(
                  `(?<![\\w$.])${escapeIdentifier(importer.via)}\\s*\\.\\s*${escapeIdentifier(importer.name)}(?![\\w$])`,
                  "gu",
                ),
              ),
            ].map((match) => match.index);
      for (const at of positions) {
        if (outside(importerStatements, at)) {
          reach(importer.file, at, sites, visited);
        }
      }
    }
  };

  return { files, sources, reach };
};

/**
 * Every `refusedBy` pin under `<packageRoot>/tests` with the cases that run
 * it, as `{ file, module, sites: [{ file, name, template }] }` with paths
 * relative to `packageRoot`. Throws when a pin is reached by no case.
 */
export const tracedRefusalPlan = (packageRoot) => {
  const { files, sources, reach } = testTree(resolve(packageRoot, "tests"));
  const pins = [];
  const unreached = [];
  for (const file of files) {
    for (const match of sources.get(file).matchAll(pinPattern())) {
      const sites = new Map();
      reach(file, match.index, sites, new Set());
      const pin = {
        file: relative(packageRoot, file),
        module: match[1],
        sites: [...sites.values()]
          .map((site) => ({ ...site, file: relative(packageRoot, site.file) }))
          .sort((left, right) => {
            const [a, b] = [left, right].map(
              ({ file: at, name }) => `${at}\0${name}`,
            );
            return a < b ? -1 : a > b ? 1 : 0;
          }),
      };
      if (pin.sites.length === 0) {
        unreached.push(`${pin.file}: the ${pin.module} pin`);
      }
      pins.push(pin);
    }
  }
  if (unreached.length > 0) {
    throw new Error(
      `no it case reaches these refusedBy pins, so the traced run cannot check them:\n  ${unreached.join("\n  ")}`,
    );
  }
  return pins;
};

/**
 * A Vitest `-t` pattern for one planned case. An `it.each` name is a
 * template: its `$field` and `%s`-style placeholders match any text.
 */
export const caseNamePattern = ({ name, template }) => {
  const escape = (text) => text.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");
  if (!template) return escape(name);
  return name
    .split(/(\$[A-Za-z_][\w.]*|%[sdifjoO#])/u)
    .map((part, index) => (index % 2 === 1 ? ".+" : escape(part)))
    .join("");
};
