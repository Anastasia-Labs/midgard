import { existsSync, readdirSync, readFileSync, statSync } from "node:fs";
import { builtinModules } from "node:module";
import { dirname, join, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

/**
 * The workspace as the workspace bundle sees it: packages, how a specifier
 * resolves to a source file under `midgard-source`, and what a source file
 * imports, mocks or spies on. Shared by the analysis
 * (`workspace-bundle-analysis.js`) and the build (`workspace-bundle.js`).
 *
 * Plain JavaScript for the same reason as `vitest.js`: it runs at config time.
 */

export const demoDirectory = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "..",
);
export const bundleRoot = join(
  demoDirectory,
  "node_modules",
  ".midgard-test-bundles",
);

export const readJson = (path) => JSON.parse(readFileSync(path, "utf8"));
export const isInside = (directory, path) => path.startsWith(directory + sep);

/** Every workspace package under demo/, keyed by package name. */
export const workspacePackages = () => {
  const packages = new Map();
  for (const entry of readdirSync(demoDirectory, { withFileTypes: true })) {
    const manifest = join(demoDirectory, entry.name, "package.json");
    if (!entry.isDirectory() || !existsSync(manifest)) continue;
    const pkg = readJson(manifest);
    packages.set(pkg.name, {
      name: pkg.name,
      directory: join(demoDirectory, entry.name),
      exports: pkg.exports ?? {},
      dependencies: Object.keys(pkg.dependencies ?? {}),
    });
  }
  return packages;
};

export const packageOfFile = (packages, file) => {
  for (const pkg of packages.values())
    if (
      isInside(pkg.directory, file) &&
      !file.includes(`${sep}node_modules${sep}`)
    )
      return pkg;
  return undefined;
};

export const splitSpecifier = (specifier) => {
  const match = /^(@[^/]+\/[^/]+|[^/@][^/]*)(\/.*)?$/u.exec(specifier);
  return match === null
    ? undefined
    : {
        name: match[1],
        subpath: match[2] === undefined ? "." : `.${match[2]}`,
      };
};

/** The source file a workspace specifier resolves to under `midgard-source`. */
export const workspaceSourceTarget = (pkg, subpath) => {
  const pick = (target) =>
    typeof target === "string"
      ? target
      : (target?.["midgard-source"] ?? target?.import ?? target?.default);
  const exact = pkg.exports[subpath];
  if (exact !== undefined) {
    const target = pick(exact);
    return target === undefined ? undefined : join(pkg.directory, target);
  }
  // Node's rule: of the matching patterns, the longest prefix wins.
  let best;
  for (const [pattern, target] of Object.entries(pkg.exports)) {
    const star = pattern.indexOf("*");
    if (star < 0) continue;
    const prefix = pattern.slice(0, star);
    const suffix = pattern.slice(star + 1);
    if (
      subpath.length >= prefix.length + suffix.length &&
      subpath.startsWith(prefix) &&
      subpath.endsWith(suffix) &&
      (best === undefined || prefix.length > best.prefix.length)
    )
      best = { prefix, suffix, target };
  }
  if (best === undefined) return undefined;
  const picked = pick(best.target);
  if (picked === undefined) return undefined;
  const stem = subpath.slice(
    best.prefix.length,
    subpath.length - best.suffix.length,
  );
  return join(pkg.directory, picked.replace("*", stem));
};

export const resolveRelative = (from, specifier) => {
  const base = resolve(dirname(from), specifier);
  for (const candidate of [
    base,
    base.replace(/\.js$/u, ".ts"),
    base.replace(/\.js$/u, ".tsx"),
    base.replace(/\.mjs$/u, ".mts"),
    `${base}.ts`,
    join(base, "index.ts"),
  ])
    if (existsSync(candidate) && statSync(candidate).isFile()) return candidate;
  return undefined;
};

// Value imports and re-exports (type-only statements are erased by every
// transform and are skipped), dynamic imports, and `vi.importActual`.
const IMPORT_PATTERN =
  /(?:^|[^\w.$])(?:import(?!\s+type\b)\s*(?:[\w*{}\s,$]+\s+from\s*)?|export(?!\s+type\b)\s*(?:[\w*{}\s,$]+\s+)?from\s*|import\s*\(\s*|vi\.importActual\s*(?:<[^>]*>)?\s*\(\s*)["']([^"']+)["']/gu;
const MOCK_PATTERN =
  /vi\s*\.\s*(?:mock|doMock|unmock|doUnmock)\s*(?:<[^>]*>)?\s*\(\s*["']([^"']+)["']/gu;
const MODULE_RESET_PATTERN = /vi\s*\.\s*(?:resetModules|isolateModules)\s*\(/u;
const NAMESPACE_PATTERNS = [
  /import\s+\*\s+as\s+([\w$]+)\s+from\s*["']([^"']+)["']/gu,
  /(?:const|let|var)\s+([\w$]+)\s*=\s*await\s+import\s*\(\s*["']([^"']+)["']\s*\)/gu,
];
/**
 * Specifiers whose module namespace object the file passes to `spyOn`. Under
 * vite-node a namespace is a plain object that every importer reads through,
 * so such a spy reaches callers in other modules; a bundled module's
 * namespace is a frozen native one that bundled callers bypass.
 */
const spiedNamespaces = (text) => {
  const spied = [];
  for (const pattern of NAMESPACE_PATTERNS)
    for (const [, name, specifier] of text.matchAll(pattern))
      if (
        new RegExp(
          `spyOn\\s*\\(\\s*${name.replace(/\$/gu, "\\$")}\\s*,`,
          "u",
        ).test(text)
      )
        spied.push(specifier);
  return spied;
};

const scanCache = new Map();
export const scan = (file) => {
  let scanned = scanCache.get(file);
  if (scanned === undefined) {
    const text = readFileSync(file, "utf8");
    scanned = {
      imports: [...text.matchAll(IMPORT_PATTERN)].map((match) => match[1]),
      mocks: [...text.matchAll(MOCK_PATTERN)].map((match) => match[1]),
      resetsModules: MODULE_RESET_PATTERN.test(text),
      readsImportMetaEnv: /\bimport\.meta\.env\b/u.test(text),
      spiedNamespaces: spiedNamespaces(text),
    };
    scanCache.set(file, scanned);
  }
  return scanned;
};

export const isNodeBuiltin = (specifier) =>
  specifier.startsWith("node:") ||
  builtinModules.includes(specifier.split("/")[0]);
