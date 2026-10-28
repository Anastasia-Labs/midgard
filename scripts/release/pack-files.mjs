#!/usr/bin/env node
// Puts into a package, while it is packed, the files it publishes from outside
// its own directory, and takes them out again afterwards: each published
// package's `prepack` runs `copy` and its `postpack` runs `remove`.
//
// - LICENSE: the repository licence. A package cannot list a file above its
//   own directory in `files`, so the tree keeps one licence file.
// - Vendored package imports: an `imports` entry whose workspace target is a
//   package (validation's `#merkle-patricia-forestry`, which the workspace
//   installs from the patched tarball in vendor/). The installed package is
//   copied verbatim to `vendor/<name>/`, where the entry's `publishConfig`
//   form points, so the registry package runs exactly the vendored code.
//
// A LICENSE or vendored copy that differs from what `copy` would write is
// never replaced or deleted.
import {
  cpSync,
  existsSync,
  readdirSync,
  readFileSync,
  realpathSync,
  rmSync,
  statSync,
} from "node:fs";
import { dirname, join, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

const REPOSITORY_LICENSE = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../../LICENSE",
);

const filesUnder = (root, base = root) =>
  readdirSync(root, { withFileTypes: true }).flatMap((entry) => {
    const path = join(root, entry.name);
    if (entry.isDirectory()) return filesUnder(path, base);
    return entry.isFile() || statSync(path).isFile()
      ? [relative(base, path).split(sep).join("/")]
      : [];
  });

/**
 * Paths under `left` or `right` whose bytes differ or that only one side has;
 * empty when the two trees are the same files with the same bytes.
 */
export const treeDifferences = (left, right) => {
  const leftFiles = new Set(filesUnder(left));
  const rightFiles = new Set(filesUnder(right));
  return [...new Set([...leftFiles, ...rightFiles])]
    .sort()
    .filter(
      (file) =>
        !leftFiles.has(file) ||
        !rightFiles.has(file) ||
        !readFileSync(join(left, file)).equals(readFileSync(join(right, file))),
    );
};

/** Where `copy` puts the package an `imports` entry names. */
export const vendoredDirectory = (dependency) =>
  `vendor/${dependency.split("/").pop()}`;

/**
 * The vendored package imports of the package in `packageDir`: each
 * `imports` entry whose target is a package name, with the installed copy of
 * that package.
 */
export const vendoredImports = (packageDir) => {
  const manifest = JSON.parse(
    readFileSync(join(packageDir, "package.json"), "utf8"),
  );
  return Object.entries(manifest.imports ?? {})
    .filter(
      ([, target]) => typeof target === "string" && !target.startsWith("."),
    )
    .map(([specifier, dependency]) => {
      const installed = join(packageDir, "node_modules", dependency);
      return {
        specifier,
        dependency,
        installed: existsSync(installed) ? realpathSync(installed) : undefined,
        directory: vendoredDirectory(dependency),
      };
    });
};

const sameAsRepository = (path) =>
  readFileSync(path).equals(readFileSync(REPOSITORY_LICENSE));

/** Copies the repository LICENSE and every vendored import into `packageDir`. */
export const copyPackFiles = (packageDir) => {
  const license = join(packageDir, "LICENSE");
  if (!existsSync(license)) cpSync(REPOSITORY_LICENSE, license);
  else if (!sameAsRepository(license))
    throw new Error(`${license} differs from the repository LICENSE`);
  for (const { dependency, installed, directory } of vendoredImports(
    packageDir,
  )) {
    if (installed === undefined)
      throw new Error(`${dependency} is not installed in ${packageDir}`);
    const target = join(packageDir, directory);
    if (!existsSync(target))
      cpSync(installed, target, { recursive: true, dereference: true });
    else if (treeDifferences(installed, target).length > 0)
      throw new Error(`${target} differs from the installed ${dependency}`);
  }
};

/** Removes what `copyPackFiles` put into `packageDir`. */
export const removePackFiles = (packageDir) => {
  const license = join(packageDir, "LICENSE");
  if (existsSync(license) && sameAsRepository(license)) rmSync(license);
  for (const { installed, directory } of vendoredImports(packageDir)) {
    const target = join(packageDir, directory);
    if (
      installed !== undefined &&
      existsSync(target) &&
      treeDifferences(installed, target).length === 0
    ) {
      rmSync(target, { recursive: true });
      const parent = dirname(target);
      if (readdirSync(parent).length === 0) rmSync(parent, { recursive: true });
    }
  }
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const action = { copy: copyPackFiles, remove: removePackFiles }[
    process.argv[2] ?? ""
  ];
  if (action === undefined) {
    console.error("Usage: pack-files.mjs copy|remove (run in the package)");
    process.exitCode = 2;
  } else action(process.cwd());
}
