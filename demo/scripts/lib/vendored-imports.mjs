#!/usr/bin/env node
// Vendored package imports: a package `imports` entry whose `midgard-source`
// target is an installed package (validation's `#merkle-patricia-forestry`,
// which the workspace installs from the patched tarball in demo/vendor/).
// Workspace sources resolve that package directly; every other condition
// points at a verbatim copy of it under `vendor/<name>/`, which the package's
// build writes beside dist. So built output, a `pnpm deploy --prod` copy of
// it and the registry tarball all run the same vendored bytes, and none of
// them needs the package installed.
// A vendored package under a licence that requires it gets a `SOURCE.md`
// beside it that says where its modified source is.
//
// Run from a package directory (a build step), it replaces that package's
// vendor/ with fresh copies of its vendored imports.
import {
  cpSync,
  existsSync,
  readdirSync,
  readFileSync,
  realpathSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { join, relative, sep } from "node:path";
import { fileURLToPath } from "node:url";

/** The workspace-only condition that points at TypeScript sources. */
const SOURCE_CONDITION = "midgard-source";

/** The directory of a package that holds its vendored copies. */
const VENDOR = "vendor";

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

/** The notice file written beside a vendored package that needs one. */
export const SOURCE_NOTICE = "SOURCE.md";

const SOURCE_NOTICES = {
  "@aiken-lang/merkle-patricia-forestry": `# Source

This directory is a modified copy of \`@aiken-lang/merkle-patricia-forestry\`.
The modified source is at
https://github.com/Anastasia-Labs/midgard/tree/main/demo/vendor/mpf-source.
The files in this directory are under the Mozilla Public License 2.0
(MPL-2.0); see LICENSE.
`,
};

/** The `SOURCE.md` text for the vendored `dependency`, or undefined. */
export const vendoredSourceNotice = (dependency) => SOURCE_NOTICES[dependency];

/**
 * Paths under the vendored copy `copy` of `dependency`, installed at
 * `installed`, that are not what the build writes: the installed package
 * byte for byte plus its source notice, if it has one.
 */
export const vendoredCopyDifferences = (installed, copy, dependency) => {
  const notice = vendoredSourceNotice(dependency);
  if (notice === undefined) return treeDifferences(installed, copy);
  const differences = treeDifferences(installed, copy).filter(
    (file) => file !== SOURCE_NOTICE,
  );
  const noticePath = join(copy, SOURCE_NOTICE);
  return existsSync(noticePath) && readFileSync(noticePath, "utf8") === notice
    ? differences
    : [...differences, SOURCE_NOTICE];
};

/** Where the build puts the package an `imports` entry names. */
export const vendoredDirectory = (dependency) =>
  `${VENDOR}/${dependency.split("/").pop()}`;

/**
 * The vendored package imports of the package in `packageDir`: each
 * `imports` entry whose `midgard-source` target is a package name, with its
 * declared conditions and the installed copy of that package.
 */
export const vendoredImports = (packageDir) => {
  const manifest = JSON.parse(
    readFileSync(join(packageDir, "package.json"), "utf8"),
  );
  return Object.entries(manifest.imports ?? {})
    .filter(
      ([, target]) =>
        target !== null &&
        typeof target === "object" &&
        typeof target[SOURCE_CONDITION] === "string" &&
        !target[SOURCE_CONDITION].startsWith("."),
    )
    .map(([specifier, target]) => {
      const dependency = target[SOURCE_CONDITION];
      const installed = join(packageDir, "node_modules", dependency);
      return {
        specifier,
        target,
        dependency,
        installed: existsSync(installed) ? realpathSync(installed) : undefined,
        directory: vendoredDirectory(dependency),
      };
    });
};

/**
 * Replaces the vendor/ directory of the package in `packageDir` with a copy
 * of each of its vendored imports, as installed, and its source notice.
 */
export const writeVendoredImports = (packageDir) => {
  const imports = vendoredImports(packageDir);
  if (imports.length === 0)
    throw new Error(`${packageDir} declares no vendored package imports`);
  for (const { dependency, installed } of imports)
    if (installed === undefined)
      throw new Error(`${dependency} is not installed in ${packageDir}`);
  rmSync(join(packageDir, VENDOR), { recursive: true, force: true });
  for (const { dependency, installed, directory } of imports) {
    const target = join(packageDir, directory);
    cpSync(installed, target, { recursive: true, dereference: true });
    const notice = vendoredSourceNotice(dependency);
    if (notice !== undefined)
      writeFileSync(join(target, SOURCE_NOTICE), notice);
  }
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  if (process.argv.length !== 2) {
    console.error("Usage: vendored-imports.mjs (run in the package)");
    process.exitCode = 2;
  } else writeVendoredImports(process.cwd());
}
