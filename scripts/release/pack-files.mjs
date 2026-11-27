#!/usr/bin/env node
// Puts the repository LICENSE into a package while it is packed, and takes it
// out again afterwards: each published package's `prepack` runs `copy` and
// its `postpack` runs `remove`. A package cannot list a file above its own
// directory in `files`, so the tree keeps one licence file.
//
// A LICENSE that differs from the repository's is never replaced or deleted.
// Validation's vendored package copies are not pack files: its build writes
// them (see demo/scripts/lib/vendored-imports.mjs).
import { cpSync, existsSync, readFileSync, rmSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const REPOSITORY_LICENSE = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../../LICENSE",
);

const sameAsRepository = (path) =>
  readFileSync(path).equals(readFileSync(REPOSITORY_LICENSE));

/** Copies the repository LICENSE into `packageDir`. */
export const copyPackFiles = (packageDir) => {
  const license = join(packageDir, "LICENSE");
  if (!existsSync(license)) cpSync(REPOSITORY_LICENSE, license);
  else if (!sameAsRepository(license))
    throw new Error(`${license} differs from the repository LICENSE`);
};

/** Removes what `copyPackFiles` put into `packageDir`. */
export const removePackFiles = (packageDir) => {
  const license = join(packageDir, "LICENSE");
  if (existsSync(license) && sameAsRepository(license)) rmSync(license);
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
