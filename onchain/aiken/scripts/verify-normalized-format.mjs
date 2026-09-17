#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import {
  copyFileSync,
  lstatSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, isAbsolute, join, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

const usage =
  "usage: node scripts/verify-normalized-format.mjs <project-relative-file.ak> [<project-relative-file.ak> ...]";
const MAX_PATH_LENGTH = 4096;
const DISALLOWED_PATH_CHARACTER = /[\u0000-\u001f\u007f\\*?[\]{}]/u;
const TRAILING_LINE_WHITESPACE = new Set([0x09, 0x0b, 0x0c, 0x0d, 0x20]);

const projectDirectory = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const realProjectDirectory = realpathSync(projectDirectory);

const isWithin = (candidate, directory) => {
  const pathFromDirectory = relative(directory, candidate);
  return (
    pathFromDirectory !== "" &&
    !pathFromDirectory.startsWith(`..${sep}`) &&
    pathFromDirectory !== ".." &&
    !isAbsolute(pathFromDirectory)
  );
};

const parseRequestedFiles = (args) => {
  if (args.length === 0) {
    throw new Error(usage);
  }

  const files = args.map((argument) => {
    const segments = argument.split("/");
    if (
      argument.length > MAX_PATH_LENGTH ||
      argument.startsWith("-") ||
      isAbsolute(argument) ||
      !argument.endsWith(".ak") ||
      DISALLOWED_PATH_CHARACTER.test(argument) ||
      segments.some(
        (segment) => segment === "" || segment === "." || segment === "..",
      )
    ) {
      throw new Error(`invalid Aiken project-relative file path: ${argument}`);
    }

    const sourcePath = resolve(projectDirectory, argument);
    if (!isWithin(sourcePath, projectDirectory)) {
      throw new Error(`Aiken file path is outside the project: ${argument}`);
    }

    const metadata = lstatSync(sourcePath);
    if (!metadata.isFile() || metadata.isSymbolicLink()) {
      throw new Error(`Aiken file must be a regular file: ${argument}`);
    }
    if (!isWithin(realpathSync(sourcePath), realProjectDirectory)) {
      throw new Error(
        `Aiken file path resolves outside the project: ${argument}`,
      );
    }

    return { argument, sourcePath };
  });

  const uniqueArguments = new Set(files.map(({ argument }) => argument));
  if (uniqueArguments.size !== files.length) {
    throw new Error("Aiken file paths must be unique");
  }

  return files;
};

const stripTrailingLineWhitespace = (bytes) => {
  const normalized = [];
  let lineStart = 0;

  for (let index = 0; index <= bytes.length; index += 1) {
    if (index !== bytes.length && bytes[index] !== 0x0a) {
      continue;
    }

    let lineEnd = index;
    while (
      lineEnd > lineStart &&
      TRAILING_LINE_WHITESPACE.has(bytes[lineEnd - 1])
    ) {
      lineEnd -= 1;
    }
    normalized.push(bytes.subarray(lineStart, lineEnd));
    if (index !== bytes.length) {
      normalized.push(Buffer.from([0x0a]));
    }
    lineStart = index + 1;
  }

  return Buffer.concat(normalized);
};

// Issue #579, owner ruling A (2026-08-13): the patched fork is the sole
// authority for `fmt` as well as `build` and `check`, so this gate must not run
// whatever `aiken` happens to be first on PATH. Formatting is not cosmetic
// here — this script decides whether a tracked source is correctly formatted,
// and two compilers with different formatters disagree about that, so an
// unpinned binary would publish one compiler's verdict under the other's name.
//
// Resolution order matches the Q62/Q63 gates: `MIDGARD_AIKEN_BIN`, then
// `MIDGARD_FORK_AIKEN_BIN`, then a bare `aiken` from PATH. The PATH fallback is
// kept because every caller in the F05 task manifest invokes this script with
// no environment at all, and CI puts the pinned fork on PATH; it is safe only
// because the identity below is then MEASURED rather than assumed. A stock
// binary reaching this point fails closed.
//
// The version alone is not the identity. Upstream will one day release a stock
// v1.1.23, and a version-prefix-only check would then accept it here — the exact
// failure this gate exists to prevent, arriving silently. So the REV is checked
// too: `.github/workflows/aiken-ci.yml` pins AIKEN_FORK_REV
// 5adf7837cbddb5d329fd51d9c0cd73f561eaf95c at tag midgard-5adf7837 and asserts
// the installed binary reports `aiken v1.1.23+${AIKEN_FORK_REV:0:7}` (line 105).
// This gate asserts the same two halves, so a stock build of the same version,
// or a fork built from a different commit, fails closed rather than formatting.
const FORK_VERSION_PREFIX = "aiken v1.1.23";
const FORK_REV_SUFFIX = "+5adf783";

const resolveForkBinary = () => {
  const named = ["MIDGARD_AIKEN_BIN", "MIDGARD_FORK_AIKEN_BIN"].find(
    (name) =>
      typeof process.env[name] === "string" && process.env[name].length > 0,
  );
  const binary = named === undefined ? "aiken" : process.env[named];
  const source =
    named === undefined ? "the bare `aiken` on PATH" : `${named}=${binary}`;

  const version = spawnSync(binary, ["--version"], { encoding: "utf8" });
  if (version.error !== undefined || version.status !== 0) {
    throw new Error(
      `ERR_AIKEN_BINARY_UNPINNED: could not run \`${binary} --version\` (${source}) — this gate is fork-only and will not guess a formatter`,
    );
  }

  const reported = version.stdout.trim();
  if (
    !reported.startsWith(FORK_VERSION_PREFIX) ||
    !reported.endsWith(FORK_REV_SUFFIX)
  ) {
    throw new Error(
      `ERR_AIKEN_BINARY_UNPINNED: ${source} reports "${reported}", which is not the patched Aiken fork this gate requires (expected a ${FORK_VERSION_PREFIX}${FORK_REV_SUFFIX} build — the version AND the pinned rev, because a future upstream stock ${FORK_VERSION_PREFIX} would otherwise pass this check). Stock is retired from all roles by the 2026-08-13 owner ruling on #579; set MIDGARD_AIKEN_BIN to the pinned fork, or put it on PATH.`,
    );
  }

  return binary;
};

const runFormatter = (temporaryDirectory, files) => {
  const result = spawnSync(
    resolveForkBinary(),
    ["fmt", ...files.map(({ argument }) => argument)],
    {
      cwd: temporaryDirectory,
      encoding: "utf8",
      maxBuffer: 16 * 1024 * 1024,
    },
  );

  if (result.stdout) {
    process.stdout.write(result.stdout);
  }
  if (result.stderr) {
    process.stderr.write(result.stderr);
  }
  if (result.error !== undefined) {
    throw new Error(`could not run aiken fmt: ${result.error.message}`);
  }
  if (result.status !== 0) {
    throw new Error(`aiken fmt exited with status ${String(result.status)}`);
  }
};

let temporaryDirectory;
let exitCode = 1;

try {
  const files = parseRequestedFiles(process.argv.slice(2));
  temporaryDirectory = mkdtempSync(
    join(tmpdir(), "midgard-aiken-normalized-format-"),
  );

  for (const { argument, sourcePath } of files) {
    const temporaryPath = resolve(temporaryDirectory, argument);
    if (!isWithin(temporaryPath, temporaryDirectory)) {
      throw new Error(`temporary Aiken file path is unsafe: ${argument}`);
    }
    mkdirSync(dirname(temporaryPath), { recursive: true });
    copyFileSync(sourcePath, temporaryPath);
  }

  runFormatter(temporaryDirectory, files);
  const failures = files
    .filter(({ argument, sourcePath }) => {
      const formattedPath = resolve(temporaryDirectory, argument);
      return !stripTrailingLineWhitespace(readFileSync(formattedPath)).equals(
        readFileSync(sourcePath),
      );
    })
    .map(({ argument }) => argument);

  if (failures.length === 0) {
    console.log(
      `normalized Aiken format: PASS (${String(files.length)}/${String(files.length)})`,
    );
    exitCode = 0;
  } else {
    console.error(
      `normalized Aiken format: FAIL (${String(failures.length)}/${String(files.length)})`,
    );
    for (const file of failures) {
      console.error(file);
    }
  }
} catch (error) {
  console.error(error instanceof Error ? error.message : String(error));
} finally {
  if (temporaryDirectory !== undefined) {
    try {
      rmSync(temporaryDirectory, { recursive: true, force: true });
    } catch (error) {
      console.error(
        `could not remove temporary Aiken format directory: ${error instanceof Error ? error.message : String(error)}`,
      );
      exitCode = 1;
    }
  }
}

process.exit(exitCode);
