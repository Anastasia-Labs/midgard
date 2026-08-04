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

const runFormatter = (temporaryDirectory, files) => {
  const result = spawnSync(
    "aiken",
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
