#!/usr/bin/env node

import { spawn } from "node:child_process";
import { randomUUID } from "node:crypto";
import {
  copyFileSync,
  existsSync,
  lstatSync,
  mkdtempSync,
  readFileSync,
  renameSync,
  rmSync,
  unlinkSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const MAX_BLUEPRINT_BYTES = 64 * 1024 * 1024;
const MAX_VALIDATOR_COUNT = 64;
const VALID_TITLE = /^[a-z0-9_/-]+\.[a-z0-9_-]+\.[a-z0-9_-]+$/u;
const VALID_ENVIRONMENT = /^[a-z0-9_-]+$/u;

const usage =
  "usage: node scripts/run-diagnostic-build.mjs <validator-title> [<validator-title> ...]";

export const parseValidatorTitles = (args) => {
  if (
    args.length === 0 ||
    args.length > MAX_VALIDATOR_COUNT ||
    args.some((title) => title.length > 256 || !VALID_TITLE.test(title))
  ) {
    throw new Error(usage);
  }

  const titles = new Set(args);
  if (titles.size !== args.length) {
    throw new Error("validator titles must be unique");
  }

  return titles;
};

const requireBoundedRegularFile = (path, label) => {
  const metadata = lstatSync(path);
  if (!metadata.isFile() || metadata.isSymbolicLink()) {
    throw new Error(`${label} must be a regular file`);
  }
  if (metadata.size > MAX_BLUEPRINT_BYTES) {
    throw new Error(
      `${label} exceeds the ${String(MAX_BLUEPRINT_BYTES)}-byte diagnostic limit`,
    );
  }
};

export const snapshotBlueprint = (blueprintPath, snapshotDirectory) => {
  const snapshotPath = join(snapshotDirectory, "plutus.json");
  const existed = existsSync(blueprintPath);

  if (existed) {
    requireBoundedRegularFile(blueprintPath, "existing plutus.json");
    copyFileSync(blueprintPath, snapshotPath);
  }

  return { blueprintPath, existed, snapshotPath };
};

export const restoreBlueprint = (snapshot) => {
  const { blueprintPath, existed, snapshotPath } = snapshot;

  if (!existed) {
    if (existsSync(blueprintPath)) {
      requireBoundedRegularFile(blueprintPath, "generated plutus.json");
      unlinkSync(blueprintPath);
    }
    return;
  }

  requireBoundedRegularFile(snapshotPath, "saved plutus.json");
  const restorePath = join(
    dirname(blueprintPath),
    `.plutus.json.midgard-restore-${String(process.pid)}-${randomUUID()}`,
  );

  try {
    copyFileSync(snapshotPath, restorePath);
    renameSync(restorePath, blueprintPath);
  } finally {
    if (existsSync(restorePath)) {
      unlinkSync(restorePath);
    }
  }
};

export const readValidatorMeasurements = (blueprintPath, requestedTitles) => {
  requireBoundedRegularFile(blueprintPath, "generated plutus.json");

  let blueprint;
  try {
    blueprint = JSON.parse(readFileSync(blueprintPath, "utf8"));
  } catch (error) {
    throw new Error(
      `generated plutus.json is invalid JSON: ${error instanceof Error ? error.message : String(error)}`,
    );
  }

  if (!Array.isArray(blueprint.validators)) {
    throw new Error("generated plutus.json does not contain validators[]");
  }

  const validatorsByTitle = new Map();
  for (const validator of blueprint.validators) {
    if (
      typeof validator?.title !== "string" ||
      typeof validator?.compiledCode !== "string"
    ) {
      continue;
    }
    if (validatorsByTitle.has(validator.title)) {
      throw new Error(
        `generated plutus.json contains duplicate validator title ${validator.title}`,
      );
    }
    validatorsByTitle.set(validator.title, validator);
  }

  return [...requestedTitles].map((title) => {
    const validator = validatorsByTitle.get(title);
    if (validator === undefined) {
      throw new Error(`generated plutus.json is missing ${title}`);
    }
    if (
      validator.compiledCode.length % 2 !== 0 ||
      !/^[0-9a-f]*$/u.test(validator.compiledCode)
    ) {
      throw new Error(`${title} has invalid compiledCode hex`);
    }

    return {
      title,
      rawBytes: validator.compiledCode.length / 2,
      parameterCount: Array.isArray(validator.parameters)
        ? validator.parameters.length
        : 0,
    };
  });
};

const runBuild = async (binary, args, projectDirectory) => {
  const child = spawn(binary, args, {
    cwd: projectDirectory,
    stdio: "inherit",
  });
  let interruptedBy;

  const forwardSignal = (signal) => {
    interruptedBy ??= signal;
    child.kill(signal);
  };
  const signals = ["SIGINT", "SIGTERM", "SIGHUP"];
  const signalHandlers = new Map(
    signals.map((signal) => [signal, () => forwardSignal(signal)]),
  );
  for (const [signal, handler] of signalHandlers) {
    process.on(signal, handler);
  }

  try {
    return await new Promise((resolvePromise, rejectPromise) => {
      child.once("error", rejectPromise);
      child.once("exit", (code, signal) => {
        resolvePromise({ code, signal, interruptedBy });
      });
    });
  } finally {
    for (const [signal, handler] of signalHandlers) {
      process.off(signal, handler);
    }
  }
};

const isMain =
  process.argv[1] !== undefined &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url);

if (isMain) {
  let exitCode = 1;
  let snapshotDirectory;
  let snapshot;

  try {
    const requestedTitles = parseValidatorTitles(process.argv.slice(2));
    const environment = process.env.MIDGARD_AIKEN_ENV;
    if (environment !== undefined && !VALID_ENVIRONMENT.test(environment)) {
      throw new Error("MIDGARD_AIKEN_ENV contains an invalid environment name");
    }

    const projectDirectory = resolve(
      dirname(fileURLToPath(import.meta.url)),
      "..",
    );
    const blueprintPath = join(projectDirectory, "plutus.json");
    snapshotDirectory = mkdtempSync(
      join(tmpdir(), "midgard-aiken-diagnostic-"),
    );
    snapshot = snapshotBlueprint(blueprintPath, snapshotDirectory);

    const buildArgs = ["build"];
    if (environment !== undefined) {
      buildArgs.push("--env", environment);
    }

    const result = await runBuild(
      process.env.MIDGARD_AIKEN_BIN ?? "aiken",
      buildArgs,
      projectDirectory,
    );
    if (
      result.interruptedBy !== undefined ||
      result.signal !== null ||
      result.code !== 0
    ) {
      const reason =
        result.interruptedBy ??
        result.signal ??
        `status ${String(result.code)}`;
      throw new Error(`Aiken build ended with ${reason}`);
    }

    const measurements = readValidatorMeasurements(
      blueprintPath,
      requestedTitles,
    );
    process.stdout.write(`${JSON.stringify({ validators: measurements })}\n`);
    exitCode = 0;
  } catch (error) {
    console.error(error instanceof Error ? error.message : String(error));
  } finally {
    try {
      if (snapshot !== undefined) {
        restoreBlueprint(snapshot);
      }
    } catch (error) {
      exitCode = 1;
      console.error(
        `failed to restore plutus.json: ${error instanceof Error ? error.message : String(error)}`,
      );
    }
    if (snapshotDirectory !== undefined) {
      rmSync(snapshotDirectory, { recursive: true, force: true });
    }
    process.exitCode = exitCode;
  }
}
