/** Explicit database-test setup and child-process lifetime. Importing this module does not register suite hooks; callers invoke registerDatabaseSetup. */
import { spawn, spawnSync } from "node:child_process";
import { default as fs } from "node:fs";
import { default as path } from "node:path";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { beforeAll, onTestFinished } from "vitest";

import * as MigrationRunner from "../../src/database/migrations/runner.js";
import { provideDatabaseLayers, resetApplicationTables } from "../utils.js";

export const collectChildProcess = (child: ReturnType<typeof spawn>) => {
  let stdout = "";
  let stderr = "";
  child.stdout?.on("data", (chunk: Buffer) => {
    stdout += chunk.toString("utf8");
  });
  child.stderr?.on("data", (chunk: Buffer) => {
    stderr += chunk.toString("utf8");
  });
  return new Promise<{
    readonly code: number | null;
    readonly signal: NodeJS.Signals | null;
    readonly stdout: string;
    readonly stderr: string;
  }>((resolve, reject) => {
    child.once("error", reject);
    child.once("exit", (code, signal) =>
      resolve({ code, signal, stdout, stderr }),
    );
  });
};

export const bundleChildProcessHelper = (
  relativeSourcePath: string,
): string => {
  const cwd = path.resolve(databaseTestDirectory, "..");
  const sourcePath = path.resolve(databaseTestDirectory, relativeSourcePath);
  const outputDirectory = path.resolve(cwd, ".probe-dist");
  fs.mkdirSync(outputDirectory, { recursive: true });
  const outputPath = path.resolve(
    outputDirectory,
    `${path.basename(relativeSourcePath, ".ts")}-${process.pid.toString()}.mjs`,
  );
  // The bundle, and a log its caller writes beside it, end with the test.
  onTestFinished(() => {
    fs.rmSync(outputPath, { force: true });
    fs.rmSync(`${outputPath}.log`, { force: true });
  });
  const esbuild = path.resolve(cwd, "node_modules/.bin/esbuild");
  const result = spawnSync(
    esbuild,
    [
      sourcePath,
      "--bundle",
      "--platform=node",
      "--format=esm",
      "--packages=external",
      "--alias:@=./src",
      "--loader:.sql=text",
      `--outfile=${outputPath}`,
    ],
    { cwd, encoding: "utf8" },
  );
  if (result.status !== 0) {
    const diagnostic =
      result.error?.message ??
      (result.stderr?.trim() || undefined) ??
      `status=${String(result.status)}, signal=${String(result.signal)}`;
    throw new Error(
      `Failed to bundle child helper ${relativeSourcePath}: ${diagnostic}`,
    );
  }
  return outputPath;
};

export const databaseChildProcessEnv = (): NodeJS.ProcessEnv => {
  const env = { ...process.env };
  const required = [
    "POSTGRES_HOST",
    "POSTGRES_PORT",
    "POSTGRES_USER",
    "POSTGRES_PASSWORD",
    "POSTGRES_DB",
  ] as const;
  for (const key of required) {
    const value = process.env[key];
    if (value === undefined || value === "") {
      throw new Error(`Missing explicit child database setting: ${key}`);
    }
    env[key] = value;
  }
  return env;
};

export const registerDatabaseSetup = () => {
  beforeAll(async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          // Ensure a clean schema: drop tables (and thus indexes) if they exist
          yield* sql`
          DROP SCHEMA public CASCADE;
          CREATE SCHEMA public;`;
          yield* MigrationRunner.migrate({
            appVersion: "test",
            actor: "database.test",
          });
          yield* resetApplicationTables;
        }),
      ),
    );
  });
};

export const databaseTestDirectory = path.resolve(__dirname, "..");

export const isolatedDb = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      return yield* effect;
    }),
  );
