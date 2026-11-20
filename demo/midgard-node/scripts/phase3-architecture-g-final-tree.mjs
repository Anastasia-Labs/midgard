#!/usr/bin/env node

import { spawn } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  absoluteArg,
  captureClosureIdentity,
  sameSourceIdentity,
  sha256File,
  sourceIdentity,
  writeAtomicImmutableJson,
} from "./phase3-architecture-g-closure-lib.mjs";
import {
  evaluatePhase3FinalTreeReport,
  PHASE3_FINAL_TREE_AUTHORIZATION,
  PHASE3_FINAL_TREE_SCENARIO,
  PHASE3_FINAL_TREE_SCHEMA,
  PHASE3_FINAL_TREE_SUITES,
} from "./verify-phase3-architecture-g-final-tree-report.mjs";

const scriptPath = fileURLToPath(import.meta.url);
const scriptDir = path.dirname(scriptPath);
const packageRoot = path.resolve(scriptDir, "..");
const verifierPath = path.join(
  scriptDir,
  "verify-phase3-architecture-g-final-tree-report.mjs",
);
const SUITE_TIMEOUT_MS = 3 * 60 * 60_000;

const runSuite = ({ suite, artifactDirectory }) =>
  new Promise((resolve) => {
    const startedAtMs = Date.now();
    const stdoutPath = path.join(artifactDirectory, `${suite.id}.stdout.log`);
    const stderrPath = path.join(artifactDirectory, `${suite.id}.stderr.log`);
    for (const filePath of [stdoutPath, stderrPath]) {
      if (fs.existsSync(filePath))
        throw new Error(`refusing to overwrite ${filePath}`);
    }
    const stdout = fs.createWriteStream(stdoutPath, {
      flags: "wx",
      mode: 0o600,
    });
    const stderr = fs.createWriteStream(stderrPath, {
      flags: "wx",
      mode: 0o600,
    });
    const [command, ...args] = suite.argv;
    const child = spawn(command, args, {
      cwd: packageRoot,
      env: {
        ...process.env,
        PATH: `${path.dirname(process.execPath)}:${process.env.PATH ?? ""}`,
        NODE_ENV: "test",
        TMPDIR: "/tmp",
      },
      stdio: ["ignore", "pipe", "pipe"],
    });
    child.stdout.pipe(stdout);
    child.stderr.pipe(stderr);
    let timedOut = false;
    let killTimeout;
    const timeout = setTimeout(() => {
      timedOut = true;
      child.kill("SIGTERM");
      killTimeout = setTimeout(() => child.kill("SIGKILL"), 10_000);
    }, SUITE_TIMEOUT_MS);
    child.once("error", (error) => {
      stderr.write(`${error.message}\n`);
    });
    child.once("close", (exitCode, signal) => {
      clearTimeout(timeout);
      clearTimeout(killTimeout);
      const streamsClosed = Promise.all([
        new Promise((done) => stdout.once("close", done)),
        new Promise((done) => stderr.once("close", done)),
      ]);
      stdout.end();
      stderr.end();
      streamsClosed.then(() => {
        resolve({
          id: suite.id,
          argv: suite.argv,
          coverage: suite.coverage,
          startedAtMs,
          completedAtMs: Date.now(),
          exitCode,
          signal,
          timedOut,
          completed: exitCode === 0 && signal === null && !timedOut,
          stdout: {
            path: stdoutPath,
            sha256: sha256File(stdoutPath),
            bytes: fs.statSync(stdoutPath).size,
          },
          stderr: {
            path: stderrPath,
            sha256: sha256File(stderrPath),
            bytes: fs.statSync(stderrPath).size,
          },
        });
      });
    });
  });

const main = async () => {
  const argv = process.argv.slice(2);
  const reportPath = absoluteArg(argv, "--report");
  const runtimePath = absoluteArg(argv, "--runtime-fingerprint");
  const deploymentPath = absoluteArg(argv, "--deployment-manifest");
  const phase1Path = absoluteArg(argv, "--phase1-formal-binding");
  const ownerBinaryPath = absoluteArg(argv, "--owner-binary");
  const ownerSha256ManifestPath = absoluteArg(argv, "--owner-sha256-manifest");
  const authorization = process.env.MIDGARD_PHASE3_FINAL_TREE;
  const databaseName = String(process.env.POSTGRES_DB ?? "");
  const databaseHost = String(process.env.POSTGRES_HOST ?? "127.0.0.1");
  if (authorization !== PHASE3_FINAL_TREE_AUTHORIZATION) {
    throw new Error(
      `MIDGARD_PHASE3_FINAL_TREE must equal ${PHASE3_FINAL_TREE_AUTHORIZATION}`,
    );
  }
  if (
    !/^midgard_phase3_arch_g_final_tree_[a-z0-9_]+$/u.test(databaseName) ||
    !["127.0.0.1", "localhost"].includes(databaseHost)
  ) {
    throw new Error(
      "POSTGRES_DB must be an isolated phase3 final-tree database on localhost",
    );
  }
  if (fs.existsSync(reportPath))
    throw new Error(`refusing to overwrite ${reportPath}`);
  const artifactDirectory = path.join(
    path.dirname(reportPath),
    "final-tree-suite-logs",
  );
  fs.mkdirSync(artifactDirectory, { recursive: true, mode: 0o700 });
  const identity = await captureClosureIdentity({
    packageRoot,
    runtimePath,
    deploymentPath,
    phase1Path,
    ownerBinaryPath,
    ownerSha256ManifestPath,
    runnerPath: scriptPath,
    verifierPath,
  });
  const startedAtMs = Date.now();
  const suites = [];
  for (const suite of PHASE3_FINAL_TREE_SUITES) {
    const result = await runSuite({ suite, artifactDirectory });
    suites.push(result);
    if (!result.completed) break;
  }
  const sourceAtCompletion = await sourceIdentity(packageRoot);
  const report = {
    schemaVersion: PHASE3_FINAL_TREE_SCHEMA,
    scenario: PHASE3_FINAL_TREE_SCENARIO,
    authorization,
    database: { host: databaseHost, name: databaseName },
    startedAtMs,
    completedAtMs: Date.now(),
    identity,
    sourceAtCompletion,
    suites,
    verdict:
      suites.length === PHASE3_FINAL_TREE_SUITES.length &&
      suites.every(({ completed }) => completed) &&
      sameSourceIdentity(identity.source, sourceAtCompletion)
        ? "passed"
        : "failed",
  };
  let evaluation = evaluatePhase3FinalTreeReport(report);
  report.verdict = evaluation.passed ? "passed" : "failed";
  evaluation = evaluatePhase3FinalTreeReport(report);
  writeAtomicImmutableJson(reportPath, report);
  process.stdout.write(
    `${JSON.stringify({ reportPath, ...evaluation }, null, 2)}\n`,
  );
  if (!evaluation.passed) process.exitCode = 1;
};

main().catch((error) => {
  process.stderr.write(
    `${error instanceof Error ? error.stack : String(error)}\n`,
  );
  process.exitCode = 1;
});
