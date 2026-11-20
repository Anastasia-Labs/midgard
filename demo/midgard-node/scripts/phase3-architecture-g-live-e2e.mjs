#!/usr/bin/env node

import { spawn } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  absoluteArg,
  assertRegularFile,
  captureClosureIdentity,
  createSecretScanningLog,
  readJson,
  sameSourceIdentity,
  sha256File,
  sourceIdentity,
  writeAtomicImmutableJson,
} from "./phase3-architecture-g-closure-lib.mjs";
import {
  evaluatePhase3LiveE2EReport,
  PHASE3_LIVE_COMMAND_SCHEMA,
  PHASE3_LIVE_E2E_AUTHORIZATION,
  PHASE3_LIVE_E2E_SCENARIO,
  PHASE3_LIVE_E2E_SCHEMA,
  PHASE3_LIVE_STEP_IDS,
  PHASE3_LIVE_STEP_SCHEMA,
} from "./verify-phase3-architecture-g-live-e2e-report.mjs";

const scriptPath = fileURLToPath(import.meta.url);
const scriptDir = path.dirname(scriptPath);
const packageRoot = path.resolve(scriptDir, "..");
const verifierPath = path.join(
  scriptDir,
  "verify-phase3-architecture-g-live-e2e-report.mjs",
);

const artifactIdentity = (filePath) => ({
  path: filePath,
  sha256: sha256File(filePath),
  bytes: fs.statSync(filePath).size,
});

const containsForbiddenEvidence = (value) => {
  if (typeof value === "string") return value.length > 4_096;
  if (Array.isArray(value)) return value.some(containsForbiddenEvidence);
  if (typeof value !== "object" || value === null) return false;
  return Object.entries(value).some(
    ([key, entry]) =>
      /(seed|mnemonic|phrase|private|secret|signed.*cbor|txcbor|rawcbor)/iu.test(
        key,
      ) || containsForbiddenEvidence(entry),
  );
};

const validateCommandManifest = (manifest, identity) => {
  if (manifest?.schemaVersion !== PHASE3_LIVE_COMMAND_SCHEMA) {
    throw new Error(
      `command manifest schema must be ${PHASE3_LIVE_COMMAND_SCHEMA}`,
    );
  }
  if (manifest?.authorization !== PHASE3_LIVE_E2E_AUTHORIZATION) {
    throw new Error("command manifest lacks the live E2E authorization token");
  }
  const binding = manifest?.binding;
  if (
    binding?.runtimeSha256 !== identity.runtime.sha256 ||
    binding?.deploymentSha256 !== identity.deployment.sha256 ||
    binding?.phase1Sha256 !== identity.phase1.sha256 ||
    binding?.ownerSha256 !== identity.ownerBinary.sha256
  ) {
    throw new Error(
      "command manifest does not bind the supplied closure identities",
    );
  }
  if (
    !Array.isArray(manifest?.steps) ||
    manifest.steps.length !== PHASE3_LIVE_STEP_IDS.length
  ) {
    throw new Error(
      "command manifest must contain every live step exactly once",
    );
  }
  for (const [index, stepId] of PHASE3_LIVE_STEP_IDS.entries()) {
    const step = manifest.steps[index];
    if (step?.id !== stepId) throw new Error(`expected command step ${stepId}`);
    if (typeof step?.command !== "string" || !path.isAbsolute(step.command)) {
      throw new Error(`${stepId} command must be absolute`);
    }
    assertRegularFile(step.command, `${stepId} command`);
    if ((fs.statSync(step.command).mode & 0o111) === 0) {
      throw new Error(`${stepId} command is not executable`);
    }
    if (
      !Array.isArray(step?.args) ||
      step.args.some((value) => typeof value !== "string") ||
      typeof step?.cwd !== "string" ||
      !path.isAbsolute(step.cwd) ||
      !fs.statSync(step.cwd).isDirectory() ||
      !Number.isSafeInteger(step?.timeoutMs) ||
      step.timeoutMs < 10_000 ||
      step.timeoutMs > 3 * 60 * 60_000
    ) {
      throw new Error(`${stepId} command configuration is invalid`);
    }
  }
};

const executeStep = ({ step, identity, artifactDirectory }) =>
  new Promise((resolve) => {
    const driverSha256 = sha256File(step.command);
    const stdoutPath = path.join(artifactDirectory, `${step.id}.stdout.log`);
    const stderrPath = path.join(artifactDirectory, `${step.id}.stderr.log`);
    const resultPath = path.join(artifactDirectory, `${step.id}.result.json`);
    for (const filePath of [stdoutPath, stderrPath, resultPath]) {
      if (fs.existsSync(filePath))
        throw new Error(`refusing to overwrite ${filePath}`);
    }
    const stdout = createSecretScanningLog(stdoutPath);
    const stderr = createSecretScanningLog(stderrPath);
    const child = spawn(step.command, step.args, {
      cwd: step.cwd,
      env: {
        ...process.env,
        PATH: `${path.dirname(process.execPath)}:${process.env.PATH ?? ""}`,
        MIDGARD_PHASE3_ARCH_G_E2E: PHASE3_LIVE_E2E_AUTHORIZATION,
        PHASE3_ARCH_G_STEP_ID: step.id,
        PHASE3_ARCH_G_STEP_OUTPUT_PATH: resultPath,
        PHASE3_ARCH_G_RUNTIME_SHA256: identity.runtime.sha256,
        PHASE3_ARCH_G_DEPLOYMENT_SHA256: identity.deployment.sha256,
        PHASE3_ARCH_G_PHASE1_SHA256: identity.phase1.sha256,
        PHASE3_ARCH_G_OWNER_SHA256: identity.ownerBinary.sha256,
      },
      stdio: ["ignore", "pipe", "pipe"],
    });
    child.stdout.pipe(stdout.stream, { end: false });
    child.stderr.pipe(stderr.stream, { end: false });
    let timedOut = false;
    let killTimeout;
    const timeout = setTimeout(() => {
      timedOut = true;
      child.kill("SIGTERM");
      killTimeout = setTimeout(() => child.kill("SIGKILL"), 10_000);
    }, step.timeoutMs);
    child.once("error", (error) => stderr.stream.write(`${error.message}\n`));
    child.once("close", (exitCode, signal) => {
      clearTimeout(timeout);
      clearTimeout(killTimeout);
      stdout.stream.end();
      stderr.stream.end();
      Promise.all([stdout.complete(), stderr.complete()]).then(
        ([stdoutArtifact, stderrArtifact]) => {
          const driverSha256AtCompletion = sha256File(step.command);
          const base = {
            id: step.id,
            driver: {
              path: step.command,
              sha256: driverSha256,
              args: step.args,
              cwd: step.cwd,
              timeoutMs: step.timeoutMs,
            },
            exitCode,
            signal,
            timedOut,
            completed: false,
            driverStable: driverSha256AtCompletion === driverSha256,
            stdout: stdoutArtifact,
            stderr: stderrArtifact,
          };
          if (
            exitCode !== 0 ||
            signal !== null ||
            timedOut ||
            !fs.existsSync(resultPath)
          ) {
            resolve(base);
            return;
          }
          try {
            assertRegularFile(resultPath, `${step.id} result`);
            if (fs.statSync(resultPath).size > 1024 * 1024) {
              throw new Error(`${step.id} result exceeds 1 MiB`);
            }
            const result = readJson(resultPath);
            if (containsForbiddenEvidence(result)) {
              throw new Error(
                `${step.id} result contains forbidden sensitive fields`,
              );
            }
            const completed =
              stdoutArtifact.secretScan.passed === true &&
              stderrArtifact.secretScan.passed === true &&
              driverSha256AtCompletion === driverSha256 &&
              result?.schemaVersion === PHASE3_LIVE_STEP_SCHEMA &&
              result?.stepId === step.id &&
              result?.verdict === "passed" &&
              result?.completed === true &&
              result?.binding?.runtimeSha256 === identity.runtime.sha256 &&
              result?.binding?.deploymentSha256 ===
                identity.deployment.sha256 &&
              result?.binding?.phase1Sha256 === identity.phase1.sha256 &&
              result?.binding?.ownerSha256 === identity.ownerBinary.sha256;
            resolve({
              ...base,
              completed,
              result,
              resultArtifact: artifactIdentity(resultPath),
            });
          } catch (error) {
            resolve({
              ...base,
              resultError:
                error instanceof Error ? error.message : String(error),
            });
          }
        },
        () => {
          resolve({
            id: step.id,
            driver: {
              path: step.command,
              sha256: driverSha256,
              args: step.args,
              cwd: step.cwd,
              timeoutMs: step.timeoutMs,
            },
            exitCode,
            signal,
            timedOut,
            completed: false,
            driverStable: false,
            resultError: "driver log secret scan failed closed",
          });
        },
      );
    });
  });

const main = async () => {
  const argv = process.argv.slice(2);
  const reportPath = absoluteArg(argv, "--report");
  const commandManifestPath = absoluteArg(argv, "--commands");
  const runtimePath = absoluteArg(argv, "--runtime-fingerprint");
  const deploymentPath = absoluteArg(argv, "--deployment-manifest");
  const phase1Path = absoluteArg(argv, "--phase1-formal-binding");
  const ownerBinaryPath = absoluteArg(argv, "--owner-binary");
  const ownerSha256ManifestPath = absoluteArg(argv, "--owner-sha256-manifest");
  const authorization = process.env.MIDGARD_PHASE3_ARCH_G_E2E;
  if (authorization !== PHASE3_LIVE_E2E_AUTHORIZATION) {
    throw new Error(
      `MIDGARD_PHASE3_ARCH_G_E2E must equal ${PHASE3_LIVE_E2E_AUTHORIZATION}`,
    );
  }
  if (fs.existsSync(reportPath))
    throw new Error(`refusing to overwrite ${reportPath}`);
  assertRegularFile(commandManifestPath, "command manifest");
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
  const commandManifestArtifact = artifactIdentity(commandManifestPath);
  const commandManifest = readJson(commandManifestPath);
  validateCommandManifest(commandManifest, identity);
  const artifactDirectory = path.join(
    path.dirname(reportPath),
    "live-e2e-steps",
  );
  fs.mkdirSync(artifactDirectory, { recursive: true, mode: 0o700 });
  const startedAtMs = Date.now();
  const steps = [];
  for (const step of commandManifest.steps) {
    const result = await executeStep({ step, identity, artifactDirectory });
    steps.push(result);
    if (!result.completed) break;
  }
  const sourceAtCompletion = await sourceIdentity(packageRoot);
  const report = {
    schemaVersion: PHASE3_LIVE_E2E_SCHEMA,
    scenario: PHASE3_LIVE_E2E_SCENARIO,
    authorization,
    startedAtMs,
    completedAtMs: Date.now(),
    identity,
    sourceAtCompletion,
    commandManifest: commandManifestArtifact,
    steps,
    verdict:
      steps.length === PHASE3_LIVE_STEP_IDS.length &&
      steps.every(({ completed }) => completed) &&
      sameSourceIdentity(identity.source, sourceAtCompletion)
        ? "passed"
        : "failed",
  };
  let evaluation = evaluatePhase3LiveE2EReport(report);
  report.verdict = evaluation.passed ? "passed" : "failed";
  evaluation = evaluatePhase3LiveE2EReport(report);
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
