#!/usr/bin/env node

import { execFile } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { promisify } from "node:util";
import { fileURLToPath } from "node:url";

import {
  absoluteArg,
  captureClosureIdentity,
  sameSourceIdentity,
  sourceIdentity,
  writeAtomicImmutableJson,
} from "./phase3-architecture-g-closure-lib.mjs";
import {
  evaluatePhase3ReleaseImageReport,
  PHASE3_RELEASE_IMAGE_AUTHORIZATION,
  PHASE3_RELEASE_IMAGE_SCENARIO,
  PHASE3_RELEASE_IMAGE_SCHEMA,
} from "./verify-phase3-architecture-g-release-image-report.mjs";

const execFileAsync = promisify(execFile);
const scriptPath = fileURLToPath(import.meta.url);
const scriptDir = path.dirname(scriptPath);
const packageRoot = path.resolve(scriptDir, "..");
const verifierPath = path.join(
  scriptDir,
  "verify-phase3-architecture-g-release-image-report.mjs",
);

const requiredTextArg = (argv, name) => {
  const index = argv.indexOf(name);
  const value = index < 0 ? undefined : argv[index + 1];
  if (
    value === undefined ||
    value.startsWith("--") ||
    value.trim().length === 0
  ) {
    throw new Error(`missing required ${name}`);
  }
  return value;
};

const probeSource = String.raw`
void (async () => {
const fs = require("node:fs");
const crypto = require("node:crypto");
const v8 = require("node:v8");
const path = require("node:path");
const ownerPath = "/app/native/architecture-g-owner";
const manifestPath = "/app/native/architecture-g-owner.sha256";
const bytes = fs.readFileSync(ownerPath);
if (bytes.length < 64 || bytes[0] !== 0x7f || bytes.subarray(1, 4).toString() !== "ELF" || bytes[4] !== 2 || bytes[5] !== 1) throw new Error("owner is not ELF64 little-endian");
const sectionOffset = Number(bytes.readBigUInt64LE(40));
const sectionEntrySize = bytes.readUInt16LE(58);
const sectionCount = bytes.readUInt16LE(60);
const sectionStringIndex = bytes.readUInt16LE(62);
if (sectionOffset <= 0 || sectionEntrySize < 64 || sectionCount <= 0 || sectionStringIndex >= sectionCount) throw new Error("invalid ELF section table");
const sectionHeader = (index) => sectionOffset + index * sectionEntrySize;
const stringsHeader = sectionHeader(sectionStringIndex);
const stringsOffset = Number(bytes.readBigUInt64LE(stringsHeader + 24));
const stringsSize = Number(bytes.readBigUInt64LE(stringsHeader + 32));
const strings = bytes.subarray(stringsOffset, stringsOffset + stringsSize);
const sectionNames = [];
for (let index = 0; index < sectionCount; index += 1) {
  const nameOffset = bytes.readUInt32LE(sectionHeader(index));
  const end = strings.indexOf(0, nameOffset);
  sectionNames.push(strings.subarray(nameOffset, end < 0 ? strings.length : end).toString());
}
const findCommand = (command) => {
  const directories = new Set([...String(process.env.PATH || "").split(path.delimiter), "/usr/local/cargo/bin", "/usr/local/bin", "/usr/bin", "/bin"]);
  for (const directory of directories) {
    const candidate = path.join(directory, command);
    try { fs.accessSync(candidate, fs.constants.X_OK); return candidate; } catch {}
  }
  return null;
};
const response = await fetch("http://127.0.0.1:3000/readyz");
const readiness = await response.json();
const memoryMax = fs.readFileSync("/sys/fs/cgroup/memory.max", "utf8").trim();
const configured = Object.fromEntries(["MPF_ENGINE", "MPF_NATIVE_OWNER_BINARY_PATH", "MPF_NATIVE_OWNER_BINARY_SHA256"].map((name) => [name, process.env[name] ?? null]));
process.stdout.write(JSON.stringify({
  nodeVersion: process.version,
  nativeEntries: fs.readdirSync("/app/native").sort(),
  ownerExecutable: (fs.statSync(ownerPath).mode & 0o111) !== 0,
  ownerElf64LittleEndian: true,
  ownerSha256: crypto.createHash("sha256").update(bytes).digest("hex"),
  manifestOwnerSha256: fs.readFileSync(manifestPath, "utf8").trim().split(/\s+/)[0].toLowerCase(),
  manifestSha256: crypto.createHash("sha256").update(fs.readFileSync(manifestPath)).digest("hex"),
  hasStaticSymbolTable: sectionNames.includes(".symtab"),
  debugSections: sectionNames.filter((name) => name.startsWith(".debug")),
  compilerPaths: Object.fromEntries(["cargo", "rustc", "gcc", "cc", "clang", "make"].map((command) => [command, findCommand(command)])),
  cgroupMemoryLimitBytes: memoryMax === "max" ? null : Number(memoryMax),
  v8HeapLimitBytes: v8.getHeapStatistics().heap_size_limit,
  configured,
  readiness: { httpStatus: response.status, ready: readiness.ready === true, reasons: Array.isArray(readiness.reasons) ? readiness.reasons : null },
}));
})().catch((error) => { process.stderr.write(String(error && error.stack || error) + "\n"); process.exit(1); });
`;

const main = async () => {
  const argv = process.argv.slice(2);
  const reportPath = absoluteArg(argv, "--report");
  const imageReference = requiredTextArg(argv, "--image");
  const container = requiredTextArg(argv, "--container");
  const runtimePath = absoluteArg(argv, "--runtime-fingerprint");
  const deploymentPath = absoluteArg(argv, "--deployment-manifest");
  const phase1Path = absoluteArg(argv, "--phase1-formal-binding");
  const ownerBinaryPath = absoluteArg(argv, "--owner-binary");
  const ownerSha256ManifestPath = absoluteArg(argv, "--owner-sha256-manifest");
  const authorization = process.env.MIDGARD_PHASE3_RELEASE_IMAGE_INSPECTION;
  if (authorization !== PHASE3_RELEASE_IMAGE_AUTHORIZATION) {
    throw new Error(
      `MIDGARD_PHASE3_RELEASE_IMAGE_INSPECTION must equal ${PHASE3_RELEASE_IMAGE_AUTHORIZATION}`,
    );
  }
  if (fs.existsSync(reportPath))
    throw new Error(`refusing to overwrite ${reportPath}`);
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
  const [{ stdout: imageRaw }, { stdout: containerRaw }, { stdout: probeRaw }] =
    await Promise.all([
      execFileAsync("docker", ["image", "inspect", imageReference], {
        maxBuffer: 16 * 1024 * 1024,
      }),
      execFileAsync("docker", ["inspect", container], {
        maxBuffer: 16 * 1024 * 1024,
      }),
      execFileAsync("docker", ["exec", container, "node", "-e", probeSource], {
        maxBuffer: 16 * 1024 * 1024,
      }),
    ]);
  const imageInspect = JSON.parse(imageRaw)[0];
  const containerInspect = JSON.parse(containerRaw)[0];
  const probe = JSON.parse(probeRaw);
  const sourceAtCompletion = await sourceIdentity(packageRoot);
  const report = {
    schemaVersion: PHASE3_RELEASE_IMAGE_SCHEMA,
    scenario: PHASE3_RELEASE_IMAGE_SCENARIO,
    authorization,
    observedAtMs: Date.now(),
    identity,
    sourceAtCompletion,
    image: {
      reference: imageReference,
      imageId: imageInspect?.Id,
      inspectedReferences: [
        ...(imageInspect?.RepoTags ?? []),
        ...(imageInspect?.RepoDigests ?? []),
      ].sort(),
      containerImageId: containerInspect?.Image,
      containerConfiguredReference: containerInspect?.Config?.Image,
      healthcheckCommand: containerInspect?.Config?.Healthcheck?.Test,
    },
    filesystem: {
      nativeEntries: probe.nativeEntries,
      ownerExecutable: probe.ownerExecutable,
      ownerElf64LittleEndian: probe.ownerElf64LittleEndian,
      ownerSha256: probe.ownerSha256,
      manifestOwnerSha256: probe.manifestOwnerSha256,
      manifestSha256: probe.manifestSha256,
      hasStaticSymbolTable: probe.hasStaticSymbolTable,
      debugSections: probe.debugSections,
      compilerPaths: probe.compilerPaths,
    },
    runtime: {
      nodeVersion: probe.nodeVersion,
      engine: probe?.configured?.MPF_ENGINE,
      ownerBinaryPath: probe?.configured?.MPF_NATIVE_OWNER_BINARY_PATH,
      configuredOwnerSha256: probe?.configured?.MPF_NATIVE_OWNER_BINARY_SHA256,
      dockerMemoryLimitBytes: containerInspect?.HostConfig?.Memory,
      cgroupMemoryLimitBytes: probe.cgroupMemoryLimitBytes,
      v8HeapLimitBytes: probe.v8HeapLimitBytes,
      containerRunning: containerInspect?.State?.Running,
      containerHealth: containerInspect?.State?.Health?.Status,
      readiness: probe.readiness,
    },
    verdict:
      sameSourceIdentity(identity.source, sourceAtCompletion) &&
      probe.ownerSha256 === identity.ownerBinary.sha256
        ? "passed"
        : "failed",
  };
  const evaluation = evaluatePhase3ReleaseImageReport(report);
  report.verdict = evaluation.passed ? "passed" : "failed";
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
