#!/usr/bin/env node

import fs from "node:fs";
import { fileURLToPath } from "node:url";

import {
  absoluteArg,
  evaluateClosureIdentity,
  evaluateClosureIdentityArtifacts,
  sameSourceIdentity,
  SHA256,
} from "./phase3-architecture-g-closure-lib.mjs";

export const PHASE3_RELEASE_IMAGE_SCHEMA =
  "midgard-phase3-architecture-g-release-image-v1";
export const PHASE3_RELEASE_IMAGE_SCENARIO =
  "phase3-architecture-g-release-image-inspection-v1";
export const PHASE3_RELEASE_IMAGE_AUTHORIZATION =
  "architecture-g-release-image-inspection-v1";
export const PHASE3_RELEASE_MIN_HEADROOM_BYTES = 3 * 1024 ** 3;
const IMAGE_ID = /^(?:sha256:)?[0-9a-f]{64}$/u;

export const evaluatePhase3ReleaseImageReport = (
  report,
  { checkArtifacts = true } = {},
) => {
  const reasons = [];
  if (report?.schemaVersion !== PHASE3_RELEASE_IMAGE_SCHEMA) {
    reasons.push("unexpected release-image report schema");
  }
  if (report?.scenario !== PHASE3_RELEASE_IMAGE_SCENARIO) {
    reasons.push("unexpected release-image scenario");
  }
  if (report?.authorization !== PHASE3_RELEASE_IMAGE_AUTHORIZATION) {
    reasons.push("release-image inspection authorization is absent");
  }
  reasons.push(...evaluateClosureIdentity(report?.identity));
  if (checkArtifacts) {
    reasons.push(...evaluateClosureIdentityArtifacts(report?.identity));
  }
  if (
    !sameSourceIdentity(report?.identity?.source, report?.sourceAtCompletion)
  ) {
    reasons.push("source tree changed during release-image inspection");
  }
  const image = report?.image;
  if (
    typeof image?.reference !== "string" ||
    image.reference.length === 0 ||
    !Array.isArray(image?.inspectedReferences) ||
    !image.inspectedReferences.includes(image.reference) ||
    !IMAGE_ID.test(image?.imageId ?? "") ||
    image?.imageId !== image?.containerImageId ||
    image?.containerConfiguredReference !== image.reference ||
    image?.imageId.replace(/^sha256:/u, "") !==
      String(report?.identity?.phase1?.nodeImageId ?? "").replace(
        /^sha256:/u,
        "",
      )
  ) {
    reasons.push("running container does not use the inspected image ID");
  }
  if (
    !Array.isArray(image?.healthcheckCommand) ||
    !image.healthcheckCommand.some(
      (value) => typeof value === "string" && value.includes("/readyz"),
    )
  ) {
    reasons.push("release image has no /readyz healthcheck");
  }
  const filesystem = report?.filesystem;
  if (
    JSON.stringify(filesystem?.nativeEntries) !==
    JSON.stringify(["architecture-g-owner", "architecture-g-owner.sha256"])
  ) {
    reasons.push(
      "/app/native contains files beyond the owner and SHA manifest",
    );
  }
  if (
    filesystem?.ownerExecutable !== true ||
    filesystem?.ownerElf64LittleEndian !== true ||
    filesystem?.ownerSha256 !== report?.identity?.ownerBinary?.sha256 ||
    filesystem?.manifestOwnerSha256 !== filesystem?.ownerSha256 ||
    !SHA256.test(filesystem?.manifestSha256 ?? "")
  ) {
    reasons.push("runtime owner binary is not the executable pinned artifact");
  }
  if (
    filesystem?.hasStaticSymbolTable !== false ||
    !Array.isArray(filesystem?.debugSections) ||
    filesystem.debugSections.length !== 0
  ) {
    reasons.push("runtime owner binary is not stripped");
  }
  const compilers = filesystem?.compilerPaths;
  const requiredAbsent = ["cargo", "rustc", "gcc", "cc", "clang", "make"];
  if (
    typeof compilers !== "object" ||
    compilers === null ||
    requiredAbsent.some((command) => compilers[command] !== null)
  ) {
    reasons.push(
      "Cargo, Rust, compiler, or build tooling is present in runtime",
    );
  }
  const runtime = report?.runtime;
  if (
    runtime?.nodeVersion !== "v22.22.2" ||
    runtime?.engine !== "architecture_g" ||
    runtime?.ownerBinaryPath !== "/app/native/architecture-g-owner" ||
    runtime?.configuredOwnerSha256 !== report?.identity?.ownerBinary?.sha256
  ) {
    reasons.push(
      "running node is not pinned to the inspected Architecture G owner",
    );
  }
  if (
    !Number.isSafeInteger(runtime?.dockerMemoryLimitBytes) ||
    !Number.isSafeInteger(runtime?.cgroupMemoryLimitBytes) ||
    runtime.dockerMemoryLimitBytes !== runtime.cgroupMemoryLimitBytes ||
    !Number.isSafeInteger(runtime?.v8HeapLimitBytes) ||
    runtime.cgroupMemoryLimitBytes - runtime.v8HeapLimitBytes <
      PHASE3_RELEASE_MIN_HEADROOM_BYTES
  ) {
    reasons.push(
      "cgroup memory does not provide the required V8 plus 3 GiB headroom",
    );
  }
  if (
    runtime?.containerRunning !== true ||
    runtime?.containerHealth !== "healthy" ||
    runtime?.readiness?.httpStatus !== 200 ||
    runtime?.readiness?.ready !== true ||
    !Array.isArray(runtime?.readiness?.reasons) ||
    runtime.readiness.reasons.length !== 0
  ) {
    reasons.push("running release container is not healthy and ready");
  }
  if (report?.verdict !== "passed")
    reasons.push("report verdict is not passed");
  return { passed: reasons.length === 0, reasons };
};

const isMain = process.argv[1] === fileURLToPath(import.meta.url);
if (isMain) {
  try {
    const reportPath = absoluteArg(process.argv.slice(2), "--report");
    const result = evaluatePhase3ReleaseImageReport(
      JSON.parse(fs.readFileSync(reportPath, "utf8")),
    );
    process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
    if (!result.passed) process.exitCode = 1;
  } catch (error) {
    process.stderr.write(
      `${error instanceof Error ? error.message : String(error)}\n`,
    );
    process.exitCode = 1;
  }
}
