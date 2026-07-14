#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";
import { createHash } from "node:crypto";

import {
  canonicalJsonSha256,
  PHASE4_ENVIRONMENT_ARTIFACT_SCHEMA,
  PHASE4_ENVIRONMENT_SCHEMA,
  PHASE4_RESOURCE_PROFILE,
  parseCpuSet,
  sameCpuSet,
  validImageId,
} from "./phase4-environment-fingerprint-lib.mjs";

const outputPath = process.argv[2];
if (outputPath === undefined) {
  throw new Error(
    "usage: capture-phase4-environment-fingerprint.mjs <output.json>",
  );
}

const required = (name) => {
  const value = String(process.env[name] ?? "").trim();
  if (value.length === 0) throw new Error(`${name} is required`);
  return value;
};
const positiveInteger = (name) => {
  const value = Number(required(name));
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${name} must be a positive safe integer`);
  }
  return value;
};
const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");
const component = (prefix, name) => {
  const value = {
    cpuSet: required(`${prefix}_CPUSET`),
    nanoCpus: positiveInteger(`${prefix}_NANO_CPUS`),
    imageId: required(`${prefix}_IMAGE_ID`),
    memoryLimitBytes: positiveInteger(`${prefix}_MEMORY_LIMIT_BYTES`),
  };
  const expected = PHASE4_RESOURCE_PROFILE[name];
  if (!sameCpuSet(parseCpuSet(value.cpuSet), expected.cpus)) {
    throw new Error(
      `${prefix}_CPUSET must identify exactly ${expected.cpus.join(",")}`,
    );
  }
  if (value.nanoCpus !== expected.nanoCpus) {
    throw new Error(`${prefix}_NANO_CPUS must equal ${expected.nanoCpus}`);
  }
  if (value.memoryLimitBytes < expected.minMemoryLimitBytes) {
    throw new Error(
      `${prefix}_MEMORY_LIMIT_BYTES must be at least ${expected.minMemoryLimitBytes}`,
    );
  }
  if (!validImageId(value.imageId)) {
    throw new Error(`${prefix}_IMAGE_ID must be a SHA-256 image ID or digest`);
  }
  return value;
};

const deploymentManifestPath = path.resolve(
  required("MIDGARD_DEPLOYMENT_MANIFEST_PATH"),
);
const placement = required("STRESS_LOAD_GENERATOR_PLACEMENT");
if (placement !== "separate-host" && placement !== "separate-container") {
  throw new Error(
    "load generator must use separate-host or separate-container",
  );
}
const cohostedRaw = required("STRESS_LOADGEN_COHOSTED");
if (cohostedRaw !== "true" && cohostedRaw !== "false") {
  throw new Error("STRESS_LOADGEN_COHOSTED must be true or false");
}
const clockOffsetMs = Number(required("STRESS_CLOCK_OFFSET_MS"));
if (!Number.isFinite(clockOffsetMs)) {
  throw new Error("STRESS_CLOCK_OFFSET_MS must be finite");
}

const document = {
  schemaVersion: PHASE4_ENVIRONMENT_SCHEMA,
  capturedAtIso: new Date().toISOString(),
  node: component("PHASE4_NODE", "node"),
  loadGenerator: {
    ...component("PHASE4_LOADGEN", "loadGenerator"),
    placement,
    cohosted: cohostedRaw === "true",
  },
  postgres: component("PHASE4_POSTGRES", "postgres"),
  provider: {
    kind: required("L1_PROVIDER"),
    routeSha256: sha256(
      Buffer.from(
        JSON.stringify({
          kupo: required("L1_KUPO_KEY"),
          ogmios: required("L1_OGMIOS_KEY"),
        }),
      ),
    ),
  },
  deploymentManifest: {
    path: deploymentManifestPath,
    sha256: sha256(fs.readFileSync(deploymentManifestPath)),
  },
  clockOffsetMs,
};

const artifact = {
  schemaVersion: PHASE4_ENVIRONMENT_ARTIFACT_SCHEMA,
  documentSha256: canonicalJsonSha256(document),
  document,
};

fs.mkdirSync(path.dirname(path.resolve(outputPath)), { recursive: true });
fs.writeFileSync(outputPath, `${JSON.stringify(artifact, null, 2)}\n`);
console.log(
  JSON.stringify(
    { outputPath: path.resolve(outputPath), ...artifact },
    null,
    2,
  ),
);
