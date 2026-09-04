import { createHash } from "node:crypto";

export const PHASE4_ENVIRONMENT_ARTIFACT_SCHEMA =
  "midgard-phase4-environment-artifact-v1";
export const PHASE4_ENVIRONMENT_SCHEMA = "midgard-phase4-environment-v1";

export const PHASE4_RESOURCE_PROFILE = Object.freeze({
  node: Object.freeze({
    cpus: Object.freeze([0, 1, 2, 3, 4, 5, 6, 7]),
    nanoCpus: 4_000_000_000,
    minMemoryLimitBytes: 8 * 1024 ** 3,
  }),
  loadGenerator: Object.freeze({
    cpus: Object.freeze([8, 9, 10, 11, 12, 13, 14, 15]),
    nanoCpus: 4_000_000_000,
    minMemoryLimitBytes: 4 * 1024 ** 3,
  }),
  postgres: Object.freeze({
    cpus: Object.freeze([16, 17, 18, 19, 20, 21, 22, 23]),
    nanoCpus: 4_000_000_000,
    minMemoryLimitBytes: 8 * 1024 ** 3,
  }),
});

const canonicalize = (value) => {
  if (Array.isArray(value)) return value.map(canonicalize);
  if (value !== null && typeof value === "object") {
    return Object.fromEntries(
      Object.keys(value)
        .sort()
        .map((key) => [key, canonicalize(value[key])]),
    );
  }
  return value;
};

export const canonicalJson = (value) => JSON.stringify(canonicalize(value));
export const canonicalJsonSha256 = (value) => {
  if (
    value !== null &&
    typeof value === "object" &&
    !Array.isArray(value) &&
    value.schemaVersion === PHASE4_ENVIRONMENT_SCHEMA
  ) {
    try {
      decodePhase4EnvironmentDocumentV1(value);
    } catch {
      // Hash consumers compare against a 64-hex digest. An invalid exact-V1
      // document therefore cannot be rehashed into an apparently valid one.
      return "";
    }
  }
  return createHash("sha256").update(canonicalJson(value)).digest("hex");
};

export const parseCpuSet = (value) => {
  if (typeof value !== "string" || value.trim().length === 0) return null;
  const cpus = new Set();
  for (const token of value.split(",")) {
    const trimmed = token.trim();
    const match = /^(\d+)(?:-(\d+))?$/u.exec(trimmed);
    if (match === null) return null;
    const start = Number(match[1]);
    const end = match[2] === undefined ? start : Number(match[2]);
    if (
      !Number.isSafeInteger(start) ||
      !Number.isSafeInteger(end) ||
      end < start
    ) {
      return null;
    }
    for (let cpu = start; cpu <= end; cpu += 1) {
      if (cpus.has(cpu)) return null;
      cpus.add(cpu);
    }
  }
  return [...cpus].sort((left, right) => left - right);
};

export const sameCpuSet = (actual, expected) =>
  actual !== null &&
  actual.length === expected.length &&
  actual.every((cpu, index) => cpu === expected[index]);

export const validImageId = (value) =>
  typeof value === "string" && /^(?:sha256:)?[0-9a-f]{64}$/u.test(value);

const object = (value) =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const exactKeys = (value, expected) =>
  object(value) &&
  Object.keys(value).length === expected.length &&
  expected.every((key) => Object.hasOwn(value, key));

const sha256Hex = (value) =>
  typeof value === "string" && /^[a-f0-9]{64}$/u.test(value);

const canonicalIsoTimestamp = (value) =>
  typeof value === "string" &&
  Number.isFinite(Date.parse(value)) &&
  new Date(value).toISOString() === value;

const canonicalCpuSet = (cpus) => {
  const first = cpus[0];
  const last = cpus.at(-1);
  return first === last ? String(first) : `${String(first)}-${String(last)}`;
};

const decodeComponent = (value, profile, label) => {
  if (
    !exactKeys(value, ["cpuSet", "nanoCpus", "imageId", "memoryLimitBytes"])
  ) {
    throw new Error(`${label} fields do not match the exact V1 schema`);
  }
  if (
    value.cpuSet !== canonicalCpuSet(profile.cpus) ||
    value.nanoCpus !== profile.nanoCpus ||
    typeof value.imageId !== "string" ||
    !/^sha256:[a-f0-9]{64}$/u.test(value.imageId) ||
    !Number.isSafeInteger(value.memoryLimitBytes) ||
    value.memoryLimitBytes < profile.minMemoryLimitBytes
  ) {
    throw new Error(`${label} contains a noncanonical V1 value`);
  }
};

export const decodePhase4EnvironmentDocumentV1 = (value) => {
  if (
    !exactKeys(value, [
      "schemaVersion",
      "capturedAtIso",
      "node",
      "loadGenerator",
      "postgres",
      "provider",
      "deploymentManifest",
      "clockOffsetMs",
    ])
  ) {
    throw new Error(
      "Phase 4 environment document fields do not match the exact V1 schema",
    );
  }
  if (
    value.schemaVersion !== PHASE4_ENVIRONMENT_SCHEMA ||
    !canonicalIsoTimestamp(value.capturedAtIso)
  ) {
    throw new Error(
      "Phase 4 environment document has a noncanonical schema or timestamp",
    );
  }
  decodeComponent(value.node, PHASE4_RESOURCE_PROFILE.node, "node");
  decodeComponent(value.postgres, PHASE4_RESOURCE_PROFILE.postgres, "postgres");
  if (
    !exactKeys(value.loadGenerator, [
      "cpuSet",
      "nanoCpus",
      "imageId",
      "memoryLimitBytes",
      "placement",
      "cohosted",
    ])
  ) {
    throw new Error("loadGenerator fields do not match the exact V1 schema");
  }
  decodeComponent(
    {
      cpuSet: value.loadGenerator.cpuSet,
      nanoCpus: value.loadGenerator.nanoCpus,
      imageId: value.loadGenerator.imageId,
      memoryLimitBytes: value.loadGenerator.memoryLimitBytes,
    },
    PHASE4_RESOURCE_PROFILE.loadGenerator,
    "loadGenerator",
  );
  if (
    !["separate-host", "separate-container"].includes(
      value.loadGenerator.placement,
    ) ||
    typeof value.loadGenerator.cohosted !== "boolean"
  ) {
    throw new Error("loadGenerator contains a noncanonical V1 value");
  }
  if (
    !exactKeys(value.provider, ["kind", "routeSha256"]) ||
    value.provider.kind !== "Kupmios" ||
    !sha256Hex(value.provider.routeSha256)
  ) {
    throw new Error("provider does not match the exact V1 schema");
  }
  if (
    !exactKeys(value.deploymentManifest, ["path", "sha256"]) ||
    typeof value.deploymentManifest.path !== "string" ||
    !pathIsCanonicalAbsolute(value.deploymentManifest.path) ||
    !sha256Hex(value.deploymentManifest.sha256)
  ) {
    throw new Error("deploymentManifest does not match the exact V1 schema");
  }
  if (
    typeof value.clockOffsetMs !== "number" ||
    !Number.isFinite(value.clockOffsetMs) ||
    Object.is(value.clockOffsetMs, -0)
  ) {
    throw new Error("clockOffsetMs must be a finite V1 number");
  }
  return value;
};

const pathIsCanonicalAbsolute = (value) =>
  value.startsWith("/") &&
  !value.includes("/../") &&
  !value.includes("/./") &&
  !value.endsWith("/..") &&
  !value.endsWith("/.");

export const decodePhase4EnvironmentArtifactV1 = (value) => {
  if (!exactKeys(value, ["schemaVersion", "documentSha256", "document"])) {
    throw new Error(
      "Phase 4 environment artifact fields do not match the exact V1 schema",
    );
  }
  if (value.schemaVersion !== PHASE4_ENVIRONMENT_ARTIFACT_SCHEMA) {
    throw new Error("Phase 4 environment artifact schema is not V1");
  }
  const document = decodePhase4EnvironmentDocumentV1(value.document);
  if (
    !sha256Hex(value.documentSha256) ||
    value.documentSha256 !== canonicalJsonSha256(document)
  ) {
    throw new Error("Phase 4 environment artifact document digest is invalid");
  }
  return value;
};
