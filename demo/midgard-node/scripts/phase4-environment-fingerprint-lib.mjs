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
export const canonicalJsonSha256 = (value) =>
  createHash("sha256").update(canonicalJson(value)).digest("hex");

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
