import { createHash } from "node:crypto";

import { decodeSingleCbor, encodeCbor } from "@al-ft/midgard-core/codec/cbor";

import { watcherSha256CanonicalJson } from "../storage/durable-store.js";

export const HEX = /^(?:[0-9a-f]{2})+$/u;

export const HEX32 = /^[0-9a-f]{64}$/u;

export const HEX28 = /^[0-9a-f]{56}$/u;

export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

export function requireCondition(
  condition: unknown,
  field: string,
): asserts condition {
  if (!condition)
    throw new Error(`persisted replay record binding is invalid: ${field}`);
}

export const text = (
  value: unknown,
  field: string,
  pattern?: RegExp,
): string => {
  requireCondition(
    typeof value === "string" && (pattern === undefined || pattern.test(value)),
    field,
  );
  return value;
};

export const count = (value: unknown, field: string): number => {
  requireCondition(
    typeof value === "number" &&
      Number.isSafeInteger(value) &&
      value >= 0 &&
      !Object.is(value, -0),
    field,
  );
  return value;
};

export const list = (value: unknown, field: string): readonly unknown[] => {
  requireCondition(Array.isArray(value), field);
  return value;
};

export const record = (
  value: unknown,
  keys: readonly string[],
  field: string,
): Record<string, unknown> => {
  requireCondition(
    typeof value === "object" &&
      value !== null &&
      Object.getPrototypeOf(value) === Object.prototype,
    field,
  );
  requireCondition(
    Reflect.ownKeys(value).length === keys.length &&
      keys.every((key) => Object.prototype.hasOwnProperty.call(value, key)),
    `${field}.keys`,
  );
  return value as Record<string, unknown>;
};

export const equal = (left: unknown, right: unknown, field: string): void => {
  requireCondition(encodeCbor(left).equals(encodeCbor(right)), field);
};

export const nullableText = (
  value: unknown,
  field: string,
  pattern?: RegExp,
): void => {
  if (value !== null) text(value, field, pattern);
};

export const stringFields = (
  value: Record<string, unknown>,
  keys: readonly string[],
  field: string,
  pattern?: RegExp,
): void => {
  keys.forEach((key) => text(value[key], `${field}.${key}`, pattern));
};

export const countFields = (
  value: Record<string, unknown>,
  keys: readonly string[],
  field: string,
): void => {
  keys.forEach((key) => count(value[key], `${field}.${key}`));
};

export const stringList = (
  value: unknown,
  field: string,
  pattern?: RegExp,
): void => {
  list(value, field).forEach((entry) => text(entry, field, pattern));
};

const plainCbor = (value: unknown): unknown => {
  if (value instanceof Map) {
    const result: Record<string, unknown> = {};
    for (const [key, entry] of value) {
      requireCondition(
        typeof key === "string" &&
          !Object.prototype.hasOwnProperty.call(result, key),
        "CBOR map key",
      );
      Object.defineProperty(result, key, {
        enumerable: true,
        configurable: true,
        writable: true,
        value: plainCbor(entry),
      });
    }
    return result;
  }
  if (Array.isArray(value)) return value.map(plainCbor);
  return value;
};

/** Descriptive decoding only. This never admits an L1, event or replay authority. */
export const decodeWatcherReplayRawRecord = (cborHex: string): unknown => {
  text(cborHex, "CBOR", HEX);
  const decoded = plainCbor(decodeSingleCbor(Buffer.from(cborHex, "hex")));
  equal(encodeCbor(decoded).toString("hex"), cborHex, "canonical CBOR");
  return decoded;
};

export const resultDigest = (
  value: Record<string, unknown>,
  field: string,
): void => {
  const { resultDigest: digest, ...material } = value;
  equal(
    text(digest, `${field}.resultDigest`, HEX32),
    watcherSha256CanonicalJson(material),
    `${field}.resultDigest`,
  );
};

export const ROOT_KEYS = [
  "sequence",
  "txIndex",
  "txId",
  "stepIndex",
  "phase",
  "operation",
  "outRef",
  "preRoot",
  "postRoot",
] as const;

export const TX_ROOT_KEYS = [
  "txIndex",
  "txId",
  "preRoot",
  "postRoot",
  "mutationCount",
  "committedStepIndex",
  "committedPreRoot",
  "committedPostRoot",
] as const;

export const EVENT_ROOT_KEYS = [
  "stepIndex",
  "phase",
  "eventKeyFingerprint",
  "preRoot",
  "postRoot",
  "mutationCount",
] as const;

export const FACT_KEYS = [
  "eventKeyFingerprint",
  "stepIndex",
  "authenticatedOperatorValidity",
  "canonicalOperatorValidity",
  "phaseAStatus",
  "phaseARejectCode",
  "phaseBStatus",
  "phaseBRejectCode",
  "canonicalEffectDigest",
  "canonicalEffectMutationCount",
] as const;

export const REJECTION_KEYS = [
  "index",
  "txId",
  "code",
  "consensusPhase",
  "consensusPhasePriority",
  "stage",
  "detail",
] as const;

export const W25_KEYS = [
  "schemaVersion",
  "action",
  "reasonCodes",
  "verifiedRequires",
  "downstreamPrerequisite",
  "rejectionSelection",
  "consensusProfileId",
  "headerHash",
  "payloadEnvelopeSha256",
  "payloadSha256",
  "reconstructionDigest",
  "phaseAResultDigest",
  "ruleBundleCommitment",
  "authorityManifestDigest",
  "sourceManifestDigest",
  "effectManifestDigest",
  "priorStateRoot",
  "expectedPriorStateRoot",
  "postStateRoot",
  "expectedPostStateRoot",
  "transactionCount",
  "acceptedCount",
  "acceptedTxIds",
  "intermediateRoots",
  "transactionRoots",
  "eventRoots",
  "forcedValidationFacts",
  "stageMismatches",
  "rejections",
  "selectedRejection",
  "resultDigest",
] as const;
