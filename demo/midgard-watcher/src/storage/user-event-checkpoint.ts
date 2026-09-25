import { createHash } from "node:crypto";
import { isProxy } from "node:util/types";

import {
  type DeploymentMarker,
  parseDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  watcherCanonicalJson,
  watcherSameCanonicalJson,
  watcherSha256CanonicalJson,
} from "./durable-store.js";
import { verifyWatcherSqliteArchiveRecords } from "./sqlite-record-store.js";

export const WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION =
  "midgard-watcher-user-event-checkpoint-v1" as const;

export const WATCHER_USER_EVENT_VALIDATION_SCHEMA_VERSION =
  "midgard-watcher-user-event-validation-v1" as const;
export type WatcherUserEventValidation = Readonly<{
  schemaVersion: typeof WATCHER_USER_EVENT_VALIDATION_SCHEMA_VERSION;
  checkpointDigest: string;
  payloadDigest: string;
  policyDigest: string;
}>;

export const WATCHER_USER_EVENT_CHECKPOINT_BOUNDS = Object.freeze({
  frameBytes: 1024 * 1024,
  requiredArchiveObjects: 16_384,
  archiveObjectBytes: 128 * 1024 * 1024,
});

const HEX_32 = /^[0-9a-f]{64}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const UINT64_MAX = 18_446_744_073_709_551_615n;
const encoder = new TextEncoder();
const decoder = new TextDecoder("utf-8", { fatal: true });

export type WatcherUserEventCheckpointBinding = Readonly<{
  deploymentMarker: DeploymentMarker;
  network: "Mainnet" | "Preprod" | "Preview" | "Custom";
  blueprintHash: string;
  finalityPolicyDigest: string;
}>;

/**
 * Structural publication only. The archived payload owns the event view,
 * cursor, state, anchor and their semantic cross-bindings. A valid frame or
 * archive object does not establish user-event admission or challenge authority.
 */
export type WatcherUserEventCheckpoint = WatcherUserEventCheckpointBinding &
  Readonly<{
    schemaVersion: typeof WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION;
    userEventPolicyDigest: string;
    checkpointSequence: string;
    predecessorCheckpointDigest: string | null;
    rollbackGeneration: string;
    payloadDigest: string;
    requiredArchiveDigests: readonly string[];
    checkpointDigest: string;
  }>;

export type WatcherUserEventCheckpointExpectation = Readonly<{
  expectedCheckpointDigest: string | null;
  expectedCheckpointSequence: string | null;
}>;

/** Storage dependency, not an event-evidence authority. */
export type WatcherUserEventArchive = Readonly<{
  put(bytes: Uint8Array): Promise<string>;
  read(digest: string): Promise<Uint8Array | null>;
}>;

const fail = (): never => {
  throw new Error("watcher user-event checkpoint framing is invalid");
};

const exactRecord = (
  value: unknown,
  keys: readonly string[],
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    isProxy(value) ||
    Array.isArray(value) ||
    (Object.getPrototypeOf(value) !== Object.prototype &&
      Object.getPrototypeOf(value) !== null)
  ) {
    return fail();
  }
  const actual = Reflect.ownKeys(value);
  if (
    actual.length !== keys.length ||
    actual.some((key) => typeof key !== "string" || !keys.includes(key)) ||
    Object.values(Object.getOwnPropertyDescriptors(value)).some(
      (descriptor) => !("value" in descriptor) || !descriptor.enumerable,
    )
  ) {
    return fail();
  }
  return value as Readonly<Record<string, unknown>>;
};

const digest = (value: unknown): string =>
  typeof value === "string" && HEX_32.test(value) ? value : fail();

const natural = (value: unknown): string =>
  typeof value === "string" &&
  value.length <= 20 &&
  NATURAL.test(value) &&
  BigInt(value) <= UINT64_MAX
    ? value
    : fail();

const content = (
  value: Omit<WatcherUserEventCheckpoint, "checkpointDigest">,
): Omit<WatcherUserEventCheckpoint, "checkpointDigest"> => ({
  schemaVersion: value.schemaVersion,
  deploymentMarker: value.deploymentMarker,
  network: value.network,
  blueprintHash: value.blueprintHash,
  userEventPolicyDigest: value.userEventPolicyDigest,
  finalityPolicyDigest: value.finalityPolicyDigest,
  checkpointSequence: value.checkpointSequence,
  predecessorCheckpointDigest: value.predecessorCheckpointDigest,
  rollbackGeneration: value.rollbackGeneration,
  payloadDigest: value.payloadDigest,
  requiredArchiveDigests: value.requiredArchiveDigests,
});

export const parseWatcherUserEventCheckpoint = (
  value: unknown,
  binding: WatcherUserEventCheckpointBinding,
): WatcherUserEventCheckpoint => {
  const record = exactRecord(value, [
    "schemaVersion",
    "deploymentMarker",
    "network",
    "blueprintHash",
    "userEventPolicyDigest",
    "finalityPolicyDigest",
    "checkpointSequence",
    "predecessorCheckpointDigest",
    "rollbackGeneration",
    "payloadDigest",
    "requiredArchiveDigests",
    "checkpointDigest",
  ]);
  const markerRecord = exactRecord(record.deploymentMarker, [
    "schemaVersion",
    "manifestId",
  ]);
  const deploymentMarker = Object.freeze({
    ...parseDeploymentMarker(markerRecord),
  });
  if (
    record.schemaVersion !== WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION ||
    typeof record.network !== "string" ||
    !["Mainnet", "Preprod", "Preview", "Custom"].includes(record.network) ||
    !watcherSameCanonicalJson(deploymentMarker, binding.deploymentMarker) ||
    record.network !== binding.network ||
    record.blueprintHash !== binding.blueprintHash ||
    record.finalityPolicyDigest !== binding.finalityPolicyDigest ||
    !Array.isArray(record.requiredArchiveDigests) ||
    isProxy(record.requiredArchiveDigests) ||
    Object.getPrototypeOf(record.requiredArchiveDigests) !== Array.prototype ||
    record.requiredArchiveDigests.length === 0 ||
    record.requiredArchiveDigests.length >
      WATCHER_USER_EVENT_CHECKPOINT_BOUNDS.requiredArchiveObjects
  ) {
    return fail();
  }
  const archiveDescriptors = Object.getOwnPropertyDescriptors(
    record.requiredArchiveDigests,
  );
  if (
    Reflect.ownKeys(record.requiredArchiveDigests).length !==
      record.requiredArchiveDigests.length + 1 ||
    Array.from(
      { length: record.requiredArchiveDigests.length },
      (_, index) => archiveDescriptors[index],
    ).some(
      (descriptor) =>
        descriptor === undefined ||
        !("value" in descriptor) ||
        !descriptor.enumerable,
    )
  ) {
    return fail();
  }
  const requiredArchiveDigests = Object.freeze(
    record.requiredArchiveDigests.map(digest),
  );
  if (
    requiredArchiveDigests.some(
      (entry, index) =>
        index > 0 && entry <= requiredArchiveDigests[index - 1]!,
    )
  ) {
    return fail();
  }
  const checkpointSequence = natural(record.checkpointSequence);
  const predecessorCheckpointDigest =
    record.predecessorCheckpointDigest === null
      ? null
      : digest(record.predecessorCheckpointDigest);
  const payloadDigest = digest(record.payloadDigest);
  if (
    (checkpointSequence === "0") !== (predecessorCheckpointDigest === null) ||
    !requiredArchiveDigests.includes(payloadDigest)
  ) {
    return fail();
  }
  const canonical = content({
    schemaVersion: WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION,
    deploymentMarker,
    network: binding.network,
    blueprintHash: digest(record.blueprintHash),
    userEventPolicyDigest: digest(record.userEventPolicyDigest),
    finalityPolicyDigest: digest(record.finalityPolicyDigest),
    checkpointSequence,
    predecessorCheckpointDigest,
    rollbackGeneration: natural(record.rollbackGeneration),
    payloadDigest,
    requiredArchiveDigests,
  });
  const checkpointDigest = digest(record.checkpointDigest);
  const frame = Object.freeze({ ...canonical, checkpointDigest });
  if (
    watcherSha256CanonicalJson(canonical) !== checkpointDigest ||
    encoder.encode(watcherCanonicalJson(frame)).byteLength >
      WATCHER_USER_EVENT_CHECKPOINT_BOUNDS.frameBytes
  ) {
    return fail();
  }
  return frame;
};

export const makeWatcherUserEventCheckpoint = (
  value: Omit<WatcherUserEventCheckpoint, "checkpointDigest">,
): WatcherUserEventCheckpoint =>
  parseWatcherUserEventCheckpoint(
    {
      ...value,
      checkpointDigest: watcherSha256CanonicalJson(content(value)),
    },
    value,
  );

export const watcherUserEventCheckpointExpectationMatches = (
  current: WatcherUserEventCheckpoint | null,
  expectation: WatcherUserEventCheckpointExpectation,
): boolean => {
  const expectedDigest = expectation.expectedCheckpointDigest;
  const expectedSequence = expectation.expectedCheckpointSequence;
  if (
    (expectedDigest === null) !== (expectedSequence === null) ||
    (expectedDigest !== null && digest(expectedDigest) !== expectedDigest) ||
    (expectedSequence !== null &&
      natural(expectedSequence) !== expectedSequence)
  ) {
    return fail();
  }
  return (
    (current?.checkpointDigest ?? null) === expectedDigest &&
    (current?.checkpointSequence ?? null) === expectedSequence
  );
};

export const assertWatcherUserEventCheckpointSuccessor = (
  current: WatcherUserEventCheckpoint | null,
  next: WatcherUserEventCheckpoint,
): void => {
  if (
    next.checkpointSequence !==
      (current === null
        ? "0"
        : (BigInt(current.checkpointSequence) + 1n).toString()) ||
    next.predecessorCheckpointDigest !== (current?.checkpointDigest ?? null) ||
    (current !== null &&
      (next.userEventPolicyDigest !== current.userEventPolicyDigest ||
        next.network !== current.network ||
        next.blueprintHash !== current.blueprintHash ||
        next.finalityPolicyDigest !== current.finalityPolicyDigest ||
        !watcherSameCanonicalJson(
          next.deploymentMarker,
          current.deploymentMarker,
        ) ||
        BigInt(next.rollbackGeneration) < BigInt(current.rollbackGeneration)))
  ) {
    throw new Error("watcher user-event checkpoint is not the exact successor");
  }
};

export const copyWatcherUserEventArchiveBytes = (
  value: Uint8Array,
): Uint8Array => {
  if (
    !(value instanceof Uint8Array) ||
    isProxy(value) ||
    value.byteLength === 0 ||
    value.byteLength > WATCHER_USER_EVENT_CHECKPOINT_BOUNDS.archiveObjectBytes
  ) {
    throw new Error(
      "watcher user-event archive object exceeds its byte bounds",
    );
  }
  return Uint8Array.from(value);
};

export const watcherUserEventArchiveDigest = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

/** Verifies direct declared objects only; the upper owner verifies closure. */
export const readWatcherUserEventCheckpointPayload = async (
  checkpoint: WatcherUserEventCheckpoint,
  archive: WatcherUserEventArchive,
): Promise<Uint8Array> => {
  let payload: Uint8Array | undefined;
  const verified = verifyWatcherSqliteArchiveRecords(
    archive,
    checkpoint.requiredArchiveDigests,
  );
  const requiredReads = verified
    ? [checkpoint.payloadDigest]
    : checkpoint.requiredArchiveDigests;
  for (const expectedDigest of requiredReads) {
    const stored = await archive.read(expectedDigest);
    if (stored === null) {
      throw new Error(
        "watcher user-event checkpoint archive object is missing",
      );
    }
    const bytes = copyWatcherUserEventArchiveBytes(stored);
    if (watcherUserEventArchiveDigest(bytes) !== expectedDigest) {
      throw new Error("watcher user-event checkpoint archive digest differs");
    }
    if (expectedDigest === checkpoint.payloadDigest) payload = bytes;
  }
  if (payload === undefined) {
    throw new Error("watcher user-event checkpoint payload is missing");
  }
  const text = decoder.decode(payload);
  const parsed = JSON.parse(text) as unknown;
  if (
    typeof parsed !== "object" ||
    parsed === null ||
    Array.isArray(parsed) ||
    watcherCanonicalJson(parsed) !== text
  ) {
    throw new Error(
      "watcher user-event checkpoint payload is not canonical JSON",
    );
  }
  return payload;
};
