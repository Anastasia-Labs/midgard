import { createHash } from "node:crypto";

import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  type AuthenticatedStateQueueHeaderObservation,
  CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  Header,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import {
  type WatcherBlockReplayPriorUtxo,
  type WatcherBlockReplayResult,
} from "./block-replay.js";
import {
  readWatcherUserEventAuthority,
  type WatcherUserEventAuthority,
} from "./user-event.js";

export const WATCHER_AUTHENTICATED_REPLAY_TRANSCRIPT =
  "midgard-watcher-production-authenticated-replay-transcript-v2" as const;

export const HEX_32 = /^[0-9a-f]{64}$/u;

const EVEN_HEX = /^(?:[0-9a-f]{2})*$/u;

const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export type WatcherReplayCoordinate = Readonly<{
  domain: "block" | "transaction" | "mutation" | "event" | "transition_step";
  index: string;
}>;

/**
 * Exact W22/W24/W25 capture and descriptive canonical event records. Opaque
 * event authorities never serialize. Category, finding, violation and decision
 * digests are outputs of independent fault-proof replay and classification.
 */
export type WatcherAuthenticatedReplayTranscript = Readonly<{
  schemaVersion: typeof WATCHER_AUTHENTICATED_REPLAY_TRANSCRIPT;
  deploymentFingerprint: string;
  stateQueueObservationDigest: string;
  headerHash: string;
  inclusionPoint: Readonly<{
    transactionHash: string;
    blockHash: string;
    blockNo: string;
    slot: string;
    chainPointId: string;
    finalityDepth: string;
  }>;
  coordinate: WatcherReplayCoordinate;
  payloadEnvelopeCborHex: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  daProvenanceCborHex: string;
  authenticatedHeaderObservationCborHex: string;
  stateQueueHeaderObservationCborHex: string;
  priorState: readonly WatcherBlockReplayPriorUtxo[];
  reconstructionRecordCborHex: string;
  phaseARecordCborHex: string;
  ruleBundleCborHex: string;
  ruleBundleCommitment: string;
  eventAuthorityRecordsCborHex: readonly string[];
  blockReplayRecordCborHex: string;
  blockReplayResultDigest: string;
  transcriptDigest: string;
}>;

export const admittedTranscripts = new WeakSet<object>();

export const assertWatcherAuthenticatedReplayTranscript = (
  transcript: WatcherAuthenticatedReplayTranscript,
): void => {
  if (!admittedTranscripts.has(transcript)) {
    throw new Error("production replay transcript is not admitted");
  }
};

export const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

export const readUserEventAuthorityForHeader = async (
  authority: WatcherUserEventAuthority,
  header: WatcherStateQueueHeaderObservation,
): Promise<void> => {
  const { throughHeader } = await readWatcherUserEventAuthority(authority);
  if (
    throughHeader.headerHash !== header.headerHash ||
    throughHeader.headerCborHex !== header.headerCborHex ||
    throughHeader.queueOutRef !== header.queueOutRef ||
    throughHeader.observedTransactionHash !== header.observedTransactionHash ||
    throughHeader.observedBlockHash !== header.observedBlockHash ||
    throughHeader.observedSlot !== header.observedSlot ||
    throughHeader.observedBlockNo !== header.observedBlockNo
  ) {
    throw new Error("user-event authority cutoff differs from replay header");
  }
};

const assertRawCborValue = (
  value: unknown,
  path: string,
  seen: Set<object>,
): void => {
  if (
    value === null ||
    typeof value === "string" ||
    typeof value === "boolean" ||
    typeof value === "bigint"
  ) {
    return;
  }
  if (typeof value === "number") {
    if (!Number.isSafeInteger(value) || Object.is(value, -0)) {
      throw new Error(`${path} contains a noncanonical number`);
    }
    return;
  }
  if (value instanceof Uint8Array) return;
  if (typeof value !== "object" || value === undefined) {
    throw new Error(`${path} contains a non-CBOR value`);
  }
  if (seen.has(value)) throw new Error(`${path} contains a cycle or alias`);
  seen.add(value);
  if (Array.isArray(value)) {
    value.forEach((entry, index) =>
      assertRawCborValue(entry, `${path}[${index.toString()}]`, seen),
    );
  } else {
    if (
      Object.getPrototypeOf(value) !== Object.prototype ||
      Reflect.ownKeys(value).length !== Object.keys(value).length
    ) {
      throw new Error(`${path} is not an exact plain record`);
    }
    for (const [key, entry] of Object.entries(value)) {
      const descriptor = Object.getOwnPropertyDescriptor(value, key);
      if (
        descriptor === undefined ||
        descriptor.get !== undefined ||
        descriptor.set !== undefined ||
        entry === undefined
      ) {
        throw new Error(`${path}.${key} is not an exact data property`);
      }
      assertRawCborValue(entry, `${path}.${key}`, seen);
    }
  }
  seen.delete(value);
};

/** Canonical RFC 8949 bytes for an exact raw watcher replay record. */
export const watcherReplayRawRecordCborHex = (value: unknown): string => {
  assertRawCborValue(value, "$", new Set());
  return encodeCbor(value).toString("hex");
};

export const authenticatedHeaderObservation = (input: {
  readonly stateQueueObservation: WatcherAuthenticatedStateQueueObservation;
  readonly header: WatcherStateQueueHeaderObservation;
  readonly minimumConfirmationDepth: number;
}): AuthenticatedStateQueueHeaderObservation => {
  const decoded = Data.from(input.header.headerCborHex, Header);
  if (Data.to(decoded, Header) !== input.header.headerCborHex) {
    throw new Error("production replay HeaderV1 CBOR is noncanonical");
  }
  const depth = BigInt(input.header.finalityDepth);
  if (
    !NATURAL.test(input.header.finalityDepth) ||
    depth < BigInt(input.minimumConfirmationDepth) ||
    depth > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error("production replay HeaderV1 finality is invalid");
  }
  return Object.freeze({
    schemaVersion: CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
    sourceMode: "local_node" as const,
    provenance: Object.freeze({
      trustClass: "authenticated_cardano_l1" as const,
      sourceId: input.stateQueueObservation.sourceId,
      grade: "security" as const,
    }),
    chainPoint: Object.freeze({
      slot: BigInt(input.header.observedSlot),
      blockHash: input.header.observedBlockHash,
    }),
    confirmationDepth: Number(depth),
    headerHash: input.header.headerHash,
    header: decoded,
  });
};

export const orderedPriorState = (
  values: readonly WatcherBlockReplayPriorUtxo[],
): readonly WatcherBlockReplayPriorUtxo[] => {
  const result = values.map((entry, index) => {
    if (!EVEN_HEX.test(entry.outRef) || !EVEN_HEX.test(entry.outputCbor)) {
      throw new Error(
        `production replay prior state ${index.toString()} is not canonical hex`,
      );
    }
    return Object.freeze({
      outRef: entry.outRef,
      outputCbor: entry.outputCbor,
    });
  });
  result.sort((left, right) => left.outRef.localeCompare(right.outRef));
  if (
    result.some(
      (entry, index) => index > 0 && result[index - 1]!.outRef === entry.outRef,
    )
  ) {
    throw new Error("production replay prior state repeats an out-ref");
  }
  return Object.freeze(result);
};

export const coordinate = (
  input: WatcherReplayCoordinate,
  replay: WatcherBlockReplayResult,
): WatcherReplayCoordinate => {
  if (!NATURAL.test(input.index)) {
    throw new Error("production replay coordinate is invalid");
  }
  const index = BigInt(input.index);
  const present =
    input.domain === "block"
      ? index === 0n
      : input.domain === "transaction"
        ? index < BigInt(replay.transactionCount)
        : input.domain === "mutation"
          ? replay.intermediateRoots.some(
              (root) => BigInt(root.sequence) === index,
            )
          : input.domain === "event"
            ? index < BigInt(replay.eventRoots.length)
            : input.domain === "transition_step"
              ? replay.transactionRoots.some(
                  (root) =>
                    root.committedStepIndex !== null &&
                    BigInt(root.committedStepIndex) === index,
                ) ||
                replay.eventRoots.some(
                  (root) => BigInt(root.stepIndex) === index,
                )
              : false;
  if (!present) {
    throw new Error("production replay coordinate is outside exact replay");
  }
  return Object.freeze({ domain: input.domain, index: input.index });
};
