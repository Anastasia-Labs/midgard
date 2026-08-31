import { createHash } from "node:crypto";

import { decodeSingleCbor, encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { computeDeploymentManifestV1JsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import {
  type AuthenticatedStateQueueHeaderObservationV1,
  CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION,
  type EvidenceProvenanceV1,
  HeaderV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  assertWatcherFullBlockReplayResultV1,
  evaluateWatcherBlockReplayV1,
  type WatcherBlockReplayEventAuthorityV1,
  type WatcherBlockReplayPriorUtxoV1,
  type WatcherBlockReplayResultV1,
} from "./block-replay.js";
import {
  assertVerifiedWatcherDeploymentIdentityV1,
  type VerifiedWatcherDeploymentIdentityV1,
  watcherDeploymentReleaseFinalityAuthorityV1,
} from "./deployment-identity.js";
import { evaluateWatcherHeaderRootReconstructionV1 } from "./header-root-reconstruction.js";
import { evaluateWatcherPhaseABlockV1 } from "./phase-a-verifier.js";
import {
  assertWatcherProductionStateQueueHeaderObservationV1,
  assertWatcherProductionStateQueueObservationV1,
  type WatcherProductionStateQueueHeaderObservationV1,
  type WatcherProductionStateQueueObservationV1,
} from "./production-state-queue-observation-v1.js";
import type { WatcherRuleBundleV1 } from "./rule-bundle-v1.js";

export const WATCHER_PRODUCTION_AUTHENTICATED_REPLAY_TRANSCRIPT_V1 =
  "midgard-watcher-production-authenticated-replay-transcript-v1" as const;

const HEX_32 = /^[0-9a-f]{64}$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})*$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export type WatcherProductionReplayCoordinateV1 = Readonly<{
  domain: "block" | "transaction" | "mutation" | "event" | "transition_step";
  index: string;
}>;

/**
 * Exact raw W15/W16/W22/W24/W25 capture. It deliberately has no category,
 * finding, violation, or decision digest. Those are outputs of fault-proofs'
 * independent replay and classifier, not watcher inputs.
 */
export type WatcherProductionAuthenticatedReplayTranscriptV1 = Readonly<{
  schemaVersion: typeof WATCHER_PRODUCTION_AUTHENTICATED_REPLAY_TRANSCRIPT_V1;
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
  coordinate: WatcherProductionReplayCoordinateV1;
  payloadEnvelopeCborHex: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  daProvenanceCborHex: string;
  authenticatedHeaderObservationCborHex: string;
  stateQueueHeaderObservationCborHex: string;
  priorState: readonly WatcherBlockReplayPriorUtxoV1[];
  reconstructionRecordCborHex: string;
  phaseARecordCborHex: string;
  ruleBundleCborHex: string;
  ruleBundleCommitment: string;
  eventAuthorityRecordsCborHex: readonly string[];
  blockReplayRecordCborHex: string;
  blockReplayResultDigest: string;
  transcriptDigest: string;
}>;

const admittedTranscripts = new WeakSet<object>();

export const assertWatcherProductionAuthenticatedReplayTranscriptV1 = (
  transcript: WatcherProductionAuthenticatedReplayTranscriptV1,
): void => {
  if (!admittedTranscripts.has(transcript)) {
    throw new Error("production replay transcript is not admitted");
  }
};

const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

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
export const watcherProductionReplayRawRecordCborHexV1 = (
  value: unknown,
): string => {
  assertRawCborValue(value, "$", new Set());
  return encodeCbor(value).toString("hex");
};

const authenticatedHeaderObservation = (input: {
  readonly stateQueueObservation: WatcherProductionStateQueueObservationV1;
  readonly header: WatcherProductionStateQueueHeaderObservationV1;
  readonly minimumConfirmationDepth: number;
}): AuthenticatedStateQueueHeaderObservationV1 => {
  const decoded = Data.from(input.header.headerCborHex, HeaderV1);
  if (Data.to(decoded, HeaderV1) !== input.header.headerCborHex) {
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
    schemaVersion: CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION,
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

const orderedPriorState = (
  values: readonly WatcherBlockReplayPriorUtxoV1[],
): readonly WatcherBlockReplayPriorUtxoV1[] => {
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

const coordinate = (
  input: WatcherProductionReplayCoordinateV1,
  replay: WatcherBlockReplayResultV1,
): WatcherProductionReplayCoordinateV1 => {
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

/**
 * Recomputes W22, W24, and W25 from authenticated L1/public-DA/raw-event
 * inputs before minting a transcript. A caller-supplied receipt or digest is
 * never accepted as authority.
 */
export const createWatcherProductionAuthenticatedReplayTranscriptV1 =
  async (input: {
    readonly deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
    readonly stateQueueObservation: WatcherProductionStateQueueObservationV1;
    readonly header: WatcherProductionStateQueueHeaderObservationV1;
    readonly payloadEnvelopeCbor: Uint8Array;
    readonly daProvenance: EvidenceProvenanceV1;
    readonly priorState: readonly WatcherBlockReplayPriorUtxoV1[];
    readonly ruleBundle: WatcherRuleBundleV1;
    readonly ruleBundleCommitment: string;
    readonly eventAuthorities?: readonly WatcherBlockReplayEventAuthorityV1[];
    readonly coordinate: WatcherProductionReplayCoordinateV1;
  }): Promise<WatcherProductionAuthenticatedReplayTranscriptV1> => {
    assertVerifiedWatcherDeploymentIdentityV1(input.deploymentIdentity);
    assertWatcherProductionStateQueueObservationV1(input.stateQueueObservation);
    assertWatcherProductionStateQueueHeaderObservationV1(input.header);
    if (
      input.stateQueueObservation.deploymentIdentityDigest !==
        input.deploymentIdentity.manifestId ||
      input.ruleBundleCommitment !==
        input.deploymentIdentity.ruleBundleCommitment ||
      input.ruleBundle.deploymentManifestId !==
        input.deploymentIdentity.manifestId ||
      input.ruleBundle.network !== input.deploymentIdentity.network ||
      input.ruleBundle.releaseEvidenceDigest !==
        input.deploymentIdentity.releaseEvidenceDigest ||
      JSON.stringify(input.ruleBundle.programCommitments) !==
        JSON.stringify(input.deploymentIdentity.programCommitments) ||
      !input.stateQueueObservation.finalizedHeaders.includes(input.header)
    ) {
      throw new Error(
        "production replay header differs from deployment queue authority",
      );
    }
    const releaseFinality = await watcherDeploymentReleaseFinalityAuthorityV1(
      input.deploymentIdentity,
    ).verifyForWorkflow({
      deploymentFingerprint: input.deploymentIdentity.manifestId,
    });
    const observation = authenticatedHeaderObservation({
      stateQueueObservation: input.stateQueueObservation,
      header: input.header,
      minimumConfirmationDepth: releaseFinality.policy.confirmationDepth,
    });
    const priorState = orderedPriorState(input.priorState);
    const reconstruction = await evaluateWatcherHeaderRootReconstructionV1({
      observation,
      payloadEnvelopeCbor: input.payloadEnvelopeCbor,
      daProvenance: input.daProvenance,
      minimumConfirmationDepth: releaseFinality.policy.confirmationDepth,
    });
    if (
      reconstruction.action !== "accept" ||
      reconstruction.payloadSha256 === null
    ) {
      throw new Error("production replay W22 reconstruction did not accept");
    }
    const phaseA = await evaluateWatcherPhaseABlockV1({
      observation,
      reconstruction,
      payloadEnvelopeCbor: input.payloadEnvelopeCbor,
      daProvenance: input.daProvenance,
      ruleBundle: input.ruleBundle,
      ruleBundleCommitment: input.ruleBundleCommitment,
      minimumConfirmationDepth: releaseFinality.policy.confirmationDepth,
    });
    if (phaseA.action !== "accept") {
      throw new Error("production replay W24 Phase A did not accept");
    }
    const blockReplay = await evaluateWatcherBlockReplayV1({
      observation,
      reconstruction,
      phaseA,
      payloadEnvelopeCbor: input.payloadEnvelopeCbor,
      daProvenance: input.daProvenance,
      priorState,
      ruleBundle: input.ruleBundle,
      ruleBundleCommitment: input.ruleBundleCommitment,
      eventAuthorities: input.eventAuthorities ?? [],
      minimumConfirmationDepth: releaseFinality.policy.confirmationDepth,
    });
    assertWatcherFullBlockReplayResultV1(blockReplay);
    if (
      blockReplay.action === "error" ||
      blockReplay.headerHash !== input.header.headerHash ||
      blockReplay.payloadEnvelopeSha256 !==
        reconstruction.payloadEnvelopeSha256 ||
      blockReplay.payloadSha256 !== reconstruction.payloadSha256 ||
      blockReplay.reconstructionDigest !== reconstruction.resultDigest ||
      blockReplay.phaseAResultDigest !== phaseA.resultDigest ||
      blockReplay.ruleBundleCommitment !== input.ruleBundleCommitment ||
      !HEX_32.test(blockReplay.resultDigest)
    ) {
      throw new Error(
        "production replay W25 result is not an exact usable receipt",
      );
    }
    const eventAuthorityRecordsCborHex = Object.freeze(
      (input.eventAuthorities ?? []).map(
        watcherProductionReplayRawRecordCborHexV1,
      ),
    );
    const payloadEnvelopeCborHex = Buffer.from(
      input.payloadEnvelopeCbor,
    ).toString("hex");
    const transcriptInput = Object.freeze({
      schemaVersion: WATCHER_PRODUCTION_AUTHENTICATED_REPLAY_TRANSCRIPT_V1,
      deploymentFingerprint: input.deploymentIdentity.manifestId,
      stateQueueObservationDigest:
        input.stateQueueObservation.observationDigest,
      headerHash: input.header.headerHash,
      inclusionPoint: Object.freeze({
        transactionHash: input.header.observedTransactionHash,
        blockHash: input.header.observedBlockHash,
        blockNo: input.header.observedBlockNo,
        slot: input.header.observedSlot,
        chainPointId: input.header.observedChainPointId,
        finalityDepth: input.header.finalityDepth,
      }),
      coordinate: coordinate(input.coordinate, blockReplay),
      payloadEnvelopeCborHex,
      payloadEnvelopeSha256: sha256(input.payloadEnvelopeCbor),
      payloadSha256: reconstruction.payloadSha256,
      daProvenanceCborHex: watcherProductionReplayRawRecordCborHexV1(
        input.daProvenance,
      ),
      authenticatedHeaderObservationCborHex:
        watcherProductionReplayRawRecordCborHexV1(observation),
      stateQueueHeaderObservationCborHex:
        watcherProductionReplayRawRecordCborHexV1(input.header),
      priorState,
      reconstructionRecordCborHex:
        watcherProductionReplayRawRecordCborHexV1(reconstruction),
      phaseARecordCborHex: watcherProductionReplayRawRecordCborHexV1(phaseA),
      ruleBundleCborHex: watcherProductionReplayRawRecordCborHexV1(
        input.ruleBundle,
      ),
      ruleBundleCommitment: input.ruleBundleCommitment,
      eventAuthorityRecordsCborHex,
      blockReplayRecordCborHex:
        watcherProductionReplayRawRecordCborHexV1(blockReplay),
      blockReplayResultDigest: blockReplay.resultDigest,
    });
    const transcript = Object.freeze({
      ...transcriptInput,
      transcriptDigest: computeDeploymentManifestV1JsonDigest(transcriptInput),
    });
    admittedTranscripts.add(transcript);
    return transcript;
  };

const decodedCborPlainValue = (value: unknown, path = "$"): unknown => {
  if (value instanceof Map) {
    const result: Record<string, unknown> = {};
    for (const [key, entry] of value) {
      if (
        typeof key !== "string" ||
        Object.prototype.hasOwnProperty.call(result, key)
      ) {
        throw new Error(`${path} contains a non-string or duplicate key`);
      }
      result[key] = decodedCborPlainValue(entry, `${path}.${key}`);
    }
    return result;
  }
  if (Array.isArray(value)) {
    return value.map((entry, index) =>
      decodedCborPlainValue(entry, `${path}[${index.toString()}]`),
    );
  }
  return value;
};

const persistedCoordinate = (
  value: unknown,
): WatcherProductionReplayCoordinateV1 => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error("persisted replay coordinate is not an exact record");
  }
  const record = value as Readonly<Record<string, unknown>>;
  if (
    Object.keys(record).length !== 2 ||
    !Object.prototype.hasOwnProperty.call(record, "domain") ||
    !Object.prototype.hasOwnProperty.call(record, "index") ||
    (record.domain !== "block" &&
      record.domain !== "transaction" &&
      record.domain !== "mutation" &&
      record.domain !== "event" &&
      record.domain !== "transition_step") ||
    typeof record.index !== "string" ||
    !NATURAL.test(record.index)
  ) {
    throw new Error("persisted replay coordinate is invalid");
  }
  return Object.freeze({ domain: record.domain, index: record.index });
};

export const watcherProductionAuthenticatedReplayTranscriptCborHexV1 = (
  transcript: WatcherProductionAuthenticatedReplayTranscriptV1,
): string => {
  assertWatcherProductionAuthenticatedReplayTranscriptV1(transcript);
  return watcherProductionReplayRawRecordCborHexV1(transcript);
};

/**
 * Re-admits persisted transcript bytes only by recomputing W22/W24/W25 from
 * freshly authenticated deployment/L1/public-DA inputs. Persisted derived
 * records and digests are compared as outputs and never used as authority.
 */
export const replayWatcherProductionAuthenticatedReplayTranscriptV1 =
  async (input: {
    readonly persistedTranscriptCborHex: string;
    readonly deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
    readonly stateQueueObservation: WatcherProductionStateQueueObservationV1;
    readonly header: WatcherProductionStateQueueHeaderObservationV1;
    readonly payloadEnvelopeCbor: Uint8Array;
    readonly daProvenance: EvidenceProvenanceV1;
    readonly priorState: readonly WatcherBlockReplayPriorUtxoV1[];
    readonly ruleBundle: WatcherRuleBundleV1;
    readonly ruleBundleCommitment: string;
    readonly eventAuthorities?: readonly WatcherBlockReplayEventAuthorityV1[];
  }): Promise<WatcherProductionAuthenticatedReplayTranscriptV1> => {
    if (!/^(?:[0-9a-f]{2})+$/u.test(input.persistedTranscriptCborHex)) {
      throw new Error("persisted production replay transcript is not CBOR hex");
    }
    const decoded = decodedCborPlainValue(
      decodeSingleCbor(Buffer.from(input.persistedTranscriptCborHex, "hex")),
    );
    if (
      typeof decoded !== "object" ||
      decoded === null ||
      Array.isArray(decoded) ||
      Object.getPrototypeOf(decoded) !== Object.prototype
    ) {
      throw new Error("persisted production replay transcript is not a record");
    }
    if (
      watcherProductionReplayRawRecordCborHexV1(decoded) !==
      input.persistedTranscriptCborHex
    ) {
      throw new Error("persisted production replay transcript is noncanonical");
    }
    const recomputed =
      await createWatcherProductionAuthenticatedReplayTranscriptV1({
        deploymentIdentity: input.deploymentIdentity,
        stateQueueObservation: input.stateQueueObservation,
        header: input.header,
        payloadEnvelopeCbor: input.payloadEnvelopeCbor,
        daProvenance: input.daProvenance,
        priorState: input.priorState,
        ruleBundle: input.ruleBundle,
        ruleBundleCommitment: input.ruleBundleCommitment,
        eventAuthorities: input.eventAuthorities,
        coordinate: persistedCoordinate(
          (decoded as Readonly<Record<string, unknown>>).coordinate,
        ),
      });
    if (
      watcherProductionAuthenticatedReplayTranscriptCborHexV1(recomputed) !==
      input.persistedTranscriptCborHex
    ) {
      throw new Error(
        "persisted production replay transcript differs from fresh authenticated replay",
      );
    }
    return recomputed;
  };
