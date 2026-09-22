import { createHash } from "node:crypto";

import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type AuthenticatedStateQueueHeaderObservation,
  CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  type EvidenceProvenance,
  Header,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  assertWatcherStateQueueHeaderObservation,
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import {
  assertWatcherLocalUserEventAuthorityCurrent,
  readWatcherLocalUserEventAuthority,
  type WatcherLocalUserEventAuthority,
} from "../indexers/user-event-indexer.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentReleaseFinalityAuthority,
} from "../runtime/deployment-identity.js";
import {
  assertWatcherFullBlockReplayResult,
  evaluateWatcherBlockReplay,
  readWatcherBlockReplayEventAuthorityRecords,
  snapshotWatcherBlockReplayEventAuthorities,
  type WatcherBlockReplayEventAuthority,
  type WatcherBlockReplayPriorUtxo,
  type WatcherBlockReplayResult,
} from "./block-replay.js";
import { evaluateWatcherHeaderRootReconstruction } from "./header-root-reconstruction.js";
import { evaluateWatcherPhaseABlock } from "./phase-a-verifier.js";
import {
  readWatcherReplayTranscriptRecords,
  watcherReplayTranscriptSemanticProjection,
} from "./replay-transcript-records.js";
import type { WatcherRuleBundle } from "./rule-bundle.js";

export const WATCHER_AUTHENTICATED_REPLAY_TRANSCRIPT =
  "midgard-watcher-production-authenticated-replay-transcript-v1" as const;

const HEX_32 = /^[0-9a-f]{64}$/u;
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

const admittedTranscripts = new WeakSet<object>();

export const assertWatcherAuthenticatedReplayTranscript = (
  transcript: WatcherAuthenticatedReplayTranscript,
): void => {
  if (!admittedTranscripts.has(transcript)) {
    throw new Error("production replay transcript is not admitted");
  }
};

const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

const readLocalEventAuthorityForHeader = async (
  authority: WatcherLocalUserEventAuthority,
  header: WatcherStateQueueHeaderObservation,
): Promise<void> => {
  const { throughHeader } = await readWatcherLocalUserEventAuthority(authority);
  if (
    throughHeader !== null &&
    (throughHeader.headerHash !== header.headerHash ||
      throughHeader.headerCborHex !== header.headerCborHex ||
      throughHeader.queueOutRef !== header.queueOutRef ||
      throughHeader.observedTransactionHash !==
        header.observedTransactionHash ||
      throughHeader.observedBlockHash !== header.observedBlockHash ||
      throughHeader.observedSlot !== header.observedSlot ||
      throughHeader.observedBlockNo !== header.observedBlockNo)
  ) {
    throw new Error("local event authority cutoff differs from replay header");
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

const authenticatedHeaderObservation = (input: {
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

const orderedPriorState = (
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

const coordinate = (
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

/**
 * Recomputes W22, W24, and W25 from authenticated L1/public-DA/raw-event
 * inputs before minting a transcript. A caller-supplied receipt or digest is
 * never accepted as authority.
 */
export const createWatcherAuthenticatedReplayTranscript = async (input: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly stateQueueObservation: WatcherAuthenticatedStateQueueObservation;
  readonly header: WatcherStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: EvidenceProvenance;
  readonly priorState: readonly WatcherBlockReplayPriorUtxo[];
  readonly ruleBundle: WatcherRuleBundle;
  readonly ruleBundleCommitment: string;
  readonly eventAuthorities?: readonly WatcherBlockReplayEventAuthority[];
  readonly coordinate: WatcherReplayCoordinate;
}): Promise<WatcherAuthenticatedReplayTranscript> => {
  // Ordinary input material belongs to this invocation before any release,
  // reconstruction or local-checkpoint read can yield. Opaque authorities keep
  // their original identities and are checked again at admission.
  const captured = Object.freeze({
    deploymentIdentity: input.deploymentIdentity,
    stateQueueObservation: input.stateQueueObservation,
    header: input.header,
    payloadEnvelopeCbor: Buffer.from(input.payloadEnvelopeCbor),
    daProvenance: structuredClone(input.daProvenance),
    priorState: orderedPriorState(input.priorState),
    ruleBundle: structuredClone(input.ruleBundle),
    ruleBundleCommitment: input.ruleBundleCommitment,
    eventAuthorities: snapshotWatcherBlockReplayEventAuthorities(
      input.eventAuthorities ?? [],
    ),
    coordinate: Object.freeze({
      domain: input.coordinate.domain,
      index: input.coordinate.index,
    }),
  });
  const payloadEnvelopeCborHex = captured.payloadEnvelopeCbor.toString("hex");
  const payloadEnvelopeSha256 = sha256(captured.payloadEnvelopeCbor);
  const daProvenanceCborHex = watcherReplayRawRecordCborHex(
    captured.daProvenance,
  );
  const ruleBundleCborHex = watcherReplayRawRecordCborHex(captured.ruleBundle);
  const stateQueueHeaderObservationCborHex = watcherReplayRawRecordCborHex(
    captured.header,
  );
  assertVerifiedWatcherDeploymentIdentity(captured.deploymentIdentity);
  assertWatcherStateQueueObservation(captured.stateQueueObservation);
  assertWatcherStateQueueHeaderObservation(captured.header);
  if (
    captured.stateQueueObservation.deploymentIdentityDigest !==
      captured.deploymentIdentity.manifestId ||
    captured.ruleBundleCommitment !==
      captured.deploymentIdentity.ruleBundleCommitment ||
    captured.ruleBundle.deploymentManifestId !==
      captured.deploymentIdentity.manifestId ||
    captured.ruleBundle.network !== captured.deploymentIdentity.network ||
    captured.ruleBundle.blueprintHash !==
      captured.deploymentIdentity.blueprintHash ||
    JSON.stringify(captured.ruleBundle.programCommitments) !==
      JSON.stringify(captured.deploymentIdentity.programCommitments) ||
    !captured.stateQueueObservation.finalizedHeaders.includes(captured.header)
  ) {
    throw new Error(
      "production replay header differs from deployment queue authority",
    );
  }
  const releaseFinality = await watcherDeploymentReleaseFinalityAuthority(
    captured.deploymentIdentity,
  ).verifyForWorkflow({
    deploymentFingerprint: captured.deploymentIdentity.manifestId,
  });
  const observation = authenticatedHeaderObservation({
    stateQueueObservation: captured.stateQueueObservation,
    header: captured.header,
    minimumConfirmationDepth: releaseFinality.policy.confirmationDepth,
  });
  const priorState = captured.priorState;
  const reconstruction = await evaluateWatcherHeaderRootReconstruction({
    observation,
    payloadEnvelopeCbor: captured.payloadEnvelopeCbor,
    daProvenance: captured.daProvenance,
    minimumConfirmationDepth: releaseFinality.policy.confirmationDepth,
  });
  if (
    reconstruction.action !== "accept" ||
    reconstruction.payloadSha256 === null
  ) {
    throw new Error("production replay W22 reconstruction did not accept");
  }
  const phaseA = await evaluateWatcherPhaseABlock({
    observation,
    reconstruction,
    payloadEnvelopeCbor: captured.payloadEnvelopeCbor,
    daProvenance: captured.daProvenance,
    ruleBundle: captured.ruleBundle,
    ruleBundleCommitment: captured.ruleBundleCommitment,
    minimumConfirmationDepth: releaseFinality.policy.confirmationDepth,
  });
  if (phaseA.action !== "accept") {
    throw new Error("production replay W24 Phase A did not accept");
  }
  const blockReplay = await evaluateWatcherBlockReplay({
    observation,
    reconstruction,
    phaseA,
    payloadEnvelopeCbor: captured.payloadEnvelopeCbor,
    daProvenance: captured.daProvenance,
    priorState,
    ruleBundle: captured.ruleBundle,
    ruleBundleCommitment: captured.ruleBundleCommitment,
    eventAuthorities: captured.eventAuthorities ?? [],
    minimumConfirmationDepth: releaseFinality.policy.confirmationDepth,
  });
  assertWatcherFullBlockReplayResult(blockReplay);
  if (
    blockReplay.action === "error" ||
    blockReplay.priorStateRoot !== observation.header.prevUtxosRoot ||
    blockReplay.headerHash !== captured.header.headerHash ||
    blockReplay.payloadEnvelopeSha256 !==
      reconstruction.payloadEnvelopeSha256 ||
    blockReplay.payloadSha256 !== reconstruction.payloadSha256 ||
    blockReplay.reconstructionDigest !== reconstruction.resultDigest ||
    blockReplay.phaseAResultDigest !== phaseA.resultDigest ||
    blockReplay.ruleBundleCommitment !== captured.ruleBundleCommitment ||
    !HEX_32.test(blockReplay.resultDigest)
  ) {
    throw new Error(
      "production replay W25 result is not an exact usable receipt",
    );
  }
  await Promise.all(
    captured.eventAuthorities.map(async (authority) => {
      if (authority.localUserEvent !== undefined) {
        await readLocalEventAuthorityForHeader(
          authority.localUserEvent,
          captured.header,
        );
      }
    }),
  );
  const eventAuthorityRecordsCborHex = Object.freeze(
    readWatcherBlockReplayEventAuthorityRecords(blockReplay).map(
      watcherReplayRawRecordCborHex,
    ),
  );
  // No asynchronous work follows this all-handle fence before admission.
  for (const authority of captured.eventAuthorities) {
    if (authority.localUserEvent !== undefined) {
      assertWatcherLocalUserEventAuthorityCurrent(authority.localUserEvent);
    }
  }
  const transcriptInput = Object.freeze({
    schemaVersion: WATCHER_AUTHENTICATED_REPLAY_TRANSCRIPT,
    deploymentFingerprint: captured.deploymentIdentity.manifestId,
    stateQueueObservationDigest:
      captured.stateQueueObservation.observationDigest,
    headerHash: captured.header.headerHash,
    inclusionPoint: Object.freeze({
      transactionHash: captured.header.observedTransactionHash,
      blockHash: captured.header.observedBlockHash,
      blockNo: captured.header.observedBlockNo,
      slot: captured.header.observedSlot,
      chainPointId: captured.header.observedChainPointId,
      finalityDepth: captured.header.finalityDepth,
    }),
    coordinate: coordinate(captured.coordinate, blockReplay),
    payloadEnvelopeCborHex,
    payloadEnvelopeSha256,
    payloadSha256: reconstruction.payloadSha256,
    daProvenanceCborHex,
    authenticatedHeaderObservationCborHex:
      watcherReplayRawRecordCborHex(observation),
    stateQueueHeaderObservationCborHex,
    priorState,
    reconstructionRecordCborHex: watcherReplayRawRecordCborHex(reconstruction),
    phaseARecordCborHex: watcherReplayRawRecordCborHex(phaseA),
    ruleBundleCborHex,
    ruleBundleCommitment: captured.ruleBundleCommitment,
    eventAuthorityRecordsCborHex,
    blockReplayRecordCborHex: watcherReplayRawRecordCborHex(blockReplay),
    blockReplayResultDigest: blockReplay.resultDigest,
  });
  const transcript = Object.freeze({
    ...transcriptInput,
    transcriptDigest: computeDeploymentManifestJsonDigest(transcriptInput),
  });
  admittedTranscripts.add(transcript);
  return transcript;
};

export const watcherAuthenticatedReplayTranscriptCborHex = (
  transcript: WatcherAuthenticatedReplayTranscript,
): string => {
  assertWatcherAuthenticatedReplayTranscript(transcript);
  return watcherReplayRawRecordCborHex(transcript);
};

/**
 * Validates persisted bytes for integrity and stable semantics, then recomputes
 * W22/W24/W25 from freshly authenticated deployment/L1/public-DA/event inputs.
 * Historical provenance remains descriptive. The returned transcript carries
 * the fresh capture and its own digest; persisted records never grant authority.
 */
export const replayWatcherAuthenticatedReplayTranscript = async (input: {
  readonly persistedTranscriptCborHex: string;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly stateQueueObservation: WatcherAuthenticatedStateQueueObservation;
  readonly header: WatcherStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: EvidenceProvenance;
  readonly priorState: readonly WatcherBlockReplayPriorUtxo[];
  readonly ruleBundle: WatcherRuleBundle;
  readonly ruleBundleCommitment: string;
  readonly eventAuthorities?: readonly WatcherBlockReplayEventAuthority[];
}): Promise<WatcherAuthenticatedReplayTranscript> => {
  const captured = Object.freeze({
    persistedTranscriptCborHex: input.persistedTranscriptCborHex,
    deploymentIdentity: input.deploymentIdentity,
    stateQueueObservation: input.stateQueueObservation,
    header: input.header,
    payloadEnvelopeCbor: Buffer.from(input.payloadEnvelopeCbor),
    daProvenance: structuredClone(input.daProvenance),
    priorState: orderedPriorState(input.priorState),
    ruleBundle: structuredClone(input.ruleBundle),
    ruleBundleCommitment: input.ruleBundleCommitment,
    eventAuthorities: snapshotWatcherBlockReplayEventAuthorities(
      input.eventAuthorities ?? [],
    ),
  });
  assertVerifiedWatcherDeploymentIdentity(captured.deploymentIdentity);
  const releaseFinality = await watcherDeploymentReleaseFinalityAuthority(
    captured.deploymentIdentity,
  ).verifyForWorkflow({
    deploymentFingerprint: captured.deploymentIdentity.manifestId,
  });
  const persisted = await readWatcherReplayTranscriptRecords(
    captured.persistedTranscriptCborHex,
    releaseFinality.policy.confirmationDepth,
  );
  coordinate(persisted.transcript.coordinate, persisted.blockReplay);
  const recomputed = await createWatcherAuthenticatedReplayTranscript({
    deploymentIdentity: captured.deploymentIdentity,
    stateQueueObservation: captured.stateQueueObservation,
    header: captured.header,
    payloadEnvelopeCbor: captured.payloadEnvelopeCbor,
    daProvenance: captured.daProvenance,
    priorState: captured.priorState,
    ruleBundle: captured.ruleBundle,
    ruleBundleCommitment: captured.ruleBundleCommitment,
    eventAuthorities: captured.eventAuthorities,
    coordinate: persisted.transcript.coordinate,
  });
  const fresh = await readWatcherReplayTranscriptRecords(
    watcherAuthenticatedReplayTranscriptCborHex(recomputed),
    releaseFinality.policy.confirmationDepth,
  );
  if (
    watcherReplayRawRecordCborHex(
      watcherReplayTranscriptSemanticProjection(persisted),
    ) !==
    watcherReplayRawRecordCborHex(
      watcherReplayTranscriptSemanticProjection(fresh),
    )
  ) {
    throw new Error(
      "persisted production replay transcript differs from fresh authenticated replay semantics",
    );
  }
  await Promise.all(
    captured.eventAuthorities.map(async (authority) => {
      if (authority.localUserEvent !== undefined)
        await readLocalEventAuthorityForHeader(
          authority.localUserEvent,
          captured.header,
        );
    }),
  );
  for (const authority of captured.eventAuthorities) {
    if (authority.localUserEvent !== undefined)
      assertWatcherLocalUserEventAuthorityCurrent(authority.localUserEvent);
  }
  // The original CBOR string is unchanged. Only this fresh independently
  // admitted transcript is returned; its capture provenance and digest remain new.
  return recomputed;
};
