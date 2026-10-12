import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type EvidenceProvenance } from "@al-ft/midgard-sdk";

import {
  assertWatcherStateQueueHeaderObservation,
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentReleaseFinalityAuthority,
} from "../runtime/deployment-identity.js";
import {
  admittedTranscripts,
  assertWatcherAuthenticatedReplayTranscript,
  authenticatedHeaderObservation,
  coordinate,
  HEX_32,
  orderedPriorState,
  readUserEventAuthorityForHeader,
  sha256,
  WATCHER_AUTHENTICATED_REPLAY_TRANSCRIPT,
  type WatcherAuthenticatedReplayTranscript,
  type WatcherReplayCoordinate,
  watcherReplayRawRecordCborHex,
} from "./authenticated-replay-transcript.assert-raw-cbor-value.js";
import {
  assertWatcherFullBlockReplayResult,
  evaluateWatcherBlockReplay,
  readWatcherBlockReplayEventAuthorityRecords,
  snapshotWatcherBlockReplayEventAuthorities,
  type WatcherBlockReplayEventAuthority,
  type WatcherBlockReplayPriorUtxo,
} from "./block-replay.js";
import { evaluateWatcherHeaderRootReconstruction } from "./header-root-reconstruction.js";
import { evaluateWatcherPhaseABlock } from "./phase-a-verifier.js";
import {
  readWatcherReplayTranscriptRecords,
  watcherReplayTranscriptSemanticProjection,
} from "./replay-transcript-records.js";
import type { WatcherRuleBundle } from "./rule-bundle.js";
import { assertWatcherUserEventAuthorityCurrent } from "./user-event.js";

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
    captured.eventAuthorities.map(
      async (authority) =>
        await readUserEventAuthorityForHeader(
          authority.userEvent,
          captured.header,
        ),
    ),
  );
  const eventAuthorityRecordsCborHex = Object.freeze(
    readWatcherBlockReplayEventAuthorityRecords(blockReplay).map(
      watcherReplayRawRecordCborHex,
    ),
  );
  // No asynchronous work follows this all-handle fence before admission.
  for (const authority of captured.eventAuthorities)
    assertWatcherUserEventAuthorityCurrent(authority.userEvent);
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
    captured.eventAuthorities.map(
      async (authority) =>
        await readUserEventAuthorityForHeader(
          authority.userEvent,
          captured.header,
        ),
    ),
  );
  for (const authority of captured.eventAuthorities)
    assertWatcherUserEventAuthorityCurrent(authority.userEvent);
  // The original CBOR string is unchanged. Only this fresh independently
  // admitted transcript is returned; its capture provenance and digest remain new.
  return recomputed;
};
