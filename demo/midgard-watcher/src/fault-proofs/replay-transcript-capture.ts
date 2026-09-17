import {
  admitValidationTraceChallengeFromReplayContext,
  completeCanonicalReplayPredecessorEvidence,
  type HeaderDecision,
  headerDecisionCanonicalEvidence,
  headerDecisionReplayContext,
  readValidationTraceReplaySelection,
  W25_CHALLENGE_COORDINATE,
} from "@al-ft/midgard-fault-proofs";

import {
  assertWatcherStateQueueHeaderObservation,
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import {
  assertWatcherLocalUserEventAuthorityCurrent,
  readWatcherLocalUserEventAuthority,
} from "../indexers/user-event-indexer.js";
import {
  assertWatcherVerifiedDeploymentAuthority,
  type VerifiedWatcherDeploymentAuthority,
} from "../runtime/deployment-authority.js";
import {
  assertWatcherUserEventRuntime,
  type WatcherUserEventRuntime,
} from "../runtime/user-event-runtime.js";
import {
  createWatcherAuthenticatedReplayTranscript,
  replayWatcherAuthenticatedReplayTranscript,
  watcherReplayRawRecordCborHex,
} from "../verification/authenticated-replay-transcript.js";
import { type WatcherBlockReplayEventAuthority } from "../verification/block-replay.js";
import type { WatcherCommittedEventClaim } from "../verification/event-claims.js";
import { deriveWatcherLocalEventReplayAuthority } from "../verification/local-event-replay-authority.js";

const captureAuthorities = new WeakMap<
  object,
  Readonly<{
    userEventRuntime: WatcherUserEventRuntime;
    generation: number;
    authorities: readonly WatcherBlockReplayEventAuthority[];
  }>
>();

const assertRuntimeGeneration = (
  runtime: WatcherUserEventRuntime,
  generation: number,
): void => {
  assertWatcherUserEventRuntime(runtime);
  if (runtime.read().generation !== generation) {
    throw new Error("Validation replay capture's event history was retired");
  }
};

const assertAuthoritiesCurrent = (
  authorities: readonly WatcherBlockReplayEventAuthority[],
): void => {
  for (const authority of authorities) {
    if (authority.localUserEvent !== undefined) {
      assertWatcherLocalUserEventAuthorityCurrent(authority.localUserEvent);
    }
  }
};

const refreshAuthorities = async (
  authorities: readonly WatcherBlockReplayEventAuthority[],
): Promise<void> => {
  for (const authority of authorities) {
    if (authority.localUserEvent !== undefined) {
      await readWatcherLocalUserEventAuthority(authority.localUserEvent);
    }
  }
  assertAuthoritiesCurrent(authorities);
};

/** Refresh after archive or workflow-loader I/O; descriptive copies cannot pass. */
export const refreshWatcherValidationReplayCapture = async (
  capture: object,
): Promise<void> => {
  const owner = captureAuthorities.get(capture);
  if (owner === undefined) {
    throw new Error("Validation replay capture is not privately admitted");
  }
  assertRuntimeGeneration(owner.userEventRuntime, owner.generation);
  await refreshAuthorities(owner.authorities);
  assertRuntimeGeneration(owner.userEventRuntime, owner.generation);
};

/** Call synchronously after the last await, immediately before using a capture. */
export const assertWatcherValidationReplayCaptureCurrent = (
  capture: object,
): void => {
  const owner = captureAuthorities.get(capture);
  if (owner === undefined) {
    throw new Error("Validation replay capture is not privately admitted");
  }
  assertRuntimeGeneration(owner.userEventRuntime, owner.generation);
  assertAuthoritiesCurrent(owner.authorities);
};

/**
 * The selected canonical detection supplies the coordinate; the authenticated
 * watcher replay supplies its transcript. Neither proof material nor a claimed
 * replay result is accepted from the application caller.
 */
export const captureWatcherValidationReplayTranscript = async ({
  deploymentAuthority,
  stateQueueObservation,
  header,
  decision,
  userEventRuntime,
  persistedTranscriptCborHex,
}: {
  readonly deploymentAuthority: VerifiedWatcherDeploymentAuthority;
  readonly stateQueueObservation: WatcherAuthenticatedStateQueueObservation;
  readonly header: WatcherStateQueueHeaderObservation;
  readonly decision: HeaderDecision;
  readonly userEventRuntime: WatcherUserEventRuntime;
  readonly persistedTranscriptCborHex?: string;
}) => {
  assertWatcherUserEventRuntime(userEventRuntime);
  const generation = userEventRuntime.read().generation;
  assertWatcherVerifiedDeploymentAuthority(deploymentAuthority);
  assertWatcherStateQueueObservation(stateQueueObservation);
  assertWatcherStateQueueHeaderObservation(header);
  const { deploymentIdentity, ruleBundle } = deploymentAuthority;
  if (
    userEventRuntime.deploymentFingerprint !== deploymentIdentity.manifestId ||
    userEventRuntime.blueprintHash !== deploymentIdentity.blueprintHash ||
    decision.decision !== "fault_detected" ||
    decision.category !== "validationTraceDispute" ||
    decision.deploymentFingerprint !== deploymentIdentity.manifestId ||
    decision.headerHash !== header.headerHash ||
    stateQueueObservation.deploymentIdentityDigest !==
      deploymentIdentity.manifestId ||
    !stateQueueObservation.finalizedHeaders.includes(header)
  ) {
    throw new Error(
      "Validation transcript requires this deployment's selected decision",
    );
  }
  const context = headerDecisionReplayContext(decision);
  const evidence = await headerDecisionCanonicalEvidence(decision);
  if (evidence === undefined || context?.validationTraceReplay === undefined) {
    throw new Error(
      "Validation transcript requires live classifier replay authority",
    );
  }
  const claims: readonly WatcherCommittedEventClaim[] = [
    ...evidence.reconstruction.deposits.map((entry) => ({
      phase: "Deposit" as const,
      eventIdCborHex: entry.keyBytes.toString("hex"),
      valueCborHex: entry.valueBytes.toString("hex"),
      canonicalNativeTxCborHex: null,
    })),
    ...evidence.reconstruction.withdrawals.map((entry) => ({
      phase: "Withdrawal" as const,
      eventIdCborHex: entry.keyBytes.toString("hex"),
      valueCborHex: entry.valueBytes.toString("hex"),
      canonicalNativeTxCborHex: null,
    })),
    ...evidence.reconstruction.forcedTransactions.map((entry) => ({
      phase: "ForcedTransaction" as const,
      eventIdCborHex: entry.keyBytes.toString("hex"),
      valueCborHex: entry.valueBytes.toString("hex"),
      canonicalNativeTxCborHex: entry.fullTransactionCbor.toString("hex"),
    })),
  ];
  const authorities: WatcherBlockReplayEventAuthority[] = [];
  for (const claim of claims) {
    const localUserEvent = await userEventRuntime.eventAuthority({
      kind:
        claim.phase === "Deposit"
          ? "deposit"
          : claim.phase === "Withdrawal"
            ? "withdrawal"
            : "forced_order",
      eventId: claim.eventIdCborHex,
      throughHeader: header,
    });
    const local = await readWatcherLocalUserEventAuthority(localUserEvent);
    const cutoff = local.throughHeader;
    if (
      cutoff === null ||
      cutoff.headerHash !== header.headerHash ||
      cutoff.headerCborHex !== header.headerCborHex ||
      cutoff.queueOutRef !== header.queueOutRef ||
      cutoff.observedTransactionHash !== header.observedTransactionHash ||
      cutoff.observedBlockHash !== header.observedBlockHash ||
      cutoff.observedSlot !== header.observedSlot ||
      cutoff.observedBlockNo !== header.observedBlockNo
    ) {
      throw new Error(
        "Local event authority is not scoped to the selected header",
      );
    }
    authorities.push(
      await deriveWatcherLocalEventReplayAuthority({
        localUserEvent,
        committedClaim: claim,
        programMaterial:
          evidence.reconstruction.payload.block_body.cek_program_material,
      }),
    );
  }
  const selectedInput = {
    evidence,
    context: context.validationTraceReplay,
    predecessor: context.predecessor,
    transitionTraceEvents: context.transitionTraceEvents,
    detectionId: decision.detectionId,
  };
  const selection = readValidationTraceReplaySelection(selectedInput);
  const predecessor = completeCanonicalReplayPredecessorEvidence({
    evidence,
    context,
  });
  const replayInput = {
    deploymentIdentity,
    stateQueueObservation,
    header,
    payloadEnvelopeCbor: Buffer.from(
      evidence.reconstruction.payloadEnvelopeCbor,
    ),
    daProvenance: evidence.provenance.da,
    priorState: (predecessor?.reconstruction.utxos ?? []).map((entry) => ({
      outRef: entry.key.toString("hex"),
      outputCbor: entry.value.toString("hex"),
    })),
    ruleBundle: ruleBundle.ruleBundle,
    ruleBundleCommitment: ruleBundle.ruleBundleCommitment,
    eventAuthorities: authorities,
  };
  const transcript =
    persistedTranscriptCborHex === undefined
      ? await createWatcherAuthenticatedReplayTranscript({
          ...replayInput,
          coordinate: selection.coordinate,
        })
      : await replayWatcherAuthenticatedReplayTranscript({
          ...replayInput,
          persistedTranscriptCborHex,
        });
  if (
    transcript.headerHash !== decision.headerHash ||
    transcript.payloadEnvelopeSha256 !== decision.payloadEnvelopeSha256 ||
    transcript.payloadSha256 !== decision.payloadSha256 ||
    transcript.coordinate.domain !== selection.coordinate.domain ||
    transcript.coordinate.index !== selection.coordinate.index ||
    transcript.authenticatedHeaderObservationCborHex !==
      watcherReplayRawRecordCborHex(evidence.observation)
  ) {
    throw new Error(
      "Validation transcript differs from the selected header observation",
    );
  }
  const challenge = await admitValidationTraceChallengeFromReplayContext({
    ...selectedInput,
    coordinate: {
      schemaVersion: W25_CHALLENGE_COORDINATE,
      deploymentFingerprint: transcript.deploymentFingerprint,
      stateQueueObservationDigest: transcript.stateQueueObservationDigest,
      headerHash: transcript.headerHash,
      payloadEnvelopeSha256: transcript.payloadEnvelopeSha256,
      payloadSha256: transcript.payloadSha256,
      transcriptDigest: transcript.transcriptDigest,
      blockReplayResultDigest: transcript.blockReplayResultDigest,
      coordinate: transcript.coordinate,
    },
  });
  // Challenge construction independently replays validation and may yield after
  // transcript admission. Refresh all protected heads, then fence every handle
  // synchronously before making the challenge available to the application.
  await refreshAuthorities(authorities);
  assertAuthoritiesCurrent(authorities);
  const capture = Object.freeze({
    decisionDigest: decision.decisionDigest,
    transcript,
    challenge,
  });
  assertRuntimeGeneration(userEventRuntime, generation);
  captureAuthorities.set(
    capture,
    Object.freeze({
      userEventRuntime,
      generation,
      authorities: Object.freeze(authorities),
    }),
  );
  return capture;
};
