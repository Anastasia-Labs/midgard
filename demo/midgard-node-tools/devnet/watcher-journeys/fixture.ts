import type { HistoricalNativeScriptHistoryProviderIdentity } from "@al-ft/midgard-fault-proofs";
import type * as SDK from "@al-ft/midgard-sdk";
import type { PublishedDaAttestationOutcome } from "midgard-watcher/tests/support/published-block-actor";

import type { loadJourneyContext } from "./live-context.js";
import type { SignedCommitReconciliationPorts } from "./signed-commit-reconciliation.js";

export type JourneyCategory = Exclude<
  SDK.FraudProofCatalogueCategoryName,
  "validationTraceDispute"
>;

export type JourneyContext = Awaited<ReturnType<typeof loadJourneyContext>>;

export type JourneyBlock = {
  header: SDK.Header;
  headerHash: string;
  payloadEnvelopeCbor: Buffer;
};

export type JourneySuccessor = JourneyBlock & { commitTxHash: string };

/** Successor progress persisted before signing, before submission, and after inclusion. */
export type JourneySuccessorCheckpoint = {
  block: JourneyBlock;
  signedCommit?: { txHash: string; signedCbor: string };
  commitTxHash?: string;
};

export type StagedJourney = {
  predecessor: JourneyBlock;
  current: JourneyBlock;
  /**
   * How the fault's DA attestation ended. A watcher racing staging may correct
   * the fault before DA apply; that authenticated correction is a complete
   * staging outcome, and the journey continues to observe it rather than
   * failing on the missing target.
   */
  target: PublishedDaAttestationOutcome;
  commitHonestSuccessor(options: {
    beforeCommit(block: JourneyBlock): Promise<void>;
    resume?: JourneySuccessorCheckpoint;
    onCheckpoint?(checkpoint: JourneySuccessorCheckpoint): Promise<void>;
  }): Promise<JourneySuccessor>;
};

export type JourneyFixtureStage = {
  context: JourneyContext;
  directory: string;
  historicalNativeScriptProviders: readonly HistoricalNativeScriptHistoryProviderIdentity[];
  /** Publish the retained payload and archive its real canonical L1 point. */
  retain(block: JourneyBlock, commitTxHash: string): Promise<void>;
  /** Launch observation once a genuine healthy predecessor is retained. */
  onHealthyPredecessor?(headerHash: string): Promise<void>;
  readSignedCommitRecovery: SignedCommitReconciliationPorts["readRecovery"];
  readConfirmedTransaction?(txHash: string): Promise<{ cbor: string }>;
  onStage(name: string): void;
};

/**
 * Fixtures stage actual operator actions and preserve their own checkpoints.
 * The category is an acceptance assertion only: it never enters the watcher
 * configuration, classifier, proof application, or workflow submission.
 */
export type JourneyFixture = {
  category: JourneyCategory;
  stage(input: JourneyFixtureStage): Promise<StagedJourney>;
};
