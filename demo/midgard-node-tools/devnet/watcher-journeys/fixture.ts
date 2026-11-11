import type { InstalledFraudProofCategory } from "@al-ft/midgard-fault-proofs";
import type { LedgerAt } from "@al-ft/midgard-fault-proofs";
import type * as SDK from "@al-ft/midgard-sdk";

import type { StackShiftCoverInput } from "./journey-timing.js";
import type { loadJourneyContext } from "./live-context.js";
import type { SignedCommitReconciliationPorts } from "./signed-commit-reconciliation.js";
import type { StackJourneyContext } from "./stack-context.js";
import type { StackDaOutcome } from "./stack-da-publication.js";

/**
 * How far past ledger time a staged block's header ends, set when the block
 * is built. Its end time bounds the validity of the block's commit.
 */
export const JOURNEY_HEADER_END_AHEAD_MS = 59_999;

/** A category with an installed off-chain workflow. */
export type JourneyCategory = InstalledFraudProofCategory;

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
   * failing on the missing target. On an attached stack a fault whose payload
   * the committee refuses ends refused instead: unattested past its
   * attestation deadline.
   */
  target: StackDaOutcome;
  commitHonestSuccessor(options: {
    beforeCommit(block: JourneyBlock): Promise<void>;
    resume?: JourneySuccessorCheckpoint;
    onCheckpoint?(checkpoint: JourneySuccessorCheckpoint): Promise<void>;
  }): Promise<JourneySuccessor>;
};

export type JourneyFixtureStage = {
  /** A devnet journey's own context, or an attached stack's. */
  context: JourneyContext | StackJourneyContext;
  /** Independently replayed state addressed by its admitted L1 header hash. */
  ledgerAt: LedgerAt;
  directory: string;
  /** Publish the retained payload once its commit is on the recorded chain. */
  retain(block: JourneyBlock, commitTxHash: string): Promise<void>;
  /** Launch observation once a genuine healthy predecessor is retained. */
  onHealthyPredecessor?(headerHash: string): Promise<void>;
  readSignedCommitRecovery: SignedCommitReconciliationPorts["readRecovery"];
  readConfirmedTransaction?(txHash: string): Promise<{ cbor: string }>;
  /**
   * The fault's commit is on L1, before its DA publication; again on every
   * resumed staging. An attached stack starts watching our node's hold here.
   */
  onFaultCommitted?(fault: JourneyBlock): Promise<void> | void;
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
  /**
   * How much of operator-2's shift this family's staging holds on an
   * attached stack: the one value its staging asks the shift for and its
   * time limit is derived from. Refuses when no shift holds it.
   */
  stackShiftCoverMs(input: StackShiftCoverInput): number;
};
