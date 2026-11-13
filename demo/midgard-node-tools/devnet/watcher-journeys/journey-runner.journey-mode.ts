import type {
  FraudProofWorkflowJournalEntry,
  HeaderDecision,
} from "@al-ft/midgard-fault-proofs";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type { JourneyWorkflowStore } from "./correction.js";
import type { JourneyBlock, JourneyFixture, StagedJourney } from "./fixture.js";
import type { JourneySuccessorPort } from "./honest-successor.js";
import type { JourneyWatcherOperations } from "./journey-session.types.js";
import type {
  JourneyTiming,
  ReadJourneyTimingOptions,
} from "./journey-timing.journey-timing-for-plan.js";
import type { StackDaRefused } from "./stack-da-publication.js";
import type { JourneyChainTransactions } from "./stack-transactions.js";

/** The runner's bounded waits, shared by both modes. */
export type JourneyRunPolls = Readonly<{
  stage<T>(name: string, action: () => Promise<T>): Promise<T>;
  poll<T>(
    name: string,
    action: () => Promise<T | undefined>,
    timeoutMs?: number,
  ): Promise<T>;
  /**
   * A wait budgeted by the watcher's progress rather than wall clock: it
   * fails once `progress`, a count, has not risen for `stallTimeoutMs`.
   */
  pollWhileReplaying<T>(
    name: string,
    action: () => Promise<T | undefined>,
    progress: () => Promise<number>,
    stallTimeoutMs: number,
  ): Promise<T>;
}>;

/** The watcher a family's journey observes, whoever runs it. */
export type JourneyRunWatcher = JourneyWatcherOperations &
  Readonly<{ observe?(): { pid: number | null } }>;

/**
 * Everything a family's journey reads or drives that depends on who runs
 * the watcher: a devnet journey's own watcher and recorder, or an attached
 * stack's watcher, Kupo, Ogmios and node.
 */
export type JourneyRunMode = Readonly<{
  /** The watcher's workflow rows for `category` before staging. */
  captureWorkflowBaseline(
    category: FraudProofCatalogueCategoryName,
  ): Promise<ReadonlyMap<string, readonly FraudProofWorkflowJournalEntry[]>>;
  readTiming(
    category: string,
    options: ReadJourneyTimingOptions,
  ): Promise<JourneyTiming>;
  stageFixture(
    fixture: JourneyFixture,
    input: Readonly<{ directory: string; onStage(name: string): void }>,
  ): Promise<StagedJourney>;
  /** Proves the staged blocks' payloads are publicly retrievable. */
  verifyPublicDa(
    input: Readonly<{
      directory: string;
      predecessor: JourneyBlock;
      current: JourneyBlock;
    }>,
  ): Promise<unknown>;
  /**
   * The watcher once it serves, launched first when the journey runs it;
   * `onWatcher` sees it as soon as it exists, before it serves.
   */
  startWatcher(
    input: Readonly<{
      directory: string;
      polls: JourneyRunPolls;
      onWatcher(watcher: JourneyRunWatcher): void;
    }>,
  ): Promise<JourneyRunWatcher>;
  readDecisions(): Promise<readonly { decision: HeaderDecision }[]>;
  watcherStore: JourneyWorkflowStore;
  transactions: JourneyChainTransactions;
  /** The prover wallet the correction's reward must pay. */
  proverAddress: string;
  successor(
    input: Readonly<{
      directory: string;
      staged: StagedJourney;
      polls: JourneyRunPolls;
    }>,
  ): JourneySuccessorPort;
  /**
   * Ends a fault the committee refused: our node held it awaiting its DA,
   * and it aged out unattested. Only an attached stack's own committee
   * refuses a payload; a devnet journey signs as its committee.
   */
  refusal?(
    input: Readonly<{
      fault: JourneyBlock;
      refused: StackDaRefused;
      polls: JourneyRunPolls;
    }>,
  ): Promise<unknown>;
  /** Stops what the mode still runs for the family, whatever ended its run. */
  close?(): Promise<void>;
}>;
