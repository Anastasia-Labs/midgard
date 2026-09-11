import type { HistoricalNativeScriptHistoryProviderIdentity } from "@al-ft/midgard-fault-proofs";
import type * as SDK from "@al-ft/midgard-sdk";

import type { loadJourneyContext } from "./live-context.js";

export type JourneyCategory = Exclude<
  SDK.FraudProofCatalogueCategoryName,
  "validationTraceDispute"
>;

export type JourneyContext = Awaited<ReturnType<typeof loadJourneyContext>>;

export type JourneyBlock = {
  header: SDK.Header;
  headerHash: string;
  payloadEnvelopeCbor: Uint8Array;
};

export type JourneySuccessor = JourneyBlock & { commitTxHash: string };

export type StagedJourney = {
  predecessor: JourneyBlock;
  current: JourneyBlock;
  commitHonestSuccessor(options: {
    beforeCommit(block: JourneyBlock): Promise<void>;
  }): Promise<JourneySuccessor>;
};

export type JourneyFixtureStage = {
  context: JourneyContext;
  directory: string;
  historicalNativeScriptProviders: readonly HistoricalNativeScriptHistoryProviderIdentity[];
  /** Publish the retained payload and archive its real canonical L1 point. */
  retain(block: JourneyBlock, commitTxHash: string): Promise<void>;
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
