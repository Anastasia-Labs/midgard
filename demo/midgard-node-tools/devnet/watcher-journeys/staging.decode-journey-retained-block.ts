import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import {
  type CheckedLedger,
  ledgerAfter,
  type LedgerAt,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import type {
  PublishedDaTargetCorrection,
  PublishedDaTransactionRecord,
} from "midgard-watcher/tests/support/published-da-target-consumption";

import type {
  JourneyBlock,
  JourneyCategory,
  JourneyFixtureStage,
  JourneySuccessor,
} from "./fixture.js";
import type { StackShiftCoverInput } from "./journey-timing.js";
import type {
  StackDaDelivery,
  StackDaRefused,
} from "./stack-da-publication.js";

export type JourneyRetainedBlock = JourneyBlock & {
  payload: SDK.DaPayload;
  ledger: CheckedLedger;
};

export type JourneyFaultBuildInput = {
  predecessor: JourneyRetainedBlock;
  ledgerOwnerSeedPhrase: string;
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
};

export type JourneyPreparedFault = {
  /** An actual additional history block committed and retained during preparation. */
  predecessor?: JourneySuccessor;
  buildFault(input: JourneyFaultBuildInput): Promise<JourneyBlock>;
  /** Required when recovery must honestly consume a pending L1 event. */
  buildSuccessor?(input: JourneyFaultBuildInput): Promise<JourneyBlock>;
};

export type JourneyFaultPreparationInput = JourneyFixtureStage & {
  predecessor: JourneyRetainedBlock;
  ledgerOwnerSeedPhrase: string;
  /** Commit genuine prerequisite history through the same operator and journal. */
  commitHistoryBlock(
    name: string,
    build: (input: JourneyFaultBuildInput) => Promise<JourneyBlock>,
  ): Promise<JourneySuccessor>;
  /**
   * Called before building each user event the family publishes. On an
   * attached stack it holds the event back to the publication floor of
   * operator-2's shift, or refuses (`stackUserEventGate`); elsewhere it
   * returns at once.
   */
  beforeUserEvent(): Promise<void>;
};

export type StagingCheckpoint = {
  deploymentFingerprint: string;
  predecessor: JourneySuccessor;
  current: JourneyBlock;
  commitTxHash?: string;
  signedCommit?: { txHash: string; signedCbor: string };
  /** DA transactions signed for the fault, recorded before each submission. */
  daTransactions?: PublishedDaTransactionRecord[];
  /**
   * The fault's authenticated correction when it consumed the target before
   * DA apply, or on an attached stack the committee's observed refusal.
   */
  target?: PublishedDaTargetCorrection | StackDaRefused;
  /** On an attached stack, the committee peers that took the fault's payload. */
  daDelivery?: StackDaDelivery;
};

export const decodeJourneyRetainedBlock = async (
  block: JourneyBlock,
  ledgerAt: LedgerAt,
): Promise<JourneyRetainedBlock> => {
  const envelope = await unwrapDaPayload(
    Buffer.from(block.payloadEnvelopeCbor),
    {
      maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
    },
  );
  return {
    ...block,
    payload: SDK.decodeDaPayload(Buffer.from(envelope.innerBytes)),
    ledger: await ledgerAfter({
      ledgerAt,
      headerHash: block.headerHash,
      utxosRoot: block.header.utxosRoot,
      label: "journey retained predecessor",
    }),
  };
};

export type FaultPreparation = {
  /** The family staged; on an attached stack it decides the committee's expected answer. */
  category: JourneyCategory;
  prepare(input: JourneyFaultPreparationInput): Promise<JourneyPreparedFault>;
  /**
   * How many history blocks `prepare` commits before the fault (none by
   * default). An attached stack sizes operator-2's shift work by it.
   */
  historyBlocks?: number;
  /**
   * How many user events (deposits, withdrawals, tx orders) `prepare`
   * publishes and awaits the inclusion of (none by default). It publishes
   * them one after another and then awaits their inclusion once. An attached
   * stack holds operator-2's shift through each one's publication and that
   * one event wait.
   */
  userEvents?: number;
  /**
   * The family's staging shift cover from `historyBlocks` and `userEvents`
   * (`stackStagingShiftCover`); its fixture's limit reads the same function.
   */
  stackShiftCoverMs(input: StackShiftCoverInput): number;
};
