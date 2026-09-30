import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import type {
  PublishedDaTargetCorrection,
  PublishedDaTransactionRecord,
} from "midgard-watcher/tests/support/published-da-target-consumption";

import type {
  JourneyBlock,
  JourneyFixtureStage,
  JourneySuccessor,
} from "./fixture.js";

export type JourneyRetainedBlock = JourneyBlock & { payload: SDK.DaPayload };

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
};

export type StagingCheckpoint = {
  deploymentFingerprint: string;
  predecessor: JourneySuccessor;
  current: JourneyBlock;
  commitTxHash?: string;
  signedCommit?: { txHash: string; signedCbor: string };
  /** DA transactions signed for the fault, recorded before each submission. */
  daTransactions?: PublishedDaTransactionRecord[];
  /** The fault's authenticated correction when it consumed the target before DA apply. */
  target?: PublishedDaTargetCorrection;
};

export const decodeJourneyRetainedBlock = async (
  block: JourneyBlock,
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
  };
};

export type HistoryPreparation = {
  mode: "history";
  prepare(input: JourneyFaultPreparationInput): Promise<void>;
};

export type FaultPreparation = {
  mode: "fault";
  prepare(input: JourneyFaultPreparationInput): Promise<JourneyPreparedFault>;
};
