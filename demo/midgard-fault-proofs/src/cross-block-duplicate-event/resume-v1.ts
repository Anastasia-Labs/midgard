import type { CommittedDuplicateEventProofV1 } from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network, UTxO } from "@lucid-evolution/lucid";

import {
  fetchUtxoByOutRef,
  parseOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { CrossBlockDuplicateEventContractsV1 } from "./contracts-v1.js";
import { crossBlockDuplicateEventSubmitError } from "./submit-common-v1.js";
import {
  submitCrossBlockDuplicateEventStep01,
  type SubmitCrossBlockDuplicateEventStep01Result,
} from "./submit-cross-block-duplicate-event-step-01.js";
import {
  submitCrossBlockDuplicateEventStep02,
  type SubmitCrossBlockDuplicateEventStep02Result,
} from "./submit-cross-block-duplicate-event-step-02.js";

export type ResumeCrossBlockDuplicateEventResultV1 =
  | {
      readonly resumedStep: "step-01";
      readonly result: SubmitCrossBlockDuplicateEventStep01Result;
    }
  | {
      readonly resumedStep: "step-02";
      readonly result: SubmitCrossBlockDuplicateEventStep02Result;
    };

/** Resume exactly the step address that currently owns the live thread NFT. */
export const resumeCrossBlockDuplicateEventV1 = async ({
  lucid,
  network,
  contracts,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  challengedEvent,
  settlementOutRef,
  settledHeaderHash,
  settledEvent,
  referenceScriptUtxos,
  witnessReferenceScripts,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly contracts: CrossBlockDuplicateEventContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly challengedEvent: CommittedDuplicateEventProofV1;
  readonly settlementOutRef: string;
  readonly settledHeaderHash: string;
  readonly settledEvent: CommittedDuplicateEventProofV1;
  readonly referenceScriptUtxos: readonly [UTxO, UTxO];
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly awaitConfirmation?: boolean;
}): Promise<ResumeCrossBlockDuplicateEventResultV1> => {
  const thread = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "cross-block-duplicate-event resumable thread",
  });
  if (thread.address === contracts.steps[0].spendingScriptAddress) {
    return {
      resumedStep: "step-01",
      result: await submitCrossBlockDuplicateEventStep01({
        lucid,
        network,
        contracts,
        signer,
        threadOutRef,
        stateQueueBlockOutRef,
        committedEvent: challengedEvent,
        referenceScriptUtxo: referenceScriptUtxos[0],
        awaitConfirmation,
      }),
    };
  }
  if (thread.address === contracts.steps[1].spendingScriptAddress) {
    return {
      resumedStep: "step-02",
      result: await submitCrossBlockDuplicateEventStep02({
        lucid,
        contracts,
        signer,
        threadOutRef,
        settlementOutRef,
        settledHeaderHash,
        settledEvent,
        referenceScriptUtxo: referenceScriptUtxos[1],
        witnessReferenceScripts,
        awaitConfirmation,
      }),
    };
  }
  throw crossBlockDuplicateEventSubmitError(
    "thread is not live at either family step; it may already be finalized or cancelled",
  );
};
