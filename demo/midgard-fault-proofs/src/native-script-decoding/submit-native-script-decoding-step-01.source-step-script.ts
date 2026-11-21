import { type NativeScriptDecodingBindState } from "@al-ft/midgard-sdk/fraud-proof";
import { type UTxO } from "@lucid-evolution/lucid";

import {
  type LocallyEvaluatedTransaction,
  type OutsideWorkflowSubmission,
} from "../workflow/transaction-boundary.js";
import type { NativeScriptDecodingContracts } from "./contracts.js";
import {
  nativeScriptDecodingStepLabel,
  requireNativeScriptDecodingReferenceScript,
} from "./submit-common.js";

export const STEP_LABEL = nativeScriptDecodingStepLabel(0);

export type NativeScriptDecodingStep01Build = {
  readonly transaction: LocallyEvaluatedTransaction;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  /** The `BindStateV1` the thread now carries. */
  readonly bindState: NativeScriptDecodingBindState;
  readonly inputIndex: number;
  readonly outputIndex: number;
};

export type SubmitNativeScriptDecodingStep01Result =
  OutsideWorkflowSubmission<NativeScriptDecodingStep01Build>;

/** Source the mandatory authenticated step-01 reference script. */
export const sourceStepScript = <
  T extends {
    readFrom: (utxos: UTxO[]) => T;
  },
>({
  tx,
  contracts,
  referenceScriptUtxo,
}: {
  readonly tx: T;
  readonly contracts: NativeScriptDecodingContracts;
  readonly referenceScriptUtxo: UTxO;
}): T =>
  tx.readFrom([
    requireNativeScriptDecodingReferenceScript({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[0].spendingScriptHash,
      stepIndex: 0,
    }),
  ]);
