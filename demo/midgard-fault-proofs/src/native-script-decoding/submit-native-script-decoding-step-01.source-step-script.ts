import { type NativeScriptDecodingBindState } from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";

import type { NativeScriptDecodingContracts } from "./contracts.js";
import {
  nativeScriptDecodingStepLabel,
  requireNativeScriptDecodingReferenceScript,
} from "./submit-common.js";

export const STEP_LABEL = nativeScriptDecodingStepLabel(0);

export type SubmitNativeScriptDecodingStep01Result = {
  readonly txHash: string;
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
  readonly awaitedConfirmation: boolean;
};

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
