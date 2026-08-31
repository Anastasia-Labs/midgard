import {
  MissingNativeScriptTxStep03Datum,
  MissingNativeScriptTxStep03SpendRedeemer,
  type MissingNativeScriptTxStep03State,
  MissingNativeScriptTxStep04Datum,
  missingNativeScriptTxStep04StateV1,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import { type ResolvedProverSigner } from "../runtime.js";
import { type SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { MissingNativeScriptTxContractsV1 } from "./contracts-v1.js";
import {
  missingNativeScriptTxSubmitError,
  requireMissingNativeScriptTxStepStateV1,
  requireMissingNativeScriptTxThreadUtxoV1,
} from "./submit-common-v1.js";
import { submitMissingNativeScriptTxBindingV1 } from "./submit-native-binding-v1.js";

export type SubmitMissingNativeScriptTxStep03Result = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly producingTxId: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitMissingNativeScriptTxStep03 = async ({
  lucid,
  blueprint,
  network,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: MissingNativeScriptTxContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMissingNativeScriptTxStep03Result> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 2,
      threadOutRef,
    });
  const state: MissingNativeScriptTxStep03State =
    requireMissingNativeScriptTxStepStateV1({
      threadUtxo,
      signer,
      schema: MissingNativeScriptTxStep03Datum,
      stepIndex: 2,
    });
  if (txInclusion.nativeTxId !== state.input_with_missing_script.tx_id) {
    throw missingNativeScriptTxSubmitError(
      `producing transaction ${txInclusion.nativeTxId} does not match accused input transaction ${state.input_with_missing_script.tx_id}.`,
    );
  }
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: missingNativeScriptTxStep04StateV1({
        producingTxId: txInclusion.nativeTxId,
        badInputOutputIndex: state.input_with_missing_script.output_index,
        badTxId: state.bad_tx_id,
        badTxWitnessSetHash: state.bad_tx_witness_set_hash,
      }),
    },
    MissingNativeScriptTxStep04Datum,
  );
  const result = await submitMissingNativeScriptTxBindingV1({
    lucid,
    blueprint,
    network,
    contracts,
    signer,
    stepIndex: 2,
    threadUtxo,
    threadToken,
    stateQueueBlockOutRef,
    txInclusion,
    nextDatum,
    spendRedeemerSchema: MissingNativeScriptTxStep03SpendRedeemer,
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
  return {
    ...result,
    producingTxId: txInclusion.nativeTxId,
    awaitedConfirmation: awaitConfirmation,
  };
};
