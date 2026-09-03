import {
  MissingNativeScriptTxStep01SpendRedeemer,
  MissingNativeScriptTxStep02Datum,
  missingNativeScriptTxStep02StateFromBadTx,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import { type ResolvedProverSigner } from "../runtime.js";
import {
  requireInitialStepDatum,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { MissingNativeScriptTxContracts } from "./contracts.js";
import { requireMissingNativeScriptTxThreadUtxo } from "./submit-common.js";
import { submitMissingNativeScriptTxBinding } from "./submit-native-binding.js";

export type SubmitMissingNativeScriptTxStep01Result = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly nativeTxId: string;
  readonly badTxWitnessSetHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitMissingNativeScriptTxStep01 = async ({
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
  readonly contracts: MissingNativeScriptTxContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMissingNativeScriptTxStep01Result> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: 0,
      threadOutRef,
    });
  requireInitialStepDatum({ threadUtxo, signer });
  const badTxWitnessSetHash = txInclusion.nativeTx.witness_set_hash;
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: missingNativeScriptTxStep02StateFromBadTx({
        badTxId: txInclusion.nativeTxId,
        badTxWitnessSetHash,
      }),
    },
    MissingNativeScriptTxStep02Datum,
  );
  const result = await submitMissingNativeScriptTxBinding({
    lucid,
    blueprint,
    network,
    contracts,
    signer,
    stepIndex: 0,
    threadUtxo,
    threadToken,
    stateQueueBlockOutRef,
    txInclusion,
    nextDatum,
    spendRedeemerSchema: MissingNativeScriptTxStep01SpendRedeemer,
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
  return {
    ...result,
    nativeTxId: txInclusion.nativeTxId,
    badTxWitnessSetHash,
    awaitedConfirmation: awaitConfirmation,
  };
};
