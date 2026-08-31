import {
  MissingNativeScriptTxStep01SpendRedeemer,
  MissingNativeScriptTxStep02Datum,
  missingNativeScriptTxStep02StateFromBadTxV1,
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
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { MissingNativeScriptTxContractsV1 } from "./contracts-v1.js";
import { requireMissingNativeScriptTxThreadUtxoV1 } from "./submit-common-v1.js";
import { submitMissingNativeScriptTxBindingV1 } from "./submit-native-binding-v1.js";

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
}): Promise<SubmitMissingNativeScriptTxStep01Result> => {
  const { threadUtxo, threadToken } =
    await requireMissingNativeScriptTxThreadUtxoV1({
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
      data: missingNativeScriptTxStep02StateFromBadTxV1({
        badTxId: txInclusion.nativeTxId,
        badTxWitnessSetHash,
      }),
    },
    MissingNativeScriptTxStep02Datum,
  );
  const result = await submitMissingNativeScriptTxBindingV1({
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
