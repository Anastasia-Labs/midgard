import {
  MissingNativeScriptUtxoStep01SpendRedeemerSchema,
  MissingNativeScriptUtxoStep02DatumSchema,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultInitialDatum,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import type { MissingNativeScriptTxContracts } from "../missing-native-script-tx/contracts.js";
import { submitMissingNativeScriptTxBinding } from "../missing-native-script-tx/submit-native-binding.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  MISSING_NATIVE_SCRIPT_UTXO_CATEGORY_LABEL,
  type MissingNativeScriptUtxoContracts,
} from "./contracts.js";

type Step02Datum = Data.Static<typeof MissingNativeScriptUtxoStep02DatumSchema>;
const Step02Datum =
  MissingNativeScriptUtxoStep02DatumSchema as unknown as Step02Datum;

export const submitMissingNativeScriptUtxoStep01 = async ({
  lucid,
  blueprint,
  network,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  prevUtxosRoot,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: MissingNativeScriptUtxoContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly prevUtxosRoot: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: MISSING_NATIVE_SCRIPT_UTXO_CATEGORY_LABEL,
    stepIndex: 0,
    threadOutRef,
  });
  requireLinearFaultInitialDatum({
    threadUtxo,
    signer,
    family: MISSING_NATIVE_SCRIPT_UTXO_CATEGORY_LABEL,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bad_tx_id: txInclusion.nativeTxId,
        bad_tx_witness_set_hash: txInclusion.nativeTx.witness_set_hash,
        prev_utxos_root: prevUtxosRoot,
      },
    },
    Step02Datum,
  );
  return await submitMissingNativeScriptTxBinding({
    lucid,
    blueprint,
    network,
    contracts: contracts as unknown as MissingNativeScriptTxContracts,
    signer,
    stepIndex: 0,
    threadUtxo,
    threadToken,
    stateQueueBlockOutRef,
    txInclusion,
    nextDatum,
    spendRedeemerSchema: MissingNativeScriptUtxoStep01SpendRedeemerSchema,
    wrapInclusionArgs: (args) => ({
      carriage: { RedeemerCarriedInclusion: [args] },
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
