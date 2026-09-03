import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import { submitMissingNativeScriptTxBinding } from "../missing-native-script-tx/submit-native-binding.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { SpendInputSignerMissingContracts } from "./contracts.js";
import {
  SpendInputSignerStep01RedeemerSchema,
  SpendInputSignerStep02DatumSchema,
} from "./schemas.js";
import {
  classifySpendInputSignerMissingFinding,
  type SpendInputSignerMissingEvidence,
} from "./spend-input-signer-missing.js";

export const submitSpendInputSignerMissingStep01Accepted = async ({
  lucid,
  blueprint,
  network,
  contracts,
  signer,
  evidence,
  threadUtxo,
  threadToken,
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
  readonly contracts: SpendInputSignerMissingContracts;
  readonly signer: ResolvedProverSigner;
  readonly evidence: SpendInputSignerMissingEvidence;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  classifySpendInputSignerMissingFinding(evidence);
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        input_index: BigInt(evidence.inputIndex),
        prior_root: evidence.resolved.priorRoot,
        witness_set_hash: evidence.witnessSetHashHex,
      },
    } as never,
    SpendInputSignerStep02DatumSchema as never,
  );
  return await submitMissingNativeScriptTxBinding({
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
    spendRedeemerSchema: SpendInputSignerStep01RedeemerSchema,
    wrapInclusionArgs: (inclusion) => ({
      source: {
        AcceptedSource: {
          inclusion: { RedeemerCarriedInclusion: [inclusion] },
        },
      },
      input_index: BigInt(evidence.inputIndex),
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
