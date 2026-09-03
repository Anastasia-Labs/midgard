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
import type { ProtectedOutputSignerMissingContracts } from "./contracts.js";
import {
  classifyProtectedOutputSignerMissingFinding,
  type ProtectedOutputSignerMissingEvidence,
} from "./protected-output-signer-missing.js";
import {
  ProtectedOutputSignerStep01RedeemerSchema,
  ProtectedOutputSignerStep02DatumSchema,
} from "./schemas.js";

export const submitProtectedOutputSignerMissingStep01Accepted = async ({
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
  readonly contracts: ProtectedOutputSignerMissingContracts;
  readonly signer: ResolvedProverSigner;
  readonly evidence: ProtectedOutputSignerMissingEvidence;
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
  classifyProtectedOutputSignerMissingFinding(evidence);
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bound: {
          subject: evidence.subject,
          output_index: BigInt(evidence.outputIndex),
        },
        witness_set_hash: evidence.witnessSetHashHex,
      },
    } as never,
    ProtectedOutputSignerStep02DatumSchema as never,
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
    spendRedeemerSchema: ProtectedOutputSignerStep01RedeemerSchema,
    wrapInclusionArgs: (inclusion) => ({
      source: {
        AcceptedSource: {
          inclusion: { RedeemerCarriedInclusion: [inclusion] },
        },
      },
      output_index: BigInt(evidence.outputIndex),
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
