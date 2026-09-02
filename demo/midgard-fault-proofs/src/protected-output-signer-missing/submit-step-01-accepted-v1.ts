import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import { submitMissingNativeScriptTxBindingV1 } from "../missing-native-script-tx/submit-native-binding-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { ProtectedOutputSignerMissingContractsV1 } from "./contracts-v1.js";
import {
  classifyProtectedOutputSignerMissingFindingV1,
  type ProtectedOutputSignerMissingEvidenceV1,
} from "./protected-output-signer-missing-v1.js";
import {
  ProtectedOutputSignerStep01RedeemerV1Schema,
  ProtectedOutputSignerStep02DatumV1Schema,
} from "./schemas-v1.js";

export const submitProtectedOutputSignerMissingStep01AcceptedV1 = async ({
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
  readonly contracts: ProtectedOutputSignerMissingContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly evidence: ProtectedOutputSignerMissingEvidenceV1;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  classifyProtectedOutputSignerMissingFindingV1(evidence);
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
    ProtectedOutputSignerStep02DatumV1Schema as never,
  );
  return await submitMissingNativeScriptTxBindingV1({
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
    spendRedeemerSchema: ProtectedOutputSignerStep01RedeemerV1Schema,
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
