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
import type { OutputReferenceScriptDecodingContractsV1 } from "./contracts-v1.js";
import {
  classifyOutputReferenceScriptDecodingFindingV1,
  type OutputReferenceScriptDecodingFindingV1,
  OutputReferenceScriptResultClassesV1,
} from "./output-reference-script-decoding-v1.js";
import {
  OutputReferenceStep01RedeemerV1Schema,
  OutputReferenceStep02DatumV1Schema,
} from "./schemas-v1.js";

export const submitOutputReferenceScriptDecodingStep01AcceptedV1 = async ({
  lucid,
  blueprint,
  network,
  contracts,
  signer,
  finding,
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
  readonly contracts: OutputReferenceScriptDecodingContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly finding: OutputReferenceScriptDecodingFindingV1;
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
  classifyOutputReferenceScriptDecodingFindingV1(finding);
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: finding.subject,
        output_index: BigInt(finding.outputIndex),
        accused_class: BigInt(OutputReferenceScriptResultClassesV1.Pending),
      },
    } as never,
    OutputReferenceStep02DatumV1Schema as never,
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
    spendRedeemerSchema: OutputReferenceStep01RedeemerV1Schema,
    wrapInclusionArgs: (inclusion) => ({
      source: {
        AcceptedSource: {
          inclusion: { RedeemerCarriedInclusion: [inclusion] },
        },
      },
      output_index: BigInt(finding.outputIndex),
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
