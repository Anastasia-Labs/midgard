import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import { buildNativeTxBinding } from "../native-tx-binding.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../step-support.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { OutputReferenceScriptDecodingContracts } from "./contracts.js";
import {
  classifyOutputReferenceScriptDecodingFinding,
  type OutputReferenceScriptDecodingFinding,
  OutputReferenceScriptResultClasses,
} from "./output-reference-script-decoding.js";
import {
  OutputReferenceStep01RedeemerSchema,
  OutputReferenceStep02DatumSchema,
} from "./schemas.js";

export const buildOutputReferenceScriptDecodingStep01Accepted = async ({
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
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: OutputReferenceScriptDecodingContracts;
  readonly signer: ResolvedProverSigner;
  readonly finding: OutputReferenceScriptDecodingFinding;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
}) => {
  classifyOutputReferenceScriptDecodingFinding(finding);
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: finding.subject,
        output_index: finding.outputIndex,
        accused_class: BigInt(OutputReferenceScriptResultClasses.Pending),
      },
    } as never,
    OutputReferenceStep02DatumSchema as never,
  );
  return await buildNativeTxBinding({
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
    spendRedeemerSchema: OutputReferenceStep01RedeemerSchema,
    wrapInclusionArgs: (inclusion) => ({
      source: {
        AcceptedSource: {
          inclusion: { RedeemerCarriedInclusion: [inclusion] },
        },
      },
      output_index: finding.outputIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
  });
};
