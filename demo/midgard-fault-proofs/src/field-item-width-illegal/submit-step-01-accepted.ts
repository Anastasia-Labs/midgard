import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import { submitNativeTxBinding } from "../native-tx-binding.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  nativeTxFromCoreCompact,
  requireNativeTxMatchesCompactCbor,
  type SubmitStep01TxInclusion,
} from "../step-support.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { FieldItemWidthIllegalContracts } from "./contracts.js";
import {
  classifyFieldItemWidthFinding,
  fieldItemWidthBoundCoordinate,
  type FieldItemWidthFinding,
} from "./field-item-width-illegal.js";
import {
  FieldItemWidthStep01RedeemerSchema,
  FieldItemWidthStep02DatumSchema,
} from "./schemas.js";

export const submitFieldItemWidthIllegalStep01Accepted = async ({
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
  readonly contracts: FieldItemWidthIllegalContracts;
  readonly signer: ResolvedProverSigner;
  readonly finding: FieldItemWidthFinding;
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
  const exact = classifyFieldItemWidthFinding(finding);
  const compact = nativeTxFromCoreCompact(
    requireNativeTxMatchesCompactCbor(txInclusion),
  );
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: fieldItemWidthBoundCoordinate(exact, compact),
    } as never,
    FieldItemWidthStep02DatumSchema as never,
  );
  return await submitNativeTxBinding({
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
    spendRedeemerSchema: FieldItemWidthStep01RedeemerSchema,
    wrapInclusionArgs: (inclusion) => ({
      source: {
        AcceptedSource: {
          inclusion: { RedeemerCarriedInclusion: [inclusion] },
        },
      },
      field_index: BigInt(finding.fieldIndex),
      item_index: BigInt(finding.itemIndex),
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
