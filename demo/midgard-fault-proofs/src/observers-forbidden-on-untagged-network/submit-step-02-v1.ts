import { decodeMidgardFieldPreimageV1 } from "@al-ft/midgard-core";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  faultProofFieldOpeningV1,
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import {
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultFinalizeV1 } from "../linear-fault-finalize-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { ObserversForbiddenContractsV1 } from "./contracts-v1.js";
import {
  observersForbiddenEvidenceClosesV1,
  type ObserversForbiddenEvidenceV1,
} from "./family-v1.js";
import {
  ObserversForbiddenStep02DatumV1Schema,
  ObserversForbiddenStep02RedeemerV1Schema,
} from "./schemas-v1.js";

export const submitObserversForbiddenStep02V1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserversForbiddenContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ObserversForbiddenEvidenceV1;
  readonly nativeTxCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!observersForbiddenEvidenceClosesV1(evidence))
    throw new Error("observersForbidden: terminal evidence is honest");
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "observers-forbidden-on-untagged-network",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    subject: unknown;
    network_id: bigint;
  }>({
    threadUtxo,
    signer,
    schema: ObserversForbiddenStep02DatumV1Schema as never,
    family: "observers-forbidden-on-untagged-network",
    stepIndex,
  });
  if (state.network_id !== BigInt(evidence.networkId))
    throw new Error("observersForbidden: bound network scalar changed");
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: 3,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: decodeMidgardFieldPreimageV1(
      Buffer.from(evidence.observerFieldPreimageCbor, "hex"),
    ),
    owner: signer.paymentKeyHash,
    publish: true,
    label: "observers forbidden field 3",
  });
  const carriageUtxos = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid,
    publisherAddress: signer.address,
    planned,
  });
  if (carriageUtxos === undefined)
    throw new Error(
      `observersForbidden: field carriage disappeared (${planned.plan.tier}, ${planned.plan.publications.length.toString()} publications)`,
    );
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? await resolveFaultProofFieldPreimageCertificateV1({
          lucid,
          network: lucid.config().network!,
          planned,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        })
      : undefined;
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
    throw new Error("observersForbidden: field certificate disappeared");
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "observers-forbidden-on-untagged-network",
    stepIndex,
  });
  const opening = faultProofFieldOpeningV1({
    planned,
    referenceInputs: [
      ...carriageUtxos,
      stepReference,
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
      ...(witnessReferenceScripts.computationThreadMint === undefined
        ? []
        : [witnessReferenceScripts.computationThreadMint]),
      ...(witnessReferenceScripts.fraudProofMint === undefined
        ? []
        : [witnessReferenceScripts.fraudProofMint]),
    ],
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: "observers forbidden field 3",
  });
  return await submitLinearFaultFinalizeV1({
    lucid,
    family: "observers-forbidden-on-untagged-network",
    stepIndex,
    step: contracts.steps[1],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ObserversForbiddenStep02RedeemerV1Schema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
      observer_opening: opening,
    }),
    referenceScriptUtxo,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
