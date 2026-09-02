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
import type { ZeroInputContractsV1 } from "./contracts-v1.js";
import {
  zeroInputEvidenceClosesV1,
  type ZeroInputEvidenceV1,
} from "./family-v1.js";
import {
  ZeroInputStep02DatumV1Schema,
  ZeroInputStep02RedeemerV1Schema,
} from "./schemas-v1.js";

export const submitZeroInputStep02V1 = async ({
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
  readonly contracts: ZeroInputContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ZeroInputEvidenceV1;
  readonly nativeTxCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!zeroInputEvidenceClosesV1(evidence))
    throw new Error("zeroInput: terminal evidence is honest");
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "zero-input",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    subject: { transaction_id: string };
  }>({
    threadUtxo,
    signer,
    schema: ZeroInputStep02DatumV1Schema as never,
    family: "zero-input",
    stepIndex,
  });
  if (state.subject.transaction_id !== evidence.subject.transaction_id)
    throw new Error("zeroInput: bound transaction changed");
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: 0,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: decodeMidgardFieldPreimageV1(
      Buffer.from(evidence.inputFieldPreimageCbor, "hex"),
    ),
    owner: signer.paymentKeyHash,
    publish: true,
    label: "zero input field 0",
  });
  const carriageUtxos = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid,
    publisherAddress: signer.address,
    planned,
  });
  if (carriageUtxos === undefined)
    throw new Error(
      `zeroInput: field carriage disappeared (${planned.plan.tier})`,
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
    throw new Error("zeroInput: field certificate disappeared");
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "zero-input",
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
    label: "zero input field 0",
  });
  return await submitLinearFaultFinalizeV1({
    lucid,
    family: "zero-input",
    stepIndex,
    step: contracts.steps[1],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ZeroInputStep02RedeemerV1Schema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
      spend_inputs_opening: opening,
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
