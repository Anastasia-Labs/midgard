import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ZeroInputContracts } from "./contracts.js";
import { type ZeroInputEvidence, zeroInputEvidenceCloses } from "./family.js";
import {
  ZeroInputStep02DatumSchema,
  ZeroInputStep02RedeemerSchema,
} from "./schemas.js";

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
  readonly contracts: ZeroInputContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ZeroInputEvidence;
  readonly nativeTxCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  if (!zeroInputEvidenceCloses(evidence))
    throw new Error("zeroInput: terminal evidence is honest");
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "zero-input",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: { transaction_id: string };
  }>({
    threadUtxo,
    signer,
    schema: ZeroInputStep02DatumSchema as never,
    family: "zero-input",
    stepIndex,
  });
  if (state.subject.transaction_id !== evidence.subject.transaction_id)
    throw new Error("zeroInput: bound transaction changed");
  const planned = planFaultProofFieldOpening({
    fieldIndex: 0,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: decodeMidgardFieldPreimage(
      Buffer.from(evidence.inputFieldPreimageCbor, "hex"),
    ),
    owner: signer.paymentKeyHash,
    publish: true,
    label: "zero input field 0",
  });
  const carriageUtxos = await resolveFaultProofFieldCarriagePublications({
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
      ? await resolveFaultProofFieldPreimageCertificate({
          lucid,
          network: lucid.config().network!,
          planned,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        })
      : undefined;
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
    throw new Error("zeroInput: field certificate disappeared");
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "zero-input",
    stepIndex,
  });
  const opening = faultProofFieldOpening({
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
  return await submitLinearFaultFinalize({
    lucid,
    family: "zero-input",
    stepIndex,
    step: contracts.steps[1],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ZeroInputStep02RedeemerSchema,
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
