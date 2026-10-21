import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  createOwnOutputJournal,
  type OwnOutputJournal,
} from "../workflow/own-outputs.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ObserversForbiddenContracts } from "./contracts.js";
import {
  type ObserversForbiddenEvidence,
  observersForbiddenEvidenceCloses,
} from "./family.js";
import {
  ObserversForbiddenStep02DatumSchema,
  ObserversForbiddenStep02RedeemerSchema,
} from "./schemas.js";

export const submitObserversForbiddenStep02 = async ({
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
  ownOutputs = createOwnOutputJournal(),
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ObserversForbiddenContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ObserversForbiddenEvidence;
  readonly nativeTxCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  /** Our carriage claims; a fresh in-memory journal by default. */
  readonly ownOutputs?: OwnOutputJournal;
}) => {
  if (!observersForbiddenEvidenceCloses(evidence))
    throw new Error("observersForbidden: terminal evidence is honest");
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "observers-forbidden-on-untagged-network",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    network_id: bigint;
  }>({
    threadUtxo,
    signer,
    schema: ObserversForbiddenStep02DatumSchema as never,
    family: "observers-forbidden-on-untagged-network",
    stepIndex,
  });
  if (state.network_id !== BigInt(evidence.networkId))
    throw new Error("observersForbidden: bound network scalar changed");
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: 3,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: decodeMidgardFieldPreimage(
      Buffer.from(evidence.observerFieldPreimageCbor, "hex"),
    ),
    owner: signer.paymentKeyHash,
    publish: true,
    label: "observers forbidden field 3",
  });
  const carriageUtxos = await resolveFaultProofFieldCarriagePublications({
    ownOutputs,
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
      ? await resolveFaultProofFieldPreimageCertificate({
          ownOutputs,
          lucid,
          network: lucid.config().network!,
          planned,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        })
      : undefined;
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
    throw new Error("observersForbidden: field certificate disappeared");
  return await submitLinearFaultFinalize({
    lucid,
    family: "observers-forbidden-on-untagged-network",
    stepIndex,
    step: contracts.steps[1],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: ObserversForbiddenStep02RedeemerSchema,
    buildFamilyArgs: (
      { inputIndex, outputIndex, fraudProofMintRedeemerIndex },
      ctx,
    ) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
      observer_opening: faultProofFieldOpening({
        planned,
        referenceInputs: ctx.referenceInputs,
        certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        label: "observers forbidden field 3",
      }),
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
