import {
  computeHash32,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxWitnessSetCompact,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";

import {
  faultProofRawFieldCarriage,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import { createRawCommittedFieldCarriagePlan } from "../workflow/field-carriage-prerequisite.js";
import type { ManifestBoundFieldPreimageLengthWorkflow } from "./authenticated-workflow.js";
import type { AuthenticatedFieldPreimageLengthEvidence } from "./evidence.js";
import { fieldPreimageLengthCommittedClaim } from "./prepare-accepted.js";

export const planFieldPreimageLengthCarriage = ({
  workflow,
  evidence,
}: {
  readonly workflow: Pick<ManifestBoundFieldPreimageLengthWorkflow, "config">;
  readonly evidence: AuthenticatedFieldPreimageLengthEvidence;
}) => {
  const compact = (
    evidence.prepared.sourceKind === "forced"
      ? decodeMidgardForcedTxCompact
      : decodeMidgardNativeTxCompact
  )(Buffer.from(evidence.fieldMaterial.nativeTxCompactCbor, "hex"));
  const witnessSet = decodeMidgardNativeTxWitnessSetCompact(
    Buffer.from(evidence.fieldMaterial.witnessSetCompactCbor, "hex"),
  );
  if (
    computeMidgardNativeTxId(compact).toString("hex") !==
    evidence.prepared.transactionId
  )
    throw new Error(
      "fieldPreimageLengthMismatch compact differs from the authenticated transaction id",
    );
  if (
    !computeHash32(
      Buffer.from(evidence.fieldMaterial.witnessSetCompactCbor, "hex"),
    ).equals(compact.transactionWitnessSetHash)
  )
    throw new Error(
      "fieldPreimageLengthMismatch witness set differs from the compact identity",
    );
  const body = compact.transactionBody;
  const commitments = [
    body.spendInputsHash,
    body.referenceInputsHash,
    body.outputsHash,
    body.requiredObserversHash,
    body.requiredSignersHash,
    body.mintHash,
    witnessSet.scriptTxWitsHash,
    witnessSet.addrTxWitsHash,
    witnessSet.redeemerTxWitsHash,
  ];
  const preimage = Buffer.from(evidence.prepared.preimageHex, "hex");
  const expected = commitments[evidence.prepared.fieldIndex];
  if (
    expected === undefined ||
    !midgardFieldCommitment(preimage).equals(expected) ||
    preimage.length !== evidence.prepared.actualLength
  )
    throw new Error(
      "fieldPreimageLengthMismatch raw field differs from the authenticated commitment or length",
    );
  return createRawCommittedFieldCarriagePlan({
    sourceKind: evidence.prepared.sourceKind === "forced" ? 1n : 0n,
    fieldIndex: evidence.prepared.fieldIndex,
    nativeTxId: evidence.prepared.transactionId,
    preimage,
    owner: workflow.config.signer.paymentKeyHash,
  });
};

export const resolveFieldPreimageLengthCarriage = async ({
  workflow,
  evidence,
}: {
  readonly workflow: Pick<ManifestBoundFieldPreimageLengthWorkflow, "config">;
  readonly evidence: AuthenticatedFieldPreimageLengthEvidence;
}) => {
  const planned = planFieldPreimageLengthCarriage({ workflow, evidence });
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: workflow.config.lucid,
    publisherAddress: workflow.config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new WorkflowActionChangedError(
      "fieldPreimageLengthMismatch publication changed before capture",
    );
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: workflow.config.lucid,
    network: workflow.config.binding.network,
    planned,
    certificatePolicyId:
      workflow.config.contracts.fieldPreimageCertificate.policyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new WorkflowActionChangedError(
      "fieldPreimageLengthMismatch certificate changed before capture",
    );
  }
  const carriageReferences = [
    ...publications,
    ...(certificate === undefined ? [] : [certificate]),
  ];
  const claimResolver = (
    completeReferenceInputs: readonly (typeof carriageReferences)[number][],
  ) =>
    fieldPreimageLengthCommittedClaim({
      fieldIndex: evidence.prepared.fieldIndex,
      witnessSetCompactCbor: Buffer.from(
        evidence.fieldMaterial.witnessSetCompactCbor,
        "hex",
      ),
      carriage: faultProofRawFieldCarriage({
        plan: planned.plan,
        referenceInputs: completeReferenceInputs,
        certificatePolicyId:
          workflow.config.contracts.fieldPreimageCertificate.policyId,
        label: "fieldPreimageLengthMismatch authenticated field",
      }),
    });
  return Object.freeze({ carriageReferences, claimResolver });
};
