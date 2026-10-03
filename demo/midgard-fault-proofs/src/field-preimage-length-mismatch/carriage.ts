import {
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";
import { isMidgardWitnessSetField } from "@al-ft/midgard-sdk";

import {
  faultProofFieldCarriage,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
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
    evidence.prepared.direction === "wrongfulRejection"
      ? decodeMidgardForcedTxCompact
      : decodeMidgardNativeTxCompact
  )(Buffer.from(evidence.fieldMaterial.nativeTxCompactCbor, "hex"));
  const witnessSet = decodeMidgardNativeTxWitnessSetCompact(
    Buffer.from(evidence.fieldMaterial.witnessSetCompactCbor, "hex"),
  );
  const witnessField = isMidgardWitnessSetField(evidence.prepared.fieldIndex);
  return planFaultProofFieldOpening({
    anchorSourceKind:
      evidence.prepared.direction === "wrongfulRejection" ? 1n : 0n,
    fieldIndex: evidence.prepared.fieldIndex,
    anchorTxId: evidence.prepared.transactionId,
    nativeTxCompactCbor: evidence.fieldMaterial.nativeTxCompactCbor,
    itemCbors: evidence.fieldMaterial.itemCbors.map((item) =>
      Buffer.from(item, "hex"),
    ),
    owner: workflow.config.signer.paymentKeyHash,
    ...(witnessField
      ? {
          witnessSet: {
            addr_tx_wits_hash: witnessSet.addrTxWitsHash.toString("hex"),
            script_tx_wits_hash: witnessSet.scriptTxWitsHash.toString("hex"),
            redeemer_tx_wits_hash:
              witnessSet.redeemerTxWitsHash.toString("hex"),
          },
          anchorWitnessSetHash:
            compact.transactionWitnessSetHash.toString("hex"),
        }
      : {}),
    label: "fieldPreimageLengthMismatch authenticated field",
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
      carriage: faultProofFieldCarriage({
        planned,
        referenceInputs: completeReferenceInputs,
        certificatePolicyId:
          workflow.config.contracts.fieldPreimageCertificate.policyId,
        label: "fieldPreimageLengthMismatch authenticated field",
      }),
    });
  return Object.freeze({ carriageReferences, claimResolver });
};
