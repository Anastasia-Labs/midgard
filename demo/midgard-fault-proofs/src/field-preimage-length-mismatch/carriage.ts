import {
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";
import { isMidgardWitnessSetField } from "@al-ft/midgard-sdk";

import {
  certifyFaultProofFieldCarriage,
  faultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import type {
  FieldPreimageLengthJournalPort,
  ManifestBoundFieldPreimageLengthWorkflow,
} from "./authenticated-workflow.js";
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
  journal,
  allowPublication = true,
}: {
  readonly workflow: Pick<ManifestBoundFieldPreimageLengthWorkflow, "config">;
  readonly evidence: AuthenticatedFieldPreimageLengthEvidence;
  readonly journal?: FieldPreimageLengthJournalPort;
  /** Shared recovery captures proof transactions only after its prerequisite has completed. */
  readonly allowPublication?: boolean;
}) => {
  const planned = planFieldPreimageLengthCarriage({ workflow, evidence });
  let publications = await resolveFaultProofFieldCarriagePublications({
    lucid: workflow.config.lucid,
    publisherAddress: workflow.config.signer.address,
    planned,
  });
  if (publications === undefined) {
    if (!allowPublication)
      throw new WorkflowActionChangedError(
        "fieldPreimageLengthMismatch publication changed before capture",
      );
    if (journal?.auxiliaryBoundary === undefined) {
      throw new Error(
        "fieldPreimageLengthMismatch non-inline carriage requires a durable production journal",
      );
    }
    publications = await publishFaultProofFieldCarriage({
      lucid: workflow.config.lucid,
      signer: workflow.config.signer,
      planned,
      publisherAddress: workflow.config.signer.address,
      label: "fieldPreimageLengthMismatch authenticated field",
      preSubmitBoundary: journal.auxiliaryBoundary("publication"),
      beforePublication: async () => await journal.begin?.("publication"),
      publicationConfirmed: async (txHash) =>
        await journal?.auxiliaryConfirmed?.("publication", [txHash]),
    });
  }
  await journal?.auxiliaryConfirmed?.(
    "publication",
    publications.map(({ txHash }) => txHash),
  );
  let certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: workflow.config.lucid,
    network: workflow.config.binding.network,
    planned,
    certificatePolicyId:
      workflow.config.contracts.fieldPreimageCertificate.policyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    if (!allowPublication)
      throw new WorkflowActionChangedError(
        "fieldPreimageLengthMismatch certificate changed before capture",
      );
    if (journal?.auxiliaryBoundary === undefined) {
      throw new Error(
        "fieldPreimageLengthMismatch certification requires a durable production journal",
      );
    }
    await journal.begin?.("certificate");
    const certified = await certifyFaultProofFieldCarriage({
      lucid: workflow.config.lucid,
      network: workflow.config.binding.network,
      signer: workflow.config.signer,
      planned,
      certificatePolicyId:
        workflow.config.contracts.fieldPreimageCertificate.policyId,
      certificateMintingScript:
        workflow.config.contracts.fieldPreimageCertificate.mintingScript,
      certificateReferenceScriptUtxo:
        workflow.config.referenceScripts.fieldPreimageCertificateMint,
      chunkUtxos: publications,
      compactCbor: evidence.fieldMaterial.nativeTxCompactCbor,
      witnessSetCompactCbor: evidence.fieldMaterial.witnessSetCompactCbor,
      preSubmitBoundary: journal.auxiliaryBoundary("certificate"),
    });
    certificate = certified.certificateUtxo;
  }
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error(
      "fieldPreimageLengthMismatch field certificate disappeared",
    );
  }
  if (certificate !== undefined) {
    await journal?.auxiliaryConfirmed?.("certificate", [certificate.txHash]);
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
