import { MIDGARD_FIELD_INDEX } from "@al-ft/midgard-sdk/fraud-proof";

import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import { addressFieldCarriageOwnOutputs } from "./bespoke-adapter-field-carriage.js";
import type {
  DoubleSpendArtifact,
  DoubleSpendConstrainedWorkflowAdapterConfig,
} from "./double-spend-adapter.preflight-of.js";
import type { FraudProofWorkflowJournalEntry } from "./journal.js";
import { workflowConfirmedTransactions } from "./own-outputs.js";

export const createDoubleSpendFieldOpeningPlan = (
  config: DoubleSpendConstrainedWorkflowAdapterConfig,
) => {
  const fieldPlan = (
    proofStage: "step_03" | "step_04",
    artifact: DoubleSpendArtifact,
  ) => {
    const tx = proofStage === "step_03" ? artifact.tx1 : artifact.tx2;
    return planFaultProofFieldOpening({
      anchorSourceKind: 0n,
      fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
      anchorTxId: tx.nativeTxId,
      nativeTxCompactCbor: tx.nativeTxCompactCbor,
      itemCbors: tx.spendInputCbors.map((cbor) => Buffer.from(cbor, "hex")),
      owner: config.signer.paymentKeyHash,
      label: `${proofStage} production preflight`,
    });
  };

  return fieldPlan;
};

/**
 * The double-spend proof chunks and field carriage of a workflow, read at
 * the proof-materials and certificate addresses among the outputs of the
 * transactions its journal `entries` confirmed only.
 */
export const createDoubleSpendOwnMaterials = (
  config: DoubleSpendConstrainedWorkflowAdapterConfig,
) => ({
  proofChunks: async (
    proofCbor: string,
    entries: readonly FraudProofWorkflowJournalEntry[],
  ) =>
    await resolvePublishedProofChunks({
      lucid: config.lucid,
      address: config.signer.proofMaterialsAddress,
      proofCbor,
      ownTransactions: workflowConfirmedTransactions(entries),
    }),
  fieldCarriage: async (
    planned: ReturnType<ReturnType<typeof createDoubleSpendFieldOpeningPlan>>,
    entries: readonly FraudProofWorkflowJournalEntry[],
  ) =>
    await addressFieldCarriageOwnOutputs({
      lucid: config.lucid,
      network: config.network,
      publisherAddress: config.signer.proofMaterialsAddress,
      planned,
      certificatePolicyId: config.fieldPreimageCertificate.policyId,
      ownTransactions: workflowConfirmedTransactions(entries),
    }),
});
