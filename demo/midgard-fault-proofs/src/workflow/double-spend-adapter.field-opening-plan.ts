import { MIDGARD_FIELD_INDEX } from "@al-ft/midgard-sdk/fraud-proof";

import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import type {
  DoubleSpendArtifact,
  DoubleSpendConstrainedWorkflowAdapterConfig,
} from "./double-spend-adapter.preflight-of.js";
import {
  proofMaterialsAccess,
  type ProofMaterialsHolder,
} from "./proof-materials-view.js";

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
 * The double-spend proof chunks of a workflow, found by content at the
 * proof-materials address, and the workflow's view of that address its
 * field publications are found through.
 */
export const createDoubleSpendOwnMaterials = (
  config: DoubleSpendConstrainedWorkflowAdapterConfig,
) => ({
  proofChunks: async (holder: ProofMaterialsHolder, proofCbor: string) =>
    await resolvePublishedProofChunks({
      lucid: config.lucid,
      address: config.signer.proofMaterialsAddress,
      proofCbor,
      view: config.signer.proofMaterials,
      holder,
    }),
  materials: (holder: ProofMaterialsHolder) =>
    proofMaterialsAccess({
      signer: config.signer,
      lucid: config.lucid,
      holder,
    }),
});
