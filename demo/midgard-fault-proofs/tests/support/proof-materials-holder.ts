import type { LucidEvolution } from "@lucid-evolution/lucid";

import {
  type ProofMaterialsAccess,
  proofMaterialsAccess,
  type ProofMaterialsHolder,
  type ProofMaterialsView,
} from "../../src/workflow/proof-materials-view.js";

/** The one workflow a port-level test finds proof materials for. */
export const TEST_PROOF_MATERIALS_HOLDER: ProofMaterialsHolder = Object.freeze({
  workflowId: "test-workflow",
  objective: Object.freeze({ category: "test", headerHash: "00" }),
});

/** What the test workflow finds through: `signer`'s view over `lucid`. */
export const testProofMaterials = (
  signer: Readonly<{
    proofMaterialsAddress: string;
    proofMaterials?: ProofMaterialsView;
  }>,
  lucid: LucidEvolution,
): ProofMaterialsAccess =>
  proofMaterialsAccess({ signer, lucid, holder: TEST_PROOF_MATERIALS_HOLDER });
