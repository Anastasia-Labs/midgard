import type { FraudProofWorkflowIdentity } from "./journal.js";
import type { ProofMaterialsHolder } from "./proof-materials-view.js";

/**
 * The holder of a header workflow's proof-materials needs: the workflow, and
 * its objective, the category on the challenged header.
 */
export const workflowProofMaterialsHolder = ({
  workflowId,
  identity,
}: {
  readonly workflowId: string;
  readonly identity: FraudProofWorkflowIdentity;
}): ProofMaterialsHolder => {
  if (identity.target.kind !== "state_queue_header")
    throw new Error(
      `${identity.category} proof materials belong to a header workflow`,
    );
  return Object.freeze({
    workflowId,
    objective: Object.freeze({
      category: identity.category,
      headerHash: identity.target.headerHash,
    }),
  });
};
