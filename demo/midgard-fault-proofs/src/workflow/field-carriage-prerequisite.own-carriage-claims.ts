import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import type { OwnOutputJournal } from "./own-outputs.js";
import type { ProofMaterialsAccess } from "./proof-materials-view.js";

/**
 * The field carriage a workflow step reads, found by content: the
 * field-carriage resolvers find each publication at the proof-materials
 * address through the workflow's proof-materials access, whoever published
 * it, and each certificate by its token and datum. The claims start empty,
 * since nothing is recorded across a restart.
 *
 * The view only reads: a workflow publishes through its own publish and
 * certify actions, so `claim` refuses, and a step that would publish inside
 * a workflow capture fails before it reaches the network.
 */
export const workflowFieldCarriageOwnOutputs = ({
  category,
  materials,
}: {
  readonly category: FraudProofCatalogueCategoryName;
  readonly materials: ProofMaterialsAccess;
}): OwnOutputJournal =>
  Object.freeze({
    claims: () => Object.freeze([]),
    claim: () => {
      throw new Error(
        `${category} field carriage is published only by its workflow's publish and certify actions`,
      );
    },
    materials,
  });
