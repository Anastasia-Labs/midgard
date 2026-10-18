import { defineDeploymentFamily } from "../members.types.js";

export const mintItemNonCanonicalMembers = defineDeploymentFamily(
  "mintItemNonCanonical",
  {
    fraudProofMintItemNonCanonical: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-item-non-canonical step-01",
      manifestPosition: 282,
      publicationPosition: 188,
      referenceRolePosition: 417,
      commands: [],
      validatorTitle: "fraud_proofs/mint_item_non_canonical/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofMintItemNonCanonicalStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-item-non-canonical step-02",
      manifestPosition: 283,
      publicationPosition: 189,
      referenceRolePosition: 419,
      commands: [],
      validatorTitle: "fraud_proofs/mint_item_non_canonical/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofMintItemNonCanonicalStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-item-non-canonical step-03",
      manifestPosition: 284,
      publicationPosition: 190,
      referenceRolePosition: 422,
      commands: [],
      validatorTitle: "fraud_proofs/mint_item_non_canonical/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofMintItemNonCanonicalStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-item-non-canonical step-04",
      manifestPosition: 285,
      publicationPosition: 191,
      referenceRolePosition: 424,
      commands: [],
      validatorTitle: "fraud_proofs/mint_item_non_canonical/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
