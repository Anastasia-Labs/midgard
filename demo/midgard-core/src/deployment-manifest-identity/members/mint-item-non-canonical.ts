import { defineDeploymentFamily } from "../members.types.js";

export const mintItemNonCanonicalMembers = defineDeploymentFamily(
  "mintItemNonCanonical",
  {
    fraudProofMintItemNonCanonical: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-item-non-canonical step-01",
      manifestPosition: 276,
      publicationPosition: 182,
      referenceRolePosition: 408,
      commands: [],
      validatorTitle: "fraud_proofs/mint_item_non_canonical/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofMintItemNonCanonicalStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-item-non-canonical step-02",
      manifestPosition: 277,
      publicationPosition: 183,
      referenceRolePosition: 410,
      commands: [],
      validatorTitle: "fraud_proofs/mint_item_non_canonical/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofMintItemNonCanonicalStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-item-non-canonical step-03",
      manifestPosition: 278,
      publicationPosition: 184,
      referenceRolePosition: 413,
      commands: [],
      validatorTitle: "fraud_proofs/mint_item_non_canonical/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofMintItemNonCanonicalStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-item-non-canonical step-04",
      manifestPosition: 279,
      publicationPosition: 185,
      referenceRolePosition: 415,
      commands: [],
      validatorTitle: "fraud_proofs/mint_item_non_canonical/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
