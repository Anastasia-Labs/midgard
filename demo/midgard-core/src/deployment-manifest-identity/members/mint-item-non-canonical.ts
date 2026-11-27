import { defineDeploymentFamily } from "../members.types.js";

export const mintItemNonCanonicalMembers = defineDeploymentFamily(
  "mintItemNonCanonical",
  {
    fraudProofMintItemNonCanonical: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-item-non-canonical step-01",
        token: "V1FpMintItemNonCanonicalS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/mint_item_non_canonical/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofMintItemNonCanonicalStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-item-non-canonical step-02",
        token: "V1FpMintItemNonCanonicalS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/mint_item_non_canonical/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofMintItemNonCanonicalStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-item-non-canonical step-03",
        token: "V1FpMintItemNonCanonicalS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/mint_item_non_canonical/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofMintItemNonCanonicalStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-item-non-canonical step-04",
        token: "V1FpMintItemNonCanonicalS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/mint_item_non_canonical/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
