import { defineDeploymentFamily } from "../members.types.js";

export const mintAuthorizationMembers = defineDeploymentFamily(
  "mintAuthorization",
  {
    fraudProofMintAuthorization: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-authorization step-01",
        token: "V1FpMintAuthorizationS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/mint_authorization/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofMintAuthorizationStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-authorization step-02",
        token: "V1FpMintAuthorizationS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/mint_authorization/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofMintAuthorizationStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-authorization step-03",
        token: "V1FpMintAuthorizationS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/mint_authorization/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofMintAuthorizationStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-authorization step-04",
        token: "V1FpMintAuthorizationS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/mint_authorization/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofMintAuthorizationStep05: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-authorization step-05",
        token: "V1FpMintAuthorizationS05",
      },
      commands: [],
      validatorTitle: "fraud_proofs/mint_authorization/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofMintAuthorizationStep06: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-authorization step-06",
        token: "V1FpMintAuthorizationS06",
      },
      commands: [],
      validatorTitle: "fraud_proofs/mint_authorization/evaluate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofMintAuthorizationStep07: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-authorization step-07",
        token: "V1FpMintAuthorizationS07",
      },
      commands: [],
      validatorTitle: "fraud_proofs/mint_authorization/witness_scan.main.spend",
      familyPaths: [["steps", "6"]],
    },
  },
);
