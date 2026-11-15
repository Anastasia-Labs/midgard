import { defineDeploymentFamily } from "../members.types.js";

export const missingSignatureMembers = defineDeploymentFamily(
  "missingSignature",
  {
    fraudProofMissingSignature: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-signature step-01",
      commands: [],
      validatorTitle: "fraud_proofs/missing_signature/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofMissingSignatureStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-signature step-02",
      commands: [],
      validatorTitle: "fraud_proofs/missing_signature/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofMissingSignatureStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-signature step-03",
      commands: [],
      validatorTitle: "fraud_proofs/missing_signature/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofMissingSignatureStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-signature step-04",
      commands: [],
      validatorTitle: "fraud_proofs/missing_signature/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofMissingSignatureForcedStep: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-signature forced step",
      commands: [],
      validatorTitle: "fraud_proofs/missing_signature/forced_step.main.spend",
      familyPaths: [["forcedStep"]],
    },
    fraudProofMissingSignatureForcedSigner: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-signature forced signer",
      commands: [],
      validatorTitle: "fraud_proofs/missing_signature/forced_signer.main.spend",
      familyPaths: [["forcedSigner"]],
    },
    fraudProofMissingSignatureForcedWitness: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-signature forced witness",
      commands: [],
      validatorTitle:
        "fraud_proofs/missing_signature/forced_witness.main.spend",
      familyPaths: [["forcedWitness"]],
    },
  },
);
