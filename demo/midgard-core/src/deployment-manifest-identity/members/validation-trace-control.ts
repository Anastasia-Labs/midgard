import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceControlMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDispute: {
      purpose: "spend",
      referenceRole: "V1 validation-trace dispute",
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/dispute_v1.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"], ["opener"]],
    },
    validationTraceDisputeSource: {
      purpose: "spend",
      referenceRole: "V1 validation-trace source",
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/source_v1.main.spend",
      familyPaths: [["steps", "1"], ["source"]],
    },
    validationTraceDisputeGame: {
      purpose: "spend",
      referenceRole: "V1 validation-trace game",
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/game_v1.main.spend",
      familyPaths: [["steps", "2"], ["game"]],
    },
    validationTraceDisputeBoundary: {
      purpose: "spend",
      referenceRole: "V1 validation-trace boundary",
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/boundary_v1.main.spend",
      familyPaths: [["steps", "3"], ["boundary"]],
    },
    validationTraceDisputeTimeout: {
      purpose: "spend",
      referenceRole: "V1 validation-trace timeout",
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/timeout_v1.main.spend",
      familyPaths: [["steps", "4"], ["timeout"]],
    },
    validationTraceDisputeAward: {
      purpose: "spend",
      referenceRole: "V1 validation-trace award",
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/award_v1.main.spend",
      familyPaths: [["steps", "5"], ["award"]],
    },
    validationTraceDisputeAwardMint: {
      purpose: "mint",
      referenceRole: "V1 validation-trace award minting",
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/award_v1.main.mint",
      familyPaths: [["award"]],
    },
    validationTraceDisputeCase: {
      purpose: "spend",
      referenceRole: "V1 validation-trace case",
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/case_v1.main.spend",
      familyPaths: [["case"]],
    },
    validationTraceDisputeCaseMint: {
      purpose: "mint",
      referenceRole: "V1 validation-trace case minting",
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/case_v1.main.mint",
      familyPaths: [["case"]],
    },
    validationTraceDisputeCaseConfigMint: {
      purpose: "mint",
      referenceRole: "V1 validation-trace case configuration minting",
      commands: ["protocol-init"],
      validatorTitle: "fraud_proofs/validation_trace/case_config_v1.main.mint",
      familyPaths: [["caseConfig"]],
    },
    validationTraceDisputeThreadExpiryWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace thread expiry",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/thread_expiry_v1.main.withdraw",
      familyPaths: [["threadExpiry"]],
    },
    validationTraceDisputeBond: {
      purpose: "spend",
      referenceRole: "V1 validation-trace dispute bond",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/dispute_bond_v1.main.spend",
      familyPaths: [["disputeBond"]],
    },
  },
);
