import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceControlMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDispute: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace dispute",
        token: "V1ValidationTraceDispute",
      },
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/dispute_v1.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"], ["opener"]],
    },
    validationTraceDisputeSource: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace source",
        token: "V1ValidationTraceSource",
      },
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/source_v1.main.spend",
      familyPaths: [["steps", "1"], ["source"]],
    },
    validationTraceDisputeGame: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace game",
        token: "V1ValidationTraceGame",
      },
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/game_v1.main.spend",
      familyPaths: [["steps", "2"], ["game"]],
    },
    validationTraceDisputeBoundary: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace boundary",
        token: "V1ValidationTraceBoundary",
      },
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/boundary_v1.main.spend",
      familyPaths: [["steps", "3"], ["boundary"]],
    },
    validationTraceDisputeTimeout: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace timeout",
        token: "V1ValidationTraceTimeout",
      },
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/timeout_v1.main.spend",
      familyPaths: [["steps", "4"], ["timeout"]],
    },
    validationTraceDisputeAward: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace award",
        token: "V1ValidationTraceAward",
      },
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/award_v1.main.spend",
      familyPaths: [["steps", "5"], ["award"]],
    },
    validationTraceDisputeAwardMint: {
      purpose: "mint",
      reference: {
        role: "V1 validation-trace award minting",
        token: "V1ValidationTraceAwardMint",
      },
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/award_v1.main.mint",
      familyPaths: [["award"]],
    },
    validationTraceDisputeCase: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace case",
        token: "V1ValidationTraceCase",
      },
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/case_v1.main.spend",
      familyPaths: [["case"]],
    },
    validationTraceDisputeCaseMint: {
      purpose: "mint",
      reference: {
        role: "V1 validation-trace case minting",
        token: "V1ValidationTraceCaseMint",
      },
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/case_v1.main.mint",
      familyPaths: [["case"]],
    },
    validationTraceDisputeCaseConfigMint: {
      purpose: "mint",
      reference: {
        role: "V1 validation-trace case configuration minting",
        token: "V1VtdCaseConfigMint",
      },
      commands: ["protocol-init"],
      validatorTitle: "fraud_proofs/validation_trace/case_config_v1.main.mint",
      familyPaths: [["caseConfig"]],
    },
    validationTraceDisputeThreadExpiryWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 validation-trace thread expiry",
        token: "V1VtdThreadExpiry",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/thread_expiry_v1.main.withdraw",
      familyPaths: [["threadExpiry"]],
    },
    validationTraceDisputeBond: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace dispute bond",
        token: "V1VtdDisputeBond",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/dispute_bond_v1.main.spend",
      familyPaths: [["disputeBond"]],
    },
  },
);
