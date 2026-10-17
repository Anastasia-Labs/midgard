import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCekContextItemsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekContextItemEntry: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item entry",
      manifestPosition: 464,
      publicationPosition: 497,
      referenceRolePosition: 338,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_cek_envelope.main.spend",
      familyPaths: [
        ["steps", "149"],
        ["cekContextItemStages", "entry"],
      ],
    },
    validationTraceDisputeCekContextItemSettlement: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item settlement",
      manifestPosition: 465,
      publicationPosition: 498,
      referenceRolePosition: 339,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_cek_settlement.main.spend",
      familyPaths: [
        ["steps", "150"],
        ["cekContextItemStages", "settlement"],
      ],
    },
  },
);
