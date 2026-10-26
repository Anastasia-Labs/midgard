import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCekContextItemsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekContextItemEntry: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item entry",
      manifestPosition: 544,
      publicationPosition: 578,
      referenceRolePosition: 338,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_cek_envelope.main.spend",
      familyPaths: [
        ["cekContextItemStages", "entry"],
        ["steps", "149"],
      ],
    },
    validationTraceDisputeCekContextItemSettlement: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item settlement",
      manifestPosition: 545,
      publicationPosition: 579,
      referenceRolePosition: 339,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_cek_settlement.main.spend",
      familyPaths: [
        ["cekContextItemStages", "settlement"],
        ["steps", "150"],
      ],
    },
  },
);
