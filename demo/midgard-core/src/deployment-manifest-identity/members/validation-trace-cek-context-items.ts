import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCekContextItemsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekContextItemEntry: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item entry",
      manifestPosition: 458,
      publicationPosition: 491,
      referenceRolePosition: 332,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_cek_envelope.main.spend",
      familyPaths: [
        ["steps", "157"],
        ["cekContextItemStages", "entry"],
      ],
    },
    validationTraceDisputeCekContextItemSettlement: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item settlement",
      manifestPosition: 459,
      publicationPosition: 492,
      referenceRolePosition: 333,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_cek_settlement.main.spend",
      familyPaths: [
        ["steps", "158"],
        ["cekContextItemStages", "settlement"],
      ],
    },
  },
);
