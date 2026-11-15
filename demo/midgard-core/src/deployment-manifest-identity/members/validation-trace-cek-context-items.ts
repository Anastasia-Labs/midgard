import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCekContextItemsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekContextItemEntry: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK context item entry",
        token: "V1VtCekCtxIteEnt",
      },
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
      reference: {
        role: "V1 validation-trace CEK context item settlement",
        token: "V1VtCekCtxIteSet",
      },
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
