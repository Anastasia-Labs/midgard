import { defineDeploymentFamily } from "../members.types.js";

/** The CEK phase's one-step resolver and its semantic resolvers, which a
 * thread visits between the boundary and the CEK stages that award it. */
export const validationTraceCekResolversMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekPrepare: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK prepare",
        token: "V1VtCekPrepare",
      },
      commands: [],
      validatorTitle: "fraud_proofs/validation_trace/cek_v1.main.spend",
      familyPaths: [
        ["prepareResolvers", "11"],
        ["resolvers", "11"],
        ["steps", "166"],
      ],
    },
    validationTraceDisputeCekFinishSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK finish semantic",
        token: "V1VtCekFinishSem",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_finish_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "67"],
        ["steps", "74"],
      ],
    },
    validationTraceDisputeCekExecutionSelectionSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK execution selection semantic",
        token: "V1VtCekSelectionSem",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "68"],
        ["steps", "75"],
      ],
    },
    validationTraceDisputeCekContextStepSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK context step semantic",
        token: "V1VtCekContextStepSem",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_step_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "69"],
        ["steps", "76"],
      ],
    },
    validationTraceDisputeCekCoreStepSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK core step semantic",
        token: "V1VtCekCoreStepSem",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_core_step_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "70"],
        ["steps", "77"],
      ],
    },
  },
);
