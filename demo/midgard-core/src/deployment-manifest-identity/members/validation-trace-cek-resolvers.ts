import { defineDeploymentFamily } from "../members.types.js";

/** The CEK phase's one-step resolver and its semantic resolvers, which a
 * thread visits between the boundary and the CEK stages that award it. */
export const validationTraceCekResolversMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK prepare",
      manifestPosition: 570,
      publicationPosition: 565,
      referenceRolePosition: 565,
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
      referenceRole: "V1 validation-trace CEK finish semantic",
      manifestPosition: 571,
      publicationPosition: 566,
      referenceRolePosition: 566,
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
      referenceRole: "V1 validation-trace CEK execution selection semantic",
      manifestPosition: 572,
      publicationPosition: 567,
      referenceRolePosition: 567,
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
      referenceRole: "V1 validation-trace CEK context step semantic",
      manifestPosition: 573,
      publicationPosition: 568,
      referenceRolePosition: 568,
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
      referenceRole: "V1 validation-trace CEK core step semantic",
      manifestPosition: 574,
      publicationPosition: 569,
      referenceRolePosition: 569,
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
