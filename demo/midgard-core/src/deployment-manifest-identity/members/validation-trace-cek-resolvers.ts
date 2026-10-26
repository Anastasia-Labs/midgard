import { defineDeploymentFamily } from "../members.types.js";

/** The CEK phase's one-step resolver and its semantic resolvers, which a
 * thread visits between the boundary and the CEK stages that award it. */
export const validationTraceCekResolversMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekPrepare: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK prepare",
      manifestPosition: 601,
      publicationPosition: 596,
      referenceRolePosition: 596,
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
      manifestPosition: 602,
      publicationPosition: 597,
      referenceRolePosition: 597,
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
      manifestPosition: 603,
      publicationPosition: 598,
      referenceRolePosition: 598,
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
      manifestPosition: 604,
      publicationPosition: 599,
      referenceRolePosition: 599,
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
      manifestPosition: 605,
      publicationPosition: 600,
      referenceRolePosition: 600,
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
