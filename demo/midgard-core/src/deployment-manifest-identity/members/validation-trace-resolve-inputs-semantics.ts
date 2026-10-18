import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceResolveInputsSemanticsMembers =
  defineDeploymentFamily("validationTraceDispute", {
    validationTraceDisputeResolveInputsInitialSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace resolve-inputs Initial semantic",
      manifestPosition: 68,
      publicationPosition: 376,
      referenceRolePosition: 183,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/resolve_inputs_initial_semantic_v1.main.spend",
      familyPaths: [
        ["steps", "33"],
        ["semanticResolvers", "26"],
      ],
    },
    validationTraceDisputeResolveInputsFinishSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace resolve-inputs Finish semantic",
      manifestPosition: 69,
      publicationPosition: 377,
      referenceRolePosition: 184,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/resolve_inputs_finish_semantic_v1.main.spend",
      familyPaths: [
        ["steps", "34"],
        ["semanticResolvers", "27"],
      ],
    },
    validationTraceDisputeResolveInputsMembershipBeginSemantic: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace resolve-inputs MembershipBegin semantic",
      manifestPosition: 70,
      publicationPosition: 378,
      referenceRolePosition: 185,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/resolve_inputs_membership_begin_semantic_v1.main.spend",
      familyPaths: [
        ["steps", "35"],
        ["semanticResolvers", "28"],
      ],
    },
    validationTraceDisputeResolveInputsMembershipStepSemantic: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace resolve-inputs MembershipStep semantic",
      manifestPosition: 71,
      publicationPosition: 379,
      referenceRolePosition: 186,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/resolve_inputs_membership_step_semantic_v1.main.spend",
      familyPaths: [
        ["steps", "36"],
        ["semanticResolvers", "29"],
      ],
    },
    validationTraceDisputeResolveInputsMembershipFinalizeSemantic: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace resolve-inputs MembershipFinalize semantic",
      manifestPosition: 72,
      publicationPosition: 380,
      referenceRolePosition: 187,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/resolve_inputs_membership_finalize_semantic_v1.main.spend",
      familyPaths: [
        ["steps", "37"],
        ["semanticResolvers", "30"],
      ],
    },
    validationTraceDisputeResolveInputsNonMembershipSemantic: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace resolve-inputs NonMembership semantic",
      manifestPosition: 73,
      publicationPosition: 381,
      referenceRolePosition: 188,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/resolve_inputs_non_membership_semantic_v1.main.spend",
      familyPaths: [
        ["steps", "38"],
        ["semanticResolvers", "31"],
      ],
    },
  });
