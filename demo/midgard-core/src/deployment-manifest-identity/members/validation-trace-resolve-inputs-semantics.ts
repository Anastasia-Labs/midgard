import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceResolveInputsSemanticsMembers =
  defineDeploymentFamily("validationTraceDispute", {
    validationTraceDisputeResolveInputsInitialSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace resolve-inputs Initial semantic",
      manifestPosition: 63,
      publicationPosition: 341,
      referenceRolePosition: 178,
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
      manifestPosition: 64,
      publicationPosition: 342,
      referenceRolePosition: 179,
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
      manifestPosition: 65,
      publicationPosition: 343,
      referenceRolePosition: 180,
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
      manifestPosition: 66,
      publicationPosition: 344,
      referenceRolePosition: 181,
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
      manifestPosition: 67,
      publicationPosition: 345,
      referenceRolePosition: 182,
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
      manifestPosition: 68,
      publicationPosition: 346,
      referenceRolePosition: 183,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/resolve_inputs_non_membership_semantic_v1.main.spend",
      familyPaths: [
        ["steps", "38"],
        ["semanticResolvers", "31"],
      ],
    },
  });
