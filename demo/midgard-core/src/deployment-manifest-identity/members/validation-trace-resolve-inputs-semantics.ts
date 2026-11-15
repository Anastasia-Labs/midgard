import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceResolveInputsSemanticsMembers =
  defineDeploymentFamily("validationTraceDispute", {
    validationTraceDisputeResolveInputsInitialSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace resolve-inputs Initial semantic",
        token: "V1VtRiInitial",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/resolve_inputs_initial_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "26"],
        ["steps", "33"],
      ],
    },
    validationTraceDisputeResolveInputsFinishSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace resolve-inputs Finish semantic",
        token: "V1VtRiFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/resolve_inputs_finish_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "27"],
        ["steps", "34"],
      ],
    },
    validationTraceDisputeResolveInputsMembershipBeginSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace resolve-inputs MembershipBegin semantic",
        token: "V1VtRiMembershipBegin",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/resolve_inputs_membership_begin_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "28"],
        ["steps", "35"],
      ],
    },
    validationTraceDisputeResolveInputsMembershipStepSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace resolve-inputs MembershipStep semantic",
        token: "V1VtRiMembershipStep",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/resolve_inputs_membership_step_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "29"],
        ["steps", "36"],
      ],
    },
    validationTraceDisputeResolveInputsMembershipFinalizeSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace resolve-inputs MembershipFinalize semantic",
        token: "V1VtRiMembershipFinalize",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/resolve_inputs_membership_finalize_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "30"],
        ["steps", "37"],
      ],
    },
    validationTraceDisputeResolveInputsNonMembershipSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace resolve-inputs NonMembership semantic",
        token: "V1VtRiNonMembership",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/resolve_inputs_non_membership_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "31"],
        ["steps", "38"],
      ],
    },
  });
