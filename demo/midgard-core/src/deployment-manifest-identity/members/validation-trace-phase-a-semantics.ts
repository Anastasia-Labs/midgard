import { defineDeploymentFamily } from "../members.types.js";

export const validationTracePhaseASemanticsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputePhaseANativeScriptsAdvanceSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace phase-A native Advance semantic",
        token: "V1VtPaNsAdvance",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_advance_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "10"],
        ["steps", "17"],
      ],
    },
    validationTraceDisputePhaseANativeScriptsItemSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace phase-A native Item semantic",
        token: "V1VtPaNsItem",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_item_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "11"],
        ["steps", "18"],
      ],
    },
    validationTraceDisputePhaseANativeScriptsTokenHeadSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace phase-A native TokenHead semantic",
        token: "V1VtPaNsHead",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_token_head_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "12"],
        ["steps", "19"],
      ],
    },
    validationTraceDisputePhaseANativeScriptsAllOrAnyContainerFramePayloadSemantic:
      {
        purpose: "spend",
        reference: {
          role: "V1 validation-trace phase-A native AllOrAnyContainerFramePayload semantic",
          token: "V1VtPaNsAllAnyFrame",
        },
        commands: [],
        validatorTitle:
          "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_container_frame_payload_semantic_v1.main.spend",
        familyPaths: [
          ["semanticResolvers", "13"],
          ["steps", "20"],
        ],
      },
    validationTraceDisputePhaseANativeScriptsAllOrAnyEmptyContainerPayloadSemantic:
      {
        purpose: "spend",
        reference: {
          role: "V1 validation-trace phase-A native AllOrAnyEmptyContainerPayload semantic",
          token: "V1VtPaNsAllAnyEmpty",
        },
        commands: [],
        validatorTitle:
          "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_empty_container_payload_semantic_v1.main.spend",
        familyPaths: [
          ["semanticResolvers", "14"],
          ["steps", "21"],
        ],
      },
    validationTraceDisputePhaseANativeScriptsAtLeastContainerFramePayloadSemantic:
      {
        purpose: "spend",
        reference: {
          role: "V1 validation-trace phase-A native AtLeastContainerFramePayload semantic",
          token: "V1VtPaNsAtLeastFrame",
        },
        commands: [],
        validatorTitle:
          "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_container_frame_payload_semantic_v1.main.spend",
        familyPaths: [
          ["semanticResolvers", "15"],
          ["steps", "22"],
        ],
      },
    validationTraceDisputePhaseANativeScriptsAtLeastEmptyContainerPayloadSemantic:
      {
        purpose: "spend",
        reference: {
          role: "V1 validation-trace phase-A native AtLeastEmptyContainerPayload semantic",
          token: "V1VtPaNsAtLeastEmpty",
        },
        commands: [],
        validatorTitle:
          "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_empty_container_payload_semantic_v1.main.spend",
        familyPaths: [
          ["semanticResolvers", "16"],
          ["steps", "23"],
        ],
      },
    validationTraceDisputePhaseANativeScriptsTimelockPayloadSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace phase-A native TimelockPayload semantic",
        token: "V1VtPaNsTime",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_timelock_payload_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "17"],
        ["steps", "24"],
      ],
    },
    validationTraceDisputePhaseANativeScriptsSignatureMembershipPayloadSemantic:
      {
        purpose: "spend",
        reference: {
          role: "V1 validation-trace phase-A native SignatureMembershipPayload semantic",
          token: "V1VtPaNsSigMember",
        },
        commands: [],
        validatorTitle:
          "fraud_proofs/validation_trace/phase_a_native_scripts_signature_membership_payload_semantic_v1.main.spend",
        familyPaths: [
          ["semanticResolvers", "18"],
          ["steps", "25"],
        ],
      },
    validationTraceDisputePhaseANativeScriptsSignatureEmptyPayloadSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace phase-A native SignatureEmptyPayload semantic",
        token: "V1VtPaNsSigEmpty",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_signature_empty_payload_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "19"],
        ["steps", "26"],
      ],
    },
    validationTraceDisputePhaseANativeScriptsSignatureBelowFirstPayloadSemantic:
      {
        purpose: "spend",
        reference: {
          role: "V1 validation-trace phase-A native SignatureBelowFirstPayload semantic",
          token: "V1VtPaNsSigBelow",
        },
        commands: [],
        validatorTitle:
          "fraud_proofs/validation_trace/phase_a_native_scripts_signature_below_first_payload_semantic_v1.main.spend",
        familyPaths: [
          ["semanticResolvers", "20"],
          ["steps", "27"],
        ],
      },
    validationTraceDisputePhaseANativeScriptsSignatureAboveLastPayloadSemantic:
      {
        purpose: "spend",
        reference: {
          role: "V1 validation-trace phase-A native SignatureAboveLastPayload semantic",
          token: "V1VtPaNsSigAbove",
        },
        commands: [],
        validatorTitle:
          "fraud_proofs/validation_trace/phase_a_native_scripts_signature_above_last_payload_semantic_v1.main.spend",
        familyPaths: [
          ["semanticResolvers", "21"],
          ["steps", "28"],
        ],
      },
    validationTraceDisputePhaseANativeScriptsSignatureBetweenPayloadSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace phase-A native SignatureBetweenPayload semantic",
        token: "V1VtPaNsSigBetween",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_signature_between_payload_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "22"],
        ["steps", "29"],
      ],
    },
    validationTraceDisputePhaseANativeScriptsFrameSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace phase-A native Frame semantic",
        token: "V1VtPaNsFrame",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_frame_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "23"],
        ["steps", "30"],
      ],
    },
    validationTraceDisputePhaseAScriptPreconditionsFinalizeSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace phase-A preconditions Finalize semantic",
        token: "V1VtPaSpFinalize",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_script_preconditions_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "24"],
        ["steps", "31"],
      ],
    },
    validationTraceDisputePhaseAScriptPreconditionsItemSemantic: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace phase-A preconditions Item semantic",
        token: "V1VtPaSpItem",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_script_preconditions_item_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "25"],
        ["steps", "32"],
      ],
    },
  },
);
