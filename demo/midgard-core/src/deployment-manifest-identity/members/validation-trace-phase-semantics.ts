import { defineDeploymentFamily } from "../members.types.js";

/** The native-scripts, script-integrity and value-and-mint phases' semantic
 * resolvers, which a thread visits between the boundary and the award. */
export const validationTracePhaseSemanticsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeNativeScriptsTerminalSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace native-scripts terminal semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/native_scripts_terminal_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "60"],
        ["steps", "67"],
      ],
    },
    validationTraceDisputeNativeScriptsNativeSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace native-scripts native semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/native_scripts_native_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "61"],
        ["steps", "68"],
      ],
    },
    validationTraceDisputeNativeScriptsEffectfulSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace native-scripts effectful semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/native_scripts_effectful_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "62"],
        ["steps", "69"],
      ],
    },
    validationTraceDisputeScriptIntegrityAuthenticationSemantic: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace script-integrity authentication semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_integrity_authentication_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "63"],
        ["steps", "70"],
      ],
    },
    validationTraceDisputeScriptIntegrityCompactSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace script-integrity compact semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_integrity_compact_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "64"],
        ["steps", "71"],
      ],
    },
    validationTraceDisputeScriptIntegrityWitnessSetSemantic: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace script-integrity witness set semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_integrity_witness_set_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "65"],
        ["steps", "72"],
      ],
    },
    validationTraceDisputeScriptIntegrityFinalizeSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace script-integrity finalize semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/script_integrity_finalize_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "66"],
        ["steps", "73"],
      ],
    },
    validationTraceDisputeValueAndMintBeginSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace value-and-mint begin semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_begin_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "71"],
        ["steps", "78"],
      ],
    },
    validationTraceDisputeValueAndMintReplayBeginSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace value-and-mint replay begin semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_replay_begin_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "72"],
        ["steps", "79"],
      ],
    },
    validationTraceDisputeValueAndMintReplayInputSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace value-and-mint replay input semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_replay_input_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "73"],
        ["steps", "80"],
      ],
    },
    validationTraceDisputeValueAndMintReplayAssetSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace value-and-mint replay asset semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_replay_asset_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "74"],
        ["steps", "81"],
      ],
    },
    validationTraceDisputeValueAndMintReplayFinishSemantic: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace value-and-mint replay finish semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_replay_finish_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "75"],
        ["steps", "82"],
      ],
    },
    validationTraceDisputeValueAndMintOutputDescriptorSemantic: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace value-and-mint output descriptor semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_output_descriptor_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "76"],
        ["steps", "83"],
      ],
    },
    validationTraceDisputeValueAndMintOutputAssetSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace value-and-mint output asset semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_output_asset_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "77"],
        ["steps", "84"],
      ],
    },
    validationTraceDisputeValueAndMintOutputFinishSemantic: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace value-and-mint output finish semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_output_finish_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "78"],
        ["steps", "85"],
      ],
    },
    validationTraceDisputeValueAndMintMintAssetSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace value-and-mint mint asset semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_mint_asset_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "79"],
        ["steps", "86"],
      ],
    },
    validationTraceDisputeValueAndMintMintFinishSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace value-and-mint mint finish semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_mint_finish_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "80"],
        ["steps", "87"],
      ],
    },
    validationTraceDisputeValueAndMintFinalizeSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace value-and-mint finalize semantic",
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_finalize_semantic_v1.main.spend",
      familyPaths: [
        ["semanticResolvers", "81"],
        ["steps", "88"],
      ],
    },
  },
);
