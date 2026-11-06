import { defineDeploymentFamily } from "../members.types.js";

/** The native-scripts, script-integrity and value-and-mint phases' semantic
 * resolvers, which a thread visits between the boundary and the award. */
export const validationTracePhaseSemanticsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeNativeScriptsTerminalSemantic: {
      purpose: "spend",
      referenceRole: "V1 validation-trace native-scripts terminal semantic",
      manifestPosition: 721,
      publicationPosition: 716,
      referenceRolePosition: 716,
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
      manifestPosition: 722,
      publicationPosition: 717,
      referenceRolePosition: 717,
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
      manifestPosition: 723,
      publicationPosition: 718,
      referenceRolePosition: 718,
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
      manifestPosition: 725,
      publicationPosition: 720,
      referenceRolePosition: 720,
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
      manifestPosition: 726,
      publicationPosition: 721,
      referenceRolePosition: 721,
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
      manifestPosition: 727,
      publicationPosition: 722,
      referenceRolePosition: 722,
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
      manifestPosition: 728,
      publicationPosition: 723,
      referenceRolePosition: 723,
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
      manifestPosition: 730,
      publicationPosition: 725,
      referenceRolePosition: 725,
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
      manifestPosition: 731,
      publicationPosition: 726,
      referenceRolePosition: 726,
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
      manifestPosition: 732,
      publicationPosition: 727,
      referenceRolePosition: 727,
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
      manifestPosition: 733,
      publicationPosition: 728,
      referenceRolePosition: 728,
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
      manifestPosition: 734,
      publicationPosition: 729,
      referenceRolePosition: 729,
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
      manifestPosition: 735,
      publicationPosition: 730,
      referenceRolePosition: 730,
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
      manifestPosition: 736,
      publicationPosition: 731,
      referenceRolePosition: 731,
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
      manifestPosition: 737,
      publicationPosition: 732,
      referenceRolePosition: 732,
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
      manifestPosition: 738,
      publicationPosition: 733,
      referenceRolePosition: 733,
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
      manifestPosition: 739,
      publicationPosition: 734,
      referenceRolePosition: 734,
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
      manifestPosition: 740,
      publicationPosition: 735,
      referenceRolePosition: 735,
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
