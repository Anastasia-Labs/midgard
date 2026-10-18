import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCekContextMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekContextSettle: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context settle",
      manifestPosition: 431,
      publicationPosition: 465,
      referenceRolePosition: 304,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_settle.main.spend",
      familyPaths: [
        ["steps", "115"],
        ["cekContextStages", "settle"],
      ],
    },
    validationTraceDisputeCekContextControl: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context control",
      manifestPosition: 432,
      publicationPosition: 466,
      referenceRolePosition: 305,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_control.main.spend",
      familyPaths: [
        ["steps", "148"],
        ["cekContextStages", "control"],
      ],
    },
    validationTraceDisputeCekContextReference: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context reference",
      manifestPosition: 433,
      publicationPosition: 467,
      referenceRolePosition: 306,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_reference.main.spend",
      familyPaths: [
        ["steps", "116"],
        ["cekContextStages", "reference"],
      ],
    },
    validationTraceDisputeCekContextSpend: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context spend",
      manifestPosition: 434,
      publicationPosition: 468,
      referenceRolePosition: 307,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_spend.main.spend",
      familyPaths: [
        ["steps", "117"],
        ["cekContextStages", "spend"],
      ],
    },
    validationTraceDisputeCekContextOutput: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context output",
      manifestPosition: 435,
      publicationPosition: 469,
      referenceRolePosition: 308,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_output.main.spend",
      familyPaths: [
        ["steps", "118"],
        ["cekContextStages", "output"],
      ],
    },
    validationTraceDisputeCekContextSigner: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context signer",
      manifestPosition: 436,
      publicationPosition: 470,
      referenceRolePosition: 309,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_signer.main.spend",
      familyPaths: [
        ["steps", "119"],
        ["cekContextStages", "signer"],
      ],
    },
    validationTraceDisputeCekContextObserverAuthenticate: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context observer authenticate",
      manifestPosition: 437,
      publicationPosition: 471,
      referenceRolePosition: 310,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_observer_authenticate.main.spend",
      familyPaths: [
        ["steps", "121"],
        ["cekContextStages", "observerAuthenticate"],
      ],
    },
    validationTraceDisputeCekContextObserverFold: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context observer fold",
      manifestPosition: 438,
      publicationPosition: 472,
      referenceRolePosition: 311,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_observer_fold.main.spend",
      familyPaths: [
        ["steps", "120"],
        ["cekContextStages", "observerFold"],
      ],
    },
    validationTraceDisputeCekContextMintInit: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context mint init",
      manifestPosition: 439,
      publicationPosition: 473,
      referenceRolePosition: 312,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_mint_init.main.spend",
      familyPaths: [
        ["steps", "122"],
        ["cekContextStages", "mintInit"],
      ],
    },
    validationTraceDisputeCekContextMintItem: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context mint item",
      manifestPosition: 440,
      publicationPosition: 474,
      referenceRolePosition: 313,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_mint_item.main.spend",
      familyPaths: [
        ["steps", "123"],
        ["cekContextStages", "mintItem"],
      ],
    },
    validationTraceDisputeCekContextAssemble: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context assemble",
      manifestPosition: 441,
      publicationPosition: 475,
      referenceRolePosition: 314,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_assemble.main.spend",
      familyPaths: [
        ["steps", "124"],
        ["cekContextStages", "assemble"],
      ],
    },
    validationTraceDisputeCekContextTxInfo: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context tx info",
      manifestPosition: 442,
      publicationPosition: 476,
      referenceRolePosition: 315,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_tx_info.main.spend",
      familyPaths: [
        ["steps", "125"],
        ["cekContextStages", "txInfo"],
      ],
    },
    validationTraceDisputeCekContextSeed: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context seed",
      manifestPosition: 443,
      publicationPosition: 477,
      referenceRolePosition: 316,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_seed.main.spend",
      familyPaths: [
        ["steps", "126"],
        ["cekContextStages", "seed"],
      ],
    },
    validationTraceDisputeCekContextFinalizeAuthenticate: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context finalize authenticate",
      manifestPosition: 444,
      publicationPosition: 478,
      referenceRolePosition: 317,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_finalize_authenticate.main.spend",
      familyPaths: [
        ["steps", "132"],
        ["cekContextStages", "finalizeAuthenticate"],
      ],
    },
    validationTraceDisputeCekContextFinalizeSpend: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context finalize spend",
      manifestPosition: 445,
      publicationPosition: 479,
      referenceRolePosition: 318,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_finalize_spend.main.spend",
      familyPaths: [
        ["steps", "127"],
        ["cekContextStages", "finalizeSpend"],
      ],
    },
    validationTraceDisputeCekContextFinalizeMint: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context finalize mint",
      manifestPosition: 446,
      publicationPosition: 480,
      referenceRolePosition: 319,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_finalize_mint.main.spend",
      familyPaths: [
        ["steps", "128"],
        ["cekContextStages", "finalizeMint"],
      ],
    },
    validationTraceDisputeCekContextFinalizeWithdraw: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context finalize withdraw",
      manifestPosition: 447,
      publicationPosition: 481,
      referenceRolePosition: 320,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_finalize_withdraw.main.spend",
      familyPaths: [
        ["steps", "129"],
        ["cekContextStages", "finalizeWithdraw"],
      ],
    },
    validationTraceDisputeCekContextFinalizeObserve: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context finalize observe",
      manifestPosition: 448,
      publicationPosition: 482,
      referenceRolePosition: 321,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_finalize_observe.main.spend",
      familyPaths: [
        ["steps", "130"],
        ["cekContextStages", "finalizeObserve"],
      ],
    },
    validationTraceDisputeCekContextFinalizeMidgard: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context finalize midgard",
      manifestPosition: 449,
      publicationPosition: 483,
      referenceRolePosition: 322,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_finalize_midgard.main.spend",
      familyPaths: [
        ["steps", "131"],
        ["cekContextStages", "finalizeMidgard"],
      ],
    },
    validationTraceDisputeCekContextRedeemerBegin: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context redeemer begin",
      manifestPosition: 450,
      publicationPosition: 484,
      referenceRolePosition: 323,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_redeemer_begin.main.spend",
      familyPaths: [
        ["steps", "133"],
        ["cekContextStages", "redeemerBegin"],
      ],
    },
    validationTraceDisputeCekContextRedeemerSelectAuthenticate: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace CEK context redeemer select authenticate",
      manifestPosition: 451,
      publicationPosition: 485,
      referenceRolePosition: 324,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_redeemer_select_authenticate.main.spend",
      familyPaths: [
        ["steps", "137"],
        ["cekContextStages", "redeemerSelectAuthenticate"],
      ],
    },
    validationTraceDisputeCekContextRedeemerSelectInitialize: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace CEK context redeemer select initialize",
      manifestPosition: 452,
      publicationPosition: 486,
      referenceRolePosition: 325,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_redeemer_select_initialize.main.spend",
      familyPaths: [
        ["steps", "136"],
        ["cekContextStages", "redeemerSelectInitialize"],
      ],
    },
    validationTraceDisputeCekContextRedeemerSelectHash: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context redeemer select hash",
      manifestPosition: 453,
      publicationPosition: 487,
      referenceRolePosition: 326,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_redeemer_select_hash.main.spend",
      familyPaths: [
        ["steps", "135"],
        ["cekContextStages", "redeemerSelectHash"],
      ],
    },
    validationTraceDisputeCekContextRedeemerSelectFinish: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context redeemer select finish",
      manifestPosition: 454,
      publicationPosition: 488,
      referenceRolePosition: 327,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_redeemer_select_finish.main.spend",
      familyPaths: [
        ["steps", "134"],
        ["cekContextStages", "redeemerSelectFinish"],
      ],
    },
    validationTraceDisputeCekContextItemBind: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item bind",
      manifestPosition: 455,
      publicationPosition: 489,
      referenceRolePosition: 328,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_bind.main.spend",
      familyPaths: [
        ["steps", "147"],
        ["cekContextStages", "itemBind"],
      ],
    },
    validationTraceDisputeCekContextItemReturn: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item return",
      manifestPosition: 456,
      publicationPosition: 490,
      referenceRolePosition: 329,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_return.main.spend",
      familyPaths: [
        ["steps", "146"],
        ["cekContextStages", "itemReturn"],
      ],
    },
    validationTraceDisputeCekContextItemSelectionHash: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item selection hash",
      manifestPosition: 457,
      publicationPosition: 491,
      referenceRolePosition: 330,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_hash.main.spend",
      familyPaths: [
        ["steps", "143"],
        ["cekContextStages", "itemSelectionHash"],
      ],
    },
    validationTraceDisputeCekContextItemDataHash: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item data hash",
      manifestPosition: 458,
      publicationPosition: 492,
      referenceRolePosition: 331,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_hash.main.spend",
      familyPaths: [
        ["steps", "144"],
        ["cekContextStages", "itemDataHash"],
      ],
    },
    validationTraceDisputeCekContextItemFinalize: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item finalize",
      manifestPosition: 459,
      publicationPosition: 493,
      referenceRolePosition: 332,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_finalize.main.spend",
      familyPaths: [
        ["steps", "145"],
        ["cekContextStages", "itemFinalize"],
      ],
    },
    validationTraceDisputeCekContextItemSelectionContinue: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item selection continue",
      manifestPosition: 460,
      publicationPosition: 494,
      referenceRolePosition: 333,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_selection_continue.main.spend",
      familyPaths: [
        ["steps", "138"],
        ["cekContextStages", "itemSelectionContinue"],
      ],
    },
    validationTraceDisputeCekContextItemSelectionFinish: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item selection finish",
      manifestPosition: 461,
      publicationPosition: 495,
      referenceRolePosition: 334,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_selection_finish.main.spend",
      familyPaths: [
        ["steps", "139"],
        ["cekContextStages", "itemSelectionFinish"],
      ],
    },
    validationTraceDisputeCekContextItemDataContinue: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item data continue",
      manifestPosition: 462,
      publicationPosition: 496,
      referenceRolePosition: 335,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_data_continue.main.spend",
      familyPaths: [
        ["steps", "140"],
        ["cekContextStages", "itemDataContinue"],
      ],
    },
    validationTraceDisputeCekContextItemDataFinishDescriptor: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace CEK context item data finish descriptor",
      manifestPosition: 463,
      publicationPosition: 497,
      referenceRolePosition: 336,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_data_finish_descriptor.main.spend",
      familyPaths: [
        ["steps", "141"],
        ["cekContextStages", "itemDataFinishDescriptor"],
      ],
    },
    validationTraceDisputeCekContextItemDataFinishValue: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item data finish value",
      manifestPosition: 464,
      publicationPosition: 498,
      referenceRolePosition: 337,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_data_finish_value.main.spend",
      familyPaths: [
        ["steps", "142"],
        ["cekContextStages", "itemDataFinishValue"],
      ],
    },
  },
);
