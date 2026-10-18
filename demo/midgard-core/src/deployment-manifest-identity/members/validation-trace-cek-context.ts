import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCekContextMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekContextSettle: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context settle",
      manifestPosition: 431,
      publicationPosition: 464,
      referenceRolePosition: 304,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_settle.main.spend",
      familyPaths: [
        ["cekContextStages", "settle"],
        ["steps", "115"],
      ],
    },
    validationTraceDisputeCekContextControl: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context control",
      manifestPosition: 432,
      publicationPosition: 465,
      referenceRolePosition: 305,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_control.main.spend",
      familyPaths: [
        ["cekContextStages", "control"],
        ["steps", "148"],
      ],
    },
    validationTraceDisputeCekContextReference: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context reference",
      manifestPosition: 433,
      publicationPosition: 466,
      referenceRolePosition: 306,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_reference.main.spend",
      familyPaths: [
        ["cekContextStages", "reference"],
        ["steps", "116"],
      ],
    },
    validationTraceDisputeCekContextSpend: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context spend",
      manifestPosition: 434,
      publicationPosition: 467,
      referenceRolePosition: 307,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_spend.main.spend",
      familyPaths: [
        ["cekContextStages", "spend"],
        ["steps", "117"],
      ],
    },
    validationTraceDisputeCekContextOutput: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context output",
      manifestPosition: 435,
      publicationPosition: 468,
      referenceRolePosition: 308,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_output.main.spend",
      familyPaths: [
        ["cekContextStages", "output"],
        ["steps", "118"],
      ],
    },
    validationTraceDisputeCekContextSigner: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context signer",
      manifestPosition: 436,
      publicationPosition: 469,
      referenceRolePosition: 309,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_signer.main.spend",
      familyPaths: [
        ["cekContextStages", "signer"],
        ["steps", "119"],
      ],
    },
    validationTraceDisputeCekContextObserverAuthenticate: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context observer authenticate",
      manifestPosition: 437,
      publicationPosition: 470,
      referenceRolePosition: 310,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_observer_authenticate.main.spend",
      familyPaths: [
        ["cekContextStages", "observerAuthenticate"],
        ["steps", "121"],
      ],
    },
    validationTraceDisputeCekContextObserverFold: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context observer fold",
      manifestPosition: 438,
      publicationPosition: 471,
      referenceRolePosition: 311,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_observer_fold.main.spend",
      familyPaths: [
        ["cekContextStages", "observerFold"],
        ["steps", "120"],
      ],
    },
    validationTraceDisputeCekContextMintInit: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context mint init",
      manifestPosition: 439,
      publicationPosition: 472,
      referenceRolePosition: 312,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_mint_init.main.spend",
      familyPaths: [
        ["cekContextStages", "mintInit"],
        ["steps", "122"],
      ],
    },
    validationTraceDisputeCekContextMintItem: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context mint item",
      manifestPosition: 440,
      publicationPosition: 473,
      referenceRolePosition: 313,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_mint_item.main.spend",
      familyPaths: [
        ["cekContextStages", "mintItem"],
        ["steps", "123"],
      ],
    },
    validationTraceDisputeCekContextAssemble: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context assemble",
      manifestPosition: 441,
      publicationPosition: 474,
      referenceRolePosition: 314,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_assemble.main.spend",
      familyPaths: [
        ["cekContextStages", "assemble"],
        ["steps", "124"],
      ],
    },
    validationTraceDisputeCekContextTxInfo: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context tx info",
      manifestPosition: 442,
      publicationPosition: 475,
      referenceRolePosition: 315,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_tx_info.main.spend",
      familyPaths: [
        ["cekContextStages", "txInfo"],
        ["steps", "125"],
      ],
    },
    validationTraceDisputeCekContextSeed: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context seed",
      manifestPosition: 443,
      publicationPosition: 476,
      referenceRolePosition: 316,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_seed.main.spend",
      familyPaths: [
        ["cekContextStages", "seed"],
        ["steps", "126"],
      ],
    },
    validationTraceDisputeCekContextFinalizeAuthenticate: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context finalize authenticate",
      manifestPosition: 444,
      publicationPosition: 477,
      referenceRolePosition: 317,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_finalize_authenticate.main.spend",
      familyPaths: [
        ["cekContextStages", "finalizeAuthenticate"],
        ["steps", "132"],
      ],
    },
    validationTraceDisputeCekContextFinalizeSpend: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context finalize spend",
      manifestPosition: 445,
      publicationPosition: 478,
      referenceRolePosition: 318,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_finalize_spend.main.spend",
      familyPaths: [
        ["cekContextStages", "finalizeSpend"],
        ["steps", "127"],
      ],
    },
    validationTraceDisputeCekContextFinalizeMint: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context finalize mint",
      manifestPosition: 446,
      publicationPosition: 479,
      referenceRolePosition: 319,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_finalize_mint.main.spend",
      familyPaths: [
        ["cekContextStages", "finalizeMint"],
        ["steps", "128"],
      ],
    },
    validationTraceDisputeCekContextFinalizeWithdraw: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context finalize withdraw",
      manifestPosition: 447,
      publicationPosition: 480,
      referenceRolePosition: 320,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_finalize_withdraw.main.spend",
      familyPaths: [
        ["cekContextStages", "finalizeWithdraw"],
        ["steps", "129"],
      ],
    },
    validationTraceDisputeCekContextFinalizeObserve: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context finalize observe",
      manifestPosition: 448,
      publicationPosition: 481,
      referenceRolePosition: 321,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_finalize_observe.main.spend",
      familyPaths: [
        ["cekContextStages", "finalizeObserve"],
        ["steps", "130"],
      ],
    },
    validationTraceDisputeCekContextFinalizeMidgard: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context finalize midgard",
      manifestPosition: 449,
      publicationPosition: 482,
      referenceRolePosition: 322,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_finalize_midgard.main.spend",
      familyPaths: [
        ["cekContextStages", "finalizeMidgard"],
        ["steps", "131"],
      ],
    },
    validationTraceDisputeCekContextRedeemerBegin: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context redeemer begin",
      manifestPosition: 450,
      publicationPosition: 483,
      referenceRolePosition: 323,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_redeemer_begin.main.spend",
      familyPaths: [
        ["cekContextStages", "redeemerBegin"],
        ["steps", "133"],
      ],
    },
    validationTraceDisputeCekContextRedeemerSelectAuthenticate: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace CEK context redeemer select authenticate",
      manifestPosition: 451,
      publicationPosition: 484,
      referenceRolePosition: 324,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_redeemer_select_authenticate.main.spend",
      familyPaths: [
        ["cekContextStages", "redeemerSelectAuthenticate"],
        ["steps", "137"],
      ],
    },
    validationTraceDisputeCekContextRedeemerSelectInitialize: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace CEK context redeemer select initialize",
      manifestPosition: 452,
      publicationPosition: 485,
      referenceRolePosition: 325,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_redeemer_select_initialize.main.spend",
      familyPaths: [
        ["cekContextStages", "redeemerSelectInitialize"],
        ["steps", "136"],
      ],
    },
    validationTraceDisputeCekContextRedeemerSelectHash: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context redeemer select hash",
      manifestPosition: 453,
      publicationPosition: 486,
      referenceRolePosition: 326,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_redeemer_select_hash.main.spend",
      familyPaths: [
        ["cekContextStages", "redeemerSelectHash"],
        ["steps", "135"],
      ],
    },
    validationTraceDisputeCekContextRedeemerSelectFinish: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context redeemer select finish",
      manifestPosition: 454,
      publicationPosition: 487,
      referenceRolePosition: 327,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_redeemer_select_finish.main.spend",
      familyPaths: [
        ["cekContextStages", "redeemerSelectFinish"],
        ["steps", "134"],
      ],
    },
    validationTraceDisputeCekContextItemBind: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item bind",
      manifestPosition: 455,
      publicationPosition: 488,
      referenceRolePosition: 328,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_bind.main.spend",
      familyPaths: [
        ["cekContextStages", "itemBind"],
        ["steps", "147"],
      ],
    },
    validationTraceDisputeCekContextItemReturn: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item return",
      manifestPosition: 456,
      publicationPosition: 489,
      referenceRolePosition: 329,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_return.main.spend",
      familyPaths: [
        ["cekContextStages", "itemReturn"],
        ["steps", "146"],
      ],
    },
    validationTraceDisputeCekContextItemSelectionHash: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item selection hash",
      manifestPosition: 457,
      publicationPosition: 490,
      referenceRolePosition: 330,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_hash.main.spend",
      familyPaths: [
        ["cekContextStages", "itemSelectionHash"],
        ["steps", "143"],
      ],
    },
    validationTraceDisputeCekContextItemDataHash: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item data hash",
      manifestPosition: 458,
      publicationPosition: 491,
      referenceRolePosition: 331,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_hash.main.spend",
      familyPaths: [
        ["cekContextStages", "itemDataHash"],
        ["steps", "144"],
      ],
    },
    validationTraceDisputeCekContextItemFinalize: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item finalize",
      manifestPosition: 459,
      publicationPosition: 492,
      referenceRolePosition: 332,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_finalize.main.spend",
      familyPaths: [
        ["cekContextStages", "itemFinalize"],
        ["steps", "145"],
      ],
    },
    validationTraceDisputeCekContextItemSelectionContinue: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item selection continue",
      manifestPosition: 460,
      publicationPosition: 493,
      referenceRolePosition: 333,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_selection_continue.main.spend",
      familyPaths: [
        ["cekContextStages", "itemSelectionContinue"],
        ["steps", "138"],
      ],
    },
    validationTraceDisputeCekContextItemSelectionFinish: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item selection finish",
      manifestPosition: 461,
      publicationPosition: 494,
      referenceRolePosition: 334,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_selection_finish.main.spend",
      familyPaths: [
        ["cekContextStages", "itemSelectionFinish"],
        ["steps", "139"],
      ],
    },
    validationTraceDisputeCekContextItemDataContinue: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item data continue",
      manifestPosition: 462,
      publicationPosition: 495,
      referenceRolePosition: 335,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_data_continue.main.spend",
      familyPaths: [
        ["cekContextStages", "itemDataContinue"],
        ["steps", "140"],
      ],
    },
    validationTraceDisputeCekContextItemDataFinishDescriptor: {
      purpose: "spend",
      referenceRole:
        "V1 validation-trace CEK context item data finish descriptor",
      manifestPosition: 463,
      publicationPosition: 496,
      referenceRolePosition: 336,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_data_finish_descriptor.main.spend",
      familyPaths: [
        ["cekContextStages", "itemDataFinishDescriptor"],
        ["steps", "141"],
      ],
    },
    validationTraceDisputeCekContextItemDataFinishValue: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context item data finish value",
      manifestPosition: 464,
      publicationPosition: 497,
      referenceRolePosition: 337,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_context_item_data_finish_value.main.spend",
      familyPaths: [
        ["cekContextStages", "itemDataFinishValue"],
        ["steps", "142"],
      ],
    },
  },
);
