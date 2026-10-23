import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCekContextMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekContextSettle: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK context settle",
      manifestPosition: 463,
      publicationPosition: 497,
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
      manifestPosition: 464,
      publicationPosition: 498,
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
      manifestPosition: 465,
      publicationPosition: 499,
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
      manifestPosition: 466,
      publicationPosition: 500,
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
      manifestPosition: 467,
      publicationPosition: 501,
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
      manifestPosition: 468,
      publicationPosition: 502,
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
      manifestPosition: 469,
      publicationPosition: 503,
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
      manifestPosition: 470,
      publicationPosition: 504,
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
      manifestPosition: 471,
      publicationPosition: 505,
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
      manifestPosition: 472,
      publicationPosition: 506,
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
      manifestPosition: 473,
      publicationPosition: 507,
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
      manifestPosition: 474,
      publicationPosition: 508,
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
      manifestPosition: 475,
      publicationPosition: 509,
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
      manifestPosition: 476,
      publicationPosition: 510,
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
      manifestPosition: 477,
      publicationPosition: 511,
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
      manifestPosition: 478,
      publicationPosition: 512,
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
      manifestPosition: 479,
      publicationPosition: 513,
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
      manifestPosition: 480,
      publicationPosition: 514,
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
      manifestPosition: 481,
      publicationPosition: 515,
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
      manifestPosition: 482,
      publicationPosition: 516,
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
      manifestPosition: 483,
      publicationPosition: 517,
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
      manifestPosition: 484,
      publicationPosition: 518,
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
      manifestPosition: 485,
      publicationPosition: 519,
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
      manifestPosition: 486,
      publicationPosition: 520,
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
      manifestPosition: 487,
      publicationPosition: 521,
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
      manifestPosition: 488,
      publicationPosition: 522,
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
      manifestPosition: 489,
      publicationPosition: 523,
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
      manifestPosition: 490,
      publicationPosition: 524,
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
      manifestPosition: 491,
      publicationPosition: 525,
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
      manifestPosition: 492,
      publicationPosition: 526,
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
      manifestPosition: 493,
      publicationPosition: 527,
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
      manifestPosition: 494,
      publicationPosition: 528,
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
      manifestPosition: 495,
      publicationPosition: 529,
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
      manifestPosition: 496,
      publicationPosition: 530,
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
