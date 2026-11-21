import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceCekContextMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekContextSettle: {
      purpose: "spend",
      reference: {
        role: "V1 validation-trace CEK context settle",
        token: "V1VtCekCtxSet",
      },
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
      reference: {
        role: "V1 validation-trace CEK context control",
        token: "V1VtCekCtxCon",
      },
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
      reference: {
        role: "V1 validation-trace CEK context reference",
        token: "V1VtCekCtxRef",
      },
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
      reference: {
        role: "V1 validation-trace CEK context spend",
        token: "V1VtCekCtxSpe",
      },
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
      reference: {
        role: "V1 validation-trace CEK context output",
        token: "V1VtCekCtxOut",
      },
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
      reference: {
        role: "V1 validation-trace CEK context signer",
        token: "V1VtCekCtxSig",
      },
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
      reference: {
        role: "V1 validation-trace CEK context observer authenticate",
        token: "V1VtCekCtxObsAut",
      },
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
      reference: {
        role: "V1 validation-trace CEK context observer fold",
        token: "V1VtCekCtxObsFol",
      },
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
      reference: {
        role: "V1 validation-trace CEK context mint init",
        token: "V1VtCekCtxMinIni",
      },
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
      reference: {
        role: "V1 validation-trace CEK context mint item",
        token: "V1VtCekCtxMinIte",
      },
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
      reference: {
        role: "V1 validation-trace CEK context assemble",
        token: "V1VtCekCtxAss",
      },
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
      reference: {
        role: "V1 validation-trace CEK context tx info",
        token: "V1VtCekCtxTxInf",
      },
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
      reference: {
        role: "V1 validation-trace CEK context seed",
        token: "V1VtCekCtxSee",
      },
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
      reference: {
        role: "V1 validation-trace CEK context finalize authenticate",
        token: "V1VtCekCtxFinAut",
      },
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
      reference: {
        role: "V1 validation-trace CEK context finalize spend",
        token: "V1VtCekCtxFinSpe",
      },
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
      reference: {
        role: "V1 validation-trace CEK context finalize mint",
        token: "V1VtCekCtxFinMin",
      },
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
      reference: {
        role: "V1 validation-trace CEK context finalize withdraw",
        token: "V1VtCekCtxFinWit",
      },
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
      reference: {
        role: "V1 validation-trace CEK context finalize observe",
        token: "V1VtCekCtxFinObs",
      },
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
      reference: {
        role: "V1 validation-trace CEK context finalize midgard",
        token: "V1VtCekCtxFinMid",
      },
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
      reference: {
        role: "V1 validation-trace CEK context redeemer begin",
        token: "V1VtCekCtxRedBeg",
      },
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
      reference: {
        role: "V1 validation-trace CEK context redeemer select authenticate",
        token: "V1VtCekCtxRedSelAut",
      },
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
      reference: {
        role: "V1 validation-trace CEK context redeemer select initialize",
        token: "V1VtCekCtxRedSelIni",
      },
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
      reference: {
        role: "V1 validation-trace CEK context redeemer select hash",
        token: "V1VtCekCtxRedSelHas",
      },
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
      reference: {
        role: "V1 validation-trace CEK context redeemer select finish",
        token: "V1VtCekCtxRedSelFin",
      },
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
      reference: {
        role: "V1 validation-trace CEK context item bind",
        token: "V1VtCekCtxIteBin",
      },
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
      reference: {
        role: "V1 validation-trace CEK context item return",
        token: "V1VtCekCtxIteRet",
      },
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
      reference: {
        role: "V1 validation-trace CEK context item selection hash",
        token: "V1VtCekCtxIteSelHas",
      },
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
      reference: {
        role: "V1 validation-trace CEK context item data hash",
        token: "V1VtCekCtxIteDatHas",
      },
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
      reference: {
        role: "V1 validation-trace CEK context item finalize",
        token: "V1VtCekCtxIteFin",
      },
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
      reference: {
        role: "V1 validation-trace CEK context item selection continue",
        token: "V1VtCekCtxIteSelCon",
      },
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
      reference: {
        role: "V1 validation-trace CEK context item selection finish",
        token: "V1VtCekCtxIteSelFin",
      },
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
      reference: {
        role: "V1 validation-trace CEK context item data continue",
        token: "V1VtCekCtxIteDatCon",
      },
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
      reference: {
        role: "V1 validation-trace CEK context item data finish descriptor",
        token: "V1VtCekCtxIteDatFinDes",
      },
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
      reference: {
        role: "V1 validation-trace CEK context item data finish value",
        token: "V1VtCekCtxIteDatFinVal",
      },
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
