import { type Data, type Network } from "@lucid-evolution/lucid";

import {
  applyBlueprintParams,
  type FaultProofBlueprint,
  makeSpendingValidator,
} from "./blueprint.js";
import {
  sharedRedeemerItemReferenceScripts,
  type SharedRedeemerItemStages,
} from "./families/shared-redeemer-item.js";

export const CEK_CONTEXT_STAGE_TITLES = {
  settle: "fraud_proofs/validation_trace/cek_context_settle.main.spend",
  control: "fraud_proofs/validation_trace/cek_context_control.main.spend",
  reference: "fraud_proofs/validation_trace/cek_context_reference.main.spend",
  spend: "fraud_proofs/validation_trace/cek_context_spend.main.spend",
  output: "fraud_proofs/validation_trace/cek_context_output.main.spend",
  signer: "fraud_proofs/validation_trace/cek_context_signer.main.spend",
  observerAuthenticate:
    "fraud_proofs/validation_trace/cek_context_observer_authenticate.main.spend",
  observerFold:
    "fraud_proofs/validation_trace/cek_context_observer_fold.main.spend",
  mintInit: "fraud_proofs/validation_trace/cek_context_mint_init.main.spend",
  mintItem: "fraud_proofs/validation_trace/cek_context_mint_item.main.spend",
  assemble: "fraud_proofs/validation_trace/cek_context_assemble.main.spend",
  txInfo: "fraud_proofs/validation_trace/cek_context_tx_info.main.spend",
  seed: "fraud_proofs/validation_trace/cek_context_seed.main.spend",
  finalizeAuthenticate:
    "fraud_proofs/validation_trace/cek_context_finalize_authenticate.main.spend",
  finalizeSpend:
    "fraud_proofs/validation_trace/cek_context_finalize_spend.main.spend",
  finalizeMint:
    "fraud_proofs/validation_trace/cek_context_finalize_mint.main.spend",
  finalizeWithdraw:
    "fraud_proofs/validation_trace/cek_context_finalize_withdraw.main.spend",
  finalizeObserve:
    "fraud_proofs/validation_trace/cek_context_finalize_observe.main.spend",
  finalizeMidgard:
    "fraud_proofs/validation_trace/cek_context_finalize_midgard.main.spend",
  redeemerBegin:
    "fraud_proofs/validation_trace/cek_context_redeemer_begin.main.spend",
  redeemerSelectAuthenticate:
    "fraud_proofs/validation_trace/cek_context_redeemer_select_authenticate.main.spend",
  redeemerSelectInitialize:
    "fraud_proofs/validation_trace/cek_context_redeemer_select_initialize.main.spend",
  redeemerSelectHash:
    "fraud_proofs/validation_trace/cek_context_redeemer_select_hash.main.spend",
  redeemerSelectFinish:
    "fraud_proofs/validation_trace/cek_context_redeemer_select_finish.main.spend",
  itemBind: "fraud_proofs/validation_trace/cek_context_item_bind.main.spend",
  itemReturn:
    "fraud_proofs/validation_trace/cek_context_item_return.main.spend",
  itemSelectionHash:
    "fraud_proofs/validation_trace/cek_context_item_hash.main.spend",
  itemDataHash:
    "fraud_proofs/validation_trace/cek_context_item_hash.main.spend",
  itemFinalize:
    "fraud_proofs/validation_trace/cek_context_item_finalize.main.spend",
  itemSelectionContinue:
    "fraud_proofs/validation_trace/cek_context_item_selection_continue.main.spend",
  itemSelectionFinish:
    "fraud_proofs/validation_trace/cek_context_item_selection_finish.main.spend",
  itemDataContinue:
    "fraud_proofs/validation_trace/cek_context_item_data_continue.main.spend",
  itemDataFinishDescriptor:
    "fraud_proofs/validation_trace/cek_context_item_data_finish_descriptor.main.spend",
  itemDataFinishValue:
    "fraud_proofs/validation_trace/cek_context_item_data_finish_value.main.spend",
} as const;

/** Build the fixed return before the shared item executors and their entry. */
export const buildCekContextTail = (
  blueprint: FaultProofBlueprint,
  network: Network,
  awardHash: string,
  ctPolicy: string,
  fieldPreimagePolicy: string,
) => {
  const build = (key: keyof typeof CEK_CONTEXT_STAGE_TITLES, params: Data[]) =>
    makeSpendingValidator(
      network,
      applyBlueprintParams(blueprint, CEK_CONTEXT_STAGE_TITLES[key], params),
    );
  const settle = build("settle", [awardHash, ctPolicy]);
  const terminal = (key: keyof typeof CEK_CONTEXT_STAGE_TITLES) =>
    build(key, [settle.spendingScriptHash, ctPolicy]);
  const reference = terminal("reference");
  const spend = terminal("spend");
  const output = terminal("output");
  const signer = terminal("signer");
  const observerFold = terminal("observerFold");
  const observerAuthenticate = build("observerAuthenticate", [
    observerFold.spendingScriptHash,
    ctPolicy,
    fieldPreimagePolicy,
  ]);
  const mintInit = terminal("mintInit");
  const mintItem = terminal("mintItem");
  const assemble = terminal("assemble");
  const txInfo = terminal("txInfo");
  const seed = terminal("seed");
  const finalizeSpend = terminal("finalizeSpend");
  const finalizeMint = terminal("finalizeMint");
  const finalizeWithdraw = terminal("finalizeWithdraw");
  const finalizeObserve = terminal("finalizeObserve");
  const finalizeMidgard = terminal("finalizeMidgard");
  const finalizeAuthenticate = build("finalizeAuthenticate", [
    [
      finalizeSpend,
      finalizeMint,
      finalizeWithdraw,
      finalizeObserve,
      finalizeMidgard,
    ].map((x) => x.spendingScriptHash),
    ctPolicy,
  ]);
  const redeemerBegin = terminal("redeemerBegin");
  const redeemerSelectFinish = terminal("redeemerSelectFinish");
  const redeemerSelectHash = build("redeemerSelectHash", [
    redeemerSelectFinish.spendingScriptHash,
    ctPolicy,
  ]);
  const redeemerSelectInitialize = build("redeemerSelectInitialize", [
    redeemerSelectHash.spendingScriptHash,
    ctPolicy,
  ]);
  const redeemerSelectAuthenticate = build("redeemerSelectAuthenticate", [
    redeemerSelectInitialize.spendingScriptHash,
    ctPolicy,
  ]);
  const itemSelectionContinue = terminal("itemSelectionContinue");
  const itemSelectionFinish = terminal("itemSelectionFinish");
  const itemDataContinue = terminal("itemDataContinue");
  const itemDataFinishDescriptor = terminal("itemDataFinishDescriptor");
  const itemDataFinishValue = terminal("itemDataFinishValue");
  const itemSelectionHash = build("itemSelectionHash", [
    itemSelectionContinue.spendingScriptHash,
    ctPolicy,
  ]);
  const itemDataHash = build("itemDataHash", [
    itemDataContinue.spendingScriptHash,
    ctPolicy,
  ]);
  const itemFinalize = build("itemFinalize", [
    itemDataFinishValue.spendingScriptHash,
    ctPolicy,
  ]);
  const itemReturn = build("itemReturn", [
    [
      itemSelectionHash,
      itemSelectionFinish,
      itemDataHash,
      itemDataFinishDescriptor,
      itemFinalize,
    ].map((x) => x.spendingScriptHash),
    ctPolicy,
  ]);
  return {
    settle,
    reference,
    spend,
    output,
    signer,
    observerFold,
    observerAuthenticate,
    mintInit,
    mintItem,
    assemble,
    txInfo,
    seed,
    finalizeSpend,
    finalizeMint,
    finalizeWithdraw,
    finalizeObserve,
    finalizeMidgard,
    finalizeAuthenticate,
    redeemerBegin,
    redeemerSelectFinish,
    redeemerSelectHash,
    redeemerSelectInitialize,
    redeemerSelectAuthenticate,
    itemSelectionContinue,
    itemSelectionFinish,
    itemDataContinue,
    itemDataFinishDescriptor,
    itemDataFinishValue,
    itemSelectionHash,
    itemDataHash,
    itemFinalize,
    itemReturn,
  };
};
export type CekContextTail = ReturnType<typeof buildCekContextTail>;

/** Complete the forward-only graph after the shared item chain pins this tail. */
export const completeCekContextStages = (
  blueprint: FaultProofBlueprint,
  network: Network,
  tail: CekContextTail,
  itemEntryHash: string,
  ctPolicy: string,
) => {
  const itemBind = makeSpendingValidator(
    network,
    applyBlueprintParams(blueprint, CEK_CONTEXT_STAGE_TITLES.itemBind, [
      itemEntryHash,
      ctPolicy,
    ]),
  );
  const entries = [
    tail.redeemerBegin,
    itemBind,
    tail.reference,
    tail.spend,
    tail.output,
    tail.signer,
    tail.observerAuthenticate,
    tail.mintInit,
    tail.mintItem,
    tail.redeemerSelectAuthenticate,
    tail.finalizeAuthenticate,
    tail.assemble,
    tail.txInfo,
    tail.seed,
  ];
  const control = makeSpendingValidator(
    network,
    applyBlueprintParams(blueprint, CEK_CONTEXT_STAGE_TITLES.control, [
      entries.map((stage) => stage.spendingScriptHash),
      ctPolicy,
    ]),
  );
  return { ...tail, itemBind, control };
};
export type CekContextStages = ReturnType<typeof completeCekContextStages>;

export const CEK_CONTEXT_STAGE_REFERENCES = {
  settle: {
    deployment: "validationTraceDisputeCekContextSettle",
    role: "V1 validation-trace CEK context settle",
  },
  control: {
    deployment: "validationTraceDisputeCekContextControl",
    role: "V1 validation-trace CEK context control",
  },
  reference: {
    deployment: "validationTraceDisputeCekContextReference",
    role: "V1 validation-trace CEK context reference",
  },
  spend: {
    deployment: "validationTraceDisputeCekContextSpend",
    role: "V1 validation-trace CEK context spend",
  },
  output: {
    deployment: "validationTraceDisputeCekContextOutput",
    role: "V1 validation-trace CEK context output",
  },
  signer: {
    deployment: "validationTraceDisputeCekContextSigner",
    role: "V1 validation-trace CEK context signer",
  },
  observerAuthenticate: {
    deployment: "validationTraceDisputeCekContextObserverAuthenticate",
    role: "V1 validation-trace CEK context observer authenticate",
  },
  observerFold: {
    deployment: "validationTraceDisputeCekContextObserverFold",
    role: "V1 validation-trace CEK context observer fold",
  },
  mintInit: {
    deployment: "validationTraceDisputeCekContextMintInit",
    role: "V1 validation-trace CEK context mint init",
  },
  mintItem: {
    deployment: "validationTraceDisputeCekContextMintItem",
    role: "V1 validation-trace CEK context mint item",
  },
  assemble: {
    deployment: "validationTraceDisputeCekContextAssemble",
    role: "V1 validation-trace CEK context assemble",
  },
  txInfo: {
    deployment: "validationTraceDisputeCekContextTxInfo",
    role: "V1 validation-trace CEK context tx info",
  },
  seed: {
    deployment: "validationTraceDisputeCekContextSeed",
    role: "V1 validation-trace CEK context seed",
  },
  finalizeAuthenticate: {
    deployment: "validationTraceDisputeCekContextFinalizeAuthenticate",
    role: "V1 validation-trace CEK context finalize authenticate",
  },
  finalizeSpend: {
    deployment: "validationTraceDisputeCekContextFinalizeSpend",
    role: "V1 validation-trace CEK context finalize spend",
  },
  finalizeMint: {
    deployment: "validationTraceDisputeCekContextFinalizeMint",
    role: "V1 validation-trace CEK context finalize mint",
  },
  finalizeWithdraw: {
    deployment: "validationTraceDisputeCekContextFinalizeWithdraw",
    role: "V1 validation-trace CEK context finalize withdraw",
  },
  finalizeObserve: {
    deployment: "validationTraceDisputeCekContextFinalizeObserve",
    role: "V1 validation-trace CEK context finalize observe",
  },
  finalizeMidgard: {
    deployment: "validationTraceDisputeCekContextFinalizeMidgard",
    role: "V1 validation-trace CEK context finalize midgard",
  },
  redeemerBegin: {
    deployment: "validationTraceDisputeCekContextRedeemerBegin",
    role: "V1 validation-trace CEK context redeemer begin",
  },
  redeemerSelectAuthenticate: {
    deployment: "validationTraceDisputeCekContextRedeemerSelectAuthenticate",
    role: "V1 validation-trace CEK context redeemer select authenticate",
  },
  redeemerSelectInitialize: {
    deployment: "validationTraceDisputeCekContextRedeemerSelectInitialize",
    role: "V1 validation-trace CEK context redeemer select initialize",
  },
  redeemerSelectHash: {
    deployment: "validationTraceDisputeCekContextRedeemerSelectHash",
    role: "V1 validation-trace CEK context redeemer select hash",
  },
  redeemerSelectFinish: {
    deployment: "validationTraceDisputeCekContextRedeemerSelectFinish",
    role: "V1 validation-trace CEK context redeemer select finish",
  },
  itemBind: {
    deployment: "validationTraceDisputeCekContextItemBind",
    role: "V1 validation-trace CEK context item bind",
  },
  itemReturn: {
    deployment: "validationTraceDisputeCekContextItemReturn",
    role: "V1 validation-trace CEK context item return",
  },
  itemSelectionHash: {
    deployment: "validationTraceDisputeCekContextItemSelectionHash",
    role: "V1 validation-trace CEK context item selection hash",
  },
  itemDataHash: {
    deployment: "validationTraceDisputeCekContextItemDataHash",
    role: "V1 validation-trace CEK context item data hash",
  },
  itemFinalize: {
    deployment: "validationTraceDisputeCekContextItemFinalize",
    role: "V1 validation-trace CEK context item finalize",
  },
  itemSelectionContinue: {
    deployment: "validationTraceDisputeCekContextItemSelectionContinue",
    role: "V1 validation-trace CEK context item selection continue",
  },
  itemSelectionFinish: {
    deployment: "validationTraceDisputeCekContextItemSelectionFinish",
    role: "V1 validation-trace CEK context item selection finish",
  },
  itemDataContinue: {
    deployment: "validationTraceDisputeCekContextItemDataContinue",
    role: "V1 validation-trace CEK context item data continue",
  },
  itemDataFinishDescriptor: {
    deployment: "validationTraceDisputeCekContextItemDataFinishDescriptor",
    role: "V1 validation-trace CEK context item data finish descriptor",
  },
  itemDataFinishValue: {
    deployment: "validationTraceDisputeCekContextItemDataFinishValue",
    role: "V1 validation-trace CEK context item data finish value",
  },
} as const;

export const CEK_CONTEXT_ITEM_REFERENCES = {
  entry: {
    deployment: "validationTraceDisputeCekContextItemEntry",
    role: "V1 validation-trace CEK context item entry",
  },
  settlement: {
    deployment: "validationTraceDisputeCekContextItemSettlement",
    role: "V1 validation-trace CEK context item settlement",
  },
} as const;

/** Unique context publications; shared item executors retain their canonical roles. */
export const cekContextReferenceScripts = (
  stages: CekContextStages,
  item: SharedRedeemerItemStages,
) => [
  ...Object.entries(CEK_CONTEXT_STAGE_REFERENCES).map(([key, spec]) => ({
    deploymentEntry: spec.deployment,
    role: spec.role,
    validator: stages[key as keyof CekContextStages],
  })),
  ...Object.entries(CEK_CONTEXT_ITEM_REFERENCES).map(([key, spec]) => ({
    deploymentEntry: spec.deployment,
    role: spec.role,
    validator: item[key as keyof typeof CEK_CONTEXT_ITEM_REFERENCES],
  })),
];

export const cekContextItemReferenceScripts = (
  item: SharedRedeemerItemStages,
) => [
  ...Object.entries(CEK_CONTEXT_ITEM_REFERENCES).map(([key, spec]) => ({
    deploymentEntry: spec.deployment,
    role: spec.role,
    validator: item[key as keyof typeof CEK_CONTEXT_ITEM_REFERENCES],
  })),
  ...sharedRedeemerItemReferenceScripts(item),
];
