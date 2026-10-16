import { spend } from "./recipes.types.js";

export const validationTraceCekContextRecipes = {
  validationTraceDisputeCekContextSettle: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.cekContextStages.settle,
  ),
  validationTraceDisputeCekContextControl: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages.control,
  ),
  validationTraceDisputeCekContextReference: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages.reference,
  ),
  validationTraceDisputeCekContextSpend: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.cekContextStages.spend,
  ),
  validationTraceDisputeCekContextOutput: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.cekContextStages.output,
  ),
  validationTraceDisputeCekContextSigner: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.cekContextStages.signer,
  ),
  validationTraceDisputeCekContextObserverAuthenticate: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .observerAuthenticate,
  ),
  validationTraceDisputeCekContextObserverFold: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .observerFold,
  ),
  validationTraceDisputeCekContextMintInit: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages.mintInit,
  ),
  validationTraceDisputeCekContextMintItem: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages.mintItem,
  ),
  validationTraceDisputeCekContextAssemble: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages.assemble,
  ),
  validationTraceDisputeCekContextTxInfo: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.cekContextStages.txInfo,
  ),
  validationTraceDisputeCekContextSeed: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.cekContextStages.seed,
  ),
  validationTraceDisputeCekContextFinalizeAuthenticate: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .finalizeAuthenticate,
  ),
  validationTraceDisputeCekContextFinalizeSpend: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .finalizeSpend,
  ),
  validationTraceDisputeCekContextFinalizeMint: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .finalizeMint,
  ),
  validationTraceDisputeCekContextFinalizeWithdraw: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .finalizeWithdraw,
  ),
  validationTraceDisputeCekContextFinalizeObserve: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .finalizeObserve,
  ),
  validationTraceDisputeCekContextFinalizeMidgard: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .finalizeMidgard,
  ),
  validationTraceDisputeCekContextRedeemerBegin: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .redeemerBegin,
  ),
  validationTraceDisputeCekContextRedeemerSelectAuthenticate: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .redeemerSelectAuthenticate,
  ),
  validationTraceDisputeCekContextRedeemerSelectInitialize: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .redeemerSelectInitialize,
  ),
  validationTraceDisputeCekContextRedeemerSelectHash: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .redeemerSelectHash,
  ),
  validationTraceDisputeCekContextRedeemerSelectFinish: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .redeemerSelectFinish,
  ),
  validationTraceDisputeCekContextItemBind: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages.itemBind,
  ),
  validationTraceDisputeCekContextItemReturn: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages.itemReturn,
  ),
  validationTraceDisputeCekContextItemSelectionHash: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .itemSelectionHash,
  ),
  validationTraceDisputeCekContextItemDataHash: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .itemDataHash,
  ),
  validationTraceDisputeCekContextItemFinalize: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .itemFinalize,
  ),
  validationTraceDisputeCekContextItemSelectionContinue: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .itemSelectionContinue,
  ),
  validationTraceDisputeCekContextItemSelectionFinish: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .itemSelectionFinish,
  ),
  validationTraceDisputeCekContextItemDataContinue: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .itemDataContinue,
  ),
  validationTraceDisputeCekContextItemDataFinishDescriptor: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .itemDataFinishDescriptor,
  ),
  validationTraceDisputeCekContextItemDataFinishValue: spend(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.cekContextStages
        .itemDataFinishValue,
  ),
} as const;
