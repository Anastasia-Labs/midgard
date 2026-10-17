import { withdraw } from "./recipes.types.js";

export const validationTraceLedgerOutputYieldsRecipes = {
  validationTraceDisputeLedgerOutputProofStructureWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofStructure,
  ),
  validationTraceDisputeLedgerOutputProofValueWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofValue,
  ),
  validationTraceDisputeLedgerOutputProofDatumFoldMapWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofDatumFoldMap,
  ),
  validationTraceDisputeLedgerOutputProofDatumFinalizeFrameWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofDatumFinalizeFrame,
  ),
  validationTraceDisputeLedgerOutputProofDatumHeadScalarWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofDatumHeadScalar,
  ),
  validationTraceDisputeLedgerOutputProofDatumAttachIntegerWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofDatumAttachInteger,
  ),
  validationTraceDisputeLedgerOutputProofDatumFoldListWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofDatumFoldList,
  ),
  validationTraceDisputeLedgerOutputProofDatumAdvanceIntegerWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofDatumAdvanceInteger,
  ),
  validationTraceDisputeLedgerOutputProofReferenceScriptWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofReferenceScript,
  ),
  validationTraceDisputeLedgerOutputProofScriptHashWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofScriptHash,
  ),
  validationTraceDisputeLedgerOutputProofNativeScriptWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofNativeScript,
  ),
  validationTraceDisputeLedgerOutputProofStructureAssetsWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofStructureAssets,
  ),
  validationTraceDisputeLedgerOutputProofStructureOptionalWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofStructureOptional,
  ),
  validationTraceDisputeLedgerOutputProofStructureFinishWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofStructureFinish,
  ),
  validationTraceDisputeLedgerOutputProofDatumHeadSequenceWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofDatumHeadSequence,
  ),
  validationTraceDisputeLedgerOutputProofDatumHeadMapWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofDatumHeadMap,
  ),
  validationTraceDisputeLedgerOutputProofDatumHeadLargeConstructorWithdraw:
    withdraw(
      (c) =>
        c.fraudProofContracts.validationTraceDispute.yields
          .ledgerOutputProofDatumHeadLargeConstructor,
    ),
  validationTraceDisputeLedgerOutputProofDatumAttachBytesWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofDatumAttachBytes,
  ),
  validationTraceDisputeLedgerOutputProofDatumAdvanceBytesWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofDatumAdvanceBytes,
  ),
  validationTraceDisputeLedgerOutputProofDatumFinishWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofDatumFinish,
  ),
  validationTraceDisputeLedgerOutputProofDatumLargeConstructorWithdraw:
    withdraw(
      (c) =>
        c.fraudProofContracts.validationTraceDispute.yields
          .ledgerOutputProofDatumLargeConstructor,
    ),
  validationTraceDisputeLedgerOutputProofDatumLargeFieldsWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofDatumLargeFields,
  ),
  validationTraceDisputeLedgerOutputProofDatumCloseWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofDatumClose,
  ),
  validationTraceDisputeLedgerOutputProofSpanWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields.ledgerOutputProofSpan,
  ),
  validationTraceDisputeLedgerOutputProofScalarIntegerWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofScalarInteger,
  ),
  validationTraceDisputeLedgerOutputProofScalarBytesWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputProofScalarBytes,
  ),
  validationTraceDisputeLedgerOutputDescriptorScanFactsWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputDescriptorScanFacts,
  ),
  validationTraceDisputeLedgerOutputDescriptorReferenceScriptWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputDescriptorReferenceScript,
  ),
  validationTraceDisputeLedgerOutputDescriptorDatumSummaryWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputDescriptorDatumSummary,
  ),
  validationTraceDisputeLedgerOutputDescriptorValueSummaryWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .ledgerOutputDescriptorValueSummary,
  ),
} as const;
