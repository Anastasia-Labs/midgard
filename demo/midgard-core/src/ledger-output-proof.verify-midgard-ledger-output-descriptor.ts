import {
  MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION,
  type MidgardLedgerOutputCommitment,
} from "./ledger-output-commitment.js";
import {
  commitMidgardLedgerOutputReferenceScriptItem,
  digestMidgardLedgerOutputReferenceScript,
  MIDGARD_LEDGER_OUTPUT_PROOF_FACT_ATTACH_GROUPS,
  midgardLedgerOutputProofFact,
  midgardLedgerOutputProofFactCommitment,
  midgardLedgerOutputProofTerminalClaimedSummaries,
  summariesEqual,
  summarizeMidgardLedgerOutputCardanoSpendDatum,
  summarizeMidgardLedgerOutputCardanoTxOut,
  summarizeMidgardLedgerOutputMidgardTxOut,
} from "./ledger-output-proof.midgard-ledger-output-proof-fact-commitment.js";
import { type MidgardLedgerOutputProofControl } from "./ledger-output-proof.midgard-ledger-output-proof-witness.js";
import { isExactMidgardLedgerOutputProofTerminal } from "./ledger-output-proof.span-chunk-witness.js";
import { commitMidgardValidationMerkleFrontier } from "./validation-merkle.js";

/**
 * One canonical fact-attachment machine step on a terminal control: record
 * the next incomplete group's fact commitments, computed from the descriptor
 * bytes and the control's own terminal summaries. `null` when every fact is
 * already recorded. Mirrors `ledger_output_proof_v1.fact_attach_v1`.
 */
export const attachMidgardLedgerOutputProofFacts = (
  control: MidgardLedgerOutputProofControl,
  descriptorCbor: Uint8Array,
): MidgardLedgerOutputProofControl | null => {
  const summaries = midgardLedgerOutputProofTerminalClaimedSummaries(control);
  if (summaries === null) return null;
  const nextGroup = MIDGARD_LEDGER_OUTPUT_PROOF_FACT_ATTACH_GROUPS.find(
    (group) =>
      group.some(
        (role) => midgardLedgerOutputProofFact(control, role) === null,
      ),
  );
  if (nextGroup === undefined) return null;
  if (
    !nextGroup.every(
      (role) => midgardLedgerOutputProofFact(control, role) === null,
    )
  ) {
    throw new Error("V1 ledger output proof fact group partially attached");
  }
  let next = control;
  for (const role of nextGroup) {
    const commitment = midgardLedgerOutputProofFactCommitment({
      role,
      descriptorCbor,
      valueSummaryDataCbor: summaries.valueSummaryDataCbor,
      datumSummaryDataCbor: summaries.datumSummaryDataCbor,
    });
    next =
      role === 0
        ? { ...next, scanFactsFact: commitment }
        : role === 1
          ? { ...next, referenceScriptFact: commitment }
          : role === 2
            ? { ...next, datumSummaryFact: commitment }
            : { ...next, valueSummaryFact: commitment };
  }
  return next;
};

/**
 * Verifies every compact ledger descriptor fact against one exact terminal
 * output proof. No descriptor may enter the ledger MPF through this boundary
 * unless the complete independently decoded descriptor is proven equal.
 */
export const verifyMidgardLedgerOutputDescriptor = ({
  control,
  descriptor,
}: {
  readonly control: MidgardLedgerOutputProofControl;
  readonly descriptor: MidgardLedgerOutputCommitment;
}): boolean => {
  if (
    !isExactMidgardLedgerOutputProofTerminal(control) ||
    descriptor.version !== MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION
  ) {
    return false;
  }
  const cardanoTxOut = summarizeMidgardLedgerOutputCardanoTxOut(control);
  const midgardTxOut = summarizeMidgardLedgerOutputMidgardTxOut(control);
  const cardanoSpendDatum =
    summarizeMidgardLedgerOutputCardanoSpendDatum(control);
  if (
    cardanoTxOut === null ||
    midgardTxOut === null ||
    cardanoSpendDatum === null
  ) {
    return false;
  }
  const referenceLanguage = control.outputScan.referenceScriptLanguage;
  const referenceHash = digestMidgardLedgerOutputReferenceScript(control);
  const referenceItemCommitment =
    commitMidgardLedgerOutputReferenceScriptItem(control);
  const referenceTotalLength =
    referenceLanguage === -1
      ? 0
      : control.totalLength - control.outputScan.referenceScriptItemOffset;
  return (
    descriptor.outputIndex === control.outputIndex &&
    descriptor.totalLength === control.totalLength &&
    Buffer.from(descriptor.itemCommitment).equals(control.itemCommitment) &&
    Buffer.from(descriptor.address).equals(control.outputScan.address) &&
    descriptor.lovelace === control.outputScan.lovelace &&
    descriptor.assetCount === control.outputScan.assetFrontier.count &&
    Buffer.from(descriptor.assetFrontierCommitment).equals(
      commitMidgardValidationMerkleFrontier(control.outputScan.assetFrontier),
    ) &&
    descriptor.cardanoValueSize === control.outputScan.cardanoValueSize &&
    descriptor.referenceScriptLanguage === referenceLanguage &&
    Buffer.from(descriptor.referenceScriptHash).equals(
      referenceHash ?? Buffer.alloc(0),
    ) &&
    descriptor.referenceScriptTotalLength === referenceTotalLength &&
    Buffer.from(descriptor.referenceScriptItemCommitment).equals(
      referenceItemCommitment ?? Buffer.alloc(0),
    ) &&
    summariesEqual(cardanoTxOut, descriptor.cardanoTxOut) &&
    summariesEqual(midgardTxOut, descriptor.midgardTxOut) &&
    summariesEqual(cardanoSpendDatum, descriptor.cardanoSpendDatum)
  );
};
