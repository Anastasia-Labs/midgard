import { commitMidgardBoundedItem } from "./bounded-item.js";
import { finalizeMidgardCekDataTraverse } from "./cek-data-traverse.js";
import { type MidgardCekDataSummary } from "./cek-semantic.js";
import {
  encodeMidgardLedgerOutputCommitment,
  MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION,
  type MidgardLedgerOutputCommitment,
  type MidgardLedgerOutputDataSummary,
  type MidgardLedgerOutputReferenceScriptLanguage,
} from "./ledger-output-commitment.js";
import {
  digestMidgardLedgerOutputReferenceScript,
  MIDGARD_LEDGER_OUTPUT_PROOF_FACT_ATTACH_GROUPS,
  midgardLedgerOutputProofFact,
  midgardLedgerOutputProofFactCommitment,
  midgardLedgerOutputProofFactDigest,
  midgardLedgerOutputProofTerminalClaimedSummaries,
  summarizeMidgardLedgerOutputSpendDatumOf,
  summarizeMidgardLedgerOutputTxOutOf,
} from "./ledger-output-proof.midgard-ledger-output-proof-fact-commitment.js";
import {
  MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
  type MidgardLedgerOutputProofControl,
} from "./ledger-output-proof.midgard-ledger-output-proof-witness.js";
import { isExactMidgardLedgerOutputProofTerminal } from "./ledger-output-proof.span-chunk-witness.js";
import { finalizeMidgardLedgerOutputValue } from "./ledger-output-value.js";
import { aikenSerialisedPlutusDataBytes } from "./plutus-data-cbor.js";
import {
  commitMidgardValidationMerkleFrontier,
  type MidgardValidationMerkleFrontier,
} from "./validation-merkle.js";

const descriptorSummary = (
  summary: MidgardCekDataSummary,
): MidgardLedgerOutputDataSummary => ({
  root: Buffer.from(summary.root),
  cborLength: summary.cborLength,
  memory: summary.memory,
});

/**
 * The leaf facts of an exact terminal output proof that determine its
 * descriptor: the output scan's fields, the folded value and datum summaries
 * (`datum` is `null` exactly when the output carries no datum) and, for an
 * output with a reference script, its language, hash, item length and chunk
 * frontier.
 */
export type MidgardLedgerOutputTerminalFacts = {
  readonly outputIndex: number;
  readonly totalLength: number;
  readonly itemCommitment: Uint8Array;
  readonly address: Uint8Array;
  readonly lovelace: bigint;
  readonly assetFrontier: MidgardValidationMerkleFrontier;
  readonly cardanoValueSize: number;
  readonly value: MidgardCekDataSummary;
  readonly datum: MidgardCekDataSummary | null;
  readonly referenceScript: {
    readonly language: MidgardLedgerOutputReferenceScriptLanguage;
    readonly digest: Uint8Array;
    readonly totalLength: number;
    readonly frontier: MidgardValidationMerkleFrontier;
  } | null;
};

/**
 * The descriptor determined by the leaf facts of an exact terminal output
 * proof. Mirrors `ledger_output_proof_v1.terminal_descriptor_v1`.
 */
export const midgardLedgerOutputDescriptorOfTerminalFacts = (
  facts: MidgardLedgerOutputTerminalFacts,
): MidgardLedgerOutputCommitment => {
  const reference = facts.referenceScript;
  const txOut = (encoding: "cardano" | "midgard") =>
    descriptorSummary(
      summarizeMidgardLedgerOutputTxOutOf({
        address: facts.address,
        encoding,
        value: facts.value,
        datum: facts.datum,
        referenceScriptDigest: reference?.digest ?? null,
      }),
    );
  return {
    version: MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION,
    outputIndex: facts.outputIndex,
    totalLength: facts.totalLength,
    itemCommitment: Buffer.from(facts.itemCommitment),
    address: Buffer.from(facts.address),
    lovelace: facts.lovelace,
    assetCount: facts.assetFrontier.count,
    assetFrontierCommitment: commitMidgardValidationMerkleFrontier(
      facts.assetFrontier,
    ),
    cardanoValueSize: facts.cardanoValueSize,
    referenceScriptLanguage: reference?.language ?? -1,
    referenceScriptHash: Buffer.from(reference?.digest ?? []),
    referenceScriptTotalLength: reference?.totalLength ?? 0,
    referenceScriptItemCommitment:
      reference === null
        ? Buffer.alloc(0)
        : commitMidgardBoundedItem({
            fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
            itemIndex: facts.outputIndex,
            totalLength: reference.totalLength,
            frontier: reference.frontier,
          }),
    cardanoTxOut: txOut("cardano"),
    midgardTxOut: txOut("midgard"),
    cardanoSpendDatum: descriptorSummary(
      summarizeMidgardLedgerOutputSpendDatumOf(facts.datum),
    ),
  };
};

/**
 * The descriptor of an exact terminal control, every field computed from the
 * control itself; `null` off the exact terminal. Mirrors
 * `ledger_output_proof_v1.terminal_descriptor_v1`.
 */
export const terminalMidgardLedgerOutputDescriptor = (
  control: MidgardLedgerOutputProofControl,
): MidgardLedgerOutputCommitment | null => {
  if (!isExactMidgardLedgerOutputProofTerminal(control)) return null;
  const value = finalizeMidgardLedgerOutputValue(control.value!);
  if (value === null) return null;
  let datum: MidgardCekDataSummary | null = null;
  if (control.outputScan.datumOffset !== -1) {
    datum = finalizeMidgardCekDataTraverse(control.datum!);
    if (datum === null) return null;
  }
  const language = control.outputScan.referenceScriptLanguage;
  let referenceScript: MidgardLedgerOutputTerminalFacts["referenceScript"] =
    null;
  if (language !== -1) {
    const digest = digestMidgardLedgerOutputReferenceScript(control);
    if (digest === null) return null;
    referenceScript = {
      language,
      digest,
      totalLength:
        control.totalLength - control.outputScan.referenceScriptItemOffset,
      frontier: control.referenceScriptFrontier,
    };
  }
  return midgardLedgerOutputDescriptorOfTerminalFacts({
    outputIndex: control.outputIndex,
    totalLength: control.totalLength,
    itemCommitment: control.itemCommitment,
    address: control.outputScan.address,
    lovelace: control.outputScan.lovelace,
    assetFrontier: control.outputScan.assetFrontier,
    cardanoValueSize: control.outputScan.cardanoValueSize,
    value,
    datum,
    referenceScript,
  });
};

/**
 * One canonical fact-attachment machine step on a terminal control: record
 * the next incomplete group's fact commitments, computed from the control's
 * own terminal descriptor and summaries. `null` when every fact is already
 * recorded. Mirrors `ledger_output_proof_v1.fact_attach_v1`.
 */
export const attachMidgardLedgerOutputProofFacts = (
  control: MidgardLedgerOutputProofControl,
): MidgardLedgerOutputProofControl | null => {
  const descriptor = terminalMidgardLedgerOutputDescriptor(control);
  const summaries = midgardLedgerOutputProofTerminalClaimedSummaries(control);
  if (descriptor === null || summaries === null) return null;
  const descriptorCbor = encodeMidgardLedgerOutputCommitment(descriptor);
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
 * The thin terminal gate: the control is an exact terminal and its recorded
 * scan fact commits exactly `descriptorCbor`. Mirrors
 * `ledger_output_proof_v1.facts_are_exact_v1`.
 */
export const midgardLedgerOutputProofFactsAreExact = (
  control: MidgardLedgerOutputProofControl,
  descriptorCbor: Uint8Array,
): boolean =>
  isExactMidgardLedgerOutputProofTerminal(control) &&
  control.scanFactsFact !== null &&
  Buffer.from(control.scanFactsFact).equals(
    midgardLedgerOutputProofFactDigest([
      aikenSerialisedPlutusDataBytes(descriptorCbor),
    ]),
  );

/**
 * Verifies a compact ledger descriptor against one exact terminal output
 * proof: the descriptor must equal the one the terminal control derives. No
 * descriptor may enter the ledger MPF through this boundary unless the
 * complete independently decoded descriptor is proven equal.
 */
export const verifyMidgardLedgerOutputDescriptor = ({
  control,
  descriptor,
}: {
  readonly control: MidgardLedgerOutputProofControl;
  readonly descriptor: MidgardLedgerOutputCommitment;
}): boolean => {
  if (descriptor.version !== MIDGARD_LEDGER_OUTPUT_COMMITMENT_VERSION) {
    return false;
  }
  const derived = terminalMidgardLedgerOutputDescriptor(control);
  if (derived === null) return false;
  try {
    return encodeMidgardLedgerOutputCommitment(descriptor).equals(
      encodeMidgardLedgerOutputCommitment(derived),
    );
  } catch {
    return false;
  }
};
