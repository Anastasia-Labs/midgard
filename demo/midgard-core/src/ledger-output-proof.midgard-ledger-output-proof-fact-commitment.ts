import { digestMidgardBlake2b224Trace } from "./blake2b-224-trace.js";
import { commitMidgardBoundedItem } from "./bounded-item.js";
import {
  buildMidgardCekDataTraverseTrace,
  finalizeMidgardCekDataTraverse,
} from "./cek-data-traverse.js";
import {
  emptyMidgardCekDataListSummary,
  type MidgardCekDataSummary,
  prependMidgardCekDataListSummary,
  summarizeMidgardCekSmallConstrData,
} from "./cek-semantic.js";
import { decodeMidgardAddressBytes } from "./codec/address.js";
import { midgardBlake2b } from "./codec/blake2b.js";
import { encodeCbor } from "./codec/cbor.js";
import { decodeMidgardLedgerOutputCommitment } from "./ledger-output-commitment.js";
import {
  MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
  type MidgardLedgerOutputProofControl,
} from "./ledger-output-proof.midgard-ledger-output-proof-witness.js";
import { isExactMidgardLedgerOutputProofTerminal } from "./ledger-output-proof.span-chunk-witness.js";
import { finalizeMidgardLedgerOutputValue } from "./ledger-output-value.js";
import { aikenSerialisedPlutusDataBytes } from "./plutus-data-cbor.js";

export const digestMidgardLedgerOutputReferenceScript = (
  control: MidgardLedgerOutputProofControl,
): Buffer | null =>
  isExactMidgardLedgerOutputProofTerminal(control) &&
  control.scriptHash !== null
    ? digestMidgardBlake2b224Trace(control.scriptHash)
    : null;

/**
 * The `Option<Data>` spend-datum summary of an output whose datum summary is
 * `datum` (`null` when the output carries no datum).
 */
export const summarizeMidgardLedgerOutputSpendDatumOf = (
  datum: MidgardCekDataSummary | null,
): MidgardCekDataSummary =>
  datum === null
    ? summarizeMidgardCekSmallConstrData(1n, emptyMidgardCekDataListSummary())
    : summarizeMidgardCekSmallConstrData(
        0n,
        prependMidgardCekDataListSummary(
          datum,
          emptyMidgardCekDataListSummary(),
        ),
      );

/** The terminal datum summary; `null` for an unfinished traversal. */
const terminalDatumSummary = (
  control: MidgardLedgerOutputProofControl,
): { readonly datum: MidgardCekDataSummary | null } | null => {
  if (control.outputScan.datumOffset === -1) return { datum: null };
  const datum = finalizeMidgardCekDataTraverse(control.datum!);
  return datum === null ? null : { datum };
};

export const summarizeMidgardLedgerOutputCardanoSpendDatum = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null => {
  if (!isExactMidgardLedgerOutputProofTerminal(control)) return null;
  const datum = terminalDatumSummary(control);
  return datum === null
    ? null
    : summarizeMidgardLedgerOutputSpendDatumOf(datum.datum);
};

export const summarizeMidgardLedgerOutputValue = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null =>
  isExactMidgardLedgerOutputProofTerminal(control)
    ? finalizeMidgardLedgerOutputValue(control.value!)
    : null;

const summarizeDirectBytesData = (bytes: Uint8Array): MidgardCekDataSummary => {
  const trace = buildMidgardCekDataTraverseTrace({
    sourceStart: 0,
    source: encodeCbor(Buffer.from(bytes)),
  });
  const summary = finalizeMidgardCekDataTraverse(trace.terminal);
  if (summary === null) {
    throw new Error("V1 direct bytes Data summary failed closed");
  }
  return summary;
};

const summarizeDataList = (items: readonly MidgardCekDataSummary[]) => {
  let summary = emptyMidgardCekDataListSummary();
  for (let index = items.length - 1; index >= 0; index -= 1) {
    summary = prependMidgardCekDataListSummary(items[index]!, summary);
  }
  return summary;
};

const summarizeSmallConstr = (
  constructor: bigint,
  fields: readonly MidgardCekDataSummary[],
): MidgardCekDataSummary =>
  summarizeMidgardCekSmallConstrData(constructor, summarizeDataList(fields));

const summarizeCredential = (
  kind: "PubKey" | "Script",
  hash: Uint8Array,
): MidgardCekDataSummary =>
  summarizeSmallConstr(kind === "PubKey" ? 0n : 1n, [
    summarizeDirectBytesData(hash),
  ]);

const summarizeOutputAddress = (
  addressBytes: Uint8Array,
  encoding: "cardano" | "midgard",
): MidgardCekDataSummary => {
  const address = decodeMidgardAddressBytes(Buffer.from(addressBytes));
  const payment = summarizeCredential(
    address.paymentCredential.kind,
    address.paymentCredential.hash,
  );
  const stake =
    address.stakeCredential === undefined
      ? summarizeSmallConstr(1n, [])
      : summarizeSmallConstr(0n, [
          summarizeSmallConstr(0n, [
            summarizeCredential(
              address.stakeCredential.kind,
              address.stakeCredential.hash,
            ),
          ]),
        ]);
  return summarizeSmallConstr(
    encoding === "midgard" && address.protected ? 1n : 0n,
    [payment, stake],
  );
};

/**
 * The `TxOut` summary of an output from its leaf facts: address bytes, value
 * summary, datum summary (`null` without a datum) and reference-script digest
 * (`null` without a reference script).
 */
export const summarizeMidgardLedgerOutputTxOutOf = ({
  address,
  encoding,
  value,
  datum,
  referenceScriptDigest,
}: {
  readonly address: Uint8Array;
  readonly encoding: "cardano" | "midgard";
  readonly value: MidgardCekDataSummary;
  readonly datum: MidgardCekDataSummary | null;
  readonly referenceScriptDigest: Uint8Array | null;
}): MidgardCekDataSummary =>
  summarizeSmallConstr(0n, [
    summarizeOutputAddress(address, encoding),
    value,
    datum === null
      ? summarizeSmallConstr(0n, [])
      : summarizeSmallConstr(2n, [datum]),
    referenceScriptDigest === null
      ? summarizeSmallConstr(1n, [])
      : summarizeSmallConstr(0n, [
          summarizeDirectBytesData(referenceScriptDigest),
        ]),
  ]);

const summarizeOutputTxOut = (
  control: MidgardLedgerOutputProofControl,
  encoding: "cardano" | "midgard",
): MidgardCekDataSummary | null => {
  if (!isExactMidgardLedgerOutputProofTerminal(control)) {
    return null;
  }
  const value = finalizeMidgardLedgerOutputValue(control.value!);
  const datum = terminalDatumSummary(control);
  const referenceScriptDigest =
    control.outputScan.referenceScriptLanguage === -1
      ? null
      : digestMidgardLedgerOutputReferenceScript(control);
  return value === null ||
    datum === null ||
    (control.outputScan.referenceScriptLanguage !== -1 &&
      referenceScriptDigest === null)
    ? null
    : summarizeMidgardLedgerOutputTxOutOf({
        address: control.outputScan.address,
        encoding,
        value,
        datum: datum.datum,
        referenceScriptDigest,
      });
};

export const summarizeMidgardLedgerOutputCardanoTxOut = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null => summarizeOutputTxOut(control, "cardano");

export const summarizeMidgardLedgerOutputMidgardTxOut = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null => summarizeOutputTxOut(control, "midgard");

export const commitMidgardLedgerOutputReferenceScriptItem = (
  control: MidgardLedgerOutputProofControl,
): Buffer | null => {
  if (
    !isExactMidgardLedgerOutputProofTerminal(control) ||
    control.outputScan.referenceScriptLanguage === -1
  ) {
    return null;
  }
  const totalLength =
    control.totalLength - control.outputScan.referenceScriptItemOffset;
  return commitMidgardBoundedItem({
    fieldIndex: MIDGARD_LEDGER_OUTPUT_PROOF_FIELD_INDEX,
    itemIndex: control.outputIndex,
    totalLength,
    frontier: control.referenceScriptFrontier,
  });
};

const summaryDataCbor = (summary: MidgardCekDataSummary): Buffer =>
  Buffer.concat([
    Buffer.from("d8799f", "hex"),
    aikenSerialisedPlutusDataBytes(Buffer.from(summary.root)),
    encodeCbor(summary.cborLength),
    encodeCbor(summary.memory),
    Buffer.from([0xff]),
  ]);

/**
 * The claimed-summary channel of the finalize dispatchers, computed from the
 * terminal control itself: the serialized `DataSummaryV1` of the output's
 * value and the serialized `Option<DataSummaryV1>` of its datum result.
 * Mirrors `ledger_output_proof_v1.terminal_claimed_summaries_v1`.
 */
export const midgardLedgerOutputProofTerminalClaimedSummaries = (
  control: MidgardLedgerOutputProofControl,
): {
  readonly valueSummaryDataCbor: Buffer;
  readonly datumSummaryDataCbor: Buffer;
} | null => {
  if (!isExactMidgardLedgerOutputProofTerminal(control)) return null;
  const valueSummary = finalizeMidgardLedgerOutputValue(control.value!);
  if (valueSummary === null) return null;
  let datumSummaryDataCbor = Buffer.from("d87a80", "hex");
  if (control.outputScan.datumOffset !== -1 && control.datum !== null) {
    const datumSummary = finalizeMidgardCekDataTraverse(control.datum);
    if (datumSummary !== null) {
      datumSummaryDataCbor = Buffer.concat([
        Buffer.from("d8799f", "hex"),
        summaryDataCbor(datumSummary),
        Buffer.from([0xff]),
      ]);
    }
  }
  return {
    valueSummaryDataCbor: summaryDataCbor(valueSummary),
    datumSummaryDataCbor,
  };
};

/**
 * The canonical fact-attachment groups, in machine order: the two leaf
 * summaries first, then the reference script, then the scan facts, whose
 * yield consumes the three facts recorded before it. Mirrors
 * `ledger_output_proof_v1.fact_attach_groups_v1`.
 */
export const MIDGARD_LEDGER_OUTPUT_PROOF_FACT_ATTACH_GROUPS: readonly (readonly number[])[] =
  Object.freeze([[2, 3], [1], [0]]);

/**
 * `blake2b_256` over the serialized Plutus data list of `payload` (each item
 * already serialized Plutus data): the shape of every descriptor fact
 * commitment. Mirrors `ledger_output_proof_v1.fact_digest_v1`.
 */
export const midgardLedgerOutputProofFactDigest = (
  payload: readonly Uint8Array[],
): Buffer =>
  Buffer.from(
    midgardBlake2b(
      Buffer.concat([
        Buffer.from([0x9f]),
        ...payload.map((item) => Buffer.from(item)),
        Buffer.from([0xff]),
      ]),
      { dkLen: 32 },
    ),
  );

/**
 * One descriptor-fact commitment. Each fact commits only what its own
 * descriptor yield pins: role 3 the value summary, role 2 the datum summary,
 * role 1 the four reference-script descriptor fields, and role 0 the whole
 * descriptor bytes. Mirrors `ledger_output_proof_v1.fact_commitment_v1`.
 */
export const midgardLedgerOutputProofFactCommitment = ({
  role,
  descriptorCbor,
  valueSummaryDataCbor,
  datumSummaryDataCbor,
}: {
  readonly role: number;
  readonly descriptorCbor: Uint8Array;
  readonly valueSummaryDataCbor: Uint8Array;
  readonly datumSummaryDataCbor: Uint8Array;
}): Buffer => {
  if (role === 0) {
    return midgardLedgerOutputProofFactDigest([
      aikenSerialisedPlutusDataBytes(descriptorCbor),
    ]);
  }
  if (role === 1) {
    const descriptor = decodeMidgardLedgerOutputCommitment(descriptorCbor);
    return midgardLedgerOutputProofFactDigest([
      encodeCbor(BigInt(descriptor.referenceScriptLanguage)),
      aikenSerialisedPlutusDataBytes(descriptor.referenceScriptHash),
      encodeCbor(BigInt(descriptor.referenceScriptTotalLength)),
      aikenSerialisedPlutusDataBytes(descriptor.referenceScriptItemCommitment),
    ]);
  }
  if (role === 2) {
    return midgardLedgerOutputProofFactDigest([datumSummaryDataCbor]);
  }
  if (role === 3) {
    return midgardLedgerOutputProofFactDigest([valueSummaryDataCbor]);
  }
  throw new Error("Invalid V1 ledger output proof fact role");
};

export const midgardLedgerOutputProofFact = (
  control: MidgardLedgerOutputProofControl,
  role: number,
): Buffer | null => {
  if (role === 0) return control.scanFactsFact;
  if (role === 1) return control.referenceScriptFact;
  if (role === 2) return control.datumSummaryFact;
  if (role === 3) return control.valueSummaryFact;
  throw new Error("Invalid V1 ledger output proof fact role");
};

export const midgardLedgerOutputProofFactsComplete = (
  control: MidgardLedgerOutputProofControl,
): boolean =>
  control.scanFactsFact !== null &&
  control.referenceScriptFact !== null &&
  control.datumSummaryFact !== null &&
  control.valueSummaryFact !== null;
