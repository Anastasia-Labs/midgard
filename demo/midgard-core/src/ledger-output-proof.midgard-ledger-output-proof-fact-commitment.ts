import { blake2b } from "@noble/hashes/blake2.js";

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
import { encodeCbor } from "./codec/cbor.js";
import { type MidgardLedgerOutputDataSummary } from "./ledger-output-commitment.js";
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

export const summarizeMidgardLedgerOutputCardanoSpendDatum = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null => {
  if (!isExactMidgardLedgerOutputProofTerminal(control)) {
    return null;
  }
  if (control.outputScan.datumOffset === -1) {
    return summarizeMidgardCekSmallConstrData(
      1n,
      emptyMidgardCekDataListSummary(),
    );
  }
  const datum = finalizeMidgardCekDataTraverse(control.datum!);
  return datum === null
    ? null
    : summarizeMidgardCekSmallConstrData(
        0n,
        prependMidgardCekDataListSummary(
          datum,
          emptyMidgardCekDataListSummary(),
        ),
      );
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
  control: MidgardLedgerOutputProofControl,
  encoding: "cardano" | "midgard",
): MidgardCekDataSummary => {
  const address = decodeMidgardAddressBytes(control.outputScan.address);
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

const summarizeOutputDatum = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null => {
  if (control.outputScan.datumOffset === -1) {
    return summarizeSmallConstr(0n, []);
  }
  const datum = finalizeMidgardCekDataTraverse(control.datum!);
  return datum === null ? null : summarizeSmallConstr(2n, [datum]);
};

const summarizeOutputReferenceScript = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null => {
  if (control.outputScan.referenceScriptLanguage === -1) {
    return summarizeSmallConstr(1n, []);
  }
  const digest = digestMidgardLedgerOutputReferenceScript(control);
  return digest === null
    ? null
    : summarizeSmallConstr(0n, [summarizeDirectBytesData(digest)]);
};

const summarizeOutputTxOut = (
  control: MidgardLedgerOutputProofControl,
  encoding: "cardano" | "midgard",
): MidgardCekDataSummary | null => {
  if (!isExactMidgardLedgerOutputProofTerminal(control)) {
    return null;
  }
  const value = finalizeMidgardLedgerOutputValue(control.value!);
  const datum = summarizeOutputDatum(control);
  const referenceScript = summarizeOutputReferenceScript(control);
  return value === null || datum === null || referenceScript === null
    ? null
    : summarizeSmallConstr(0n, [
        summarizeOutputAddress(control, encoding),
        value,
        datum,
        referenceScript,
      ]);
};

export const summarizeMidgardLedgerOutputCardanoTxOut = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null => summarizeOutputTxOut(control, "cardano");

export const summarizeMidgardLedgerOutputMidgardTxOut = (
  control: MidgardLedgerOutputProofControl,
): MidgardCekDataSummary | null => summarizeOutputTxOut(control, "midgard");

export const summariesEqual = (
  left: MidgardCekDataSummary,
  right: MidgardLedgerOutputDataSummary,
): boolean =>
  Buffer.from(left.root).equals(Buffer.from(right.root)) &&
  left.cborLength === right.cborLength &&
  left.memory === right.memory;

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
 * summaries first, then the composite scan facts, then the reference script.
 */
export const MIDGARD_LEDGER_OUTPUT_PROOF_FACT_ATTACH_GROUPS: readonly (readonly number[])[] =
  Object.freeze([[2, 3], [0], [1]]);

/**
 * One descriptor-fact commitment, binding the descriptor bytes and the
 * claimed summaries relevant to the role. Mirrors
 * `ledger_output_proof_v1.fact_commitment_v1`: `blake2b_256` over the
 * serialized Plutus data list of the role's payload.
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
  const descriptorData = aikenSerialisedPlutusDataBytes(descriptorCbor);
  let payload: readonly Uint8Array[];
  if (role === 0) {
    payload = [descriptorData, valueSummaryDataCbor, datumSummaryDataCbor];
  } else if (role === 1) {
    payload = [descriptorData];
  } else if (role === 2) {
    payload = [descriptorData, datumSummaryDataCbor];
  } else if (role === 3) {
    payload = [descriptorData, valueSummaryDataCbor];
  } else {
    throw new Error("Invalid V1 ledger output proof fact role");
  }
  const payloadCbor = Buffer.concat([
    Buffer.from([0x9f]),
    ...payload.map((item) => Buffer.from(item)),
    Buffer.from([0xff]),
  ]);
  return Buffer.from(blake2b(payloadCbor, { dkLen: 32 }));
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
