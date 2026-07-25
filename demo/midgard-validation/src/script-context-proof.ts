import {
  hashMidgardCekDataListNodeV1,
  hashMidgardCekDataNodeV1,
  hashMidgardCekDataPairNodeV1,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1,
  midgardCekDataConstrCborLengthV1,
  midgardCekDataListCborLengthV1,
  type MidgardCekDataListNodeV1,
  midgardCekDataMapCborLengthV1,
  type MidgardCekDataNodeV1,
  type MidgardCekDataPairNodeV1,
} from "@al-ft/midgard-core";
import type { MidgardTxOutput } from "@al-ft/midgard-core/codec";
import { dataFromCbor } from "@harmoniclabs/plutus-data";
import { Data, fromHex } from "@lucid-evolution/lucid";

import {
  commitMidgardCekDataTreeV1,
  type MidgardCekDataTreeCommitmentV1,
} from "./cek-data-tree.js";
import {
  type ScriptContextAddressEncoding,
  scriptContextTxInInfoData,
  scriptContextTxOutData,
} from "./script-context.js";

export type MidgardCekDataSummaryV1 = {
  readonly root: Uint8Array;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

export type MidgardCekDataSequenceSummaryV1 = {
  readonly root: Uint8Array;
  readonly length: bigint;
  readonly payloadCborLength: bigint;
  readonly memory: bigint;
};

export const emptyMidgardCekDataListSummaryV1 =
  (): MidgardCekDataSequenceSummaryV1 => ({
    root: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1,
    length: 0n,
    payloadCborLength: 0n,
    memory: 0n,
  });

export const prependMidgardCekDataListSummaryV1 = (
  head: MidgardCekDataSummaryV1,
  tail: MidgardCekDataSequenceSummaryV1,
): MidgardCekDataSequenceSummaryV1 => {
  const node: MidgardCekDataListNodeV1 = {
    head: head.root,
    headCborLength: head.cborLength,
    headMemory: head.memory,
    tail: tail.root,
    length: tail.length + 1n,
    payloadCborLength: head.cborLength + tail.payloadCborLength,
    memory: head.memory + tail.memory,
  };
  return {
    root: hashMidgardCekDataListNodeV1(node),
    length: node.length,
    payloadCborLength: node.payloadCborLength,
    memory: node.memory,
  };
};

export const emptyMidgardCekDataPairSummaryV1 =
  (): MidgardCekDataSequenceSummaryV1 => ({
    root: MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1,
    length: 0n,
    payloadCborLength: 0n,
    memory: 0n,
  });

export const prependMidgardCekDataPairSummaryV1 = (
  key: MidgardCekDataSummaryV1,
  value: MidgardCekDataSummaryV1,
  tail: MidgardCekDataSequenceSummaryV1,
): MidgardCekDataSequenceSummaryV1 => {
  const node: MidgardCekDataPairNodeV1 = {
    key: key.root,
    keyCborLength: key.cborLength,
    keyMemory: key.memory,
    value: value.root,
    valueCborLength: value.cborLength,
    valueMemory: value.memory,
    tail: tail.root,
    length: tail.length + 1n,
    payloadCborLength:
      key.cborLength + value.cborLength + tail.payloadCborLength,
    memory: key.memory + value.memory + tail.memory,
  };
  return {
    root: hashMidgardCekDataPairNodeV1(node),
    length: node.length,
    payloadCborLength: node.payloadCborLength,
    memory: node.memory,
  };
};

const summarizeNode = (
  node: MidgardCekDataNodeV1,
): MidgardCekDataSummaryV1 => ({
  root: hashMidgardCekDataNodeV1(node),
  cborLength: node.cborLength,
  memory: node.memory,
});

export const summarizeMidgardCekSmallConstrDataV1 = (
  constructor: bigint,
  fields: MidgardCekDataSequenceSummaryV1,
): MidgardCekDataSummaryV1 =>
  summarizeNode({
    kind: "constrSmall",
    constructor,
    fieldsCount: fields.length,
    fieldsRoot: fields.root,
    cborLength: midgardCekDataConstrCborLengthV1(
      constructor,
      fields.length,
      fields.payloadCborLength,
    ),
    memory: 4n + fields.memory,
  });

export const summarizeMidgardCekListDataV1 = (
  items: MidgardCekDataSequenceSummaryV1,
): MidgardCekDataSummaryV1 =>
  summarizeNode({
    kind: "list",
    itemsCount: items.length,
    itemsRoot: items.root,
    cborLength: midgardCekDataListCborLengthV1(
      items.length,
      items.payloadCborLength,
    ),
    memory: 4n + items.memory,
  });

export const summarizeMidgardCekMapDataV1 = (
  entries: MidgardCekDataSequenceSummaryV1,
): MidgardCekDataSummaryV1 =>
  summarizeNode({
    kind: "map",
    entriesCount: entries.length,
    entriesRoot: entries.root,
    cborLength: midgardCekDataMapCborLengthV1(
      entries.length,
      entries.payloadCborLength,
    ),
    memory: 4n + entries.memory,
  });

/**
 * Commits the exact `TxOut` Data subtree used by PlutusV3/MidgardV1 context
 * construction. The source output remains the independently bounded,
 * canonically decoded ledger preimage; this commitment is the semantic bridge
 * that later context-building steps can append without re-revealing the whole
 * transaction.
 */
export const commitMidgardScriptContextTxOutV1 = (
  output: MidgardTxOutput,
  addressEncoding: ScriptContextAddressEncoding,
): MidgardCekDataTreeCommitmentV1 => {
  const contextOutput = scriptContextTxOutData(output, addressEncoding);
  const cbor = fromHex(Data.to(contextOutput as never));
  return commitMidgardCekDataTreeV1(dataFromCbor(cbor));
};

export const commitMidgardScriptContextTxInInfoV1 = (
  outRefHex: string,
  output: MidgardTxOutput,
  addressEncoding: ScriptContextAddressEncoding,
): MidgardCekDataTreeCommitmentV1 => {
  const contextInput = scriptContextTxInInfoData(
    { outRefHex, output },
    addressEncoding,
  );
  const cbor = fromHex(Data.to(contextInput as never));
  return commitMidgardCekDataTreeV1(dataFromCbor(cbor));
};
