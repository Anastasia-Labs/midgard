import {
  type Bytes,
  exactHash,
  hashMidgardCekDataNode,
  type MidgardCekDataNode,
  uint32,
  uint64,
} from "./cek-semantic.decode-midgard-cek-data-node.js";
import {
  midgardCekDataConstrCborLength,
  midgardCekDataListCborLength,
  midgardCekDataMapCborLength,
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
} from "./cek-semantic.decode-midgard-cek-data-pair-node.js";

const summarizeMidgardCekDataNode = (
  node: MidgardCekDataNode,
): MidgardCekDataSummary => ({
  root: hashMidgardCekDataNode(node),
  cborLength: node.cborLength,
  memory: node.memory,
});

export const summarizeMidgardCekSmallConstrData = (
  constructor: bigint,
  fields: MidgardCekDataSequenceSummary,
): MidgardCekDataSummary =>
  summarizeMidgardCekDataNode({
    kind: "constrSmall",
    constructor,
    fieldsCount: fields.length,
    fieldsRoot: fields.root,
    cborLength: midgardCekDataConstrCborLength(
      constructor,
      fields.length,
      fields.payloadCborLength,
    ),
    memory: 4n + fields.memory,
  });

/**
 * Builds the semantic node for a constructor above 127 without materializing
 * its arbitrary-size alternative on L1. The authenticated integer submachine
 * supplies the canonical CBOR root, exact length, and exact integer memory.
 */
export const summarizeMidgardCekLargeConstrData = ({
  constructorCborRoot,
  constructorCborLength,
  constructorMemory,
  fields,
}: {
  readonly constructorCborRoot: Bytes;
  readonly constructorCborLength: bigint;
  readonly constructorMemory: bigint;
  readonly fields: MidgardCekDataSequenceSummary;
}): MidgardCekDataSummary => {
  exactHash(constructorCborRoot, "cek_data.constr_large.constructor_cbor_root");
  uint32(
    constructorCborLength,
    "cek_data.constr_large.constructor_cbor_length",
  );
  uint64(constructorMemory, "cek_data.constr_large.constructor_memory");
  if (constructorCborLength === 0n) {
    throw new RangeError(
      "cek_data.constr_large.constructor_cbor_length must be positive",
    );
  }
  if (constructorMemory < 5n) {
    throw new RangeError(
      "cek_data.constr_large.constructor_memory must be at least 5",
    );
  }
  const fieldsCborLength = midgardCekDataListCborLength(
    fields.length,
    fields.payloadCborLength,
  );
  return summarizeMidgardCekDataNode({
    kind: "constrLarge",
    constructorCborRoot,
    constructorCborLength,
    constructorMemory,
    fieldsCount: fields.length,
    fieldsRoot: fields.root,
    cborLength: 3n + constructorCborLength + fieldsCborLength,
    memory: 4n + fields.memory,
  });
};

export const summarizeMidgardCekListData = (
  items: MidgardCekDataSequenceSummary,
): MidgardCekDataSummary =>
  summarizeMidgardCekDataNode({
    kind: "list",
    itemsCount: items.length,
    itemsRoot: items.root,
    cborLength: midgardCekDataListCborLength(
      items.length,
      items.payloadCborLength,
    ),
    memory: 4n + items.memory,
  });

export const summarizeMidgardCekMapData = (
  entries: MidgardCekDataSequenceSummary,
): MidgardCekDataSummary =>
  summarizeMidgardCekDataNode({
    kind: "map",
    entriesCount: entries.length,
    entriesRoot: entries.root,
    cborLength: midgardCekDataMapCborLength(
      entries.length,
      entries.payloadCborLength,
    ),
    memory: 4n + entries.memory,
  });
