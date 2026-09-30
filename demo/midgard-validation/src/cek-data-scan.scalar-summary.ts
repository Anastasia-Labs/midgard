import {
  buildMidgardValidationMerkleFrontier,
  commitMidgardCekBlob,
  encodeCbor,
  hashMidgardCekDataNode,
  midgardCekDataBytesCborLength,
  midgardCekDataBytesMemory,
  type MidgardCekDataNode,
  summarizeMidgardCekLargeConstrData,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
  summarizeMidgardCekSmallConstrData,
} from "@al-ft/midgard-core";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
} from "@harmoniclabs/plutus-data";

import {
  encodeMidgardCekPlutusData,
  midgardCekIntegerMemorySize,
} from "./cek-constant.js";
import {
  boundedNatural,
  CHILD_DOMAIN,
  hash32,
  type MidgardCekDataScanFrame,
  validateSummary,
} from "./cek-data-scan.validate-midgard-cek-data-scan-frame.js";
import {
  isByteStringLike,
  type PlutusDataMap,
} from "./plutus-data-narrowing.js";
import {
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
} from "./script-context-proof.js";

export const hashMidgardCekDataScanChild = (
  childIndex: number,
  child: MidgardCekDataSummary,
): Buffer => {
  boundedNatural(childIndex, "cek_data_scan_child.index");
  validateSummary(child, "cek_data_scan_child.summary", false);
  return hash32(
    Buffer.concat([
      CHILD_DOMAIN,
      encodeCbor(BigInt(childIndex)),
      encodeCbor(Buffer.from(child.root)),
      encodeCbor(child.cborLength),
      encodeCbor(child.memory),
    ]),
  );
};

export type MutableFrame = {
  frame: MidgardCekDataScanFrame;
  children: MidgardCekDataSummary[];
};

export type StructuredData = DataConstr | DataList | PlutusDataMap;

export type ScanWork =
  | {
      readonly kind: "enter";
      readonly data: Data;
      readonly parent: MutableFrame | null;
    }
  | {
      readonly kind: "exit";
      readonly data: StructuredData;
      readonly frame: MutableFrame;
      readonly parent: MutableFrame | null;
    };

export const replaceFrame = (
  target: MutableFrame,
  source: MutableFrame,
): void => {
  target.frame = source.frame;
  target.children = source.children;
};

export const frameWith = (
  value: MutableFrame,
  foldCursor: number,
  sequence: MidgardCekDataSequenceSummary,
): MutableFrame => ({
  frame: { ...value.frame, foldCursor, sequence },
  children: value.children,
});

export const appendChild = (
  value: MutableFrame,
  child: MidgardCekDataSummary,
): MutableFrame => {
  const children = [...value.children, child];
  const leaves = children.map((item, index) =>
    hashMidgardCekDataScanChild(index, item),
  );
  return {
    frame: {
      ...value.frame,
      childCount: children.length,
      childFrontier: buildMidgardValidationMerkleFrontier(leaves),
    },
    children,
  };
};

export const mapHeaderLength = (pairs: number): number => {
  if (pairs < 24) return 1;
  if (pairs <= 0xff) return 2;
  if (pairs <= 0xffff) return 3;
  if (pairs <= 0xffff_ffff) return 5;
  return 9;
};

export const constructorHeaderLength = (constructor: bigint): number => {
  if (constructor <= 6n) return 3;
  if (constructor <= 127n) return 4;
  return 4 + encodeMidgardCekPlutusData(new DataI(constructor)).length;
};

export const scalarBytes = (value: DataB): Uint8Array => {
  const candidate: unknown = value.bytes;
  if (!isByteStringLike(candidate)) {
    throw new Error("CEK Data scanner received an invalid byte leaf");
  }
  const bytes = candidate.toBuffer();
  if (!(bytes instanceof Uint8Array)) {
    throw new Error("CEK Data scanner byte leaf did not produce bytes");
  }
  return bytes;
};

export const scalarSummary = (data: DataI | DataB): MidgardCekDataSummary => {
  let node: MidgardCekDataNode;
  if (data instanceof DataI) {
    const cbor = encodeMidgardCekPlutusData(data);
    node = {
      kind: "integer",
      cborRoot: commitMidgardCekBlob(cbor).root,
      cborLength: BigInt(cbor.length),
      memory: 4n + midgardCekIntegerMemorySize(data.int),
    };
  } else {
    const bytes = scalarBytes(data);
    node = {
      kind: "bytes",
      bytesRoot: commitMidgardCekBlob(bytes).root,
      bytesLength: BigInt(bytes.length),
      cborLength: midgardCekDataBytesCborLength(BigInt(bytes.length)),
      memory: midgardCekDataBytesMemory(BigInt(bytes.length)),
    };
  }
  return {
    root: Buffer.from(hashMidgardCekDataNode(node)),
    cborLength: node.cborLength,
    memory: node.memory,
  };
};

export const structuredSummary = (
  data: StructuredData,
  sequence: MidgardCekDataSequenceSummary,
): MidgardCekDataSummary => {
  if (data instanceof DataConstr) {
    if (data.constr <= 127n) {
      return summarizeMidgardCekSmallConstrData(data.constr, sequence);
    }
    const constructorCbor = encodeMidgardCekPlutusData(new DataI(data.constr));
    return summarizeMidgardCekLargeConstrData({
      constructorCborRoot: commitMidgardCekBlob(constructorCbor).root,
      constructorCborLength: BigInt(constructorCbor.length),
      constructorMemory: 4n + midgardCekIntegerMemorySize(data.constr),
      fields: sequence,
    });
  }
  if (data instanceof DataList) {
    return summarizeMidgardCekListData(sequence);
  }
  return summarizeMidgardCekMapData(sequence);
};
