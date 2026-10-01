/**
 * The recursive functions the iterative Plutus Data code replaced, kept here
 * verbatim as differential oracles (and nowhere in production source), plus
 * an explicit-stack structural comparison for harmonic `Data`.
 */
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
} from "@harmoniclabs/plutus-data";

import {
  encodeCardanoBytes,
  encodeCardanoInteger,
  encodeSmallCborArgument,
} from "../src/cek-constant.semantic-data.js";
import { midgardCekIntegerMemorySize } from "../src/plutus-data-iterative.memory.js";
import { isPlutusDataMap } from "../src/plutus-data-narrowing.js";

const encodeCardanoList = (items: readonly Buffer[]): Buffer =>
  items.length === 0
    ? Buffer.from([0x80])
    : Buffer.concat([Buffer.from([0x9f]), ...items, Buffer.from([0xff])]);

/** The recursive `encodeMidgardCekPlutusData` before the iterative rewrite. */
export const recursiveEncodeMidgardCekPlutusData = (data: Data): Buffer => {
  if (data instanceof DataI) {
    return encodeCardanoInteger(data.int);
  }
  if (data instanceof DataB) {
    return encodeCardanoBytes(data.bytes);
  }
  if (data instanceof DataList) {
    return encodeCardanoList(
      data.list.map((item) => recursiveEncodeMidgardCekPlutusData(item)),
    );
  }
  if (isPlutusDataMap(data)) {
    return Buffer.concat([
      encodeSmallCborArgument(5, BigInt(data.map.length)),
      ...data.map.flatMap((entry) => [
        recursiveEncodeMidgardCekPlutusData(entry.fst),
        recursiveEncodeMidgardCekPlutusData(entry.snd),
      ]),
    ]);
  }
  if (data instanceof DataConstr) {
    const fields = encodeCardanoList(
      data.fields.map((field) => recursiveEncodeMidgardCekPlutusData(field)),
    );
    if (data.constr <= 6n) {
      return Buffer.concat([
        encodeSmallCborArgument(6, 121n + data.constr),
        fields,
      ]);
    }
    if (data.constr <= 127n) {
      return Buffer.concat([
        encodeSmallCborArgument(6, 1280n + data.constr - 7n),
        fields,
      ]);
    }
    return Buffer.concat([
      encodeSmallCborArgument(6, 102n),
      Buffer.from([0x82]),
      encodeCardanoInteger(data.constr),
      fields,
    ]);
  }
  throw new Error("V1 constant contains unknown Plutus Data");
};

const byteLengthOrOne = (bytes: Uint8Array): bigint =>
  BigInt(Math.max(1, bytes.length));

/** The recursive `midgardCekDataMemorySize` before the iterative rewrite. */
export const recursiveMidgardCekDataMemorySize = (value: Data): bigint => {
  if (value instanceof DataConstr) {
    return (
      4n +
      value.fields.reduce(
        (total, field) => total + recursiveMidgardCekDataMemorySize(field),
        0n,
      )
    );
  }
  if (isPlutusDataMap(value)) {
    return (
      4n +
      value.map.reduce(
        (total, entry) =>
          total +
          recursiveMidgardCekDataMemorySize(entry.fst) +
          recursiveMidgardCekDataMemorySize(entry.snd),
        0n,
      )
    );
  }
  if (value instanceof DataList) {
    return (
      4n +
      value.list.reduce(
        (total, item) => total + recursiveMidgardCekDataMemorySize(item),
        0n,
      )
    );
  }
  if (value instanceof DataI) {
    return 4n + midgardCekIntegerMemorySize(value.int);
  }
  if (value instanceof DataB) {
    return 4n + byteLengthOrOne(value.bytes);
  }
  throw new Error("V1 data constant has an unknown node");
};

const describeBytes = (value: DataB): string => {
  const raw = value.bytes;
  return `${Buffer.isBuffer(raw) ? "Buffer" : raw.constructor.name}:${Buffer.from(raw).toString("hex")}`;
};

/**
 * Structural difference between two harmonic values, or undefined when they
 * are equal: same classes, constructor numbers, integers, byte strings (their
 * array class included), list lengths and map entries in order. Walks with
 * its own stack, so deep values compare.
 */
export const harmonicDataDifference = (
  expected: Data,
  actual: Data,
): string | undefined => {
  const work: [Data, Data, string][] = [[expected, actual, "$"]];
  while (work.length > 0) {
    const [left, right, path] = work.pop()!;
    if (left.constructor !== right.constructor) {
      return `${path}: ${left.constructor.name} vs ${right.constructor.name}`;
    }
    if (left instanceof DataI) {
      if (left.int !== (right as DataI).int) return `${path}: integer`;
    } else if (left instanceof DataB) {
      const a = describeBytes(left);
      const b = describeBytes(right as DataB);
      if (a !== b) return `${path}: bytes ${a} vs ${b}`;
    } else if (left instanceof DataList) {
      const other = (right as DataList).list;
      if (left.list.length !== other.length) return `${path}: list length`;
      left.list.forEach((item, index) => {
        work.push([item, other[index]!, `${path}[${index.toString()}]`]);
      });
    } else if (left instanceof DataMap) {
      const mine = (left as DataMap<Data, Data>).map;
      const other = (right as DataMap<Data, Data>).map;
      if (mine.length !== other.length) return `${path}: map length`;
      mine.forEach((entry, index) => {
        work.push(
          [entry.fst, other[index]!.fst, `${path}{${index.toString()}}.k`],
          [entry.snd, other[index]!.snd, `${path}{${index.toString()}}.v`],
        );
      });
    } else if (left instanceof DataConstr) {
      const other = right as DataConstr;
      if (left.constr !== other.constr) return `${path}: constructor`;
      if (left.fields.length !== other.fields.length) {
        return `${path}: field count`;
      }
      left.fields.forEach((field, index) => {
        work.push([field, other.fields[index]!, `${path}.${index.toString()}`]);
      });
    } else {
      return `${path}: unknown node`;
    }
  }
  return undefined;
};
