import { encodeMidgardCekPlutusData } from "../../src/cek-constant.js";
import { constT, UPLCConst } from "../../src/uplc-constant.js";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "../../src/uplc-data.js";
import { type UPLCTerm } from "../../src/uplc-term.js";
import {
  app,
  force,
  int,
  lam,
  type MidgardCekPreparedProgram,
  prepareMidgardCekProgram,
} from "./cek-uplc-terms.js";
import { builtin } from "./uplc-builtins.js";

/** The bound on a direct constant witness payload, in bytes. */
export const DIRECT_BOUND = 9_215;

export const bytes = (length: number, seed: number): Uint8Array =>
  Uint8Array.from({ length }, (_, index) => (seed + index * 31) & 0xff);
export const B = (length: number, seed = 1): DataB =>
  new DataB(bytes(length, seed));
export const I = (value: bigint): DataI => new DataI(value);
export const L = (items: readonly Data[]): DataList => new DataList([...items]);
export const C = (
  index: number | bigint,
  fields: readonly Data[],
): DataConstr => new DataConstr(index, [...fields]);
export const M = (
  entries: readonly (readonly [Data, Data])[],
): DataMap<Data, Data> =>
  new DataMap(entries.map(([key, value]) => new DataPair(key, value)));

export const dataSize = (value: Data): number =>
  encodeMidgardCekPlutusData(value).length;

/** A Data list of `B(70)` items whose encoding is at most `target` bytes. */
export const listUnder = (target: number): Data[] => {
  const items: Data[] = [];
  while (dataSize(L([...items, B(70, items.length)])) <= target) {
    items.push(B(70, items.length));
  }
  return items;
};

/**
 * A `B` item and a base list (`base`, or `base` behind one empty byte
 * string) that together encode to exactly `target` bytes.
 */
export const fillTo = (
  base: readonly Data[],
  target: number,
): { readonly item: DataB; readonly base: readonly Data[] } => {
  for (const candidate of [base, [B(0), ...base]]) {
    for (let length = 0; length < 4_096; length += 1) {
      const item = B(length, 7);
      if (dataSize(L([item, ...candidate])) === target) {
        return { item, base: candidate };
      }
    }
  }
  throw new Error(`no byte string brings the list to ${target} bytes`);
};

export const bigInteger = 2n ** 600n;
export const BASE_ITEMS = listUnder(DIRECT_BOUND - 120);
export const LARGE_LIST = L([...BASE_ITEMS, ...listUnder(4_000)]);
export const SMALL_CONTEXT = C(0, [I(1n), B(4), L([I(2n)])]);
export const LARGE_CONTEXT = C(0, [
  LARGE_LIST,
  M([
    [I(1n), B(80)],
    [B(3), LARGE_LIST],
  ]),
  I(-bigInteger),
  B(65),
]);

export const dataList = (items: readonly Data[]): UPLCTerm =>
  UPLCConst.listOf(constT.data)([...items]);
export const bytesConst = (length: number, seed = 1): UPLCTerm =>
  UPLCConst.byteString(new DataB(bytes(length, seed)).bytes);

export const chooseList = (
  list: UPLCTerm,
  whenEmpty: UPLCTerm,
  otherwise: UPLCTerm,
): UPLCTerm =>
  app(force(force(builtin("chooseList"))), list, whenEmpty, otherwise);
export const chooseData = (value: UPLCTerm): UPLCTerm =>
  app(
    force(builtin("chooseData")),
    value,
    int(0),
    int(1),
    int(2),
    int(3),
    int(4),
  );
export const listData = (list: UPLCTerm): UPLCTerm =>
  app(builtin("listData"), list);
export const unListData = (value: UPLCTerm): UPLCTerm =>
  app(builtin("unListData"), value);
export const constrData = (index: UPLCTerm, fields: UPLCTerm): UPLCTerm =>
  app(builtin("constrData"), index, fields);
export const unConstrData = (value: UPLCTerm): UPLCTerm =>
  app(builtin("unConstrData"), value);
export const mapData = (pairs: UPLCTerm): UPLCTerm =>
  app(builtin("mapData"), pairs);
export const unMapData = (value: UPLCTerm): UPLCTerm =>
  app(builtin("unMapData"), value);
export const mkPairData = (first: UPLCTerm, second: UPLCTerm): UPLCTerm =>
  app(builtin("mkPairData"), first, second);
export const equalsData = (left: UPLCTerm, right: UPLCTerm): UPLCTerm =>
  app(builtin("equalsData"), left, right);
export const serialiseData = (value: UPLCTerm): UPLCTerm =>
  app(builtin("serialiseData"), value);
export const iData = (value: UPLCTerm): UPLCTerm =>
  app(builtin("iData"), value);
export const bData = (value: UPLCTerm): UPLCTerm =>
  app(builtin("bData"), value);
export const unIData = (value: UPLCTerm): UPLCTerm =>
  app(builtin("unIData"), value);
export const unBData = (value: UPLCTerm): UPLCTerm =>
  app(builtin("unBData"), value);
export const letIn = (value: UPLCTerm, body: UPLCTerm): UPLCTerm =>
  app(lam(body), value);

export type MidgardCekDifferentialCase = {
  readonly label: string;
  readonly prepare: () => MidgardCekPreparedProgram;
};

export const program = (
  label: string,
  body: UPLCTerm,
  context: Data = SMALL_CONTEXT,
): MidgardCekDifferentialCase => ({
  label,
  prepare: () => prepareMidgardCekProgram(lam(body), context),
});
