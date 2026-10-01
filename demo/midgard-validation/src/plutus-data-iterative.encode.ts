import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
} from "@harmoniclabs/plutus-data";

import {
  encodeCardanoBytes,
  encodeCardanoInteger,
  encodeSmallCborArgument,
  type MidgardCekDataIntegerLayout,
} from "./cek-constant.semantic-data.js";
import { isPlutusDataMap } from "./plutus-data-narrowing.js";

const EMPTY_LIST = Buffer.from([0x80]);
const OPEN_LIST = Buffer.from([0x9f]);
const BREAK = Buffer.from([0xff]);
/** Work-stack marker for "emit the closing break of an open list". */
const CLOSE_LIST: unique symbol = Symbol("close list");
const TAG_102_PAIR = Buffer.from([0x82]);

/**
 * Exact `cbor.serialise(Data)`/cardano-node representation. The upstream
 * harmonic serializer loses every byte after the first 64 in dynamic byte
 * strings and rejects negative bignums below the uint64 major-1 domain, so
 * consensus code encodes both scalar classes directly.
 *
 * The walk keeps its own stack, so any nesting depth encodes: lists and
 * constructor fields are indefinite (`9f ... ff`) unless empty (`80`), maps
 * are definite, and constructors use tags 121-127, 1280-1400 or 102. A bignum
 * magnitude over 64 bytes is chunked as Cardano writes it unless the caller
 * selects another `integerLayout` (see `MidgardCekDataIntegerLayout`).
 */
export const encodeMidgardCekPlutusData = (
  data: Data,
  options: { readonly integerLayout?: MidgardCekDataIntegerLayout } = {},
): Buffer => {
  const integerLayout = options.integerLayout ?? "cardanoChunked";
  const out: Buffer[] = [];
  // Work items in reverse emission order: a Data node still to encode, or the
  // marker that closes an open list.
  const work: (Data | typeof CLOSE_LIST)[] = [data];
  const pushList = (items: readonly Data[]): void => {
    if (items.length === 0) {
      out.push(EMPTY_LIST);
      return;
    }
    out.push(OPEN_LIST);
    work.push(CLOSE_LIST);
    for (let index = items.length - 1; index >= 0; index -= 1) {
      work.push(items[index]!);
    }
  };
  while (work.length > 0) {
    const next = work.pop()!;
    if (next === CLOSE_LIST) {
      out.push(BREAK);
    } else if (next instanceof DataI) {
      out.push(encodeCardanoInteger(next.int, integerLayout));
    } else if (next instanceof DataB) {
      out.push(encodeCardanoBytes(next.bytes));
    } else if (next instanceof DataList) {
      pushList(next.list);
    } else if (isPlutusDataMap(next)) {
      out.push(encodeSmallCborArgument(5, BigInt(next.map.length)));
      for (let index = next.map.length - 1; index >= 0; index -= 1) {
        const entry = next.map[index]!;
        work.push(entry.snd, entry.fst);
      }
    } else if (next instanceof DataConstr) {
      if (next.constr <= 6n) {
        out.push(encodeSmallCborArgument(6, 121n + next.constr));
      } else if (next.constr <= 127n) {
        out.push(encodeSmallCborArgument(6, 1280n + next.constr - 7n));
      } else {
        out.push(
          encodeSmallCborArgument(6, 102n),
          TAG_102_PAIR,
          encodeCardanoInteger(next.constr, integerLayout),
        );
      }
      pushList(next.fields);
    } else {
      throw new Error("V1 constant contains unknown Plutus Data");
    }
  }
  return Buffer.concat(out);
};
