/**
 * Plutus Data values: the five constructors and their fields.
 *
 * Derived from @harmoniclabs/plutus-data 2.0.1 (Copyright Harmonic
 * Laboratories, Apache License 2.0,
 * https://github.com/HarmonicLabs/plutus-data). Changes: only the
 * constructors, their fields and coercions are kept; the recursive helpers
 * (`clone`, `toJson`, `toString`, `eqData`, `dataToCbor`, `dataFromCbor`) are
 * left out. Data CBOR is Midgard's own (`plutus-data-iterative.*`).
 *
 * The class names are part of the contract: callers compare
 * `constructor.name` against "DataMap" and "DataI".
 */

const hexByte = /^[0-9a-fA-F]{2}$/u;

/** An odd-length string reads as if it had a leading "0", as upstream does. */
const bytesFromHex = (input: string): Uint8Array => {
  const hex = input.length % 2 === 0 ? input : `0${input}`;
  const bytes = new Uint8Array(hex.length / 2);
  for (let index = 0; index < bytes.length; index += 1) {
    const pair = hex.slice(index * 2, index * 2 + 2);
    if (!hexByte.test(pair)) throw new TypeError("invalid hex string");
    bytes[index] = Number.parseInt(pair, 16);
  }
  return bytes;
};

/** A Plutus Data value. */
export type Data = DataConstr | DataMap<Data, Data> | DataList | DataI | DataB;

/** One entry of a Data map. */
export interface KV<DataKey extends Data, DataValue extends Data> {
  fst: DataKey;
  snd: DataValue;
}

/** A map entry as a class, for callers that build entries with `new`. */
export class DataPair<DataKey extends Data, DataValue extends Data>
  implements KV<DataKey, DataValue>
{
  fst: DataKey;
  snd: DataValue;

  constructor(fst: DataKey, snd: DataValue) {
    this.fst = fst;
    this.snd = snd;
  }
}

/** Whether `value` is one of the five Data constructors (not recursive). */
export const isData = (value: unknown): value is Data =>
  value instanceof DataConstr ||
  value instanceof DataMap ||
  value instanceof DataList ||
  value instanceof DataI ||
  value instanceof DataB;

/** `Constr` with a non-negative alternative and Data fields. */
export class DataConstr {
  readonly constr: bigint;
  readonly fields: Data[];

  constructor(constr: bigint | number, fields: Data[]) {
    const alternative = BigInt(constr);
    if (!(alternative >= 0n && Array.isArray(fields) && fields.every(isData)))
      throw new TypeError("invalid fields passed to constructor");
    this.constr = alternative;
    this.fields = fields;
  }
}

/** `Map` as an ordered list of entries. */
export class DataMap<DataKey extends Data, DataValue extends Data> {
  readonly map: KV<DataKey, DataValue>[];

  constructor(map: KV<DataKey, DataValue>[]) {
    if (
      !(
        Array.isArray(map) &&
        map.every(
          (entry) =>
            typeof entry === "object" &&
            entry !== null &&
            isData(entry.fst) &&
            isData(entry.snd),
        )
      )
    )
      throw new TypeError("invalid map passed to 'DataPair' constructor");
    this.map = map;
  }
}

/** `List`. */
export class DataList {
  readonly list: Data[];

  constructor(list: Data[]) {
    if (!(Array.isArray(list) && list.every(isData)))
      throw new TypeError("invalid list passed to constructor");
    this.list = list;
  }
}

/** `I`: an integer. */
export class DataI {
  readonly int: bigint;

  constructor(int: number | bigint = 0) {
    this.int = BigInt(int);
  }
}

/** `B`: a byte string, from bytes or their hex. The bytes are not copied. */
export class DataB {
  readonly bytes: Uint8Array;

  constructor(bytes: Uint8Array | string) {
    const value = typeof bytes === "string" ? bytesFromHex(bytes) : bytes;
    if (!(value instanceof Uint8Array))
      throw new TypeError("invalid bytes passed to constructor");
    this.bytes = value;
  }
}
