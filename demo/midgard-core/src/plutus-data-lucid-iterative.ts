import { Constr, type Data } from "@lucid-evolution/lucid";

import { PlutusDataStructureIds } from "./plutus-data-lucid-iterative.structure-ids.js";

export { lucidDataToCborIterative } from "./plutus-data-lucid-iterative.to-cbor.js";

/**
 * Lucid-shaped Plutus Data from CBOR, without recursion.
 *
 * `lucidDataFromCborIterative` returns what Lucid's `Data.from(cbor)` returns
 * (it decodes through CML's `PlutusData.from_cbor_hex`), for the same inputs:
 *
 * - integers in any head width, and bignums (tags 2 and 3) over byte strings;
 * - byte strings of at most 64 bytes, or indefinite strings of such chunks;
 * - lists and maps, definite or indefinite; a break where a list item or map
 *   key is due closes even a definite container early, consuming the break;
 * - constructors under tags 121 to 127, 1280 to 1400, or 102 over an array of
 *   at least two items (an unsigned alternative and a field list); the items
 *   after those two are left in the stream for the enclosing item to read, as
 *   CML does, and an indefinite array must close right after the fields;
 * - the alternative as a JS number, rounded like Lucid's `parseInt`;
 * - bytes after the first item ignored;
 * - duplicate map keys resolved as Lucid resolves them: each key maps to the
 *   value of the first entry whose key is structurally equal (encoding
 *   ignored), so primitive keys collapse into their first position.
 *
 * One difference is unobservable through values: where Lucid decodes that
 * first value again for each duplicate key, this reader shares one object.
 */

type ParsedNode =
  | { readonly kind: "int"; readonly value: bigint }
  | { readonly kind: "bytes"; readonly hex: string }
  | { readonly kind: "list" | "map"; readonly children: number[] }
  | {
      readonly kind: "constr";
      readonly alternative: bigint;
      readonly children: number[];
    };

type ParseFrame = {
  readonly children: number[];
  readonly isMap: boolean;
  /** Items left in a definite container, or `undefined` when indefinite. */
  remaining: bigint | undefined;
  /** A tag-102 constructor over an indefinite array closes after its fields. */
  readonly breakAfter: boolean;
};

type Head = {
  readonly major: number;
  /** The argument, or `undefined` for an indefinite-length head. */
  readonly argument: bigint | undefined;
};

const MAX_CHUNK_BYTES = 64n;

const fail = (reason: string): never => {
  throw new Error(`Plutus Data CBOR is invalid: ${reason}`);
};

class DataCborReader {
  offset = 0;
  constructor(readonly bytes: Uint8Array) {}

  peek(): number {
    const byte = this.bytes[this.offset];
    return byte === undefined ? fail("unexpected end of input") : byte;
  }

  head(): Head {
    const initial = this.peek();
    this.offset += 1;
    const major = initial >> 5;
    const info = initial & 0x1f;
    if (info < 24) return { major, argument: BigInt(info) };
    if (info === 31) return { major, argument: undefined };
    if (info > 27) return fail("reserved additional information");
    const width = 1 << (info - 24);
    if (this.offset + width > this.bytes.length) {
      return fail("unexpected end of input");
    }
    let argument = 0n;
    for (let index = 0; index < width; index += 1) {
      argument = (argument << 8n) | BigInt(this.bytes[this.offset + index]!);
    }
    this.offset += width;
    return { major, argument };
  }

  definite(head: Head): bigint {
    return head.argument ?? fail("unexpected indefinite length");
  }

  chunk(head: Head): string {
    const length = this.definite(head);
    if (head.major !== 2 || length > MAX_CHUNK_BYTES) {
      return fail("byte chunk is not a byte string of at most 64 bytes");
    }
    const end = this.offset + Number(length);
    if (end > this.bytes.length) return fail("unexpected end of input");
    const hex = Buffer.from(this.bytes.subarray(this.offset, end)).toString(
      "hex",
    );
    this.offset = end;
    return hex;
  }

  /** A byte string after its head: one chunk, or chunks up to a break. */
  byteString(head: Head): string {
    if (head.major !== 2) return fail("expected a byte string");
    if (head.argument !== undefined) return this.chunk(head);
    let hex = "";
    while (this.peek() !== 0xff) {
      hex += this.chunk(this.head());
    }
    this.offset += 1;
    return hex;
  }
}

const parseDataCbor = (bytes: Uint8Array): ParsedNode[] => {
  const reader = new DataCborReader(bytes);
  const nodes: ParsedNode[] = [];
  const stack: ParseFrame[] = [];
  const openList = (
    node: ParsedNode & { readonly children: number[] },
    head: Head,
    breakAfter: boolean,
  ): void => {
    if (head.major !== 4) fail("expected an array");
    stack.push({
      children: node.children,
      isMap: false,
      remaining: head.argument,
      breakAfter,
    });
  };
  const parseItem = (): void => {
    const head = reader.head();
    if (head.major === 0 || head.major === 1) {
      const argument = reader.definite(head);
      nodes.push({
        kind: "int",
        value: head.major === 0 ? argument : -1n - argument,
      });
    } else if (head.major === 2) {
      nodes.push({ kind: "bytes", hex: reader.byteString(head) });
    } else if (head.major === 4 || head.major === 5) {
      const node = {
        kind: head.major === 4 ? ("list" as const) : ("map" as const),
        children: [],
      };
      nodes.push(node);
      stack.push({
        children: node.children,
        isMap: head.major === 5,
        remaining:
          head.argument === undefined
            ? undefined
            : head.argument * (head.major === 5 ? 2n : 1n),
        breakAfter: false,
      });
    } else if (head.major === 6) {
      const tag = reader.definite(head);
      if (tag === 2n || tag === 3n) {
        const hex = reader.byteString(reader.head());
        const magnitude = hex.length === 0 ? 0n : BigInt(`0x${hex}`);
        nodes.push({
          kind: "int",
          value: tag === 2n ? magnitude : -1n - magnitude,
        });
      } else if (
        (tag >= 121n && tag <= 127n) ||
        (tag >= 1280n && tag <= 1400n)
      ) {
        const node = {
          kind: "constr" as const,
          alternative: tag <= 127n ? tag - 121n : tag - 1280n + 7n,
          children: [],
        };
        nodes.push(node);
        openList(node, reader.head(), false);
      } else if (tag === 102n) {
        const pair = reader.head();
        if (pair.major !== 4) fail("expected an array");
        if (pair.argument !== undefined && pair.argument < 2n) {
          fail("tag 102 needs an alternative and fields");
        }
        const alternativeHead = reader.head();
        if (alternativeHead.major !== 0) {
          fail("tag 102 alternative is not an unsigned integer");
        }
        const node = {
          kind: "constr" as const,
          alternative: reader.definite(alternativeHead),
          children: [],
        };
        nodes.push(node);
        openList(node, reader.head(), pair.argument === undefined);
      } else {
        fail(`unsupported tag ${tag.toString()}`);
      }
    } else {
      fail("not a Plutus Data item");
    }
  };

  parseItem();
  for (;;) {
    const top = stack[stack.length - 1];
    if (top === undefined) return nodes;
    // CML checks for a break before each list item and each map key, in
    // definite containers too: a break there closes the container early.
    const atKey = !top.isMap || top.children.length % 2 === 0;
    let done = top.remaining === 0n;
    if (!done && atKey && reader.peek() === 0xff) {
      reader.offset += 1;
      done = true;
    } else if (!done && top.remaining !== undefined) {
      top.remaining -= 1n;
    }
    if (done) {
      stack.pop();
      if (top.breakAfter) {
        if (reader.peek() !== 0xff) fail("tag 102 array has extra items");
        reader.offset += 1;
      }
      continue;
    }
    top.children.push(nodes.length);
    parseItem();
  }
};

const decodeHex = (hex: string): Uint8Array => {
  if (hex.length % 2 !== 0 || !/^[0-9a-fA-F]*$/.test(hex)) {
    fail("not an even-length hex string");
  }
  return Buffer.from(hex, "hex");
};

/** Lucid `Data.from(cbor)` (no schema), without recursion. */
export const lucidDataFromCborIterative = (cbor: Uint8Array | string): Data => {
  const nodes = parseDataCbor(
    typeof cbor === "string" ? decodeHex(cbor) : cbor,
  );
  const values: Data[] = new Array<Data>(nodes.length);
  const ids = new PlutusDataStructureIds(nodes.length);
  for (let index = nodes.length - 1; index >= 0; index -= 1) {
    const node = nodes[index]!;
    if (node.kind === "int") {
      values[index] = node.value;
      ids.integer(index, node.value);
    } else if (node.kind === "bytes") {
      values[index] = node.hex;
      ids.bytes(index, node.hex);
    } else if (node.kind === "list") {
      values[index] = node.children.map((child) => values[child]!);
      ids.list(index, node.children);
    } else if (node.kind === "constr") {
      values[index] = new Constr(
        parseInt(node.alternative.toString()),
        node.children.map((child) => values[child]!),
      );
      ids.constr(index, node.alternative, node.children);
    } else {
      const map = new Map<Data, Data>();
      const firstValueByKey = new Map<number, Data>();
      for (let entry = 0; entry < node.children.length; entry += 2) {
        const keyId = ids.of(node.children[entry]!);
        let value = firstValueByKey.get(keyId);
        if (value === undefined) {
          value = values[node.children[entry + 1]!]!;
          firstValueByKey.set(keyId, value);
        }
        map.set(values[node.children[entry]!]!, value);
      }
      values[index] = map;
      ids.map(index, node.children);
    }
  }
  return values[0]!;
};
