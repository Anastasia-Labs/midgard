/**
 * PlutusV3 scripts of exactly the 16,384-byte bound whose Flat is one shape
 * repeated: a term chain, a Data constant, a constant list of one element
 * type, or an application chain over small constants. Each holds as many
 * repetitions as fit, so recording it is the longest decode walk of that
 * shape.
 */

const SCRIPT_BOUND = 16_384;

class FlatBits {
  readonly bits: number[] = [];

  put(value: number, width: number): void {
    for (let index = width - 1; index >= 0; index -= 1) {
      this.bits.push(Math.floor(value / 2 ** index) % 2);
    }
  }

  natural(value: number): void {
    let rest = value;
    for (;;) {
      const group = rest % 128;
      rest = Math.floor(rest / 128);
      this.put(rest > 0 ? 128 + group : group, 8);
      if (rest === 0) return;
    }
  }

  filler(): void {
    while (this.bits.length % 8 !== 7) this.bits.push(0);
    this.bits.push(1);
  }

  /** A Flat byte string: filler, 255-byte chunks, an empty chunk. */
  chunks(data: Uint8Array): void {
    this.filler();
    for (let start = 0; start < data.length; start += 255) {
      const chunk = data.subarray(start, start + 255);
      this.put(chunk.length, 8);
      for (const byte of chunk) this.put(byte, 8);
    }
    this.put(0, 8);
  }

  /** Constant term with the given type tags. */
  constant(tags: readonly number[]): void {
    this.put(4, 4);
    for (const tag of tags) {
      this.put(1, 1);
      this.put(tag, 4);
    }
    this.put(0, 1);
  }

  bytes(): Buffer {
    const out = Buffer.alloc(this.bits.length / 8);
    for (let index = 0; index < out.length; index += 1) {
      let byte = 0;
      for (let bit = 0; bit < 8; bit += 1) {
        byte = byte * 2 + this.bits[8 * index + bit]!;
      }
      out[index] = byte;
    }
    return out;
  }
}

/** Program 1.1.0 around `term`, padded and wrapped as one CBOR byte string. */
const program = (term: (bits: FlatBits) => void): Buffer => {
  const bits = new FlatBits();
  bits.natural(1);
  bits.natural(1);
  bits.natural(0);
  term(bits);
  bits.filler();
  const flat = bits.bytes();
  const n = flat.length;
  const head =
    n < 24
      ? Buffer.from([0x40 + n])
      : n < 256
        ? Buffer.from([0x58, n])
        : Buffer.from([0x59, n >> 8, n & 255]);
  return Buffer.concat([head, flat]);
};

/** A balanced pair tree of `leaves` unit leaves, as type tags. */
export const unitPairTree = (leaves: number): number[] => {
  if (leaves === 1) return [3];
  const left = Math.floor(leaves / 2);
  return [7, 7, 6, ...unitPairTree(left), ...unitPairTree(leaves - left)];
};

/** `count` list elements of type `tags`, each written by `element`. */
const constantList =
  (tags: readonly number[], element: (bits: FlatBits, index: number) => void) =>
  (count: number): Buffer =>
    program((bits) => {
      bits.constant([7, 5, ...tags]);
      for (let index = 0; index < count; index += 1) {
        bits.put(1, 1);
        element(bits, index);
      }
      bits.put(0, 1);
    });

const dataConstant =
  (cbor: (count: number) => Uint8Array) =>
  (count: number): Buffer =>
    program((bits) => {
      bits.constant([8]);
      bits.chunks(cbor(count));
    });

const repeat = (
  head: number[],
  unit: number[],
  count: number,
  tail: number[],
) =>
  Uint8Array.from([
    ...head,
    ...Array.from({ length: count }, () => unit).flat(),
    ...tail,
  ]);

const nothing = () => {};

/** A pair chain over `leaf`: `depth` pairs, each with a unit first. */
const unitChainOver = (depth: number, leaf: readonly number[]): number[] =>
  depth === 0 ? [...leaf] : [7, 7, 6, 3, ...unitChainOver(depth - 1, leaf)];

/**
 * The bits of a value of `levels` nested lists whose innermost list holds
 * `inner` unit pair chains, each over `[()]`. At each level above the
 * innermost, `before` writes the elements ahead of the deeper list.
 */
const nestedValue = (
  bits: FlatBits,
  levels: number,
  inner: number,
  before: (bits: FlatBits, levels: number) => void,
): void => {
  if (levels === 1) {
    for (let index = 0; index < inner; index += 1) {
      bits.put(1, 1);
      bits.put(2, 2);
    }
  } else {
    before(bits, levels);
    bits.put(1, 1);
    nestedValue(bits, levels - 1, inner, before);
  }
  bits.put(0, 1);
};

/** A value of `levels` nested lists of one element each, down to one chain
 * over `[()]`. */
const onePath = (bits: FlatBits, levels: number): void => {
  bits.put(1, 1);
  if (levels === 1) bits.put(2, 2);
  else onePath(bits, levels - 1);
  bits.put(0, 1);
};

/**
 * A constant list of `count` elements nested `levels` deep in all whose
 * innermost lists each hold `inner` unit pair chains of `depth` over
 * `list unit`, but the last element's, which holds `last`. With `inner` just
 * over a tick's units, every element's bulk decode stops in its innermost
 * list.
 */
const nestedListsEndingIn =
  (
    levels: number,
    depth: number,
    inner: number,
    before: (bits: FlatBits, levels: number) => void = nothing,
  ) =>
  (count: number, last: number): Buffer =>
    constantList(
      [
        ...Array.from({ length: levels - 1 }, () => [7, 5]).flat(),
        ...unitChainOver(depth, [7, 5, 3]),
      ],
      (bits, index) =>
        nestedValue(
          bits,
          levels - 1,
          index === count - 1 ? last : inner,
          before,
        ),
    )(count);

/** As `nestedListsEndingIn`, the last element like the others. */
const nestedLists =
  (
    levels: number,
    depth: number,
    inner: number,
    before: (bits: FlatBits, levels: number) => void = nothing,
  ) =>
  (count: number): Buffer =>
    nestedListsEndingIn(levels, depth, inner, before)(count, inner);

/** An empty list ahead of the deeper list. */
const emptyBefore = (bits: FlatBits): void => {
  bits.put(1, 1);
  bits.put(0, 1);
};

/** A one-element-per-level list ahead of the deeper list. */
const onePathBefore = (bits: FlatBits, levels: number): void => {
  bits.put(1, 1);
  onePath(bits, levels - 1);
};

/**
 * The costliest nested-list shape found (the walk gate's "lists 6 deep, []
 * ahead of each deeper list" row), with `count` elements, the last holding
 * `last` of the 31 chains: its scripts reach every length the row's scripts
 * fall between.
 */
export const costliestNestedLists = nestedListsEndingIn(6, 4, 31, emptyBefore);

type Leaf = (bits: FlatBits, index: number) => void;

/**
 * `[[[error c0] c1] ... c(n-1)]`: an application chain over `count` small
 * constants, so decoding passes between the term and constant controls at
 * every constant.
 */
const applyChain =
  (leaf: Leaf) =>
  (count: number): Buffer =>
    program((bits) => {
      for (let index = 0; index < count; index += 1) bits.put(3, 4);
      bits.put(6, 4);
      for (let index = 0; index < count; index += 1) leaf(bits, index);
    });

const unitLeaf: Leaf = (bits) => bits.constant([3]);
const emptyValueLeaf: Leaf = (bits) => {
  bits.constant([13]);
  bits.put(0, 1);
};
const integerZeroLeaf: Leaf = (bits) => {
  bits.constant([0]);
  bits.natural(0);
};
const dataLeaf: Leaf = (bits) => {
  bits.constant([8]);
  bits.chunks(Uint8Array.from([0x00]));
};
const listUnitLeaf: Leaf = (bits) => {
  bits.constant([7, 5, 3]);
  bits.put(1, 1);
  bits.put(0, 1);
};
/** A 200-bit integer: 29 groups of 7 bits. */
const wideIntegerLeaf: Leaf = (bits) => {
  bits.constant([0]);
  for (let group = 0; group < 29; group += 1) {
    bits.put(group < 28 ? 129 : 1, 8);
  }
};
const alternating =
  (leaves: readonly Leaf[]): Leaf =>
  (bits, index) =>
    leaves[index % leaves.length]!(bits, index);

/** Each shape, as a function from repetitions to script bytes. */
export const WALK_SHAPES = {
  "lambda-delay": (count: number) =>
    program((bits) => {
      for (let index = 0; index < count; index += 1) {
        bits.put(2, 4);
        bits.put(1, 4);
      }
      bits.put(0, 4);
      bits.natural(1);
    }),
  "case of error branches": (count: number) =>
    program((bits) => {
      bits.put(9, 4);
      bits.put(6, 4);
      for (let index = 0; index < count; index += 1) {
        bits.put(1, 1);
        bits.put(6, 4);
      }
      bits.put(0, 1);
    }),
  "Data nested arrays": dataConstant((count) =>
    repeat([], [0x81], count, [0x00]),
  ),
  "Data list of I 0": dataConstant((count) =>
    repeat([0x9f], [0x00], count, [0xff]),
  ),
  "list bool": constantList([4], (bits) => bits.put(0, 1)),
  "list (list unit)": constantList([7, 5, 3], (bits) => bits.put(0, 1)),
  "list unit": constantList([3], nothing),
  "list (pair unit unit)": constantList(unitPairTree(2), nothing),
  "list of an 8-leaf unit pair tree": constantList(unitPairTree(8), nothing),
  "list integer": constantList([0], (bits) => bits.natural(0)),
  "list bytestring": constantList([1], (bits) => bits.chunks(new Uint8Array())),
  "list of a unit pair chain over bool": constantList(
    unitChainOver(7, [4]),
    (bits, index) => bits.put(index % 2, 1),
  ),
  "list of a unit pair chain over a pair of bools": constantList(
    unitChainOver(6, [7, 7, 6, 4, 4]),
    (bits, index) => bits.put(index % 4, 2),
  ),
  "list of a unit pair chain over list unit": constantList(
    unitChainOver(6, [7, 5, 3]),
    (bits, index) => bits.put(index % 2 === 0 ? 0 : 2, index % 2 === 0 ? 1 : 2),
  ),
  "list of a unit pair chain over list unit, every element [()]": constantList(
    unitChainOver(6, [7, 5, 3]),
    (bits) => bits.put(2, 2),
  ),
  "list of a unit pair chain over list Data, [] and [d] alternating":
    constantList(unitChainOver(6, [7, 5, 8]), (bits, index) => {
      if (index % 2 === 1) {
        bits.put(1, 1);
        bits.chunks(Uint8Array.from([0x00]));
      }
      bits.put(0, 1);
    }),
  "list of a unit pair chain over list Data, every element [d]": constantList(
    unitChainOver(6, [7, 5, 8]),
    (bits) => {
      bits.put(1, 1);
      bits.chunks(Uint8Array.from([0x00]));
      bits.put(0, 1);
    },
  ),
  "list of a unit pair chain over a pair of list unit and list Data":
    constantList(
      unitChainOver(5, [7, 7, 6, 7, 5, 3, 7, 5, 8]),
      (bits, index) => {
        bits.put(2, 2);
        if (index % 2 === 1) {
          bits.put(1, 1);
          bits.chunks(Uint8Array.from([0x00]));
        }
        bits.put(0, 1);
      },
    ),
  "list of a unit pair chain over a pair of list unit, every element ([()], [()])":
    constantList(unitChainOver(5, [7, 7, 6, 7, 5, 3, 7, 5, 3]), (bits) => {
      bits.put(2, 2);
      bits.put(2, 2);
    }),
  "list of a unit pair chain over a pair of list unit and unit, every element ([()], ())":
    constantList(unitChainOver(5, [7, 7, 6, 7, 5, 3, 3]), (bits) =>
      bits.put(2, 2),
    ),
  "list of a unit pair chain over value": constantList(
    unitChainOver(7, [13]),
    (bits) => bits.put(0, 1),
  ),
  "list of a unit pair chain over Data": constantList(
    unitChainOver(7, [8]),
    (bits) => bits.chunks(Uint8Array.from([0x00])),
  ),
  "lists 14 deep over 103 [()]": nestedLists(14, 0, 103),
  "lists 2 deep over 23 unit pair chains of depth 6 over [()]": nestedLists(
    2,
    6,
    23,
  ),
  "lists 6 deep, [] ahead of each deeper list, over 31 unit pair chains of depth 4 over [()]":
    (count: number) => costliestNestedLists(count, 31),
  "lists 4 deep, [] ahead of each deeper list, over 26 unit pair chains of depth 5 over [()]":
    nestedLists(4, 5, 26, emptyBefore),
  "lists 4 deep, a one-path list ahead of each deeper list, over 26 unit pair chains of depth 5 over [()]":
    nestedLists(4, 5, 26, onePathBefore),
  "apply chain of unit": applyChain(unitLeaf),
  "apply chain of empty value": applyChain(emptyValueLeaf),
  "apply chain of integer 0": applyChain(integerZeroLeaf),
  "apply chain of Data I 0 and list unit [()], alternating": applyChain(
    alternating([dataLeaf, listUnitLeaf]),
  ),
  "apply chain of 200-bit integer": applyChain(wideIntegerLeaf),
  "apply chain of integer 0 and empty value, alternating": applyChain(
    alternating([integerZeroLeaf, emptyValueLeaf]),
  ),
  "apply chain of list unit [()]": applyChain(listUnitLeaf),
} as const satisfies Record<string, (count: number) => Buffer>;

export type WalkShape = keyof typeof WALK_SHAPES;

/** The shape with as many repetitions as fit in 16,384 bytes. */
export const largestScriptOf = (
  shape: WalkShape,
): { readonly count: number; readonly script: Buffer } => {
  const make = WALK_SHAPES[shape];
  let low = 1;
  let high = 140_000;
  while (low < high) {
    const middle = Math.floor((low + high + 1) / 2);
    if (make(middle).length <= SCRIPT_BOUND) low = middle;
    else high = middle - 1;
  }
  return { count: low, script: make(low) };
};
