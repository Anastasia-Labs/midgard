/**
 * A minimal writer for the Flat encoding of untyped Plutus Core, enough to
 * apply an exported Aiken function to many argument tuples in one program.
 *
 * The exported program `(program v T)` is never decoded: its term `T` is
 * appended verbatim as the last term of the new program, so the new program
 * ends with `T` and the original program's own end filler. That is sound
 * when `T` starts on the same bit offset modulo 8 as in the exported program
 * (byte-aligned, after the three version bytes); `alignTo` pads the prefix
 * with a discarded constant term to get there.
 */

const TERM_TAG = {
  var: 0,
  delay: 1,
  lam: 2,
  apply: 3,
  constant: 4,
  force: 5,
  error: 6,
  builtin: 7,
} as const;

const TYPE_TAG = {
  integer: 0,
  bytestring: 1,
  unit: 3,
  bool: 4,
  list: 5,
  apply: 7,
  data: 8,
} as const;

/** Builtin indices of the Plutus builtin table used here. */
export const FLAT_BUILTIN = {
  mkCons: 32,
} as const;

export type FlatConstantType = "bool" | "bytestring" | "data";

/** A term of the small UPLC subset this writer emits. */
export type FlatTerm =
  | { readonly kind: "var"; readonly index: number }
  | { readonly kind: "lam"; readonly body: FlatTerm }
  | {
      readonly kind: "apply";
      readonly fn: FlatTerm;
      readonly arg: FlatTerm;
    }
  | { readonly kind: "delay"; readonly body: FlatTerm }
  | { readonly kind: "force"; readonly body: FlatTerm }
  | { readonly kind: "error" }
  | { readonly kind: "builtin"; readonly index: number }
  | { readonly kind: "unit" }
  | { readonly kind: "bool"; readonly value: boolean }
  | { readonly kind: "integer"; readonly value: bigint }
  | { readonly kind: "data"; readonly cbor: Uint8Array }
  | { readonly kind: "emptyList"; readonly element: FlatConstantType };

export const v = (index: number): FlatTerm => ({ kind: "var", index });
export const lam = (body: FlatTerm): FlatTerm => ({ kind: "lam", body });
export const app = (fn: FlatTerm, ...args: readonly FlatTerm[]): FlatTerm =>
  args.reduce<FlatTerm>((acc, arg) => ({ kind: "apply", fn: acc, arg }), fn);
export const force = (body: FlatTerm): FlatTerm => ({ kind: "force", body });
export const builtin = (index: number): FlatTerm => ({
  kind: "builtin",
  index,
});
export const dataConstant = (cbor: Uint8Array): FlatTerm => ({
  kind: "data",
  cbor,
});
export const emptyList = (element: FlatConstantType): FlatTerm => ({
  kind: "emptyList",
  element,
});

class BitWriter {
  private readonly bytes: number[] = [];
  private current = 0;
  private used = 0;

  get position(): number {
    return this.bytes.length * 8 + this.used;
  }

  bit(value: boolean): void {
    this.current = (this.current << 1) | (value ? 1 : 0);
    this.used += 1;
    if (this.used === 8) {
      this.bytes.push(this.current);
      this.current = 0;
      this.used = 0;
    }
  }

  bits(value: number, width: number): void {
    for (let shift = width - 1; shift >= 0; shift -= 1) {
      this.bit(((value >>> shift) & 1) === 1);
    }
  }

  /** `0* 1` up to the next byte boundary; a whole byte when aligned. */
  filler(): void {
    const zeros = 7 - this.used;
    for (let index = 0; index < zeros; index += 1) this.bit(false);
    this.bit(true);
  }

  natural(value: bigint): void {
    if (value < 0n) throw new Error("flat natural must be non-negative");
    let rest = value;
    for (;;) {
      const group = Number(rest & 0x7fn);
      rest >>= 7n;
      this.bit(rest !== 0n);
      this.bits(group, 7);
      if (rest === 0n) return;
    }
  }

  byteString(value: Uint8Array): void {
    this.filler();
    for (let offset = 0; offset < value.length; offset += 255) {
      const chunk = value.subarray(offset, offset + 255);
      this.bits(chunk.length, 8);
      for (const byte of chunk) this.bits(byte, 8);
    }
    this.bits(0, 8);
  }

  aligned(): Uint8Array {
    if (this.used !== 0) throw new Error("flat writer is not byte-aligned");
    return Uint8Array.from(this.bytes);
  }
}

const writeTypes = (writer: BitWriter, tags: readonly number[]): void => {
  for (const tag of tags) {
    writer.bit(true);
    writer.bits(tag, 4);
  }
  writer.bit(false);
};

const elementTypeTag = (element: FlatConstantType): number =>
  element === "bool"
    ? TYPE_TAG.bool
    : element === "bytestring"
      ? TYPE_TAG.bytestring
      : TYPE_TAG.data;

const writeTerm = (writer: BitWriter, term: FlatTerm): void => {
  switch (term.kind) {
    case "var":
      writer.bits(TERM_TAG.var, 4);
      writer.natural(BigInt(term.index));
      return;
    case "lam":
      writer.bits(TERM_TAG.lam, 4);
      writeTerm(writer, term.body);
      return;
    case "apply":
      writer.bits(TERM_TAG.apply, 4);
      writeTerm(writer, term.fn);
      writeTerm(writer, term.arg);
      return;
    case "delay":
      writer.bits(TERM_TAG.delay, 4);
      writeTerm(writer, term.body);
      return;
    case "force":
      writer.bits(TERM_TAG.force, 4);
      writeTerm(writer, term.body);
      return;
    case "error":
      writer.bits(TERM_TAG.error, 4);
      return;
    case "builtin":
      writer.bits(TERM_TAG.builtin, 4);
      writer.bits(term.index, 7);
      return;
    case "unit":
      writer.bits(TERM_TAG.constant, 4);
      writeTypes(writer, [TYPE_TAG.unit]);
      return;
    case "bool":
      writer.bits(TERM_TAG.constant, 4);
      writeTypes(writer, [TYPE_TAG.bool]);
      writer.bit(term.value);
      return;
    case "integer": {
      writer.bits(TERM_TAG.constant, 4);
      writeTypes(writer, [TYPE_TAG.integer]);
      const zigzag =
        term.value >= 0n ? term.value << 1n : (-term.value << 1n) - 1n;
      writer.natural(zigzag);
      return;
    }
    case "data":
      writer.bits(TERM_TAG.constant, 4);
      writeTypes(writer, [TYPE_TAG.data]);
      writer.byteString(term.cbor);
      return;
    case "emptyList":
      writer.bits(TERM_TAG.constant, 4);
      writeTypes(writer, [
        TYPE_TAG.apply,
        TYPE_TAG.list,
        elementTypeTag(term.element),
      ]);
      writer.bit(false);
      return;
  }
};

/**
 * Discarded argument terms that evaluate without failing. None holds a byte
 * string, so each one's Flat length is independent of its position.
 */
const PADDING_CANDIDATES: readonly FlatTerm[] = [
  { kind: "unit" },
  { kind: "bool", value: false },
  { kind: "delay", body: { kind: "error" } },
  { kind: "delay", body: { kind: "unit" } },
  { kind: "delay", body: { kind: "bool", value: false } },
  app(lam({ kind: "unit" }), { kind: "bool", value: false }),
  app(lam({ kind: "unit" }), {
    kind: "delay",
    body: { kind: "bool", value: false },
  }),
  app(lam({ kind: "unit" }), { kind: "unit" }),
];

const flatBitLength = (term: FlatTerm): number => {
  const writer = new BitWriter();
  writeTerm(writer, term);
  return writer.position;
};

/** One discarded term per Flat bit length modulo 8. */
const PADDING_BY_RESIDUE: readonly FlatTerm[] = Array.from(
  { length: 8 },
  (_, residue) => {
    const found = PADDING_CANDIDATES.find(
      (term) => flatBitLength(term) % 8 === residue,
    );
    if (found === undefined) {
      throw new Error(`no Flat padding term of length ${residue} mod 8`);
    }
    return found;
  },
);

const EXPORTED_VERSION_BYTES = 3;

/**
 * `(program v [ [(lam _ (lam f body)) padding] T ])`, where `T` is the term
 * of `exportedFlat` and `body` sees it as variable 1. `exportedFlat` is the
 * Flat encoding of a `(program 1.1.0 T)`.
 */
export const applyExportedProgram = (
  exportedFlat: Uint8Array,
  body: FlatTerm,
): Uint8Array => {
  const version = exportedFlat.subarray(0, EXPORTED_VERSION_BYTES);
  if (version[0] !== 1 || version[1] !== 1 || version[2] !== 0) {
    throw new Error("the exported program is not a 1.1.0 Flat program");
  }
  const writer = new BitWriter();
  for (const byte of version) writer.bits(byte, 8);
  writer.bits(TERM_TAG.apply, 4);
  writer.bits(TERM_TAG.apply, 4);
  writer.bits(TERM_TAG.lam, 4);
  writeTerm(writer, lam(body));
  const residue = (8 - (writer.position % 8)) % 8;
  writeTerm(writer, PADDING_BY_RESIDUE[residue]!);
  const prefix = writer.aligned();
  return Uint8Array.from([
    ...prefix,
    ...exportedFlat.subarray(EXPORTED_VERSION_BYTES),
  ]);
};

/** `[ [(force (builtin mkCons)) head] tail ]` folded right over `items`. */
export const listOf = (
  items: readonly FlatTerm[],
  element: FlatConstantType,
): FlatTerm =>
  items.reduceRight<FlatTerm>(
    (tail, head) => app(force(builtin(FLAT_BUILTIN.mkCons)), head, tail),
    emptyList(element),
  );
