/**
 * Bit-level reading of a script's Flat bytes with the primitives of the
 * Cardano ledger's Flat decoder (plutus-core 1.71, `PlutusCore.Flat`). Each
 * primitive refuses exactly where the Haskell one does, with the same class.
 */

/**
 * Why a Flat program was refused. Every class except `outsideProfile` is a
 * refusal by the Cardano ledger's decoder at protocol version 11 (Plutus V3).
 *
 * - `notEnoughSpace`: a read ran past the end of the bytes (Haskell
 *   `NotEnoughSpace`), including a missing final pad.
 * - `tooMuchSpace`: the final pad ends inside a byte or bytes follow it
 *   (Haskell `TooMuchSpace`).
 * - `fillerNotByteAligned`: a byte-string filler ends before a byte boundary
 *   (Haskell `BadEncoding "usedBits /= 0"`).
 * - `unknownTermTag`: a term tag above 9.
 * - `constrOrCaseBeforeV110`: `constr` or `case` in a program below 1.1.0.
 * - `unknownBuiltin`: a builtin tag above 105.
 * - `builtinNotAvailable`: builtin tags 101 to 105, which are not available
 *   to Plutus V3 at protocol version 11.
 * - `badConstantType`: a malformed or non-star constant type.
 * - `constantTypeTooLarge`: a constant type of more than 32 tags.
 * - `constantNotFlatDecodable`: a BLS12-381 constant, which has no Flat value.
 * - `word64Overflow`: a variable index or `constr` tag over 64 bits.
 * - `constrTooManyFields`: a `constr` with more than 1,024 fields.
 * - `invalidUtf8`: a string constant that is not strict UTF-8.
 * - `invalidData`: a Data constant the Plutus Data decoder refuses.
 * - `invalidValue`: a Value constant with a key over 32 bytes, a quantity
 *   outside signed 128 bits, keys not strictly ascending, an empty inner map
 *   or a zero quantity.
 * - `outsideProfile`: Cardano decodes the whole program, but the V1 profile
 *   cannot represent it: an array or Value constant, a variable index of 0 or
 *   above 2^53 (always unbound, so Cardano fails it before evaluating), or a
 *   version number above 255. The builder refused these before as well. It
 *   is reported only when no Cardano refusal follows; the bit offset is that
 *   of the first such item.
 */
export type CardanoFlatRefusalClass =
  | "notEnoughSpace"
  | "tooMuchSpace"
  | "fillerNotByteAligned"
  | "unknownTermTag"
  | "constrOrCaseBeforeV110"
  | "unknownBuiltin"
  | "builtinNotAvailable"
  | "badConstantType"
  | "constantTypeTooLarge"
  | "constantNotFlatDecodable"
  | "word64Overflow"
  | "constrTooManyFields"
  | "invalidUtf8"
  | "invalidData"
  | "invalidValue"
  | "outsideProfile";

/** A refused Flat program: the class and the bit offset it was found at. */
export class CardanoFlatRefusal extends Error {
  constructor(
    readonly refusal: CardanoFlatRefusalClass,
    /** Bit offset from the start of the Flat bytes where the refused item starts. */
    readonly bitOffset: number,
    readonly detail: string,
  ) {
    super(`Cardano Flat refusal ${refusal} at bit ${bitOffset}: ${detail}`);
    this.name = "CardanoFlatRefusal";
  }

  /** Whether the Cardano ledger itself refuses the program. */
  get cardanoRefuses(): boolean {
    return this.refusal !== "outsideProfile";
  }
}

const WORD64_GROUPS = 10;

/** Naturals of at most this many 7-bit groups are read by shifting. */
const SHIFTED_GROUPS = 16;

export class CardanoFlatReader {
  private position = 0;
  private readonly bitLength: number;

  constructor(private readonly bytes: Uint8Array) {
    this.bitLength = bytes.length * 8;
  }

  get bitOffset(): number {
    return this.position;
  }

  refuse(
    refusal: CardanoFlatRefusalClass,
    detail: string,
    bitOffset = this.position,
  ): never {
    throw new CardanoFlatRefusal(refusal, bitOffset, detail);
  }

  /** `ensureBits` (Prim.hs:171-172). */
  private ensure(bits: number): void {
    if (this.bitLength - this.position < bits)
      this.refuse("notEnoughSpace", `${bits} more bits needed`);
  }

  /** `dBool` (Prim.hs:200-211). */
  bit(): boolean {
    this.ensure(1);
    const byte = this.bytes[this.position >> 3]!;
    const set = (byte & (0x80 >> (this.position & 7))) !== 0;
    this.position += 1;
    return set;
  }

  /** `dBEBits8` (Prim.hs:224-227): up to 8 bits, most significant first. */
  bits(count: number): number {
    this.ensure(count);
    let value = 0;
    for (let index = 0; index < count; index += 1) {
      const byte = this.bytes[this.position >> 3]!;
      value = (value << 1) | ((byte >> (7 - (this.position & 7))) & 1);
      this.position += 1;
    }
    return value;
  }

  /** `dUnsigned` (Strict.hs:242-273): 7-bit groups, least significant first. */
  natural(): bigint {
    const groups: number[] = [];
    for (;;) {
      const tagged = this.bits(8);
      groups.push(tagged & 0x7f);
      if (tagged < 0x80) break;
    }
    if (groups.length <= SHIFTED_GROUPS) {
      let value = 0n;
      for (let index = groups.length - 1; index >= 0; index -= 1)
        value = (value << 7n) | BigInt(groups[index]!);
      return value;
    }
    // Shifting a bigint per group is quadratic in its length; pack the
    // groups into bytes and read them as one hexadecimal number instead.
    const bytes = new Uint8Array(Math.ceil((groups.length * 7) / 8));
    let pending = 0;
    let pendingBits = 0;
    let index = bytes.length;
    for (const group of groups) {
      pending |= group << pendingBits;
      pendingBits += 7;
      if (pendingBits >= 8) {
        bytes[--index] = pending & 0xff;
        pending >>>= 8;
        pendingBits -= 8;
      }
    }
    if (pendingBits > 0) bytes[--index] = pending;
    return BigInt(`0x${Buffer.from(bytes).toString("hex")}`);
  }

  /** `dInteger` (Strict.hs:113-114): zig-zag over a natural. */
  integer(): bigint {
    const natural = this.natural();
    return (natural & 1n) === 0n ? natural >> 1n : -((natural + 1n) >> 1n);
  }

  /** `dWord64` (Strict.hs:165-184, 217-239). */
  word64(): bigint {
    const start = this.position;
    let value = 0n;
    for (let group = 0; group < WORD64_GROUPS; group += 1) {
      const tagged = this.bits(8);
      const payload = tagged & 0x7f;
      value |= BigInt(payload) << BigInt(7 * group);
      if (group === WORD64_GROUPS - 1) {
        if (tagged !== payload || payload > 1)
          this.refuse("word64Overflow", "unsigned integer over 64 bits", start);
        return value;
      }
      if (tagged === payload) return value;
    }
    return this.refuse("word64Overflow", "unreachable", start);
  }

  /** `dFiller` then `getChunksInfo` (Strict.hs:301-304, Prim.hs:441-459). */
  byteString(): Uint8Array {
    const start = this.position;
    while (!this.bit());
    if ((this.position & 7) !== 0)
      this.refuse(
        "fillerNotByteAligned",
        "byte-string filler ends inside a byte",
        start,
      );
    const chunks: Uint8Array[] = [];
    let total = 0;
    for (;;) {
      this.ensure(8);
      const length = this.bytes[this.position >> 3]!;
      this.position += 8;
      if (length === 0) break;
      this.ensure(length * 8);
      const offset = this.position >> 3;
      // `slice` of the input, as the replaced decoder did: a single-chunk
      // payload keeps the input's class (a Buffer stays a Buffer).
      chunks.push(this.bytes.slice(offset, offset + length));
      total += length;
      this.position += length * 8;
    }
    if (chunks.length === 1) return chunks[0]!;
    const out = new Uint8Array(total);
    let offset = 0;
    for (const chunk of chunks) {
      out.set(chunk, offset);
      offset += chunk.length;
    }
    return out;
  }

  /**
   * The post-aligned `Filler` and the end check (Filler.hs:76-81, Class.hs:
   * 224-233, Decoder/Run.hs:19-24): zero bits, a 1 bit, and nothing after it.
   */
  finish(): void {
    while (!this.bit());
    if (this.position !== this.bitLength)
      this.refuse("tooMuchSpace", "bits remain after the final pad");
  }
}
