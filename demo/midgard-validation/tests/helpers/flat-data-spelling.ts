import { encodeMidgardCekCardanoFlatProgram } from "../../src/cek-program.cardano-flat.js";
import { ConstTyTag, UPLCConst } from "../../src/uplc-constant.js";
import { DataB } from "../../src/uplc-data.js";
import {
  Application,
  Case,
  Constr,
  Delay,
  Force,
  Lambda,
  UPLCProgram,
  type UPLCTerm,
} from "../../src/uplc-term.js";

/**
 * Flat bytes of a program whose Data constants are spelled as given rather
 * than canonically, for tests of the spellings the Cardano ledger accepts.
 *
 * Flat writes a Data constant's CBOR as a byte-aligned byte string: chunks of
 * at most 255 bytes, each after its length byte, then a 0 byte. Each Data
 * constant (top level, in Flat order) is first written as a distinct
 * placeholder, and the placeholder's byte run is then replaced by the given
 * spelling's.
 */

/** A Flat byte string's bytes after its filler. */
const flatByteString = (bytes: Uint8Array): Buffer => {
  const parts: number[] = [];
  for (let start = 0; start < bytes.length; start += 255) {
    const chunk = bytes.subarray(start, start + 255);
    parts.push(chunk.length, ...chunk);
  }
  parts.push(0);
  return Buffer.from(parts);
};

const placeholder = (index: number): DataB =>
  new DataB(Buffer.from(`midgard flat data spelling ${index}`.padEnd(48, "#")));

/** `term` with its Data constants replaced by placeholders, in Flat order. */
const withPlaceholders = (term: UPLCTerm, next: () => DataB): UPLCTerm => {
  if (term instanceof Application) {
    const func = withPlaceholders(term.func, next);
    return new Application(func, withPlaceholders(term.arg, next));
  }
  if (term instanceof Lambda)
    return new Lambda(withPlaceholders(term.body, next));
  if (term instanceof Delay)
    return new Delay(withPlaceholders(term.delayedTerm, next));
  if (term instanceof Force)
    return new Force(withPlaceholders(term.forced, next));
  if (term instanceof Constr)
    return new Constr(
      term.index,
      term.terms.map((field) => withPlaceholders(field, next)),
    );
  if (term instanceof Case) {
    const scrutinee = withPlaceholders(term.constrTerm, next);
    return new Case(
      scrutinee,
      term.continuations.map((branch) => withPlaceholders(branch, next)),
    );
  }
  if (
    term instanceof UPLCConst &&
    term.type.length === 1 &&
    term.type[0] === ConstTyTag.data
  )
    return UPLCConst.data(next());
  return term;
};

/** Flat bytes of `program` with its Data constants spelled as `spellings`. */
export const spelledDataFlat = (
  program: UPLCProgram,
  spellings: readonly Uint8Array[],
): Buffer => {
  let count = 0;
  const body = withPlaceholders(program.body, () => placeholder(count++));
  if (count !== spellings.length)
    throw new Error(`${count} Data constants, ${spellings.length} spellings`);
  let flat = encodeMidgardCekCardanoFlatProgram(
    new UPLCProgram(program.version, body),
  );
  spellings.forEach((spelling, index) => {
    const marker = placeholder(index).bytes;
    const run = flatByteString(
      Buffer.concat([Buffer.from([0x58, 48]), marker]),
    );
    const at = flat.indexOf(run);
    if (at < 0 || flat.indexOf(run, at + 1) >= 0)
      throw new Error(`placeholder ${index} is not written exactly once`);
    flat = Buffer.concat([
      flat.subarray(0, at),
      flatByteString(spelling),
      flat.subarray(at + run.length),
    ]);
  });
  return flat;
};
