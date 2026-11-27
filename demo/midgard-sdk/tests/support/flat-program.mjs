// Builds small UPLC fixture programs and writes them in Cardano's Flat
// encoding (Plutus Core specification, appendix C), for tests that need a
// real script without an Aiken build. Only the terms and constants the
// fixtures use are supported: variables, lambdas, applications, delay, force,
// builtins, and integer, bytestring and unit constants.

const TERM = {
  var: 0,
  delay: 1,
  lambda: 2,
  apply: 3,
  constant: 4,
  force: 5,
  builtin: 7,
};
const BUILTIN_TAGS = {
  lessThanInteger: 8,
  sha2_256: 18,
  ifThenElse: 26,
  sndPair: 30,
  headList: 33,
  tailList: 34,
  unConstrData: 42,
  unIData: 45,
};
const TYPE = { integer: 0, bytestring: 1, unit: 3 };

/** A variable by its de Bruijn index; 1 is the innermost lambda's. */
export const variable = (index) => ({ tag: TERM.var, index });
export const lambda = (body) => ({ tag: TERM.lambda, body });
export const delay = (body) => ({ tag: TERM.delay, body });
export const force = (body) => ({ tag: TERM.force, body });
export const apply = (fn, ...args) =>
  args.reduce((f, arg) => ({ tag: TERM.apply, fn: f, arg }), fn);
export const integer = (value) => ({
  tag: TERM.constant,
  type: TYPE.integer,
  value: BigInt(value),
});
export const bytestring = (bytes) => ({
  tag: TERM.constant,
  type: TYPE.bytestring,
  value: Uint8Array.from(bytes),
});
export const unit = { tag: TERM.constant, type: TYPE.unit };

/** A builtin under `forces` type-instantiation forces. */
export const builtin = (name, forces = 0) => {
  const tag = BUILTIN_TAGS[name];
  if (tag === undefined) throw new Error(`no flat tag for builtin ${name}`);
  let term = { tag: TERM.builtin, builtin: tag };
  for (let n = 0; n < forces; n += 1) term = force(term);
  return term;
};

class BitWriter {
  bits = [];
  write(value, width) {
    for (let bit = width - 1; bit >= 0; bit -= 1) {
      this.bits.push((value >> bit) & 1);
    }
  }
  // Seven-bit groups, least significant first, each after a continue bit.
  natural(value) {
    let rest = BigInt(value);
    do {
      const group = Number(rest & 0x7fn);
      rest >>= 7n;
      this.write(rest > 0n ? 1 : 0, 1);
      this.write(group, 7);
    } while (rest > 0n);
  }
  // Zeros up to a byte boundary's last bit, then a one.
  filler() {
    while (this.bits.length % 8 !== 7) this.bits.push(0);
    this.bits.push(1);
  }
  bytes() {
    const out = new Uint8Array(this.bits.length / 8);
    this.bits.forEach((bit, at) => {
      out[at >> 3] |= bit << (7 - (at & 7));
    });
    return out;
  }
}

const writeConstant = (writer, term) => {
  writer.write(1, 1);
  writer.write(term.type, 4);
  writer.write(0, 1);
  if (term.type === TYPE.integer) {
    writer.natural(term.value >= 0n ? term.value * 2n : -term.value * 2n - 1n);
  } else if (term.type === TYPE.bytestring) {
    writer.filler();
    for (let at = 0; at < term.value.length; at += 255) {
      const chunk = term.value.subarray(at, at + 255);
      writer.write(chunk.length, 8);
      for (const byte of chunk) writer.write(byte, 8);
    }
    writer.write(0, 8);
  }
};

const writeTerm = (writer, root) => {
  const pending = [root];
  while (pending.length > 0) {
    const term = pending.pop();
    writer.write(term.tag, 4);
    if (term.tag === TERM.var) writer.natural(term.index);
    else if (term.tag === TERM.constant) writeConstant(writer, term);
    else if (term.tag === TERM.builtin) writer.write(term.builtin, 7);
    else if (term.tag === TERM.apply) pending.push(term.arg, term.fn);
    else pending.push(term.body);
  }
};

/** The Flat bytes of `(program 1.1.0 body)`. */
export const encodeFlatProgram = (body) => {
  const writer = new BitWriter();
  for (const part of [1, 1, 0]) writer.natural(part);
  writeTerm(writer, body);
  writer.filler();
  return writer.bytes();
};
