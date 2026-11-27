/**
 * The writers behind the script-decode step golden vectors: a Flat bit writer,
 * as in `onchain/aiken/lib/midgard/script-decode-v1.test.ak`, and Plutus Data
 * written as the ledger's `serialiseData` writes it.
 *
 * This module imports nothing from the package, so either side can load it.
 */

// ## A Flat bit writer, as in script-decode-v1.test.ak

const W0 = { acc: 0n, n: 0 };

const put = (w, value, width) => ({
  acc: (w.acc << BigInt(width)) + BigInt(value),
  n: w.n + width,
});

/** A Flat natural: little-endian 7-bit groups. */
const nat = (w, z) => {
  let out = w;
  let rest = BigInt(z);
  while (rest >= 128n) {
    out = put(out, 128n + (rest % 128n), 8);
    rest /= 128n;
  }
  return put(out, rest, 8);
};

const int = (w, n) => {
  const z = BigInt(n);
  return nat(w, z >= 0n ? 2n * z : -2n * z - 1n);
};

const filler = (w) => put(w, 1, 8 - (w.n % 8));

const chunks = (w, bytes) => {
  let out = w;
  for (let offset = 0; offset < bytes.length; offset += 255) {
    const chunk = bytes.subarray(offset, offset + 255);
    out = put(out, chunk.length, 8);
    for (const byte of chunk) out = put(out, byte, 8);
  }
  return put(out, 0, 8);
};

const toBytes = (w) => {
  const hex = w.acc.toString(16).padStart(w.n / 4, "0");
  return Buffer.from(hex, "hex");
};

export const finish = (w) => toBytes(filler(w));

const enc = (w, term) => {
  switch (term.k) {
    case "var":
      return nat(put(w, 0, 4), term.index);
    case "delay":
      return enc(put(w, 1, 4), term.body);
    case "lam":
      return enc(put(w, 2, 4), term.body);
    case "app":
      return enc(enc(put(w, 3, 4), term.function), term.argument);
    case "force":
      return enc(put(w, 5, 4), term.body);
    case "err":
      return put(w, 6, 4);
    case "builtin":
      return put(put(w, 7, 4), term.tag, 7);
    case "constr":
      return encList(nat(put(w, 8, 4), term.tag), term.fields);
    case "case":
      return encList(enc(put(w, 9, 4), term.scrutinee), term.branches);
    case "con":
      return encValue(typeTags(put(w, 4, 4), term.tags), term.value);
    default:
      throw new Error(`unknown term ${term.k}`);
  }
};

const encList = (w, terms) => {
  let out = w;
  for (const term of terms) out = enc(put(out, 1, 1), term);
  return put(out, 0, 1);
};

const encValue = (w, value) => {
  switch (value.k) {
    case "int":
      return int(w, value.n);
    case "bytes":
      return chunks(filler(w), value.bytes);
    case "chunks": {
      let out = filler(w);
      for (const part of value.parts) {
        out = put(out, part.length, 8);
        for (const byte of part) out = put(out, byte, 8);
      }
      return put(out, 0, 8);
    }
    case "bool":
      return put(w, value.b ? 1 : 0, 1);
    case "list": {
      let out = w;
      for (const item of value.items) out = encValue(put(out, 1, 1), item);
      return put(out, 0, 1);
    }
    case "pair":
      return encValue(encValue(w, value.first), value.second);
    case "unit":
      return w;
    default:
      throw new Error(`unknown value ${value.k}`);
  }
};

const typeTags = (w, tags) => {
  let out = w;
  for (const tag of tags) out = put(put(out, 1, 1), tag, 4);
  return put(out, 0, 1);
};

export const program = (minor, body) =>
  finish(enc(nat(nat(nat(W0, 1), minor), 0), body));

export const varT = (index) => ({ k: "var", index });
export const delay = (body) => ({ k: "delay", body });
export const lam = (body) => ({ k: "lam", body });
export const app = (fn, argument) => ({ k: "app", function: fn, argument });
export const force = (body) => ({ k: "force", body });
export const err = () => ({ k: "err" });
export const builtin = (tag) => ({ k: "builtin", tag });
export const constr = (tag, fields) => ({ k: "constr", tag, fields });
export const caseOf = (scrutinee, branches) => ({
  k: "case",
  scrutinee,
  branches,
});
export const con = (tags, value) => ({ k: "con", tags, value });
export const vInt = (n) => ({ k: "int", n });
export const vBytes = (bytes) => ({ k: "bytes", bytes: Buffer.from(bytes) });
/** A byte string written as the given Flat chunks, each 1-255 bytes. */
export const vChunks = (parts) => ({
  k: "chunks",
  parts: parts.map((part) => Buffer.from(part)),
});
export const vBool = (b) => ({ k: "bool", b });
export const vList = (items) => ({ k: "list", items });
export const vPair = (first, second) => ({ k: "pair", first, second });
export const vUnit = () => ({ k: "unit" });
export const vValue = (entries) =>
  vList(
    entries.map(([currency, tokens]) =>
      vPair(
        vBytes(Buffer.from(currency, "hex")),
        vList(
          tokens.map(([name, quantity]) =>
            vPair(vBytes(Buffer.from(name, "hex")), vInt(quantity)),
          ),
        ),
      ),
    ),
  );

// ## Plutus Data, written as the ledger's `serialiseData` writes it

const cborHead = (major, argument) => {
  const n = BigInt(argument);
  const initial = major << 5;
  if (n < 24n) return Buffer.from([initial | Number(n)]);
  for (const [extra, bytes] of [
    [24, 1],
    [25, 2],
    [26, 4],
    [27, 8],
  ]) {
    if (n < 1n << BigInt(8 * bytes)) {
      const hex = n.toString(16).padStart(2 * bytes, "0");
      return Buffer.concat([
        Buffer.from([initial | extra]),
        Buffer.from(hex, "hex"),
      ]);
    }
  }
  throw new Error("argument over 64 bits");
};

const cborBytes = (bytes) => {
  if (bytes.length <= 64) {
    return Buffer.concat([cborHead(2, bytes.length), bytes]);
  }
  const parts = [Buffer.from([0x5f])];
  for (let offset = 0; offset < bytes.length; offset += 64) {
    const chunk = bytes.subarray(offset, offset + 64);
    parts.push(cborHead(2, chunk.length), chunk);
  }
  parts.push(Buffer.from([0xff]));
  return Buffer.concat(parts);
};

const cborList = (items) =>
  items.length === 0
    ? Buffer.from([0x80])
    : Buffer.concat([
        Buffer.from([0x9f]),
        ...items.map(cborData),
        Buffer.from([0xff]),
      ]);

export const cborData = (data) => {
  switch (data.k) {
    case "I": {
      const n = data.n;
      if (n >= 0n && n < 1n << 64n) return cborHead(0, n);
      if (n < 0n && n >= -(1n << 64n)) return cborHead(1, -n - 1n);
      const magnitude = n >= 0n ? n : -n - 1n;
      let hex = magnitude.toString(16);
      if (hex.length % 2 === 1) hex = `0${hex}`;
      return Buffer.concat([
        Buffer.from([n >= 0n ? 0xc2 : 0xc3]),
        cborBytes(Buffer.from(hex, "hex")),
      ]);
    }
    case "B":
      return cborBytes(data.bytes);
    case "List":
      return cborList(data.items);
    case "Map":
      return Buffer.concat([
        cborHead(5, data.entries.length),
        ...data.entries.flatMap(([key, value]) => [
          cborData(key),
          cborData(value),
        ]),
      ]);
    case "Constr": {
      const { tag, fields } = data;
      if (tag < 7)
        return Buffer.concat([cborHead(6, 121 + tag), cborList(fields)]);
      if (tag < 128) {
        return Buffer.concat([cborHead(6, 1280 + tag - 7), cborList(fields)]);
      }
      return Buffer.concat([
        cborHead(6, 102),
        cborHead(4, 2),
        cborHead(0, tag),
        cborList(fields),
      ]);
    }
    default:
      throw new Error(`unknown data ${data.k}`);
  }
};

export const iData = (n) => ({ k: "I", n: BigInt(n) });
export const bData = (bytes) => ({ k: "B", bytes: Buffer.from(bytes) });
export const listData = (items) => ({ k: "List", items });
export const mapData = (entries) => ({ k: "Map", entries });
export const constrData = (tag, fields) => ({ k: "Constr", tag, fields });

export const pow2 = (n) => 1n << BigInt(n);
export const repeat = (n, item) => Array.from({ length: n }, () => item);
