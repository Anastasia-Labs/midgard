// Differential for the @lucid-evolution/plutus Data.to/Data.from patch: the
// installed (patched) conversion against the unpatched algorithm, which is
// restated below over the same CML, on random and boundary Data and CBOR.
import assert from "node:assert/strict";
import { createRequire } from "node:module";
import { dirname, join } from "node:path";
import { pathToFileURL } from "node:url";

const require = createRequire(import.meta.url);
const [format, casesArgument, seedArgument] = process.argv.slice(2);
assert(["esm", "cjs"].includes(format));
const lucidRequirePath = require.resolve("@lucid-evolution/lucid");
const lucidPath = join(dirname(lucidRequirePath), "index.js");
const { CML, Constr, Data, fromHex, toHex } =
  format === "cjs"
    ? require(lucidRequirePath)
    : await import(pathToFileURL(lucidPath).href);

// ---------- the unpatched conversion (@lucid-evolution/plutus@0.1.36) ----------
const withScope = (body) => {
  const owned = [];
  try {
    return body((object) => {
      owned.push(object);
      return object;
    });
  } finally {
    for (const object of owned) object.free();
  }
};
const oracleTo = (data, canonical) => {
  const serialize = (data2) => {
    try {
      return withScope((own) => {
        if (typeof data2 === "bigint") {
          return CML.PlutusData.new_integer(
            own(CML.BigInteger.from_str(data2.toString())),
          );
        } else if (typeof data2 === "string") {
          return CML.PlutusData.new_bytes(fromHex(data2));
        } else if (data2 instanceof Constr) {
          const { index, fields } = data2;
          const plutusList = own(CML.PlutusDataList.new());
          fields.forEach((field) => plutusList.add(own(serialize(field))));
          const alternative = own(CML.BigInteger.from_str(index.toString()));
          return CML.PlutusData.new_constr_plutus_data(
            own(CML.ConstrPlutusData.new(alternative.as_u64(), plutusList)),
          );
        } else if (data2 instanceof Array) {
          const plutusList = own(CML.PlutusDataList.new());
          data2.forEach((arg) => plutusList.add(own(serialize(arg))));
          return CML.PlutusData.new_list(plutusList);
        } else if (data2 instanceof Map) {
          const plutusMap = own(CML.PlutusMap.new());
          for (const [key, value] of data2.entries()) {
            plutusMap.set(own(serialize(key)), own(serialize(value)));
          }
          return CML.PlutusData.new_map(plutusMap);
        }
        throw new Error("Unsupported type");
      });
    } catch (error) {
      throw new Error("Could not serialize the data: " + error);
    }
  };
  return withScope((own) => {
    const serialized = own(serialize(data));
    return canonical
      ? serialized.to_canonical_cbor_hex()
      : own(serialized.to_cardano_node_format()).to_cbor_hex();
  });
};
const oracleFrom = (raw) => {
  const deserialize = (data2) =>
    withScope((own) => {
      if (data2.kind() === 0) {
        const constr = own(data2.as_constr_plutus_data());
        const l = own(constr.fields());
        const desL = [];
        for (let i = 0; i < l.len(); i++) desL.push(deserialize(own(l.get(i))));
        return new Constr(parseInt(constr.alternative().toString()), desL);
      } else if (data2.kind() === 1) {
        const m = own(data2.as_map());
        const desM = new Map();
        const keys = own(m.keys());
        for (let i = 0; i < keys.len(); i++) {
          const key = own(keys.get(i));
          desM.set(deserialize(key), deserialize(own(m.get(key))));
        }
        return desM;
      } else if (data2.kind() === 2) {
        const l = own(data2.as_list());
        const desL = [];
        for (let i = 0; i < l.len(); i++) desL.push(deserialize(own(l.get(i))));
        return desL;
      } else if (data2.kind() === 3) {
        return BigInt(own(data2.as_integer()).to_str());
      } else if (data2.kind() === 4) {
        return toHex(data2.as_bytes());
      }
      throw new Error("Unsupported type");
    });
  return withScope((own) =>
    deserialize(own(CML.PlutusData.from_cbor_hex(raw))),
  );
};

// ---------- deterministic generators ----------
let seed = Number(seedArgument) >>> 0;
const random = () => {
  seed = (seed + 0x6d2b79f5) >>> 0;
  let t = seed;
  t = Math.imul(t ^ (t >>> 15), t | 1);
  t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
};
const int = (n) => Math.floor(random() * n);
const pick = (xs) => xs[int(xs.length)];
const hexBytes = (n) =>
  Array.from({ length: n }, () => int(256).toString(16).padStart(2, "0")).join(
    "",
  );
const bigPool = [
  0n,
  1n,
  -1n,
  23n,
  24n,
  -24n,
  -25n,
  255n,
  256n,
  65535n,
  65536n,
  2n ** 32n - 1n,
  2n ** 32n,
  2n ** 63n,
  2n ** 64n - 1n,
  2n ** 64n,
  -(2n ** 64n),
  -(2n ** 64n) - 1n,
  2n ** 70n,
  -(2n ** 70n),
  2n ** 200n + 12345n,
];
const randomBigInt = () => {
  const r = random();
  if (r < 0.4) return BigInt(int(200) - 100);
  if (r < 0.7) return pick(bigPool);
  let v = 0n;
  for (let i = 0, n = 1 + int(5); i < n; i++)
    v = (v << 64n) | (BigInt(int(2 ** 31)) * BigInt(int(2 ** 31)));
  return random() < 0.5 ? v : -v;
};
const mapSize = () => {
  const r = random();
  if (r < 0.15) return 0;
  if (r < 0.55) return 1 + int(7);
  if (r < 0.85) return 8 + int(20);
  if (r < 0.97) return 60 + int(60);
  return 250 + int(60);
};
const clone = (v) => {
  if (v instanceof Array) return v.map(clone);
  if (v instanceof Map)
    return new Map([...v].map(([k, x]) => [clone(k), clone(x)]));
  if (v instanceof Constr) return new Constr(v.index, v.fields.map(clone));
  return v;
};
// JS Data for Data.to, including keys equal to earlier keys as other objects
// or other hex case, and rare unsupported values.
const genData = (depth, budget) => {
  const r = random();
  if (r < 0.0003) return pick([1, undefined, null, true, 1.5]);
  budget.n--;
  if (depth <= 0 || budget.n <= 0 || r < 0.3) {
    if (random() < 0.5) return randomBigInt();
    const h = hexBytes(pick([0, 1, 2, 28, 32, 63, 64, 65, 100, int(70)]));
    return random() < 0.05 ? h.toUpperCase() : h;
  }
  if (r < 0.5)
    return new Constr(
      pick([0, 1, 2, 6, 7, 8, 100, 126, 127, 128, 1000, int(50)]),
      Array.from({ length: int(5) }, () => genData(depth - 1, budget)),
    );
  if (r < 0.65)
    return Array.from({ length: int(6) }, () => genData(depth - 1, budget));
  const map = new Map();
  const keys = [];
  for (let i = 0, n = mapSize(); i < n; i++) {
    const kr = random();
    let key;
    if (keys.length && kr < 0.04) key = clone(pick(keys));
    else if (keys.length && kr < 0.06) {
      const previous = pick(keys);
      key =
        typeof previous === "string"
          ? previous.toUpperCase() === previous
            ? previous.toLowerCase()
            : previous.toUpperCase()
          : genData(1, { n: 4 });
    } else
      key =
        random() < 0.6
          ? genData(0, { n: 1 })
          : genData(Math.min(depth - 1, 2), { n: 6 });
    keys.push(key);
    map.set(key, genData(depth - 1, budget));
  }
  return map;
};
// CBOR for Data.from in every encoding Plutus Data admits (non-minimal
// heads, indefinite lists/maps/bytes, both constructor tag forms, bignums),
// with duplicate keys, equal keys in other encodings and rare corruption.
const head = (major, n, width) => {
  const b = BigInt(n);
  const w =
    width ??
    (b < 24n ? 0 : b < 256n ? 1 : b < 65536n ? 2 : b < 4294967296n ? 4 : 8);
  const ai = w === 0 ? Number(b) : { 1: 24, 2: 25, 4: 26, 8: 27 }[w];
  let s = ((major << 5) | ai).toString(16).padStart(2, "0");
  if (w > 0) s += b.toString(16).padStart(w * 2, "0");
  return s;
};
const widthFor = (b) => {
  const minimal =
    b < 24n ? 0 : b < 256n ? 1 : b < 65536n ? 2 : b < 4294967296n ? 4 : 8;
  if (random() < 0.85) return minimal;
  return pick(
    [0, 1, 2, 4, 8].filter((w) => w >= minimal && (w !== 0 || b < 24n)),
  );
};
const encodeBytes = (hex) => {
  const n = hex.length / 2;
  if (n <= 64 && random() < 0.8) return head(2, n, widthFor(BigInt(n))) + hex;
  if (n > 64 && random() < 0.002) return head(2, n) + hex;
  let s = "5f";
  for (let i = 0; i < n; ) {
    const c = Math.min(n - i, 1 + int(64));
    s += head(2, c) + hex.slice(i * 2, (i + c) * 2);
    i += c;
  }
  return s + "ff";
};
const encodeInteger = (v) => {
  const negative = v < 0n;
  const magnitude = negative ? -1n - v : v;
  if (magnitude < 2n ** 64n && random() < 0.9)
    return head(negative ? 1 : 0, magnitude, widthFor(magnitude));
  let h = magnitude === 0n ? "" : magnitude.toString(16);
  if (h.length % 2) h = "0" + h;
  return (negative ? "c3" : "c2") + encodeBytes(h);
};
const encodeList = (items) =>
  random() < 0.5
    ? head(4, items.length, widthFor(BigInt(items.length))) + items.join("")
    : "9f" + items.join("") + "ff";
const encodeConstr = (index, fields) => {
  const list = encodeList(fields);
  if (index < 7 && random() < 0.8) return head(6, 121 + index) + list;
  if (index >= 7 && index < 128 && random() < 0.8)
    return head(6, 1280 + index - 7) + list;
  return head(6, 102) + "82" + head(0, index) + list;
};
const encodeData = (v) => {
  if (typeof v === "bigint") return encodeInteger(v);
  if (typeof v === "string") return encodeBytes(v);
  if (v instanceof Array) return encodeList(v.map(encodeData));
  if (v instanceof Map) {
    const entries = [...v].map(([k, x]) => encodeData(k) + encodeData(x));
    return random() < 0.5
      ? head(5, entries.length) + entries.join("")
      : "bf" + entries.join("") + "ff";
  }
  return encodeConstr(v.index, v.fields.map(encodeData));
};
const reencode = (hex) => {
  try {
    return encodeData(oracleFrom(hex));
  } catch {
    return hex;
  }
};
const genCbor = (depth, budget) => {
  const r = random();
  budget.n--;
  if (depth <= 0 || budget.n <= 0 || r < 0.3)
    return random() < 0.5
      ? encodeInteger(randomBigInt())
      : encodeBytes(hexBytes(pick([0, 1, 28, 32, 64, 65, 130, int(70)])));
  if (r < 0.5)
    return encodeConstr(
      pick([0, 1, 6, 7, 100, 127, 128, 135, 1000, int(10)]),
      Array.from({ length: int(5) }, () => genCbor(depth - 1, budget)),
    );
  if (r < 0.65)
    return encodeList(
      Array.from({ length: int(6) }, () => genCbor(depth - 1, budget)),
    );
  const n = mapSize();
  const entries = [];
  const keys = [];
  for (let i = 0; i < n; i++) {
    const kr = random();
    let key;
    if (keys.length && kr < 0.03) key = pick(keys);
    else if (keys.length && kr < 0.08) key = reencode(pick(keys));
    else
      key =
        random() < 0.6
          ? genCbor(0, { n: 1 })
          : genCbor(Math.min(depth - 1, 2), { n: 6 });
    keys.push(key);
    entries.push(key + genCbor(depth - 1, budget));
  }
  return random() < 0.5
    ? head(5, n, widthFor(BigInt(n))) + entries.join("")
    : "bf" + entries.join("") + "ff";
};

// ---------- comparison ----------
const show = (v) => {
  if (typeof v === "bigint") return "i" + v;
  if (typeof v === "string") return "b" + v;
  if (v instanceof Array) return "l[" + v.map(show).join(",") + "]";
  if (v instanceof Map)
    return (
      "m" +
      v.size +
      "{" +
      [...v].map(([k, x]) => show(k) + ":" + show(x)).join(",") +
      "}"
    );
  if (v instanceof Constr)
    return "c" + v.index + "(" + v.fields.map(show).join(",") + ")";
  return "?" + String(v);
};
const outcome = (f) => {
  try {
    return "ok:" + f();
  } catch (error) {
    return "err:" + String(error?.message ?? error);
  }
};
const report = {
  format,
  toCases: 0,
  toRefusals: 0,
  fromCases: 0,
  fromRefusals: 0,
  largeInputs: 0,
  mismatches: [],
};
const compareFrom = (hex) => {
  const expected = outcome(() => show(oracleFrom(hex)));
  const actual = outcome(() => show(Data.from(hex)));
  report.fromCases++;
  if (expected.startsWith("err:")) report.fromRefusals++;
  if (expected !== actual)
    report.mismatches.push({
      from: hex.slice(0, 400),
      expected: expected.slice(0, 400),
      actual: actual.slice(0, 400),
    });
};
const compareTo = (data) => {
  for (const canonical of [false, true]) {
    const expected = outcome(() => oracleTo(data, canonical));
    const actual = outcome(() => Data.to(data, undefined, { canonical }));
    report.toCases++;
    if (expected.startsWith("err:")) report.toRefusals++;
    if (expected !== actual)
      report.mismatches.push({
        to: show(data).slice(0, 400),
        canonical,
        expected: expected.slice(0, 400),
        actual: actual.slice(0, 400),
      });
    if (expected.startsWith("ok:")) compareFrom(expected.slice(3));
  }
};

// Boundary values: empty and >64-entry maps, keys repeated as distinct
// objects, in other hex case, in the other constructor tag form, as nested
// maps, out of order, integers and byte strings at encoding boundaries, and
// hex in either case, with stray characters or of half a byte.
const wide = (n, f) => new Map(Array.from({ length: n }, (_, i) => f(i)));
for (const data of [
  new Map(),
  wide(64, (i) => [BigInt(i), "aa"]),
  wide(65, (i) => [
    i.toString(16).padStart(56, "a"),
    new Constr(0, [BigInt(i)]),
  ]),
  wide(1287, (i) => [i.toString(16).padStart(64, "0"), BigInt(i)]),
  wide(300, (i) => [BigInt(299 - i), new Map([[BigInt(i), "bb"]])]),
  new Map([
    ["abcd", 1n],
    ["ABCD", 2n],
    ["00", 3n],
    ["abcd", 4n],
  ]),
  new Map([
    [new Constr(0, [1n]), 1n],
    [new Constr(0, [1n]), 2n],
    [new Constr(1, []), 3n],
  ]),
  new Map([
    [new Map([[1n, 2n]]), 1n],
    [new Map([[1n, 2n]]), 2n],
    [new Map([[2n, 1n]]), 3n],
  ]),
  new Map([
    [[1n, "00"], 1n],
    [[1n, "00"], 2n],
  ]),
  [2n ** 64n - 1n, 2n ** 64n, -(2n ** 64n), -(2n ** 64n) - 1n, 2n ** 200n],
  ["", "00".repeat(64), "00".repeat(65), "ab".repeat(129)],
  new Constr(127, []),
  new Constr(128, [new Map()]),
  new Constr(1000, [[]]),
  new Constr(2 ** 53 - 1, []),
  new Constr(-1, []),
  new Constr(1.5, []),
  new Map([[1n, 1]]),
  "abc",
  "zz",
  "ABCDEF",
  "aB".repeat(100),
  "0x00",
  " 00",
  "00\n",
  new Constr(0, ["Ff".repeat(70), "00 "]),
])
  compareTo(data);
for (const hex of [
  "a0",
  "bf01020102ff",
  "a2d8798001d905008002",
  "a2d8798001d8668200800202",
  "a3410101410102410103",
  "a2c249010000000000000000014100c24901000000000000000002",
  "5f41aa41bbff",
  "9f9fffff",
  "d866820080",
  "d8669f0080ff",
  "a1",
  "a10102",
  "",
  "zz",
  "A0",
  "Bf01020102FF",
  "D87980",
  "58" + "46" + "Ab".repeat(70),
  "a0a0",
  "a",
  "a0a",
  "a0zz",
  "0xa0",
  " a0",
  "a0\n",
])
  compareFrom(hex);

const cases = Number(casesArgument);
for (let c = 0; c < cases; c++) {
  compareTo(genData(1 + int(4), { n: 40 + int(200) }));
  let hex = genCbor(1 + int(4), { n: 40 + int(200) });
  if (hex.length > 0x2000) report.largeInputs++;
  const corruption = random();
  if (corruption < 0.01)
    hex = hex.slice(0, Math.max(2, hex.length - 2 * (1 + int(4))));
  else if (corruption < 0.02) hex += "00";
  else if (corruption < 0.03) {
    const i = 2 * int(hex.length / 2);
    hex = hex.slice(0, i) + hexBytes(1) + hex.slice(i + 2);
  }
  compareFrom(hex);
}
process.stdout.write(JSON.stringify(report));
