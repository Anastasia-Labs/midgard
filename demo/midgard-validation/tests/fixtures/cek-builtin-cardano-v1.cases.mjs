/**
 * The argument mix of the `cek-builtin-cardano-v1` golden channel: seeded
 * random cases for every Plutus V3 builtin tag plus fixed edge vectors for the
 * size, range and encoding rules each builtin enforces. Every value is a pure
 * function of its tag's seed, so the generator and any reader rebuild the same
 * list.
 *
 * Argument encoding (JSON):
 *   ty: "int" | "bs" | "str" | "unit" | "bool" | "data" | "g1" | "g2" | "ml"
 *       | { list: ty } | { pair: [ty, ty] }
 *   int: decimal string; bs, g1, g2: hex (points compressed); str: string;
 *   unit: null; bool: boolean; list: array; pair: [first, second];
 *   data: { i: decimal } | { b: hex } | { l: [data] } | { m: [[data, data]] }
 *         | { c: decimal, f: [data] };
 *   ml: { loop: [g1 hex, g2 hex] } | { mul: [ml, ml] }  (a Miller-loop
 *       expression; tags 68 and 69 are reached only inside finalVerify's
 *       arguments because their result has no literal form).
 */
import {
  CEK_BUILTIN_CARDANO_MAX_TAG,
  CONSTR_INDEXES,
  G1_POOL,
  G2_POOL,
  INT64_MAX,
  INT64_MIN,
  SIMPLE,
  UTF8_CASES,
  any,
  anyTy,
  boolArg,
  bs,
  compressedCase,
  dst,
  ecdsaCase,
  ed25519Case,
  hex,
  int,
  makeRng,
  ml,
  randomBytes,
  randomData,
  randomInteger,
  randomMl,
  randomString,
  relatedBytes,
  schnorrCase,
  strArg,
} from "./cek-builtin-cardano-v1.arguments.mjs";
import { EDGES } from "./cek-builtin-cardano-v1.edges.mjs";

/* ---------- per-tag random arguments ---------- */

const randomArguments = (tag, rng) => {
  const g1 = () => ({ ty: "g1", v: rng.pick(G1_POOL) });
  const g2 = () => ({ ty: "g2", v: rng.pick(G2_POOL) });
  const bytes = () => bs(randomBytes(rng));
  const integer = () => int(randomInteger(rng));
  const data = () => ({ ty: "data", v: randomData(rng) });
  const bool = () => boolArg(rng.chance(0.5));
  const index = (low, high) =>
    int(
      rng.chance(0.2)
        ? randomInteger(rng)
        : BigInt(low + rng.int(high - low + 1)),
    );
  if (tag <= 9) {
    return [integer(), rng.chance(0.15) ? int(0n) : integer()];
  }
  switch (tag) {
    case 10:
      return [bytes(), bytes()];
    case 11:
      return [index(-300, 600), bytes()];
    case 12:
      return [index(-5, 80), index(-5, 80), bytes()];
    case 13:
    case 18:
    case 19:
    case 20:
    case 71:
    case 72:
    case 78:
    case 84:
    case 85:
    case 86:
      return [bytes()];
    case 14:
      return [bytes(), index(-2, 70)];
    case 15:
    case 16:
    case 17: {
      const [left, right] = relatedBytes(rng);
      return [bs(left), bs(right)];
    }
    case 21:
      return ed25519Case(rng);
    case 22:
    case 23:
      return [strArg(randomString(rng)), strArg(randomString(rng))];
    case 24:
      return [strArg(randomString(rng))];
    case 25:
      return [
        bs(rng.chance(0.6) ? rng.pick(UTF8_CASES) : hex(randomBytes(rng))),
      ];
    case 26: {
      const ty = anyTy(rng);
      return [bool(), any(rng, ty), any(rng, ty)];
    }
    case 27:
      return [{ ty: "unit", v: null }, any(rng)];
    case 28:
      return [strArg(randomString(rng)), any(rng)];
    case 29:
    case 30:
      return [any(rng, { pair: [rng.pick(SIMPLE), rng.pick(SIMPLE)] })];
    case 31: {
      const ty = anyTy(rng);
      return [any(rng, { list: rng.pick(SIMPLE) }), any(rng, ty), any(rng, ty)];
    }
    case 32: {
      const ty = rng.pick(SIMPLE);
      return [any(rng, ty), any(rng, { list: ty })];
    }
    case 33:
    case 34:
    case 35:
      return [any(rng, { list: anyTy(rng) })];
    case 36: {
      const ty = anyTy(rng);
      return [data(), ...Array.from({ length: 5 }, () => any(rng, ty))];
    }
    case 37:
      // The pinned evaluator aborts on an index outside Word64, so the
      // channel keeps constructor indexes inside it.
      return [
        int(
          rng.chance(0.5)
            ? rng.pick(CONSTR_INDEXES)
            : BigInt(rng.int(1_000_001)),
        ),
        any(rng, { list: "data" }),
      ];
    case 38:
      return [any(rng, { list: { pair: ["data", "data"] } })];
    case 39:
      return [any(rng, { list: "data" })];
    case 40:
      return [integer()];
    case 41:
      return [bytes()];
    case 42:
    case 43:
    case 44:
    case 45:
    case 46:
    case 51:
      return [data()];
    case 47: {
      const left = randomData(rng);
      return [
        { ty: "data", v: left },
        { ty: "data", v: rng.chance(0.4) ? left : randomData(rng) },
      ];
    }
    case 48:
      return [data(), data()];
    case 49:
    case 50:
      return [{ ty: "unit", v: null }];
    case 52:
      return ecdsaCase(rng);
    case 53:
      return schnorrCase(rng);
    case 54:
    case 57:
      return [g1(), g1()];
    case 55:
    case 59:
      return [g1()];
    case 56:
      return [integer(), g1()];
    case 58:
    case 65:
      return [bytes(), bs(dst(rng))];
    case 60:
      return [bs(compressedCase(rng, G1_POOL, 48))];
    case 61:
    case 64:
      return [g2(), g2()];
    case 62:
    case 66:
      return [g2()];
    case 63:
      return [integer(), g2()];
    case 67:
      return [bs(compressedCase(rng, G2_POOL, 96))];
    case 68:
    case 69:
      return null;
    case 70:
      return [ml(randomMl(rng)), ml(randomMl(rng))];
    case 73:
      return [bool(), index(0, 40), integer()];
    case 74:
      return [bool(), bytes()];
    case 75:
    case 76:
    case 77:
      return [bool(), bytes(), bytes()];
    case 79:
      return [bytes(), index(-2, 90)];
    case 80:
      return [
        bytes(),
        {
          ty: { list: "int" },
          v: Array.from({ length: rng.int(4) }, () =>
            (rng.chance(0.2)
              ? randomInteger(rng)
              : BigInt(rng.int(93) - 2)
            ).toString(10),
          ),
        },
        bool(),
      ];
    case 81:
      return [index(-1, 70), index(-1, 256)];
    case 82:
    case 83:
      return [bytes(), index(-80, 80)];
    default:
      throw new Error(`no argument generator for builtin ${String(tag)}`);
  }
};

const RANDOM_CASES = (tag) => {
  if (tag === 68 || tag === 69) return 0;
  if (tag >= 54 && tag <= 67) return 6;
  if (tag === 70) return 8;
  return 16;
};

// Cardano reads sliceByteString's two integers as Int64 machine Ints and fails
// outside that range; the pinned evaluator reads them as unsigned 64-bit
// values, clamping negatives, and aborts beyond that.
const withSliceIntegerBound = (definition) =>
  definition.tag === 12 &&
  definition.args.some(
    (argument) =>
      argument.ty === "int" &&
      (BigInt(argument.v) < INT64_MIN || BigInt(argument.v) > INT64_MAX),
  )
    ? {
        ...definition,
        // Plutus: the SliceByteString denotation takes `Int` arguments
        // (plutus-core PlutusCore/Default/Builtins.hs), `Int` unlifts through
        // `Int64`, and readKnownAsInteger fails an Integer outside its bounds
        // (PlutusCore/Default/Universe.hs).
        cardano: { kind: "error" },
        reason:
          "Cardano fails sliceByteString when an integer argument is outside Int64",
      }
    : definition;

/**
 * Every case of the channel, in tag order: `{ tag, note, args }`, plus
 * `cardano` and `reason` where the Cardano ledger's outcome differs from the
 * pinned Aiken evaluator's.
 */
export const buildCekBuiltinCardanoCases = () => {
  const cases = [];
  for (let tag = 0; tag <= CEK_BUILTIN_CARDANO_MAX_TAG; tag++) {
    const rng = makeRng(0x5eed0000 + tag);
    for (let index = 0; index < RANDOM_CASES(tag); index++) {
      cases.push({
        tag,
        note: `random ${index}`,
        args: randomArguments(tag, rng),
      });
    }
    for (const edge of EDGES[tag]?.() ?? []) cases.push({ tag, ...edge });
  }
  return cases.map(withSliceIntegerBound);
};
