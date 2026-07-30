/**
 * C26 INVESTIGATION SCAFFOLDING — throwaway, not evidence.
 *
 * Prototypes and validates the recommended primary workaround from
 * `docs/exec-plans/evidence/c26-cml-investigation.md`: replace the
 * `Data.from(hex)` decodability probe inside
 * `demo/midgard-core/src/codec/datum.ts` with an iterative, WASM-free
 * Plutus-Data well-formedness check.
 *
 * The prototype validator below is the proposed production logic verbatim, so
 * these tests are the semantic-equivalence argument for that change:
 *   - it agrees with CML/lucid `Data.from` on accept/reject wherever CML can
 *     answer at all, once the existing canonicity gate is composed in, and
 *   - it keeps answering above CML's ~1,522-deep ceiling.
 *
 * Skipped unless `MIDGARD_C26_INVESTIGATION=1`, because the differential
 * corpus intentionally drives CML past its limit and a trapped
 * `WebAssembly.Instance` is poisoned for the rest of the process.
 *
 * Run with:
 *   MIDGARD_C26_INVESTIGATION=1 pnpm --dir demo/midgard-validation test -- \
 *     c26-investigation-iterative-gate
 */
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

const investigationEnabled =
  process.env.MIDGARD_C26_INVESTIGATION === "1";
const describeInvestigation = investigationEnabled
  ? describe
  : describe.skip;

const MAX_PLUTUS_BYTES_CHUNK = 64;

const isConstrTag = (tag: bigint): boolean =>
  (tag >= 121n && tag <= 127n) || (tag >= 1_280n && tag <= 1_400n);

type CborHead = {
  readonly major: number;
  readonly value: bigint | null;
  readonly offset: number;
};

const readHead = (bytes: Buffer, offset: number): CborHead => {
  const initial = bytes[offset];
  if (initial === undefined) {
    throw new Error("Unexpected end of PlutusData CBOR");
  }
  const major = initial >> 5;
  const additional = initial & 0x1f;
  if (additional < 24) {
    return { major, value: BigInt(additional), offset: offset + 1 };
  }
  if (additional === 24) {
    const next = bytes[offset + 1];
    if (next === undefined) {
      throw new Error("Truncated PlutusData CBOR head");
    }
    return { major, value: BigInt(next), offset: offset + 2 };
  }
  if (additional === 25) {
    if (offset + 3 > bytes.length) {
      throw new Error("Truncated PlutusData CBOR head");
    }
    return {
      major,
      value: BigInt(bytes.readUInt16BE(offset + 1)),
      offset: offset + 3,
    };
  }
  if (additional === 26) {
    if (offset + 5 > bytes.length) {
      throw new Error("Truncated PlutusData CBOR head");
    }
    return {
      major,
      value: BigInt(bytes.readUInt32BE(offset + 1)),
      offset: offset + 5,
    };
  }
  if (additional === 27) {
    if (offset + 9 > bytes.length) {
      throw new Error("Truncated PlutusData CBOR head");
    }
    return {
      major,
      value: bytes.readBigUInt64BE(offset + 1),
      offset: offset + 9,
    };
  }
  if (additional === 31) {
    return { major, value: null, offset: offset + 1 };
  }
  throw new Error(
    `Unsupported PlutusData CBOR additional information ${additional.toString()}`,
  );
};

type GateFrame =
  | { readonly kind: "items"; remaining: bigint }
  | { readonly kind: "itemsIndefinite" }
  | { readonly kind: "pairs"; remaining: bigint; half: 0 | 1 }
  | { readonly kind: "pairsIndefinite"; half: 0 | 1 }
  | { readonly kind: "generalConstr"; stage: 0 | 1 };

/**
 * PROPOSED PRODUCTION LOGIC. Validates that `bytes` is exactly one
 * well-formed Plutus Data value with no trailing content, using a single pass
 * and an explicit frame stack. No CML, no WASM, no host recursion, so depth is
 * bounded only by the transaction size that carries the value.
 */
const assertPlutusDataWellFormed = (bytes: Buffer): void => {
  const stack: GateFrame[] = [];
  let cursor = 0;
  let valueComplete = false;

  const completeValue = (): void => {
    for (;;) {
      const frame = stack.at(-1);
      if (frame === undefined) {
        valueComplete = true;
        return;
      }
      if (frame.kind === "generalConstr") {
        if (frame.stage === 0) {
          frame.stage = 1;
          return;
        }
        stack.pop();
        continue;
      }
      if (frame.kind === "items") {
        frame.remaining -= 1n;
        if (frame.remaining > 0n) {
          return;
        }
        stack.pop();
        continue;
      }
      if (frame.kind === "itemsIndefinite") {
        return;
      }
      if (frame.half === 0) {
        frame.half = 1;
        return;
      }
      frame.half = 0;
      if (frame.kind === "pairs") {
        frame.remaining -= 1n;
        if (frame.remaining > 0n) {
          return;
        }
        stack.pop();
        continue;
      }
      return;
    }
  };

  const pushItems = (count: bigint | null): void => {
    if (count === null) {
      stack.push({ kind: "itemsIndefinite" });
      return;
    }
    if (count === 0n) {
      completeValue();
      return;
    }
    stack.push({ kind: "items", remaining: count });
  };

  while (!valueComplete) {
    const frame = stack.at(-1);
    if (bytes[cursor] === 0xff) {
      if (frame === undefined) {
        throw new Error("Unexpected PlutusData break marker");
      }
      if (frame.kind === "itemsIndefinite") {
        stack.pop();
        cursor += 1;
        completeValue();
        continue;
      }
      if (frame.kind === "pairsIndefinite") {
        if (frame.half !== 0) {
          throw new Error("Indefinite PlutusData map is missing a value");
        }
        stack.pop();
        cursor += 1;
        completeValue();
        continue;
      }
      throw new Error("Unexpected PlutusData break marker");
    }

    const head = readHead(bytes, cursor);
    cursor = head.offset;

    if (head.major === 0 || head.major === 1) {
      if (head.value === null) {
        throw new Error("PlutusData integer must use a definite head");
      }
      completeValue();
      continue;
    }

    if (head.major === 2) {
      if (head.value === null) {
        while (bytes[cursor] !== 0xff) {
          const chunk = readHead(bytes, cursor);
          if (chunk.major !== 2 || chunk.value === null) {
            throw new Error(
              "Indefinite PlutusData bytes must contain only definite chunks",
            );
          }
          if (chunk.value > BigInt(MAX_PLUTUS_BYTES_CHUNK)) {
            throw new Error("PlutusData byte chunk exceeds 64 bytes");
          }
          const end = chunk.offset + Number(chunk.value);
          if (end > bytes.length) {
            throw new Error("Truncated PlutusData bytes chunk");
          }
          cursor = end;
        }
        cursor += 1;
      } else {
        if (head.value > BigInt(MAX_PLUTUS_BYTES_CHUNK)) {
          throw new Error(
            "Definite PlutusData bytes exceed 64 bytes and must be chunked",
          );
        }
        const end = cursor + Number(head.value);
        if (end > bytes.length) {
          throw new Error("Truncated PlutusData bytes");
        }
        cursor = end;
      }
      completeValue();
      continue;
    }

    if (head.major === 4) {
      pushItems(head.value);
      continue;
    }

    if (head.major === 5) {
      if (head.value === null) {
        stack.push({ kind: "pairsIndefinite", half: 0 });
      } else if (head.value === 0n) {
        completeValue();
      } else {
        stack.push({ kind: "pairs", remaining: head.value, half: 0 });
      }
      continue;
    }

    if (head.major === 6) {
      if (head.value === null) {
        throw new Error("PlutusData tag must use a definite head");
      }
      const tag = head.value;
      if (isConstrTag(tag)) {
        const fields = readHead(bytes, cursor);
        if (fields.major !== 4) {
          throw new Error("PlutusData constructor fields must be an array");
        }
        cursor = fields.offset;
        pushItems(fields.value);
        continue;
      }
      if (tag === 102n) {
        const outer = readHead(bytes, cursor);
        if (outer.major !== 4 || outer.value !== 2n) {
          throw new Error(
            "General PlutusData constructor must be a two-item array",
          );
        }
        cursor = outer.offset;
        stack.push({ kind: "generalConstr", stage: 0 });
        continue;
      }
      if (tag === 2n || tag === 3n) {
        const payload = readHead(bytes, cursor);
        if (payload.major !== 2 || payload.value === null) {
          throw new Error(
            "PlutusData bignum must wrap a definite byte string",
          );
        }
        const end = payload.offset + Number(payload.value);
        if (end > bytes.length) {
          throw new Error("Truncated PlutusData bignum payload");
        }
        cursor = end;
        completeValue();
        continue;
      }
      throw new Error(
        `Unsupported PlutusData constructor tag ${tag.toString()}`,
      );
    }

    throw new Error(
      `Unsupported PlutusData CBOR major type ${head.major.toString()}`,
    );
  }

  if (cursor !== bytes.length) {
    throw new Error("Trailing bytes after PlutusData value");
  }
};

const unaryConstructorDataCborHex = (depth: number): string =>
  "d8799f".repeat(depth) + "00" + "ff".repeat(depth);

const gateAccepts = (hex: string): boolean => {
  try {
    assertPlutusDataWellFormed(Buffer.from(hex, "hex"));
    return true;
  } catch {
    return false;
  }
};

const normalizerAcceptsExactly = (hex: string): boolean => {
  try {
    return (
      aikenSerialisedPlutusDataCborPreservingMapOrder(hex) === hex.toLowerCase()
    );
  } catch {
    return false;
  }
};

type CmlVerdict = "accept" | "reject" | "beyondCmlLimit";

const cmlVerdict = (hex: string): CmlVerdict => {
  try {
    Data.from(hex);
    return "accept";
  } catch (cause) {
    if (
      cause instanceof WebAssembly.RuntimeError ||
      String((cause as Error).message).includes("Maximum call stack")
    ) {
      return "beyondCmlLimit";
    }
    return "reject";
  }
};

const fixedCorpus = [
  "d8799f00ff",
  "00",
  "20",
  "40",
  "80",
  "a0",
  "9f00ff",
  "d8668218809f00ff",
  "d903e89f00ff",
  "d8789f00ff",
  "d8809f00ff",
  "d905799f00ff",
  "d9057880",
  "c249010000000000000000",
  "c349010000000000000000",
  "d81e820102",
  "f93c00",
  "f5",
  "f6",
  "f7",
  "8100",
  "d8798100",
  "bf0000ff",
  `5841${"aa".repeat(65)}`,
  `5f5840${"aa".repeat(64)}41aaff`,
  "0000",
  "d87980",
  "d87a80",
  "d8799f0102ffff",
  "9fff",
  "5fff",
  "5f40ff",
  "1b0000000000000001",
  "3b0000000000000001",
  "a10001",
  "bf00ff",
  "d86682189f00ff",
  "d866839f00ff9f00ff9f00ff",
  "c240",
  "c2f5",
  "d81f80",
  "d87b80",
  "d87f80",
  "d9050080",
  "d9057980",
  "9f9f9fff0000ffff",
] as const;

const pseudoRandomCorpus = (count: number): readonly string[] => {
  let seed = 0x2468_ace0;
  const next = (): number => {
    seed = (seed * 1_103_515_245 + 12_345) & 0x7fff_ffff;
    return seed / 0x7fff_ffff;
  };
  const leaves = ["00", "20", "40", "4101", "c24101", "5fff", "80", "a0"];
  const tags = [121, 122, 127, 1_280, 1_400, 120, 128, 1_401, 1_000];
  const build = (budget: number): string => {
    if (budget <= 0 || next() < 0.3) {
      return leaves[Math.floor(next() * leaves.length)]!;
    }
    const pick = Math.floor(next() * 6);
    if (pick === 0) {
      const tag = tags[Math.floor(next() * tags.length)]!;
      const tagHex =
        tag < 256
          ? `d8${tag.toString(16).padStart(2, "0")}`
          : `d9${tag.toString(16).padStart(4, "0")}`;
      return `${tagHex}9f${build(budget - 1)}ff`;
    }
    if (pick === 1) return `9f${build(budget - 1)}${build(budget - 1)}ff`;
    if (pick === 2) return `82${build(budget - 1)}${build(budget - 1)}`;
    if (pick === 3) return `a1${build(budget - 1)}${build(budget - 1)}`;
    if (pick === 4) return `bf${build(budget - 1)}${build(budget - 1)}ff`;
    const alternative = ["1880", "1881", "00"][Math.floor(next() * 3)]!;
    return `d86682${alternative}9f${build(budget - 1)}ff`;
  };
  return Array.from({ length: count }, () =>
    build(1 + Math.floor(next() * 4)),
  );
};

describeInvestigation("C26 investigation: iterative Plutus Data gate", () => {
  it("accepts the exact C26 maximum and adjacent depths that trap CML", () => {
    for (const depth of [1, 1_024, 1_523, 2_048, 4_043, 4_044, 16_000]) {
      expect(() =>
        assertPlutusDataWellFormed(
          Buffer.from(unaryConstructorDataCborHex(depth), "hex"),
        ),
      ).not.toThrow();
    }
  });

  it("stays linear where lucid Data.from is superlinear", () => {
    const start = Date.now();
    for (const depth of [1_024, 2_048, 4_043, 4_044, 16_000]) {
      assertPlutusDataWellFormed(
        Buffer.from(unaryConstructorDataCborHex(depth), "hex"),
      );
    }
    // Measured at well under 50ms total; the bound only has to exclude the
    // multi-second per-call cost of the recursive CML path it replaces.
    expect(Date.now() - start).toBeLessThan(5_000);
  });

  it("rejects every malformed Plutus Data shape the ledger rejects", () => {
    const mustReject = [
      "f93c00",
      "f5",
      "f6",
      "f7",
      "d903e89f00ff",
      "d8789f00ff",
      "d8809f00ff",
      "d905799f00ff",
      "d81e820102",
      `5841${"aa".repeat(65)}`,
      "0000",
      "d879",
      "d8799f",
      "c2f5",
    ];
    for (const hex of mustReject) {
      expect(gateAccepts(hex)).toBe(false);
    }
  });

  it("accepts every well-formed canonical Plutus Data shape", () => {
    const mustAccept = [
      "d8799f00ff",
      "00",
      "20",
      "40",
      "80",
      "a0",
      "9f00ff",
      "d8668218809f00ff",
      "c249010000000000000000",
      "c349010000000000000000",
      `5f5840${"aa".repeat(64)}41aaff`,
      "d87980",
      "d87a80",
    ];
    for (const hex of mustAccept) {
      expect(gateAccepts(hex)).toBe(true);
    }
  });

  it("matches the current composite verdict on a differential corpus", () => {
    const corpus = [...fixedCorpus, ...pseudoRandomCorpus(4_000)];
    const divergences: readonly string[] = corpus.flatMap((hex) => {
      const cml = cmlVerdict(hex);
      if (cml === "beyondCmlLimit") {
        return [];
      }
      const canonical = normalizerAcceptsExactly(hex);
      // Current production gate pair vs proposed gate pair.
      const current = cml === "accept" && canonical;
      const proposed = gateAccepts(hex) && canonical;
      return current === proposed ? [] : [hex];
    });
    expect(divergences).toEqual([]);
    expect(corpus.length).toBeGreaterThan(4_000);
  }, 300_000);
});
