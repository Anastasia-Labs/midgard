import { describe, expect, it } from "vitest";

import { initialMidgardCekContextControl } from "../src/cek-context.summarize-midgard-cek-context-parts.js";
import {
  cekCoreWitnessCbor,
  type CekWitnessAt,
  cekWitnessCbor,
} from "../src/validation-machine/cek-producer.context-step.js";

// `cekCoreWitnessCbor` builds each core-step witness of one execution by
// copying a template and writing the active state hash into its slot. Every
// witness it builds is held to the full encoding `cekWitnessCbor` makes, and
// every witness it has handed out to staying unchanged as later ones are
// built.

/** xorshift32, so the "random" corpus is identical on every run. */
const prng = (seed: number) => {
  let state = seed >>> 0 || 1;
  return () => {
    state ^= state << 13;
    state >>>= 0;
    state ^= state >>> 17;
    state ^= state << 5;
    state >>>= 0;
    return state;
  };
};

const next = prng(0xdecaf);

const random = (length: number): Buffer => {
  const out = Buffer.alloc(length);
  for (let i = 0; i < length; i++) out[i] = next() & 0xff;
  return out;
};

const pick = <T>(values: readonly T[]): T => values[next() % values.length]!;

const U64_EDGES = [
  0n,
  1n,
  23n,
  24n,
  255n,
  256n,
  65_535n,
  65_536n,
  (1n << 32n) - 1n,
  1n << 32n,
  1n << 63n,
  (1n << 64n) - 1n,
];

const u64 = (): bigint =>
  next() % 3 === 0 ? pick(U64_EDGES) : BigInt(next()) * BigInt(next() % 1_000);

const NATIVE_CONTROL_LENGTHS = [0, 1, 23, 24, 25, 255, 256, 1_000, 65_536];

/** The witness, or the refusal as its class and message. */
const outcome = (run: () => Buffer): string => {
  try {
    return run().toString("hex");
  } catch (error) {
    return `${(error as Error).constructor.name}: ${(error as Error).message}`;
  }
};

/** An active state hash: mostly 32 random bytes, sometimes a placeholder,
 * a wrong length or a value that is not a `Uint8Array`. */
const activeStateHash = (): Uint8Array => {
  const kind = next() % 24;
  if (kind === 0) return random(pick([0, 31, 33]));
  if (kind === 1) return Buffer.alloc(32, 0x00);
  if (kind === 2) return Buffer.alloc(32, 0xff);
  if (kind === 3)
    return Buffer.from(Array.from({ length: 32 }, (_, index) => index));
  if (kind === 4) return new Uint8Array(random(32)).buffer as never;
  if (kind === 5) return "a".repeat(32) as never;
  if (kind === 6) return Uint16Array.from(random(32)) as never;
  if (kind === 7) return random(40).subarray(3, 35);
  return random(32);
};

describe("CEK core witness template", () => {
  it("builds every core-step witness as the full encoding does, and never changes one it handed out", () => {
    let executions = 0;
    let witnesses = 0;
    for (let index = 0; index < 400; index++) {
      const at: CekWitnessAt = {
        nativeControlCbor: random(pick(NATIVE_CONTROL_LENGTHS)),
        executionCursor:
          next() % 5 === 0
            ? pick([0, 23, 24, 255, 256, 65_536, 2 ** 32 - 1])
            : next() % 100,
        completedCpu: u64(),
        completedMemory: u64(),
      };
      const control = initialMidgardCekContextControl({
        languageTag: next() % 2 === 0 ? 3 : 128,
        programTermRoot: random(32),
        purposeKind: (next() % 4) as 0 | 1 | 2 | 3,
        purposeIndex: u64() % (1n << 32n),
        scriptHash: random(28),
        subject: random(next() % 40),
        redeemerLeaf: random(32),
      });
      const exUnits = { steps: u64(), memory: u64() };
      const core = cekCoreWitnessCbor(at, control, exUnits);
      const handedOut: [Buffer, string][] = [];
      const steps = 1 + (next() % 40);
      for (let step = 0; step < steps; step++) {
        const hash = activeStateHash();
        const expected = outcome(() =>
          cekWitnessCbor(at, control, { activeStateHash: hash, exUnits }),
        );
        const built: Buffer[] = [];
        const actual = outcome(() => {
          const witness = core(hash);
          built.push(witness);
          return witness;
        });
        expect(actual, `execution ${index} step ${step}`).toBe(expected);
        for (const [kept, hex] of handedOut)
          expect(kept.toString("hex"), `execution ${index} step ${step}`).toBe(
            hex,
          );
        for (const witness of built) {
          handedOut.push([witness, actual]);
          if (hash instanceof Uint8Array) hash.fill(0x5a);
          expect(witness.toString("hex")).toBe(actual);
          witnesses++;
        }
      }
      executions++;
    }
    expect(executions).toBe(400);
    expect(witnesses).toBeGreaterThan(4_000);
  });
});
