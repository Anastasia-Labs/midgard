import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import { exactOptionalHash } from "../src/cek-data-frame.validate-midgard-cek-data-frame.js";
import { exactHash as exactProofHash } from "../src/cek-proof.encode-midgard-cek-term-node.js";
import { exactHash as exactSemanticHash } from "../src/cek-semantic.decode-midgard-cek-data-node.js";
import {
  encodeMidgardCekMachineState,
  hashMidgardCekMachineState,
  type MidgardCekMachineState,
} from "../src/index.js";

// `hashMidgardCekMachineState` writes the state's CBOR straight into one
// buffer, once per CEK step. Every digest is held to the generic encoder's,
// and every refusal to the generic encoder's refusal.

const generic = (state: MidgardCekMachineState): string =>
  Buffer.from(
    blake2b(
      Buffer.concat([
        Buffer.from("MidgardCekMachineStateV1", "ascii"),
        encodeMidgardCekMachineState(state),
      ]),
      { dkLen: 32 },
    ),
  ).toString("hex");

const MODES: readonly MidgardCekMachineState["mode"][] = [
  "compute",
  "return",
  "lookup",
  "builtin",
  "haltSuccess",
  "haltError",
  "caseSelect",
  "caseApply",
  "semanticBuiltin",
];

const UINT32_EDGES = [
  0n,
  1n,
  23n,
  24n,
  255n,
  256n,
  65_535n,
  65_536n,
  0xffff_fffen,
  0xffff_ffffn,
];

const UINT64_EDGES = [
  ...UINT32_EDGES,
  0x1_0000_0000n,
  0xffff_ffff_ffff_fffen,
  0xffff_ffff_ffff_ffffn,
];

const hash = (seed: number): Buffer =>
  Buffer.from(
    Array.from({ length: 32 }, (_, index) => (seed * 37 + index) & 0xff),
  );

const state = (
  overrides: Partial<Record<keyof MidgardCekMachineState, unknown>> = {},
): MidgardCekMachineState =>
  ({
    mode: "compute",
    executionIndex: 2n,
    focusRoot: hash(1),
    environmentRoot: hash(2),
    continuationRoot: hash(3),
    auxiliary: 0n,
    cpu: 16_000n,
    memory: 100n,
    ...overrides,
  }) as MidgardCekMachineState;

/** The refusal `run` throws, as its class and message. */
const refusal = (run: () => unknown): string => {
  try {
    run();
  } catch (error) {
    return `${(error as Error).constructor.name}: ${(error as Error).message}`;
  }
  return "no refusal";
};

describe("CEK machine state hash", () => {
  it("matches the generic encoder for every mode and every integer edge", () => {
    for (const mode of MODES) {
      for (const executionIndex of UINT32_EDGES) {
        const at = state({ mode, executionIndex });
        expect(hashMidgardCekMachineState(at).toString("hex")).toBe(
          generic(at),
        );
      }
      for (const value of UINT64_EDGES) {
        for (const field of ["auxiliary", "cpu", "memory"] as const) {
          const at = state({ mode, [field]: value });
          expect(
            hashMidgardCekMachineState(at).toString("hex"),
            `${mode} ${field}=${value}`,
          ).toBe(generic(at));
        }
      }
    }
  });

  it("matches the generic encoder on random states and hash views", () => {
    let seed = 0x5eed;
    const next = () => {
      seed = (seed * 1_103_515_245 + 12_345) & 0x7fff_ffff;
      return seed;
    };
    const backing = Buffer.alloc(200);
    for (let i = 0; i < 2_000; i++) {
      for (let j = 0; j < backing.length; j++) backing[j] = next() & 0xff;
      const big = () =>
        UINT64_EDGES[next() % UINT64_EDGES.length]! ^
        BigInt(next() % 1_000_000);
      const at = state({
        mode: MODES[next() % MODES.length],
        executionIndex: UINT32_EDGES[next() % UINT32_EDGES.length],
        focusRoot: new Uint8Array(backing.buffer, next() % 100, 32),
        environmentRoot: backing.subarray(100, 132),
        continuationRoot: Buffer.from(backing.subarray(150, 182)),
        auxiliary: big() & 0xffff_ffff_ffff_ffffn,
        cpu: big() & 0xffff_ffff_ffff_ffffn,
        memory: big() & 0xffff_ffff_ffff_ffffn,
      });
      const digest = hashMidgardCekMachineState(at);
      expect(digest.length).toBe(32);
      expect(digest.toString("hex")).toBe(generic(at));
    }
  });

  it("refuses a bad state as the generic encoder refuses it", () => {
    const bad: Partial<Record<keyof MidgardCekMachineState, unknown>>[] = [
      { executionIndex: -1n },
      { executionIndex: 0x1_0000_0000n },
      { focusRoot: Buffer.alloc(31) },
      { environmentRoot: Buffer.alloc(33) },
      { continuationRoot: Buffer.alloc(0) },
      { auxiliary: -1n },
      { cpu: 0x1_0000_0000_0000_0000n },
      { memory: -5n },
      // The first bad field is the one refused, in the encoder's order.
      { executionIndex: -1n, focusRoot: Buffer.alloc(1) },
      { focusRoot: Buffer.alloc(1), cpu: -1n },
      { continuationRoot: Buffer.alloc(1), auxiliary: -1n },
      { auxiliary: -1n, memory: -1n },
    ];
    for (const overrides of bad) {
      const at = state(overrides);
      const expected = refusal(() => encodeMidgardCekMachineState(at));
      expect(expected).not.toBe("no refusal");
      expect(refusal(() => hashMidgardCekMachineState(at))).toBe(expected);
    }
  });

  it("reads or refuses a root that is not a Uint8Array as the generic encoder does", () => {
    const filled = new ArrayBuffer(32);
    new Uint8Array(filled).set(hash(9));
    const shared = new Uint8Array(new SharedArrayBuffer(36), 4, 32);
    shared.set(hash(10));
    const detached = new ArrayBuffer(40);
    const detachedView = new Uint8Array(detached, 4, 32);
    structuredClone(detached, { transfer: [detached] });
    class TaggedBytes extends Uint8Array {}
    const roots: [string, unknown][] = [
      ["ArrayBuffer(32)", filled],
      ["ArrayBuffer(31)", new ArrayBuffer(31)],
      ["SharedArrayBuffer view", shared],
      ["32-character string", "a".repeat(32)],
      ["32-byte multi-byte string", "\u00e9".repeat(16)],
      ["32-character multi-byte string", "\u00e9".repeat(32)],
      ["Uint16Array(32)", Uint16Array.from(hash(11), (byte) => byte * 257)],
      ["Int8Array(32)", Int8Array.from(hash(12), (byte) => byte - 128)],
      ["Float64Array(32)", Float64Array.from(hash(13), (byte) => byte + 0.5)],
      ["Uint8ClampedArray(32)", Uint8ClampedArray.from(hash(14))],
      ["DataView over 32 bytes", new DataView(filled)],
      ["array of 32", Array.from(hash(15), (byte) => byte * 3)],
      ["Uint8Array subclass", TaggedBytes.from(hash(16))],
      ["Proxy of a Buffer", new Proxy(hash(17), {})],
      ["detached view", detachedView],
      ["null", null],
      ["undefined", undefined],
      ["number", 32],
    ];
    const outcome = (run: () => Buffer | string): string => {
      const refused = refusal(run);
      if (refused !== "no refusal") return refused;
      const digest = run();
      return typeof digest === "string" ? digest : digest.toString("hex");
    };
    for (const [name, root] of roots) {
      for (const field of [
        "focusRoot",
        "environmentRoot",
        "continuationRoot",
      ] as const) {
        const at = state({ [field]: root });
        expect(
          outcome(() => hashMidgardCekMachineState(at)),
          `${field} = ${name}`,
        ).toBe(outcome(() => generic(at)));
      }
    }
  });

  it("hashes a state the direct writer does not cover through the encoder", () => {
    for (const overrides of [
      { mode: "unknown" },
      { executionIndex: 7 },
      { cpu: 16_000 },
    ]) {
      const at = state(overrides);
      expect(hashMidgardCekMachineState(at).toString("hex")).toBe(generic(at));
    }
  });

  it("copies a hash field it accepts, from an ArrayBuffer as from a view", () => {
    for (const read of [exactProofHash, exactSemanticHash, exactOptionalHash]) {
      const backing = new ArrayBuffer(32);
      const view = new Uint8Array(backing);
      view.set(hash(20));
      for (const value of [backing, view, Buffer.from(view)]) {
        const accepted = read(value as Uint8Array, "field");
        expect(accepted.equals(hash(20))).toBe(true);
        expect(accepted.buffer).not.toBe(backing);
        view[0] ^= 0xff;
        expect(accepted.equals(hash(20))).toBe(true);
        view[0] ^= 0xff;
      }
    }
  });
});
