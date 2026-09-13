import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  advanceMidgardBlake2b224Trace,
  buildMidgardBlake2b224Trace,
  digestMidgardBlake2b224Trace,
  encodeMidgardBlake2b224TraceControl,
  encodeMidgardNativeScript,
  hashMidgardVersionedScript,
  initialMidgardBlake2b224TraceControl,
  MIDGARD_BLAKE2B_BLOCK_BYTES,
  MIDGARD_BLAKE2B_ROUNDS,
  MidgardBlake2b224TraceStages,
  type MidgardVersionedScript,
  MidgardVersionedScriptTags,
} from "../src/index.js";

const referenceDigest = (message: Uint8Array): Buffer =>
  Buffer.from(blake2b(message, { dkLen: 28 }));

const nativeScript = {
  type: "sig",
  keyHash: Buffer.alloc(28, 0x44),
} as const;

describe("bounded BLAKE2b-224 trace V1", () => {
  it.each([1, 127, 128, 129, 255, 256, 257, 6_001, 16_385])(
    "matches the standard digest for %i bytes",
    (length) => {
      const message = Buffer.alloc(length, 0x6b);
      message[0] = 3;
      const trace = buildMidgardBlake2b224Trace(message);
      const terminal = trace.at(-1)!.next;
      const blockCount = Math.ceil(length / MIDGARD_BLAKE2B_BLOCK_BYTES);

      expect(trace).toHaveLength(blockCount * (MIDGARD_BLAKE2B_ROUNDS + 2));
      expect(digestMidgardBlake2b224Trace(terminal)).toStrictEqual(
        referenceDigest(message),
      );
    },
  );

  it("encodes the multi-block terminal state canonically", () => {
    const message = Buffer.alloc(6_001, 0x6b);
    message[0] = 3;
    const terminal = buildMidgardBlake2b224Trace(message).at(-1)!.next;
    expect(encodeMidgardBlake2b224TraceControl(terminal).toString("hex")).toBe(
      "8901031917711917715840634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098dbdd88d3a54a6646cce0bede0423668a5079fb08595004db249d66dbc8e10681056a775c40004000",
    );
  });

  it("encodes active 128-byte fields as canonical Plutus Data chunks", () => {
    const initial = initialMidgardBlake2b224TraceControl(129);
    const active = advanceMidgardBlake2b224Trace({
      control: initial,
      block: Buffer.alloc(128, 0x6b),
    })!;
    const initialChainingValue = Buffer.from(
      "14c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b",
      "hex",
    );
    const ivWithCounter = Buffer.from(
      "08c9bcf367e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa55182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b",
      "hex",
    );
    const expected = Buffer.concat([
      Buffer.from("8901010018815840", "hex"),
      initialChainingValue,
      Buffer.from("5f5840", "hex"),
      Buffer.alloc(64, 0x6b),
      Buffer.from("5840", "hex"),
      Buffer.alloc(64, 0x6b),
      Buffer.from("ff18805f5840", "hex"),
      initialChainingValue,
      Buffer.from("5840", "hex"),
      ivWithCounter,
      Buffer.from("ff00", "hex"),
    ]);
    const encoded = encodeMidgardBlake2b224TraceControl(active);
    expect(encoded).toStrictEqual(expected);
    expect(encoded.toString("hex")).not.toBe(
      "890101001881584014c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b5f58406b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b58406b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6bff18805f584014c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b584008c9bcf367e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa55182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05bff00",
    );
  });

  it.each([
    {
      language: "NativeCardano",
      scriptBytes: encodeMidgardNativeScript(nativeScript),
      nativeScript,
    },
    {
      language: "PlutusV3",
      scriptBytes: Buffer.alloc(6_000, 0x6b),
    },
    {
      language: "MidgardV1",
      scriptBytes: Buffer.alloc(6_000, 0x6b),
    },
  ] satisfies readonly MidgardVersionedScript[])(
    "matches the canonical $language script identity",
    (script) => {
      const message = Buffer.concat([
        Buffer.from([Number(MidgardVersionedScriptTags[script.language])]),
        script.scriptBytes,
      ]);
      const terminal = buildMidgardBlake2b224Trace(message).at(-1)!.next;
      expect(digestMidgardBlake2b224Trace(terminal)!.toString("hex")).toBe(
        hashMidgardVersionedScript(script),
      );
    },
  );

  it("fails closed for wrong block boundaries and malformed state", () => {
    const initial = initialMidgardBlake2b224TraceControl(129);
    expect(
      advanceMidgardBlake2b224Trace({
        control: initial,
        block: Buffer.alloc(127),
      }),
    ).toBeNull();
    expect(
      advanceMidgardBlake2b224Trace({
        control: { ...initial, chainingValue: Buffer.alloc(63) },
        block: Buffer.alloc(128),
      }),
    ).toBeNull();
    expect(
      advanceMidgardBlake2b224Trace({
        control: { ...initial, chainingValue: Buffer.alloc(64) },
        block: Buffer.alloc(128),
      }),
    ).toBeNull();

    const begun = advanceMidgardBlake2b224Trace({
      control: initial,
      block: Buffer.alloc(128),
    })!;
    expect(begun.stage).toBe(MidgardBlake2b224TraceStages.Round);
    expect(
      advanceMidgardBlake2b224Trace({
        control: begun,
        block: Buffer.alloc(1),
      }),
    ).toBeNull();

    const partialInitial = initialMidgardBlake2b224TraceControl(1);
    const partialBegun = advanceMidgardBlake2b224Trace({
      control: partialInitial,
      block: Buffer.from([3]),
    })!;
    const nonzeroPadding = Buffer.from(partialBegun.activeBlock);
    nonzeroPadding[127] = 1;
    expect(
      advanceMidgardBlake2b224Trace({
        control: { ...partialBegun, activeBlock: nonzeroPadding },
      }),
    ).toBeNull();
  });
});
