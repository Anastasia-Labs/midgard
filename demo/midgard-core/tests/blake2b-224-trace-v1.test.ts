import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  advanceMidgardBlake2b224TraceV1,
  buildMidgardBlake2b224TraceV1,
  digestMidgardBlake2b224TraceV1,
  encodeMidgardBlake2b224TraceControlV1,
  encodeMidgardNativeScript,
  hashMidgardVersionedScript,
  initialMidgardBlake2b224TraceControlV1,
  MIDGARD_BLAKE2B_BLOCK_BYTES,
  MIDGARD_BLAKE2B_ROUNDS,
  MidgardBlake2b224TraceStagesV1,
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
      const trace = buildMidgardBlake2b224TraceV1(message);
      const terminal = trace.at(-1)!.next;
      const blockCount = Math.ceil(
        length / MIDGARD_BLAKE2B_BLOCK_BYTES,
      );

      expect(trace).toHaveLength(
        blockCount * (MIDGARD_BLAKE2B_ROUNDS + 2),
      );
      expect(digestMidgardBlake2b224TraceV1(terminal)).toStrictEqual(
        referenceDigest(message),
      );
    },
  );

  it("encodes the multi-block terminal state canonically", () => {
    const message = Buffer.alloc(6_001, 0x6b);
    message[0] = 3;
    const terminal = buildMidgardBlake2b224TraceV1(message).at(-1)!.next;
    expect(
      encodeMidgardBlake2b224TraceControlV1(terminal).toString("hex"),
    ).toBe(
      "8901031917711917715840634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098dbdd88d3a54a6646cce0bede0423668a5079fb08595004db249d66dbc8e10681056a775c40004000",
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
        Buffer.from([
          Number(MidgardVersionedScriptTags[script.language]),
        ]),
        script.scriptBytes,
      ]);
      const terminal = buildMidgardBlake2b224TraceV1(message).at(-1)!.next;
      expect(digestMidgardBlake2b224TraceV1(terminal)!.toString("hex")).toBe(
        hashMidgardVersionedScript(script),
      );
    },
  );

  it("fails closed for wrong block boundaries and malformed state", () => {
    const initial = initialMidgardBlake2b224TraceControlV1(129);
    expect(
      advanceMidgardBlake2b224TraceV1({
        control: initial,
        block: Buffer.alloc(127),
      }),
    ).toBeNull();
    expect(
      advanceMidgardBlake2b224TraceV1({
        control: { ...initial, chainingValue: Buffer.alloc(63) },
        block: Buffer.alloc(128),
      }),
    ).toBeNull();
    expect(
      advanceMidgardBlake2b224TraceV1({
        control: { ...initial, chainingValue: Buffer.alloc(64) },
        block: Buffer.alloc(128),
      }),
    ).toBeNull();

    const begun = advanceMidgardBlake2b224TraceV1({
      control: initial,
      block: Buffer.alloc(128),
    })!;
    expect(begun.stage).toBe(MidgardBlake2b224TraceStagesV1.Round);
    expect(
      advanceMidgardBlake2b224TraceV1({
        control: begun,
        block: Buffer.alloc(1),
      }),
    ).toBeNull();

    const partialInitial = initialMidgardBlake2b224TraceControlV1(1);
    const partialBegun = advanceMidgardBlake2b224TraceV1({
      control: partialInitial,
      block: Buffer.from([3]),
    })!;
    const nonzeroPadding = Buffer.from(partialBegun.activeBlock);
    nonzeroPadding[127] = 1;
    expect(
      advanceMidgardBlake2b224TraceV1({
        control: { ...partialBegun, activeBlock: nonzeroPadding },
      }),
    ).toBeNull();
  });
});
