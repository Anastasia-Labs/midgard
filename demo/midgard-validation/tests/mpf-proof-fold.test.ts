import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardMpfProofFoldTrace,
  encodeMidgardMpfProofFrame,
  MIDGARD_MPF_PROOF_FRAME_MAX_BYTES,
  parseMidgardMpfProofJson,
  verifyMidgardValidationMerkleMembership,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

const exactRoot = (trie: Trie): Buffer =>
  trie.hash == null ? Buffer.alloc(32) : Buffer.from(trie.hash);

describe("bounded MPF proof folding V1", () => {
  it("reconstructs deletion roots one authenticated frame at a time", async () => {
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    const entries = [
      [Buffer.from("alpha"), Buffer.from("one")],
      [Buffer.from("bravo"), Buffer.from("two")],
      [Buffer.from("charlie"), Buffer.from("three")],
    ] as const;
    for (const [key, value] of entries) {
      await trie.insert(key, value);
    }

    const [key, value] = entries[1]!;
    const preRoot = exactRoot(trie);
    const proof = await trie.prove(key, false);
    const trace = buildMidgardMpfProofFoldTrace({
      key,
      value,
      steps: parseMidgardMpfProofJson(proof.toJSON()),
    });
    await trie.delete(key);

    expect(trace.steps).not.toHaveLength(0);
    expect(
      trace.steps.every(({ membership }) =>
        verifyMidgardValidationMerkleMembership(membership),
      ),
    ).toBe(true);
    expect(
      trace.frames.every(
        (frame) =>
          encodeMidgardMpfProofFrame(frame).length <=
          MIDGARD_MPF_PROOF_FRAME_MAX_BYTES,
      ),
    ).toBe(true);
    expect(trace.terminal.includingRoot).toEqual(preRoot);
    expect(trace.terminal.excludingRoot).toEqual(exactRoot(trie));
    expect(trace.terminal).toMatchObject({
      nextFrameIndex: -1,
      expectedNextCursor: 0,
    });
  });

  it("reconstructs insertion roots, including an empty prior trie", async () => {
    for (const seeded of [false, true]) {
      const store = new Store(undefined);
      await store.ready();
      const trie = new Trie(store);
      if (seeded) {
        await trie.insert(Buffer.from("alpha"), Buffer.from("one"));
        await trie.insert(Buffer.from("charlie"), Buffer.from("three"));
      }
      const key = Buffer.from("bravo");
      const value = Buffer.from("two");
      const preRoot = exactRoot(trie);
      const proof = await trie.prove(key, true);
      const trace = buildMidgardMpfProofFoldTrace({
        key,
        value,
        steps: parseMidgardMpfProofJson(proof.toJSON()),
      });
      await trie.insert(key, value);

      expect(trace.terminal.excludingRoot).toEqual(preRoot);
      expect(trace.terminal.includingRoot).toEqual(exactRoot(trie));
      expect(trace.descriptor.frameCount).toBe(trace.frames.length);
      expect(trace.descriptor.terminalCursor).toBe(
        trace.frames.at(-1)?.nextCursor ?? 0,
      );
    }
  });

  it("rejects malformed proof JSON before constructing a frontier", () => {
    expect(() =>
      parseMidgardMpfProofJson([
        {
          type: "branch",
          skip: 0,
          neighbors: "00",
        },
      ]),
    ).toThrow(/exactly 128 bytes/u);
    expect(() =>
      parseMidgardMpfProofJson([
        {
          type: "fork",
          skip: 64,
          neighbor: {
            nibble: 16,
            prefix: "",
            root: "00".repeat(32),
          },
        },
      ]),
    ).toThrow(/canonical integer envelope/u);
  });
});

it("preserves the skipped shared prefix when a terminal fork is compressed", () => {
  const trace = buildMidgardMpfProofFoldTrace({
    key: Buffer.from(
      "abababababababababababababababababababababababababababab001f",
      "hex",
    ),
    value: Buffer.from("01", "hex"),
    steps: parseMidgardMpfProofJson([
      {
        type: "branch",
        skip: 0,
        neighbors:
          "bd3871c02105e5ec24751ba8fb1a5e6d285cdcc8399993a0ca82b26f5ae179d65fed1c2681e29bbb4baed7e7d2a0f618637b48308f351ef786980f3dcdf001205318b031fecedfbfb987a7db1e2fcbb1c1d1c82dbf31e099558ddab0e96e3d38c73adabe2c5d1f9741c04a9dc2e856e29ecb6b96346905f7fb7b266b7865c857",
      },
      {
        type: "branch",
        skip: 0,
        neighbors:
          "42a0d1b9e50273451d2a93f195a000843d6a0c114672d0b5fadbf8fce300fb90b6be92c2f492c9bce1b8ecac7b530487537b2f448a80be834ad7a21df127a5468fe7d543b2434538a0c6d78fae96ed7f767ed36752ce7c1f54242d57eeaaf059cec5360efb71d228292cdf2b52f551f199b32bda593c4a4745a9b94aa17f8c9a",
      },
      {
        type: "fork",
        skip: 1,
        neighbor: {
          nibble: 8,
          prefix: "",
          root: "2f425ececd6ad91ad57238470324c4623ff0bcd1b51f6393ce0105874e651ccb",
        },
      },
    ]),
  });
  expect(trace.terminal.includingRoot.toString("hex")).toBe(
    "a3cb6c7cc11c8ab91629b7fa17089818c9751c5a9a0f256b0449acc17a6fb5b7",
  );
  expect(trace.terminal.excludingRoot.toString("hex")).toBe(
    "aae9218b732ed0dd511191290953ea78b87bf1d3a9e53fa0d2753fe678c9ba9b",
  );
});
