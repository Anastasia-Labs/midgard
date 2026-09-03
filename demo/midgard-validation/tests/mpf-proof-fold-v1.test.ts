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
