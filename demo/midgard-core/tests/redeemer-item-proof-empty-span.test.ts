import { describe, expect, it } from "vitest";

import {
  advanceMidgardRedeemerItemProof,
  authenticateMidgardRedeemerItemSourceWindow,
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  buildMidgardRedeemerItemProofTrace,
  finalizeMidgardRedeemerItemProof,
  hashMidgardRedeemerItemProofControl,
  initialMidgardRedeemerItemProofControl,
  MIDGARD_REDEEMER_ITEM_FIELD_INDEX,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
  nextMidgardRedeemerItemProofSpan,
} from "../src/index.js";

/**
 * The window vectors are the same as the source-span tests' in
 * onchain/aiken/lib/midgard/redeemer-item-proof-v1.test.ak, and the empty
 * content steps of the first and last Data vectors below are replayed there.
 */
const smallItemBytes = Buffer.from("8400004100820a14", "hex");
/** Two chunks: 4,095 bytes and 5 bytes, byte i being i mod 256. */
const twoChunkItemBytes = Buffer.from(
  Array.from({ length: 4_100 }, (_, index) => index % 256),
);

const windowFor = (bytes: Buffer) => {
  const item = buildMidgardBoundedItem({
    fieldIndex: MIDGARD_REDEEMER_ITEM_FIELD_INDEX,
    itemIndex: 0,
    bytes,
  });
  const control = initialMidgardRedeemerItemProofControl({
    mode: MidgardRedeemerItemProofModes.Data,
    itemIndex: 0,
    itemCount: 1,
    totalLength: bytes.length,
    itemCommitment: item.commitment,
  });
  return (
    absoluteStart: number,
    length: number,
    chunks: readonly (number | null)[],
  ): string | null => {
    const [first, next] = chunks.map((index) =>
      index === null ? null : buildMidgardBoundedItemChunkProof(item, index),
    );
    const window = authenticateMidgardRedeemerItemSourceWindow({
      control,
      span: { absoluteStart, length },
      witness: { chunkProof: first ?? null, nextChunkProof: next ?? null },
    });
    return window === null ? null : (window.sourceBytes?.toString("hex") ?? "");
  };
};

/** No span's window, authenticated with chunk 0's proof in either position. */
const noSpanWindows = (bytes: Buffer) => {
  const item = buildMidgardBoundedItem({
    fieldIndex: MIDGARD_REDEEMER_ITEM_FIELD_INDEX,
    itemIndex: 0,
    bytes,
  });
  const control = initialMidgardRedeemerItemProofControl({
    mode: MidgardRedeemerItemProofModes.Data,
    itemIndex: 0,
    itemCount: 1,
    totalLength: bytes.length,
    itemCommitment: item.commitment,
  });
  const proof = buildMidgardBoundedItemChunkProof(item, 0);
  return [
    { chunkProof: null, nextChunkProof: null },
    { chunkProof: proof, nextChunkProof: null },
    { chunkProof: null, nextChunkProof: proof },
  ].map((witness) =>
    authenticateMidgardRedeemerItemSourceWindow({
      control,
      span: null,
      witness,
    }),
  );
};

describe("redeemer item source window", () => {
  const small = windowFor(smallItemBytes);
  const twoChunk = windowFor(twoChunkItemBytes);

  it("accepts an empty span inside the item", () => {
    expect(small(0, 0, [])).toBe("");
    expect(small(3, 0, [])).toBe("");
    expect(small(8, 0, [])).toBe("");
    expect(twoChunk(4_095, 0, [])).toBe("");
    expect(twoChunk(4_096, 0, [])).toBe("");
    expect(twoChunk(4_100, 0, [])).toBe("");
  });

  it("refuses an empty span outside the item or with other evidence", () => {
    expect(small(9, 0, [])).toBeNull();
    expect(small(-1, 0, [])).toBeNull();
    expect(small(3, 0, [0])).toBeNull();
    expect(small(3, 0, [0, 0])).toBeNull();
    expect(small(3, 0, [null, 0])).toBeNull();
    expect(twoChunk(4_095, 0, [0])).toBeNull();
    expect(twoChunk(4_096, 0, [1])).toBeNull();
    expect(twoChunk(4_101, 0, [])).toBeNull();
  });

  it("takes no chunk proof where there is no span", () => {
    expect(noSpanWindows(smallItemBytes)).toEqual([
      { sourceBytes: null },
      null,
      null,
    ]);
  });

  it("reads a non-empty span as before", () => {
    expect(small(3, 2, [0])).toBe("4100");
    expect(small(3, -1, [0])).toBeNull();
    expect(small(3, -1, [])).toBeNull();
    expect(small(7, 2, [0])).toBeNull();
    expect(twoChunk(4_090, 10, [0, 1])).toBe(
      twoChunkItemBytes.subarray(4_090, 4_100).toString("hex"),
    );
    expect(twoChunk(4_090, 10, [0])).toBeNull();
    expect(twoChunk(4_095, 1, [1])).toBe("ff");
  });
});

/**
 * A list of 61 64-byte strings and 61 zeros, then an empty byte string whose
 * empty content starts at byte 4,095 of its item, the first of chunk 1.
 */
const boundaryData = Buffer.concat([
  Buffer.from([0x9f]),
  ...Array.from({ length: 61 }, () =>
    Buffer.concat([Buffer.from([0x58, 0x40]), Buffer.alloc(64, 0x11)]),
  ),
  Buffer.alloc(61, 0),
  Buffer.from([0x40, 0xff]),
]);

/** A redeemer item [0, 0, data, [10, 20]], its data as a definite byte string. */
const redeemerItem = (data: Buffer): Buffer =>
  Buffer.concat([
    Buffer.from("840000", "hex"),
    data.length < 24
      ? Buffer.from([0x40 + data.length])
      : Buffer.from([0x59, data.length >> 8, data.length & 0xff]),
    data,
    Buffer.from("820a14", "hex"),
  ]);

describe("redeemer item Data proof with an empty byte string", () => {
  it.each([
    ["the empty byte string", Buffer.from("40", "hex"), 5],
    ["a list of it", Buffer.from("9f40ff", "hex"), 6],
    ["a list ending in it", Buffer.from("9f0140ff", "hex"), 7],
    ["a constructor field", Buffer.from("d8799f40ff", "hex"), 8],
    ["it read at a chunk boundary", boundaryData, 4_095],
  ])("proves %s to its terminal summary", (_, data, emptyContentStart) => {
    const itemBytes = redeemerItem(data);
    const trace = buildMidgardRedeemerItemProofTrace({
      itemIndex: 0,
      itemCount: 1,
      itemBytes,
      mode: MidgardRedeemerItemProofModes.Data,
    });
    const emptySpanStarts = trace.steps.flatMap(({ control }) => {
      const span = nextMidgardRedeemerItemProofSpan(control);
      return span?.length === 0 ? [span.absoluteStart] : [];
    });
    expect(emptySpanStarts).toContain(emptyContentStart);
    for (const step of trace.steps) {
      const next = advanceMidgardRedeemerItemProof(step);
      expect(next).not.toBeNull();
      expect(hashMidgardRedeemerItemProofControl(next!)).toEqual(
        hashMidgardRedeemerItemProofControl(step.next),
      );
    }
    expect(trace.terminal.stage).toBe(MidgardRedeemerItemProofStages.Terminal);
    expect(trace.terminal.dataLength).toBe(data.length);
    expect(finalizeMidgardRedeemerItemProof(trace.terminal)?.cborLength).toBe(
      BigInt(data.length),
    );
  });
});
