import { describe, expect, it } from "vitest";

import {
  hashMidgardCekProgramMaterialPreimage,
  type MidgardCekProgramMaterialEntry,
} from "../src/cek-proof.decode-midgard-cek-program-envelope.js";
import { normalizeProgramMaterial } from "../src/cek-proof.program-material-task.js";

// The block-wide material limits, written out from the canonical 64 MiB DA
// payload: floor((64 MiB - 447) / 42) entries, and 64 MiB - 447 preimage bytes.
const MAX_NODES = 1_597_819;
const MAX_BYTES = 67_108_864 - 447;
const MAX_PREIMAGE = 4_098;

/**
 * `count` blob-chunk entries with distinct preimages (each starts with its
 * index) of the given sizes, each keyed by its own content root.
 */
function* entries(
  count: number,
  preimageBytes: (index: number) => number,
): Generator<MidgardCekProgramMaterialEntry> {
  for (let index = 0; index < count; index++) {
    const preimage = Buffer.alloc(preimageBytes(index), 0x01);
    preimage.writeUInt32BE(index, 0);
    yield {
      kind: "blobChunk",
      root: hashMidgardCekProgramMaterialPreimage("blobChunk", preimage),
      preimage,
    };
  }
}

/** Preimage sizes summing to `total`, each at most the per-entry cap. */
const summingTo = (total: number) => {
  const count = Math.ceil(total / MAX_PREIMAGE);
  return {
    count,
    size: (index: number) =>
      index < count - 1 ? MAX_PREIMAGE : total - MAX_PREIMAGE * (count - 1),
  };
};

describe("block-wide CEK program material limits", () => {
  it(
    "admits exactly the node limit and refuses one entry more",
    { timeout: 600_000 },
    () => {
      expect(normalizeProgramMaterial(entries(MAX_NODES, () => 4)).size).toBe(
        MAX_NODES,
      );
      expect(() =>
        normalizeProgramMaterial(entries(MAX_NODES + 1, () => 4)),
      ).toThrow(/more than 1597819 nodes/u);
    },
  );
  it(
    "admits exactly the byte limit and refuses one byte more",
    { timeout: 600_000 },
    () => {
      const exact = summingTo(MAX_BYTES);
      expect(
        normalizeProgramMaterial(entries(exact.count, exact.size)).size,
      ).toBe(exact.count);
      const over = summingTo(MAX_BYTES + 1);
      expect(() =>
        normalizeProgramMaterial(entries(over.count, over.size)),
      ).toThrow(/exceeds 67108417 bytes/u);
    },
  );
});
