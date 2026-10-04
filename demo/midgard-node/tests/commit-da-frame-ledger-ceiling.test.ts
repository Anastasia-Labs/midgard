import { maxDaPayloadInnerBytes } from "@al-ft/midgard-core/da-payload-sizing";
import { Either } from "effect";
import { describe, expect, it } from "vitest";

import { utxoPayloadEntryEncodedSize } from "../src/mpf/index.js";
import { emptyBlockDaPayloadUpperBoundBytes } from "../src/workers/utils/commit-block-planner.js";
import { MODES, preSubmit } from "./helpers/commit-da-frame-fixtures.js";

/** The largest ledger size `fits` admits; `fits` is monotone in it. */
const ceiling = async (fits: (entryCount: number) => Promise<boolean>) => {
  let low = 0;
  let high = 10_000_000;
  while (low < high) {
    const mid = Math.ceil((low + high) / 2);
    if (await fits(mid)) low = mid;
    else high = mid - 1;
  }
  return low;
};

// Every block carries the whole post-block ledger, so the frame caps the
// ledger itself. These pins measure that cap; they are not a remedy for it.
describe("DA frame ledger ceiling", () => {
  it("costs each UTxO aggregate entry its encoded tuple bytes", () => {
    // lc1 outrefs are 38 bytes; its outputs were 69, 108, 109 and 110 bytes.
    // 1,000, 5,000 and 16,384 bytes are larger outputs, the last one the
    // maximum output size.
    expect(
      [69, 108, 109, 110, 1_000, 5_000, 16_384].map((outputBytes) =>
        utxoPayloadEntryEncodedSize({
          outref: Buffer.alloc(38, 1),
          output: Buffer.alloc(outputBytes, 2),
        }),
      ),
    ).toEqual([116, 156, 157, 158, 1_076, 5_201, 16_940]);
    expect(
      emptyBlockDaPayloadUpperBoundBytes({
        entryCount: 0,
        encodedTupleBytes: 0,
      }),
    ).toBe(1_010);
  });

  // The measured ceiling: the L2 UTxO count past which an empty block stops
  // fitting the frame (inner limits identity 67,108,710 / zstd 66,847,587; an
  // empty block with no UTxOs is 1,010 bytes). Planner upper bound:
  //
  //   output bytes | entry bytes | identity | zstd
  //   69           | 116         | 578,514  | 576,263
  //   110          | 158         | 424,732  | 423,079
  //   lc1 mean     | 148         | 453,430  | 451,666
  //   1,000        | 1,076       | 62,367   | 62,125
  //   5,000        | 5,201       | 12,902   | 12,852
  //   16,384 (max) | 16,940      | 3,961    | 3,946
  //
  // [mode, entry bytes, planner upper-bound ceiling, pre-submit ceiling]
  const CEILINGS = [
    ["identity", 116, 578_514, 578_519],
    ["identity", 148, 453_430, 453_434],
    ["identity", 158, 424_732, 424_735],
    ["identity", 1_076, 62_367, 62_368],
    ["identity", 5_201, 12_902, 12_902],
    ["identity", 16_940, 3_961, 3_961],
    ["zstd", 116, 576_263, 576_268],
    ["zstd", 148, 451_666, 451_669],
    ["zstd", 158, 423_079, 423_082],
    ["zstd", 1_076, 62_125, 62_125],
    ["zstd", 5_201, 12_852, 12_852],
    ["zstd", 16_940, 3_946, 3_946],
  ] as const;

  it.each(CEILINGS)(
    "an empty block stops fitting past the pinned ledger size (%s, %i-byte entries)",
    async (mode, entryBytes, plannerCeiling, preSubmitCeiling) => {
      expect(MODES).toContain(mode);
      const limit = maxDaPayloadInnerBytes(mode);
      const aggregate = (entryCount: number) => ({
        entryCount,
        encodedTupleBytes: entryCount * entryBytes,
      });
      expect(
        await ceiling(
          async (entryCount) =>
            emptyBlockDaPayloadUpperBoundBytes(aggregate(entryCount)) <= limit,
        ),
      ).toBe(plannerCeiling);
      expect(
        await ceiling(async (entryCount) =>
          Either.isRight(
            await preSubmit([], mode, { base: aggregate(entryCount) }),
          ),
        ),
      ).toBe(preSubmitCeiling);
      // The planner's bound never admits a ledger the pre-submit check refuses.
      expect(plannerCeiling).toBeLessThanOrEqual(preSubmitCeiling);
    },
  );
});
