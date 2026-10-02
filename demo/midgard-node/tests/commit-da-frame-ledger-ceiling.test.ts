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
    expect(
      [69, 108, 109, 110].map((outputBytes) =>
        utxoPayloadEntryEncodedSize({
          outref: Buffer.alloc(38, 1),
          output: Buffer.alloc(outputBytes, 2),
        }),
      ),
    ).toEqual([116, 156, 157, 158]);
    expect(
      emptyBlockDaPayloadUpperBoundBytes({
        entryCount: 0,
        encodedTupleBytes: 0,
      }),
    ).toBe(1_010);
  });

  // [mode, entry bytes, planner upper-bound ceiling, pre-submit ceiling]
  const CEILINGS = [
    ["identity", 116, 578_514, 578_519],
    ["identity", 148, 453_430, 453_434],
    ["identity", 158, 424_732, 424_735],
    ["zstd", 116, 576_263, 576_268],
    ["zstd", 148, 451_666, 451_669],
    ["zstd", 158, 423_079, 423_082],
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
