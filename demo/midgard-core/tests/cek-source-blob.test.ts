import { describe, expect, it } from "vitest";

import {
  advanceMidgardCekSourceBlob,
  buildMidgardCekSourceBlobTrace,
  commitMidgardCekBlob,
  encodeMidgardCekSourceBlobControl,
  finalizeMidgardCekSourceBlob,
  initialMidgardCekSourceBlobControl,
  MIDGARD_BLAKE2B_256_BLOCK_BYTES,
  MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT,
  MidgardCekSourceBlobStages,
  nextMidgardCekSourceBlobSpan,
} from "../src/index.js";

describe("authenticated CEK source blob V1", () => {
  it.each([0, 1, 127, 128, 4_095, 4_096, 8_191, 16_384])(
    "reproduces the canonical CEK blob root for %i bytes",
    (length) => {
      const source = Buffer.alloc(length, 0x6a);
      const trace = buildMidgardCekSourceBlobTrace({
        sourceStart: 73,
        source,
      });
      const revealed = Buffer.concat(
        trace.steps.flatMap(({ sourceBytes }) =>
          sourceBytes === null ? [] : [sourceBytes],
        ),
      );

      expect(trace.terminal.stage).toBe(MidgardCekSourceBlobStages.Terminal);
      expect(finalizeMidgardCekSourceBlob(trace.terminal)).toStrictEqual(
        commitMidgardCekBlob(source).root,
      );
      expect(revealed).toStrictEqual(source);
      for (const { control, sourceBytes } of trace.steps) {
        const span = nextMidgardCekSourceBlobSpan(control);
        if (sourceBytes === null) {
          expect(span).toBeNull();
        } else {
          expect(span?.length).toBe(sourceBytes.length);
          expect(sourceBytes.length).toBeLessThanOrEqual(
            MIDGARD_BLAKE2B_256_BLOCK_BYTES,
          );
        }
      }
    },
  );

  it("binds the source range and encodes a terminal Aiken vector", () => {
    const source = Buffer.alloc(4_096, 0x6a);
    const trace = buildMidgardCekSourceBlobTrace({
      sourceStart: 17,
      source,
    });
    expect(
      encodeMidgardCekSourceBlobControl(trace.terminal).toString("hex"),
    ).toBe(
      "860101111910008401021910008183015820eeae7280d2825a069ee81fdde1b202e15766bb7bf1689a514224772d104bc59d191000d87a80",
    );
  });

  it("fails closed for missing, surplus, and wrong-length source bytes", () => {
    const initial = initialMidgardCekSourceBlobControl({
      sourceStart: 9,
      sourceLength: 129,
    });
    const span = nextMidgardCekSourceBlobSpan(initial)!;

    expect(
      advanceMidgardCekSourceBlob({
        control: initial,
        sourceBytes: null,
      }),
    ).toBeNull();
    expect(
      advanceMidgardCekSourceBlob({
        control: initial,
        sourceBytes: Buffer.alloc(span.length - 1),
      }),
    ).toBeNull();
    expect(
      advanceMidgardCekSourceBlob({
        control: initial,
        sourceBytes: Buffer.alloc(span.length + 1),
      }),
    ).toBeNull();
    expect(
      advanceMidgardCekSourceBlob({
        control: {
          ...initial,
          sourceStart: Number.MAX_SAFE_INTEGER,
        },
        sourceBytes: Buffer.alloc(span.length),
      }),
    ).toBeNull();
    expect(() =>
      initialMidgardCekSourceBlobControl({
        sourceStart: 0,
        sourceLength: MIDGARD_VALIDATION_MERKLE_MAX_LEAF_COUNT * 4_095 + 1,
      }),
    ).toThrow(/range/u);
  });
});
