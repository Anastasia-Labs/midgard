import { describe, expect, it } from "vitest";

import {
  advanceMidgardCekDataTraverse,
  buildMidgardCekDataTraverseTrace,
  encodeMidgardCekDataTraverseControl,
  hashMidgardCekDataTraverseControl,
  isWellFormedMidgardCekDataTraverseControl,
  MidgardBlake2b256TraceStages,
  MidgardCekDataBytesStages,
  MidgardCekDataIntegerStages,
  type MidgardCekDataTraverseControl,
  MidgardCekDataTraverseStages,
  type MidgardCekSourceBlobControl,
  MidgardCekSourceBlobStages,
  nextMidgardCekDataTraverseSpan,
} from "../src/index.js";
import {
  chunkedBignum,
  partialRoundBlob,
  readyBlob,
  sealingBlob,
  tamperBlobFrontier,
  tamperBlobHash,
  tamperPadding,
  TRAVERSE_REFUSAL,
} from "./cek-nested-control-tamper.js";

/**
 * Every exported traversal entry point must refuse a control whose only
 * defect sits in its integer or byte-string child, that child's blob, or the
 * blob's BLAKE2b trace, with the same result as a defect at its own level.
 */
describe("nested control validation at the traversal entry points", () => {
  it("refuses a traversal whose only defect is a nested integer, byte string, blob or trace", () => {
    // A list holding a 500-byte bignum and a 60-byte byte string.
    const source = Buffer.concat([
      Buffer.from("9f", "hex"),
      chunkedBignum(500),
      Buffer.from([0x58, 60]),
      Buffer.alloc(60, 0x6b),
      Buffer.from("ff", "hex"),
    ]);
    const trace = buildMidgardCekDataTraverseTrace({ sourceStart: 5, source });
    const integerStep = (
      blob: (control: MidgardCekSourceBlobControl | null) => boolean,
    ) =>
      trace.steps.find(
        ({ control }) =>
          control.stage === MidgardCekDataTraverseStages.Integer &&
          control.integer!.stage === MidgardCekDataIntegerStages.Blob &&
          blob(control.integer!.blob),
      )!;
    const bytesStep = (
      blob: (control: MidgardCekSourceBlobControl | null) => boolean,
    ) =>
      trace.steps.find(
        ({ control }) =>
          control.stage === MidgardCekDataTraverseStages.Bytes &&
          control.bytes!.stage === MidgardCekDataBytesStages.Blob &&
          blob(control.bytes!.blob),
      )!;
    const anyReady = (control: MidgardCekSourceBlobControl | null) =>
      control !== null &&
      control.stage === MidgardCekSourceBlobStages.Active &&
      control.activeHash!.stage === MidgardBlake2b256TraceStages.Ready;
    const integerReady = integerStep(readyBlob);
    const integerSealing = integerStep(sealingBlob);
    const bytesSealing = bytesStep(sealingBlob);
    const integerRound = integerStep(partialRoundBlob);
    const bytesReady = bytesStep(anyReady);
    const bytesRound = bytesStep(partialRoundBlob);

    const withIntegerBlob = (
      control: MidgardCekDataTraverseControl,
      blob: MidgardCekSourceBlobControl,
    ): MidgardCekDataTraverseControl => ({
      ...control,
      integer: { ...control.integer!, blob },
    });
    const withBytesBlob = (
      control: MidgardCekDataTraverseControl,
      blob: MidgardCekSourceBlobControl,
    ): MidgardCekDataTraverseControl => ({
      ...control,
      bytes: { ...control.bytes!, blob },
    });

    const cases = [
      [
        integerReady,
        withIntegerBlob(
          integerReady.control,
          tamperBlobHash(integerReady.control.integer!.blob!),
        ),
      ],
      [
        integerRound,
        withIntegerBlob(
          integerRound.control,
          tamperBlobHash(integerRound.control.integer!.blob!, tamperPadding),
        ),
      ],
      [
        integerReady,
        withIntegerBlob(
          integerReady.control,
          tamperBlobFrontier(integerReady.control.integer!.blob!),
        ),
      ],
      [
        bytesReady,
        withBytesBlob(
          bytesReady.control,
          tamperBlobHash(bytesReady.control.bytes!.blob!),
        ),
      ],
      [
        bytesRound,
        withBytesBlob(
          bytesRound.control,
          tamperBlobHash(bytesRound.control.bytes!.blob!),
        ),
      ],
      [
        bytesReady,
        withBytesBlob(
          bytesReady.control,
          tamperBlobFrontier(bytesReady.control.bytes!.blob!),
        ),
      ],
      [
        integerSealing,
        withIntegerBlob(
          integerSealing.control,
          tamperBlobFrontier(integerSealing.control.integer!.blob!),
        ),
      ],
      [
        bytesSealing,
        withBytesBlob(
          bytesSealing.control,
          tamperBlobFrontier(bytesSealing.control.bytes!.blob!),
        ),
      ],
    ] as const;

    for (const [step, tampered] of cases) {
      expect(isWellFormedMidgardCekDataTraverseControl(step.control)).toBe(
        true,
      );
      expect(
        advanceMidgardCekDataTraverse({
          control: step.control,
          sourceBytes: step.sourceBytes,
          action: step.action,
        }),
      ).not.toBeNull();
      expect(() =>
        encodeMidgardCekDataTraverseControl(step.control),
      ).not.toThrow();

      expect(isWellFormedMidgardCekDataTraverseControl(tampered)).toBe(false);
      expect(nextMidgardCekDataTraverseSpan(tampered)).toBeNull();
      expect(
        advanceMidgardCekDataTraverse({
          control: tampered,
          sourceBytes: step.sourceBytes,
          action: step.action,
        }),
      ).toBeNull();
      expect(() => encodeMidgardCekDataTraverseControl(tampered)).toThrow(
        TRAVERSE_REFUSAL,
      );
      expect(() => hashMidgardCekDataTraverseControl(tampered)).toThrow(
        TRAVERSE_REFUSAL,
      );
    }
    for (const step of [integerReady, bytesReady]) {
      expect(nextMidgardCekDataTraverseSpan(step.control)).not.toBeNull();
    }
  });
});
