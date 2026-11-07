import { describe, expect, it } from "vitest";

import {
  advanceMidgardBlake2b256Trace,
  advanceMidgardCekDataBytes,
  advanceMidgardCekDataInteger,
  advanceMidgardCekSourceBlob,
  buildMidgardBlake2b256Trace,
  buildMidgardCekDataBytesTrace,
  buildMidgardCekDataIntegerTrace,
  buildMidgardCekSourceBlobTrace,
  digestMidgardBlake2b256Trace,
  encodeMidgardBlake2b256TraceControl,
  encodeMidgardCekDataBytesControl,
  encodeMidgardCekDataIntegerControl,
  encodeMidgardCekSourceBlobControl,
  finalizeMidgardCekDataBytes,
  finalizeMidgardCekDataInteger,
  finalizeMidgardCekSourceBlob,
  isWellFormedMidgardBlake2b256TraceControl,
  isWellFormedMidgardCekDataBytesControl,
  isWellFormedMidgardCekDataIntegerControl,
  isWellFormedMidgardCekSourceBlobControl,
  MidgardBlake2b256TraceStages,
  type MidgardCekDataBytesControl,
  MidgardCekDataBytesStages,
  type MidgardCekDataIntegerControl,
  MidgardCekDataIntegerStages,
  type MidgardCekSourceBlobControl,
  MidgardCekSourceBlobStages,
  nextMidgardCekDataBytesSpan,
  nextMidgardCekDataIntegerSpan,
  nextMidgardCekSourceBlobSpan,
} from "../src/index.js";
import * as core from "../src/index.js";
import {
  BLAKE_REFUSAL,
  BLOB_REFUSAL,
  BYTES_REFUSAL,
  chunkedBignum,
  INTEGER_REFUSAL,
  partialRoundBlob,
  readyBlob,
  sealingBlob,
  tamperBlobFrontier,
  tamperBlobHash,
  tamperHash,
  tamperPadding,
} from "./cek-nested-control-tamper.js";

/**
 * Every exported entry point that accepts a nested control must refuse a
 * control whose only defect sits in a nested child (down to the innermost
 * BLAKE2b trace), with the same result as a defect at its own level. Each
 * case first shows the untampered control is accepted, so the refusal comes
 * from the tampered child alone.
 */

// Variants that skip the entry check for a control their caller has already
// validated. They are package-internal and must never reach the package
// surface, where a caller could hand them an unvalidated control.
const PRE_VALIDATED_VARIANTS = [
  "advanceValidatedMidgardCekDataBytes",
  "advanceValidatedMidgardCekDataInteger",
  "advanceValidatedMidgardCekDataTraverse",
  "advanceValidatedMidgardCekSourceBlob",
  "encodeValidatedMidgardCekDataBytesControl",
  "encodeValidatedMidgardCekDataIntegerControl",
  "encodeValidatedMidgardCekSourceBlobControl",
  "finalizeValidatedMidgardCekDataBytes",
  "finalizeValidatedMidgardCekDataInteger",
  "finalizeValidatedMidgardCekSourceBlob",
  "isWellFormedMidgardCekDataBytesControlWithValidatedBlob",
  "isWellFormedMidgardCekDataIntegerControlWithValidatedBlob",
  "isWellFormedMidgardCekDataTraverseControlWithValidatedBlobs",
  "nextValidatedMidgardCekDataBytesSpan",
  "nextValidatedMidgardCekDataIntegerSpan",
  "nextValidatedMidgardCekDataTraverseSpan",
  "nextValidatedMidgardCekSourceBlobSpan",
] as const;

describe("nested control validation at every exported entry point", () => {
  it("keeps the pre-validated variants off the package surface", () => {
    const surface = Object.keys(core);
    for (const name of PRE_VALIDATED_VARIANTS) {
      expect(surface).not.toContain(name);
    }
    expect(surface.filter((name) => name.includes("Validated"))).toEqual([]);
  });

  it("refuses a tampered BLAKE2b-256 trace control", () => {
    const steps = buildMidgardBlake2b256Trace(Buffer.alloc(200, 0x6b));
    const round = steps.find(
      ({ control }) =>
        control.stage === MidgardBlake2b256TraceStages.Round &&
        control.activeBlockLength < 128,
    )!.control;
    const terminal = steps.at(-1)!.next;

    for (const tampered of [tamperHash(round), tamperPadding(round)]) {
      expect(isWellFormedMidgardBlake2b256TraceControl(round)).toBe(true);
      expect(isWellFormedMidgardBlake2b256TraceControl(tampered)).toBe(false);
      expect(advanceMidgardBlake2b256Trace({ control: round })).not.toBeNull();
      expect(advanceMidgardBlake2b256Trace({ control: tampered })).toBeNull();
      expect(() => encodeMidgardBlake2b256TraceControl(tampered)).toThrow(
        BLAKE_REFUSAL,
      );
    }
    expect(digestMidgardBlake2b256Trace(terminal)).not.toBeNull();
    expect(digestMidgardBlake2b256Trace(tamperHash(terminal))).toBeNull();
  });

  it("refuses a source blob whose only defect is its nested trace", () => {
    const trace = buildMidgardCekSourceBlobTrace({
      sourceStart: 9,
      source: Buffer.alloc(300, 0x6a),
    });
    const ready = trace.steps.find(({ control }) => readyBlob(control))!;
    const round = trace.steps.find(({ control }) =>
      partialRoundBlob(control),
    )!.control;
    const span = nextMidgardCekSourceBlobSpan(ready.control)!;

    expect(span).not.toBeNull();
    expect(
      advanceMidgardCekSourceBlob({
        control: ready.control,
        sourceBytes: ready.sourceBytes,
      }),
    ).not.toBeNull();
    expect(advanceMidgardCekSourceBlob({ control: round })).not.toBeNull();
    for (const tampered of [
      tamperBlobHash(ready.control),
      tamperBlobHash(round),
      tamperBlobHash(round, tamperPadding),
    ]) {
      expect(isWellFormedMidgardCekSourceBlobControl(tampered)).toBe(false);
      expect(nextMidgardCekSourceBlobSpan(tampered)).toBeNull();
      expect(
        advanceMidgardCekSourceBlob({
          control: tampered,
          sourceBytes:
            tampered.activeHash!.stage === MidgardBlake2b256TraceStages.Ready
              ? ready.sourceBytes
              : null,
        }),
      ).toBeNull();
      expect(() => encodeMidgardCekSourceBlobControl(tampered)).toThrow(
        BLOB_REFUSAL,
      );
    }
    expect(finalizeMidgardCekSourceBlob(trace.terminal)).not.toBeNull();
    expect(
      finalizeMidgardCekSourceBlob(tamperBlobFrontier(trace.terminal)),
    ).toBeNull();
  });

  it("refuses an integer whose only defect is its nested blob or trace", () => {
    const source = chunkedBignum(500);
    const sourceEnd = 17 + source.length;
    const trace = buildMidgardCekDataIntegerTrace({ sourceStart: 17, source });
    const ready = trace.steps.find(
      ({ control }) =>
        control.stage === MidgardCekDataIntegerStages.Blob &&
        readyBlob(control.blob),
    )!;
    const round = trace.steps.find(
      ({ control }) =>
        control.stage === MidgardCekDataIntegerStages.Blob &&
        partialRoundBlob(control.blob),
    )!.control;
    const withBlob = (
      control: MidgardCekDataIntegerControl,
      blob: MidgardCekSourceBlobControl,
    ): MidgardCekDataIntegerControl => ({ ...control, blob });

    expect(nextMidgardCekDataIntegerSpan(ready.control, sourceEnd)).not.toBe(
      null,
    );
    expect(
      advanceMidgardCekDataInteger({
        control: ready.control,
        sourceBytes: ready.sourceBytes,
        sourceEnd,
      }),
    ).not.toBeNull();
    expect(
      advanceMidgardCekDataInteger({ control: round, sourceEnd }),
    ).not.toBeNull();
    for (const tampered of [
      withBlob(ready.control, tamperBlobHash(ready.control.blob!)),
      withBlob(round, tamperBlobHash(round.blob!)),
      withBlob(round, tamperBlobHash(round.blob!, tamperPadding)),
      withBlob(ready.control, tamperBlobFrontier(ready.control.blob!)),
    ]) {
      expect(isWellFormedMidgardCekDataIntegerControl(tampered)).toBe(false);
      expect(nextMidgardCekDataIntegerSpan(tampered, sourceEnd)).toBeNull();
      expect(
        advanceMidgardCekDataInteger({
          control: tampered,
          sourceBytes:
            tampered.blob!.activeHash!.stage ===
            MidgardBlake2b256TraceStages.Ready
              ? ready.sourceBytes
              : null,
          sourceEnd,
        }),
      ).toBeNull();
      expect(() => encodeMidgardCekDataIntegerControl(tampered)).toThrow(
        INTEGER_REFUSAL,
      );
    }
    const sealing = trace.steps.find(
      ({ control }) =>
        control.stage === MidgardCekDataIntegerStages.Blob &&
        sealingBlob(control.blob),
    )!.control;
    const sealingTampered = withBlob(
      sealing,
      tamperBlobFrontier(sealing.blob!),
    );
    expect(
      advanceMidgardCekDataInteger({ control: sealing, sourceEnd }),
    ).not.toBeNull();
    expect(isWellFormedMidgardCekDataIntegerControl(sealingTampered)).toBe(
      false,
    );
    expect(
      advanceMidgardCekDataInteger({ control: sealingTampered, sourceEnd }),
    ).toBeNull();
    expect(finalizeMidgardCekDataInteger(trace.terminal)).not.toBeNull();
    expect(
      finalizeMidgardCekDataInteger(
        withBlob(trace.terminal, tamperBlobFrontier(trace.terminal.blob!)),
      ),
    ).toBeNull();
  });

  it("refuses a byte string whose only defect is its nested blob or trace", () => {
    const content = Buffer.alloc(60, 0x6a);
    const source = Buffer.concat([Buffer.from([0x58, 60]), content]);
    const sourceEnd = 31 + source.length;
    const trace = buildMidgardCekDataBytesTrace({ sourceStart: 31, source });
    const ready = trace.steps.find(
      ({ control }) =>
        control.stage === MidgardCekDataBytesStages.Blob &&
        control.blob !== null &&
        control.blob.stage === MidgardCekSourceBlobStages.Active &&
        control.blob.activeHash!.stage === MidgardBlake2b256TraceStages.Ready,
    )!;
    const round = trace.steps.find(
      ({ control }) =>
        control.stage === MidgardCekDataBytesStages.Blob &&
        partialRoundBlob(control.blob),
    )!.control;
    const withBlob = (
      control: MidgardCekDataBytesControl,
      blob: MidgardCekSourceBlobControl,
    ): MidgardCekDataBytesControl => ({ ...control, blob });

    expect(nextMidgardCekDataBytesSpan(ready.control, sourceEnd)).not.toBe(
      null,
    );
    expect(
      advanceMidgardCekDataBytes({
        control: ready.control,
        sourceBytes: ready.sourceBytes,
        sourceEnd,
      }),
    ).not.toBeNull();
    expect(
      advanceMidgardCekDataBytes({ control: round, sourceEnd }),
    ).not.toBeNull();
    for (const tampered of [
      withBlob(ready.control, tamperBlobHash(ready.control.blob!)),
      withBlob(round, tamperBlobHash(round.blob!)),
      withBlob(round, tamperBlobHash(round.blob!, tamperPadding)),
      withBlob(ready.control, tamperBlobFrontier(ready.control.blob!)),
    ]) {
      expect(isWellFormedMidgardCekDataBytesControl(tampered)).toBe(false);
      expect(nextMidgardCekDataBytesSpan(tampered, sourceEnd)).toBeNull();
      expect(
        advanceMidgardCekDataBytes({
          control: tampered,
          sourceBytes:
            tampered.blob!.activeHash!.stage ===
            MidgardBlake2b256TraceStages.Ready
              ? ready.sourceBytes
              : null,
          sourceEnd,
        }),
      ).toBeNull();
      expect(() => encodeMidgardCekDataBytesControl(tampered)).toThrow(
        BYTES_REFUSAL,
      );
    }
    const sealing = trace.steps.find(
      ({ control }) =>
        control.stage === MidgardCekDataBytesStages.Blob &&
        sealingBlob(control.blob),
    )!.control;
    const sealingTampered = withBlob(
      sealing,
      tamperBlobFrontier(sealing.blob!),
    );
    expect(
      advanceMidgardCekDataBytes({ control: sealing, sourceEnd }),
    ).not.toBeNull();
    expect(isWellFormedMidgardCekDataBytesControl(sealingTampered)).toBe(false);
    expect(
      advanceMidgardCekDataBytes({ control: sealingTampered, sourceEnd }),
    ).toBeNull();
    expect(finalizeMidgardCekDataBytes(trace.terminal)).not.toBeNull();
    expect(
      finalizeMidgardCekDataBytes(
        withBlob(trace.terminal, tamperBlobFrontier(trace.terminal.blob!)),
      ),
    ).toBeNull();
  });
});
