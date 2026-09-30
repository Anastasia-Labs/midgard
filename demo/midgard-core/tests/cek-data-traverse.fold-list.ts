import { expect } from "vitest";

import {
  advanceMidgardCekDataTraverse,
  appendMidgardCekDataFrameChild,
  buildMidgardValidationMerkleMembership,
  encodeCborBytes,
  finalizeMidgardCekDataBytes,
  finalizeMidgardCekDataInteger,
  foldMidgardCekDataFrameListChild,
  hashMidgardCekDataFrameChild,
  initialMidgardCekDataTraverseControl,
  type MidgardCekDataFrame,
  type MidgardCekDataSummary,
  type MidgardCekDataTraverseAction,
  type MidgardCekDataTraverseControl,
  MidgardCekDataTraverseStages,
  nextMidgardCekDataTraverseSpan,
} from "../src/index.js";

type Harness = {
  control: MidgardCekDataTraverseControl;
  readonly source: Buffer;
  readonly sourceStart: number;
  readonly reveals: Buffer[];
};

export const transition = (
  harness: Harness,
  action: MidgardCekDataTraverseAction,
): void => {
  const span = nextMidgardCekDataTraverseSpan(harness.control);
  const sourceBytes =
    span === null
      ? null
      : harness.source.subarray(
          span.absoluteStart - harness.sourceStart,
          span.absoluteStart - harness.sourceStart + span.length,
        );
  if (sourceBytes !== null) {
    harness.reveals.push(Buffer.from(sourceBytes));
  }
  const next = advanceMidgardCekDataTraverse({
    control: harness.control,
    sourceBytes,
    action,
  });
  expect(next).not.toBeNull();
  harness.control = next!;
};

const scalarSummary = (
  control: MidgardCekDataTraverseControl,
): MidgardCekDataSummary => {
  const summary =
    control.integer !== null
      ? finalizeMidgardCekDataInteger(control.integer)
      : finalizeMidgardCekDataBytes(control.bytes!);
  expect(summary).not.toBeNull();
  return summary!;
};

export const finishScalar = (
  harness: Harness,
  parent: MidgardCekDataFrame | null,
): MidgardCekDataSummary => {
  while (
    (harness.control.stage === MidgardCekDataTraverseStages.Integer &&
      harness.control.integer!.stage !== 2) ||
    (harness.control.stage === MidgardCekDataTraverseStages.Bytes &&
      harness.control.bytes!.stage !== 3)
  ) {
    transition(harness, null);
  }
  const summary = scalarSummary(harness.control);
  transition(harness, { kind: "attachScalar", parent });
  return summary;
};

export const appendChild = (
  frame: MidgardCekDataFrame,
  child: MidgardCekDataSummary,
): MidgardCekDataFrame => {
  const next = appendMidgardCekDataFrameChild(frame, child);
  expect(next).not.toBeNull();
  return next!;
};

export const foldList = (
  harness: Harness,
  initial: MidgardCekDataFrame,
  children: readonly MidgardCekDataSummary[],
): MidgardCekDataFrame => {
  const leaves = children.map((child, index) =>
    hashMidgardCekDataFrameChild(index, child),
  );
  let frame = initial;
  for (let childIndex = children.length - 1; childIndex >= 0; childIndex -= 1) {
    const membership = buildMidgardValidationMerkleMembership(
      leaves,
      childIndex,
    );
    transition(harness, {
      kind: "foldList",
      frame,
      childIndex,
      child: children[childIndex]!,
      siblings: membership.siblings,
    });
    frame = foldMidgardCekDataFrameListChild({
      frame,
      childIndex,
      child: children[childIndex]!,
      siblings: membership.siblings,
    })!;
    expect(frame).not.toBeNull();
  }
  return frame;
};

export const harness = (source: Uint8Array, sourceStart = 17): Harness => ({
  control: initialMidgardCekDataTraverseControl({
    sourceStart,
    sourceLength: source.length,
  }),
  source: Buffer.from(source),
  sourceStart,
  reveals: [],
});

export const encodeCardanoDataBytes = (content: Uint8Array): Buffer => {
  const bytes = Buffer.from(content);
  if (bytes.length <= 64) return encodeCborBytes(bytes);
  const chunks: Buffer[] = [];
  for (let offset = 0; offset < bytes.length; offset += 64) {
    chunks.push(encodeCborBytes(bytes.subarray(offset, offset + 64)));
  }
  return Buffer.concat([Buffer.from([0x5f]), ...chunks, Buffer.from([0xff])]);
};
