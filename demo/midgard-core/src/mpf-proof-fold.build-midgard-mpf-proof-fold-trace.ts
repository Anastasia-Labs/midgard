import { ensureHash32 } from "./codec/hash.js";
import {
  buildMidgardMpfProofDescriptor,
  buildMidgardMpfProofFrames,
  foldExcludingFrame,
  foldIncludingFrame,
  hashMidgardMpfProofFrame,
} from "./mpf-proof-fold.encode-midgard-mpf-proof-frame.js";
import {
  combine,
  hash32,
  type MidgardMpfProofFoldControl,
  type MidgardMpfProofFoldStep,
  type MidgardMpfProofFoldTrace,
  type MidgardMpfProofStep,
  NULL_HASH,
  suffix,
} from "./mpf-proof-fold.parse-midgard-mpf-proof-json.js";
import { buildMidgardValidationMerkleMembership } from "./validation-merkle.js";

export const buildMidgardMpfProofFoldTrace = ({
  key,
  value,
  steps,
}: {
  readonly key: Uint8Array;
  readonly value: Uint8Array;
  readonly steps: readonly MidgardMpfProofStep[];
}): MidgardMpfProofFoldTrace => {
  const frames = buildMidgardMpfProofFrames(steps);
  const descriptor = buildMidgardMpfProofDescriptor(frames);
  const path = hash32(key);
  const leafHashes = frames.map(hashMidgardMpfProofFrame);
  let control: MidgardMpfProofFoldControl = {
    nextFrameIndex: frames.length - 1,
    expectedNextCursor: descriptor.terminalCursor,
    includingRoot: combine(
      suffix(path, descriptor.terminalCursor),
      hash32(value),
    ),
    excludingRoot: ensureHash32(NULL_HASH, "mpf_proof_fold.null_hash"),
  };
  const initial = control;
  const foldSteps: MidgardMpfProofFoldStep[] = [];
  for (let frameIndex = frames.length - 1; frameIndex >= 0; frameIndex -= 1) {
    const frame = frames[frameIndex]!;
    if (
      frame.frameIndex !== control.nextFrameIndex ||
      frame.nextCursor !== control.expectedNextCursor
    ) {
      throw new Error("MPF proof fold frame continuity is invalid");
    }
    const post: MidgardMpfProofFoldControl = {
      nextFrameIndex: frameIndex - 1,
      expectedNextCursor: frame.cursor,
      includingRoot: foldIncludingFrame(path, frame, control.includingRoot),
      excludingRoot: foldExcludingFrame(
        path,
        frame,
        control.excludingRoot,
        frameIndex === frames.length - 1,
      ),
    };
    foldSteps.push({
      frame,
      membership: buildMidgardValidationMerkleMembership(
        leafHashes,
        frameIndex,
      ),
      pre: control,
      post,
    });
    control = post;
  }
  if (control.nextFrameIndex !== -1 || control.expectedNextCursor !== 0) {
    throw new Error("MPF proof fold did not terminate at the root cursor");
  }
  return {
    descriptor,
    frames,
    initial,
    steps: foldSteps,
    terminal: control,
  };
};
