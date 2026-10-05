import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  combine,
  FRAME_DOMAIN,
  hash32,
  merkle16,
  MIDGARD_MPF_PROOF_FRAME_MAX_BYTES,
  type MidgardMpfProofDescriptor,
  type MidgardMpfProofFrame,
  type MidgardMpfProofStep,
  nibbleAt,
  PATH_NIBBLE_COUNT,
  pathNibbles,
  sparseMerkle16,
  suffix,
} from "./mpf-proof-fold.parse-midgard-mpf-proof-json.js";
import { buildMidgardValidationMerkleFrontier } from "./validation-merkle.js";

const doBranch = (
  path: Uint8Array,
  frame: MidgardMpfProofFrame,
  childRoot: Uint8Array,
): Hash32 => {
  if (frame.step.kind !== "branch") {
    throw new Error("MPF branch fold received a different frame kind");
  }
  const neighbors = frame.step.neighbors;
  return combine(
    pathNibbles(path, frame.cursor, frame.nextCursor - 1),
    merkle16(
      nibbleAt(path, frame.nextCursor - 1),
      childRoot,
      neighbors.subarray(0, 32),
      neighbors.subarray(32, 64),
      neighbors.subarray(64, 96),
      neighbors.subarray(96, 128),
    ),
  );
};

const doFork = (
  path: Uint8Array,
  frame: MidgardMpfProofFrame,
  childRoot: Uint8Array,
  neighborNibble: number,
  neighborPrefix: Uint8Array,
  neighborRoot: Uint8Array,
): Hash32 =>
  combine(
    pathNibbles(path, frame.cursor, frame.nextCursor - 1),
    sparseMerkle16(
      nibbleAt(path, frame.nextCursor - 1),
      childRoot,
      neighborNibble,
      combine(neighborPrefix, neighborRoot),
    ),
  );

export const foldIncludingFrame = (
  path: Uint8Array,
  frame: MidgardMpfProofFrame,
  childRoot: Uint8Array,
): Hash32 => {
  if (frame.step.kind === "branch") {
    return doBranch(path, frame, childRoot);
  }
  if (frame.step.kind === "fork") {
    return doFork(
      path,
      frame,
      childRoot,
      frame.step.neighbor.nibble,
      frame.step.neighbor.prefix,
      frame.step.neighbor.root,
    );
  }
  return doFork(
    path,
    frame,
    childRoot,
    nibbleAt(frame.step.key, frame.nextCursor - 1),
    suffix(frame.step.key, frame.nextCursor),
    frame.step.value,
  );
};

export const foldExcludingFrame = (
  path: Uint8Array,
  frame: MidgardMpfProofFrame,
  childRoot: Uint8Array,
  isTerminalFrame: boolean,
): Hash32 => {
  if (frame.step.kind === "branch") {
    return doBranch(path, frame, childRoot);
  }
  if (isTerminalFrame && frame.step.kind === "fork") {
    return combine(
      Buffer.concat([
        pathNibbles(path, frame.cursor, frame.nextCursor - 1),
        Buffer.from([frame.step.neighbor.nibble]),
        frame.step.neighbor.prefix,
      ]),
      frame.step.neighbor.root,
    );
  }
  if (isTerminalFrame && frame.step.kind === "leaf") {
    return combine(suffix(frame.step.key, frame.cursor), frame.step.value);
  }
  if (frame.step.kind === "fork") {
    return doFork(
      path,
      frame,
      childRoot,
      frame.step.neighbor.nibble,
      frame.step.neighbor.prefix,
      frame.step.neighbor.root,
    );
  }
  return doFork(
    path,
    frame,
    childRoot,
    nibbleAt(frame.step.key, frame.nextCursor - 1),
    suffix(frame.step.key, frame.nextCursor),
    frame.step.value,
  );
};

const validateFrameStructure = (frame: MidgardMpfProofFrame): void => {
  if (
    frame.version !== 1 ||
    !Number.isSafeInteger(frame.frameIndex) ||
    frame.frameIndex < 0 ||
    frame.frameIndex >= PATH_NIBBLE_COUNT ||
    !Number.isSafeInteger(frame.cursor) ||
    frame.cursor < 0 ||
    !Number.isSafeInteger(frame.nextCursor) ||
    frame.nextCursor !== frame.cursor + 1 + frame.step.skip ||
    frame.nextCursor > PATH_NIBBLE_COUNT ||
    !Number.isSafeInteger(frame.step.skip) ||
    frame.step.skip < 0
  ) {
    throw new Error("MPF proof frame is outside its canonical path envelope");
  }
  if (frame.step.kind === "branch") {
    if (frame.step.neighbors.length !== 4 * 32) {
      throw new Error(
        "MPF branch frame must contain exactly 128 neighbor bytes",
      );
    }
    return;
  }
  if (frame.step.kind === "fork") {
    if (
      !Number.isSafeInteger(frame.step.neighbor.nibble) ||
      frame.step.neighbor.nibble < 0 ||
      frame.step.neighbor.nibble > 15
    ) {
      throw new Error("MPF fork neighbor is outside its canonical envelope");
    }
    ensureHash32(frame.step.neighbor.root, "mpf_proof_frame.neighbor.root");
    return;
  }
  ensureHash32(frame.step.key, "mpf_proof_frame.leaf.key");
  ensureHash32(frame.step.value, "mpf_proof_frame.leaf.value");
};

export const encodeMidgardMpfProofFrame = (
  frame: MidgardMpfProofFrame,
): Buffer => {
  validateFrameStructure(frame);
  const prefix = [
    1n,
    BigInt(frame.frameIndex),
    BigInt(frame.cursor),
    BigInt(frame.nextCursor),
  ] as const;
  if (frame.step.kind === "branch") {
    const encoded = encodeCbor([
      ...prefix,
      0n,
      BigInt(frame.step.skip),
      frame.step.neighbors,
    ]);
    if (encoded.length > MIDGARD_MPF_PROOF_FRAME_MAX_BYTES) {
      throw new Error("MPF branch frame exceeds its generated proof bound");
    }
    return encoded;
  }
  if (frame.step.kind === "fork") {
    const encoded = encodeCbor([
      ...prefix,
      1n,
      BigInt(frame.step.skip),
      BigInt(frame.step.neighbor.nibble),
      frame.step.neighbor.prefix,
      frame.step.neighbor.root,
    ]);
    if (encoded.length > MIDGARD_MPF_PROOF_FRAME_MAX_BYTES) {
      throw new Error("MPF fork frame exceeds its generated proof bound");
    }
    return encoded;
  }
  const encoded = encodeCbor([
    ...prefix,
    2n,
    BigInt(frame.step.skip),
    frame.step.key,
    frame.step.value,
  ]);
  if (encoded.length > MIDGARD_MPF_PROOF_FRAME_MAX_BYTES) {
    throw new Error("MPF leaf frame exceeds its generated proof bound");
  }
  return encoded;
};

export const hashMidgardMpfProofFrame = (frame: MidgardMpfProofFrame): Hash32 =>
  hash32(Buffer.concat([FRAME_DOMAIN, encodeMidgardMpfProofFrame(frame)]));

export const buildMidgardMpfProofFrames = (
  steps: readonly MidgardMpfProofStep[],
): readonly MidgardMpfProofFrame[] => {
  if (steps.length > PATH_NIBBLE_COUNT) {
    throw new Error("MPF proof has more frames than the key path");
  }
  let cursor = 0;
  return steps.map((step, frameIndex) => {
    const nextCursor = cursor + 1 + step.skip;
    if (nextCursor > PATH_NIBBLE_COUNT) {
      throw new Error("MPF proof frame advances beyond the key path");
    }
    const frame = {
      version: 1,
      frameIndex,
      cursor,
      nextCursor,
      step,
    } as const satisfies MidgardMpfProofFrame;
    cursor = nextCursor;
    return frame;
  });
};

export const buildMidgardMpfProofDescriptor = (
  frames: readonly MidgardMpfProofFrame[],
): MidgardMpfProofDescriptor => {
  let cursor = 0;
  frames.forEach((frame, frameIndex) => {
    validateFrameStructure(frame);
    if (frame.frameIndex !== frameIndex || frame.cursor !== cursor) {
      throw new Error("MPF proof frames are not one canonical ordered path");
    }
    cursor = frame.nextCursor;
  });
  const leafHashes = frames.map(hashMidgardMpfProofFrame);
  return {
    version: 1,
    frameCount: frames.length,
    terminalCursor: frames.at(-1)?.nextCursor ?? 0,
    frontier: buildMidgardValidationMerkleFrontier(leafHashes),
  };
};

export const encodeMidgardMpfProofDescriptor = (
  descriptor: MidgardMpfProofDescriptor,
): Buffer => {
  if (
    descriptor.version !== 1 ||
    !Number.isSafeInteger(descriptor.frameCount) ||
    descriptor.frameCount < 0 ||
    descriptor.frameCount > PATH_NIBBLE_COUNT ||
    !Number.isSafeInteger(descriptor.terminalCursor) ||
    descriptor.terminalCursor < 0 ||
    descriptor.terminalCursor > PATH_NIBBLE_COUNT ||
    descriptor.frontier.count !== descriptor.frameCount ||
    (descriptor.frameCount === 0
      ? descriptor.terminalCursor !== 0
      : descriptor.terminalCursor === 0)
  ) {
    throw new Error("MPF proof descriptor is outside its canonical envelope");
  }
  return encodeCbor([
    1n,
    BigInt(descriptor.frameCount),
    BigInt(descriptor.terminalCursor),
    descriptor.frontier.peaks.map((peak) => [
      BigInt(peak.height),
      ensureHash32(peak.hash, "mpf_proof_descriptor.peak"),
    ]),
  ]);
};
