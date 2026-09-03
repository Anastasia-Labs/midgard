import { blake2b } from "@noble/hashes/blake2.js";

import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  buildMidgardValidationMerkleFrontier,
  buildMidgardValidationMerkleMembership,
  type MidgardValidationMerkleFrontier,
  type MidgardValidationMerkleMembership,
} from "./validation-merkle.js";

const FRAME_DOMAIN = Buffer.from("MidgardMpfProofFrameV1", "ascii");
const NULL_HASH = Buffer.alloc(32);
const PATH_NIBBLE_COUNT = 64;

export const MIDGARD_MPF_PROOF_FRAME_MAX_BYTES = 141;

export type MidgardMpfProofStep =
  | {
      readonly kind: "branch";
      readonly skip: number;
      readonly neighbors: Buffer;
    }
  | {
      readonly kind: "fork";
      readonly skip: number;
      readonly neighbor: {
        readonly nibble: number;
        readonly prefix: Buffer;
        readonly root: Hash32;
      };
    }
  | {
      readonly kind: "leaf";
      readonly skip: number;
      readonly key: Hash32;
      readonly value: Hash32;
    };

export type MidgardMpfProofFrame = {
  readonly version: 1;
  readonly frameIndex: number;
  readonly cursor: number;
  readonly nextCursor: number;
  readonly step: MidgardMpfProofStep;
};

export type MidgardMpfProofDescriptor = {
  readonly version: 1;
  readonly frameCount: number;
  readonly terminalCursor: number;
  readonly frontier: MidgardValidationMerkleFrontier;
};

export type MidgardMpfProofFoldControl = {
  readonly nextFrameIndex: number;
  readonly expectedNextCursor: number;
  readonly includingRoot: Hash32;
  readonly excludingRoot: Hash32;
};

export type MidgardMpfProofFoldStep = {
  readonly frame: MidgardMpfProofFrame;
  readonly membership: MidgardValidationMerkleMembership;
  readonly pre: MidgardMpfProofFoldControl;
  readonly post: MidgardMpfProofFoldControl;
};

export type MidgardMpfProofFoldTrace = {
  readonly descriptor: MidgardMpfProofDescriptor;
  readonly frames: readonly MidgardMpfProofFrame[];
  readonly initial: MidgardMpfProofFoldControl;
  readonly steps: readonly MidgardMpfProofFoldStep[];
  readonly terminal: MidgardMpfProofFoldControl;
};

type JsonRecord = Readonly<Record<string, unknown>>;

const hash32 = (bytes: Uint8Array): Hash32 =>
  ensureHash32(blake2b(bytes, { dkLen: 32 }), "mpf_proof_fold.hash");

const asRecord = (value: unknown, field: string): JsonRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object`);
  }
  return value as JsonRecord;
};

const asBoundedInteger = (
  value: unknown,
  field: string,
  maximum: number,
): number => {
  if (
    typeof value !== "number" ||
    !Number.isSafeInteger(value) ||
    value < 0 ||
    value > maximum
  ) {
    throw new Error(`${field} is outside its canonical integer envelope`);
  }
  return value;
};

const asHexBytes = (
  value: unknown,
  field: string,
  exactLength?: number,
  maximumLength?: number,
): Buffer => {
  if (
    typeof value !== "string" ||
    value.length % 2 !== 0 ||
    !/^[0-9a-f]*$/u.test(value)
  ) {
    throw new Error(`${field} must be canonical lowercase hexadecimal`);
  }
  const bytes = Buffer.from(value, "hex");
  if (exactLength !== undefined && bytes.length !== exactLength) {
    throw new Error(
      `${field} must contain exactly ${exactLength.toString()} bytes`,
    );
  }
  if (maximumLength !== undefined && bytes.length > maximumLength) {
    throw new Error(`${field} exceeds ${maximumLength.toString()} bytes`);
  }
  return bytes;
};

export const parseMidgardMpfProofJson = (
  value: unknown,
): readonly MidgardMpfProofStep[] => {
  if (!Array.isArray(value)) {
    throw new Error("MPF proof JSON must be an array");
  }
  if (value.length > PATH_NIBBLE_COUNT) {
    throw new Error("MPF proof has more frames than the key path");
  }
  return value.map((rawStep, index) => {
    const field = `mpf_proof[${index.toString()}]`;
    const step = asRecord(rawStep, field);
    const skip = asBoundedInteger(
      step.skip,
      `${field}.skip`,
      PATH_NIBBLE_COUNT,
    );
    if (step.type === "branch") {
      return {
        kind: "branch",
        skip,
        neighbors: asHexBytes(step.neighbors, `${field}.neighbors`, 4 * 32),
      };
    }
    if (step.type === "fork") {
      const neighbor = asRecord(step.neighbor, `${field}.neighbor`);
      return {
        kind: "fork",
        skip,
        neighbor: {
          nibble: asBoundedInteger(
            neighbor.nibble,
            `${field}.neighbor.nibble`,
            15,
          ),
          prefix: asHexBytes(
            neighbor.prefix,
            `${field}.neighbor.prefix`,
            undefined,
            32,
          ),
          root: ensureHash32(
            asHexBytes(neighbor.root, `${field}.neighbor.root`, 32),
            `${field}.neighbor.root`,
          ),
        },
      };
    }
    if (step.type === "leaf") {
      const neighbor = asRecord(step.neighbor, `${field}.neighbor`);
      return {
        kind: "leaf",
        skip,
        key: ensureHash32(
          asHexBytes(neighbor.key, `${field}.neighbor.key`, 32),
          `${field}.neighbor.key`,
        ),
        value: ensureHash32(
          asHexBytes(neighbor.value, `${field}.neighbor.value`, 32),
          `${field}.neighbor.value`,
        ),
      };
    }
    throw new Error(`${field}.type is not a canonical MPF proof step`);
  });
};

const nibbleAt = (path: Uint8Array, index: number): number => {
  if (!Number.isSafeInteger(index) || index < 0 || index >= PATH_NIBBLE_COUNT) {
    throw new Error("MPF nibble cursor is outside the key path");
  }
  const byte = path[Math.floor(index / 2)]!;
  return index % 2 === 0 ? Math.floor(byte / 16) : byte % 16;
};

const pathNibbles = (path: Uint8Array, start: number, end: number): Buffer => {
  const result: number[] = [];
  for (let cursor = start; cursor < end; cursor += 1) {
    result.push(nibbleAt(path, cursor));
  }
  return Buffer.from(result);
};

const suffix = (path: Uint8Array, cursor: number): Buffer => {
  if (
    !Number.isSafeInteger(cursor) ||
    cursor < 0 ||
    cursor > PATH_NIBBLE_COUNT
  ) {
    throw new Error("MPF suffix cursor is outside the key path");
  }
  if (cursor % 2 === 0) {
    return Buffer.concat([
      Buffer.from([0xff]),
      Buffer.from(path).subarray(cursor / 2),
    ]);
  }
  return Buffer.concat([
    Buffer.from([0, nibbleAt(path, cursor)]),
    Buffer.from(path).subarray((cursor + 1) / 2),
  ]);
};

const combine = (left: Uint8Array, right: Uint8Array): Hash32 =>
  hash32(Buffer.concat([Buffer.from(left), Buffer.from(right)]));

const merkle2 = (
  branch: number,
  root: Uint8Array,
  neighbor: Uint8Array,
): Hash32 => (branch <= 0 ? combine(root, neighbor) : combine(neighbor, root));

const merkle4 = (
  branch: number,
  root: Uint8Array,
  neighbor2: Uint8Array,
  neighbor1: Uint8Array,
): Hash32 =>
  branch <= 1
    ? combine(merkle2(branch, root, neighbor1), neighbor2)
    : combine(neighbor2, merkle2(branch - 2, root, neighbor1));

const merkle8 = (
  branch: number,
  root: Uint8Array,
  neighbor4: Uint8Array,
  neighbor2: Uint8Array,
  neighbor1: Uint8Array,
): Hash32 =>
  branch <= 3
    ? combine(merkle4(branch, root, neighbor2, neighbor1), neighbor4)
    : combine(neighbor4, merkle4(branch - 4, root, neighbor2, neighbor1));

const merkle16 = (
  branch: number,
  root: Uint8Array,
  neighbor8: Uint8Array,
  neighbor4: Uint8Array,
  neighbor2: Uint8Array,
  neighbor1: Uint8Array,
): Hash32 =>
  branch <= 7
    ? combine(merkle8(branch, root, neighbor4, neighbor2, neighbor1), neighbor8)
    : combine(
        neighbor8,
        merkle8(branch - 8, root, neighbor4, neighbor2, neighbor1),
      );

const sparseMerkle16 = (
  ownNibble: number,
  ownRoot: Uint8Array,
  neighborNibble: number,
  neighborRoot: Uint8Array,
): Hash32 => {
  if (ownNibble === neighborNibble) {
    throw new Error("MPF fork places both children at the same nibble");
  }
  let level = Array.from<Uint8Array>({ length: 16 }).fill(NULL_HASH);
  level[ownNibble] = ownRoot;
  level[neighborNibble] = neighborRoot;
  while (level.length > 1) {
    const next: Hash32[] = [];
    for (let index = 0; index < level.length; index += 2) {
      next.push(combine(level[index]!, level[index + 1]!));
    }
    level = next;
  }
  return ensureHash32(level[0]!, "mpf_proof_fold.sparse_root");
};

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

const foldIncludingFrame = (
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

const foldExcludingFrame = (
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
    nibbleAt(frame.step.key, frame.cursor),
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
      frame.step.neighbor.nibble > 15 ||
      frame.step.neighbor.prefix.length > 32
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
