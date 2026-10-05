import { midgardBlake2b } from "./codec/blake2b.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  type MidgardValidationMerkleFrontier,
  type MidgardValidationMerkleMembership,
} from "./validation-merkle.js";

export const FRAME_DOMAIN = Buffer.from("MidgardMpfProofFrameV1", "ascii");

export const NULL_HASH = Buffer.alloc(32);

export const PATH_NIBBLE_COUNT = 64;

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

export const hash32 = (bytes: Uint8Array): Hash32 =>
  ensureHash32(midgardBlake2b(bytes, { dkLen: 32 }), "mpf_proof_fold.hash");

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

export const nibbleAt = (path: Uint8Array, index: number): number => {
  if (!Number.isSafeInteger(index) || index < 0 || index >= PATH_NIBBLE_COUNT) {
    throw new Error("MPF nibble cursor is outside the key path");
  }
  const byte = path[Math.floor(index / 2)]!;
  return index % 2 === 0 ? Math.floor(byte / 16) : byte % 16;
};

export const pathNibbles = (
  path: Uint8Array,
  start: number,
  end: number,
): Buffer => {
  const result: number[] = [];
  for (let cursor = start; cursor < end; cursor += 1) {
    result.push(nibbleAt(path, cursor));
  }
  return Buffer.from(result);
};

export const suffix = (path: Uint8Array, cursor: number): Buffer => {
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
    Buffer.from([0x10, nibbleAt(path, cursor)]),
    Buffer.from(path).subarray((cursor + 1) / 2),
  ]);
};

export const combine = (left: Uint8Array, right: Uint8Array): Hash32 =>
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

export const merkle16 = (
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

export const sparseMerkle16 = (
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
