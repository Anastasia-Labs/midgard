import { ensureHash32 } from "./codec/hash.js";
import { midgardMpfTerminalBranchKeepsTwoChildren } from "./mpf-deletion-opening.js";
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
  type MidgardMpfProofFrame,
  type MidgardMpfProofStep,
  nibbleAt,
  NULL_HASH,
  PATH_NIBBLE_COUNT,
  suffix,
} from "./mpf-proof-fold.parse-midgard-mpf-proof-json.js";
import { buildMidgardValidationMerkleMembership } from "./validation-merkle.js";

// The terminal frame's neighbour is the one the including and excluding roots
// read two ways, so its shape is pinned to the honest trie's (the twin of
// `terminal_leaf_neighbor_is_canonical`,
// `terminal_fork_prefix_ends_inside_the_path` and
// `terminal_fork_neighbor_is_canonical` in `mpf-proof-v1.ak`). A terminal Leaf
// shares the skipped nibbles with the proven path. A terminal Fork's own
// branching nibble lies inside the path. On a deletion the excluding root is
// not compared with an authenticated root, so there the Fork neighbour must
// also open like a branch preimage: an empty prefix or a first byte below 16.
// Leaf and branch node preimages are disjoint (a leaf preimage never starts
// with a byte below 16), so that first byte is enough.
const assertTerminalNeighborIsCanonical = (
  path: Uint8Array,
  frame: MidgardMpfProofFrame,
  isDeletion: boolean,
): void => {
  if (frame.step.kind === "leaf") {
    const key = frame.step.key;
    for (
      let cursor = frame.cursor;
      cursor < frame.nextCursor - 1;
      cursor += 1
    ) {
      if (nibbleAt(key, cursor) !== nibbleAt(path, cursor)) {
        throw new Error(
          "MPF terminal leaf neighbor does not share the skipped path prefix",
        );
      }
    }
    return;
  }
  if (frame.step.kind === "fork") {
    const prefix = frame.step.neighbor.prefix;
    if (frame.nextCursor + prefix.length >= PATH_NIBBLE_COUNT) {
      throw new Error(
        "MPF terminal fork neighbor prefix does not end inside the key path",
      );
    }
    if (isDeletion && prefix.length > 0 && prefix[0]! > 15) {
      throw new Error(
        "MPF terminal fork neighbor of a deletion does not open like a branch",
      );
    }
  }
};

/**
 * `deletionOpening` marks the trace as a deletion and carries the terminal
 * Branch's group opening (`buildMidgardMpfDeletionOpening`). A deletion's
 * terminal Branch must keep two other children
 * (`midgardMpfTerminalBranchKeepsTwoChildren`); an insertion or update leaves
 * it undefined, since its excluding side is the authenticated predecessor.
 */
export const buildMidgardMpfProofFoldTrace = ({
  key,
  value,
  steps,
  deletionOpening,
}: {
  readonly key: Uint8Array;
  readonly value: Uint8Array;
  readonly steps: readonly MidgardMpfProofStep[];
  readonly deletionOpening?: Uint8Array;
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
    if (frameIndex === frames.length - 1) {
      assertTerminalNeighborIsCanonical(
        path,
        frame,
        deletionOpening !== undefined,
      );
      if (
        deletionOpening !== undefined &&
        frame.step.kind === "branch" &&
        !midgardMpfTerminalBranchKeepsTwoChildren(
          frame.step.neighbors,
          deletionOpening,
        )
      ) {
        throw new Error(
          "MPF terminal branch of a deletion does not keep two children",
        );
      }
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
