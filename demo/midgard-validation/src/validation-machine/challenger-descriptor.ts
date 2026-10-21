/**
 * An honest trace at another party's step count, and the challenger's
 * decision whether a game opens at all.
 *
 * Given the honest states h_0 .. h_T and a step count n (the operator's for a
 * challenger, the case's for a defender), the honest trace at n has leaf
 * c_i = H(h_min(i, m)) for every index i, where
 * m = min(n, T): an honest trace longer than n is truncated at n, and one
 * shorter is extended with its terminal state. Past index m, every leaf of
 * the padded tree is the same, so the padded subtrees are the constant
 * roots pad_k = H(pad_{k-1}, pad_{k-1}) and nothing past m is materialised:
 * building the descriptor costs O(m + log n) hashes, and each proof
 * O(log n).
 */
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_VALIDATION_MACHINE_VERSION,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
  midgardValidationTraceBranchHash,
  type MidgardValidationTraceDescriptor,
  midgardValidationTraceLeafHash,
  MidgardValidationTracePadding,
  type MidgardValidationTraceProof,
  validationTraceDepthForStepCount,
} from "@al-ft/midgard-core";

import {
  type ValidationTraceLeavesV1,
  validationTraceStateHashV1,
} from "./validation-trace-v1.js";

/** An honest trace at a step count it did not choose: either side's trace in a game. */
export type ValidationTraceAtStepCountV1 = {
  readonly descriptor: MidgardValidationTraceDescriptor;
  /** m = min(n, T): leaf i is honest state min(i, m). */
  readonly lastHonestIndex: number;
  /** The honest state whose hash is leaf `index`. */
  readonly honestIndex: (index: number) => number;
  readonly stateHash: (index: number) => Buffer;
  /** The proof of leaf `index` against this trace's root. */
  readonly proof: (index: number) => MidgardValidationTraceProof;
};

const assertStepCount = (stepCount: number): void => {
  if (
    !Number.isSafeInteger(stepCount) ||
    stepCount < 1 ||
    stepCount > MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount
  ) {
    throw new Error("step count is outside the descriptor range");
  }
};

/**
 * The descriptor and proofs of the honest trace `leaves` at
 * `stepCount` steps: the challenger's at the operator's step count,
 * or the defender's at the case's. The verdict and rejection hash are the
 * honest run's.
 */
export const buildTraceAtStepCountV1 = (
  leaves: Pick<
    ValidationTraceLeavesV1,
    "stepCount" | "stateHashes" | "descriptor"
  >,
  stepCount: number,
): ValidationTraceAtStepCountV1 => {
  assertStepCount(stepCount);
  const lastHonestIndex = Math.min(stepCount, leaves.stepCount);
  const depth = validationTraceDepthForStepCount(stepCount);
  const padding = new MidgardValidationTracePadding(
    validationTraceStateHashV1(leaves, lastHonestIndex),
  );
  // levels[k] holds the subtree roots at height k that cover at least one
  // index <= m; every other node at that height is padding.root(k).
  const levels: Buffer[] = [];
  const level0 = Buffer.alloc((lastHonestIndex + 1) * 32);
  for (let index = 0; index <= lastHonestIndex; index += 1) {
    midgardValidationTraceLeafHash(
      validationTraceStateHashV1(leaves, index),
    ).copy(level0, index * 32);
  }
  levels.push(level0);
  for (let height = 1; height <= depth; height += 1) {
    const below = levels[height - 1]!;
    const belowCount = below.length / 32;
    const count = Math.floor((belowCount - 1) / 2) + 1;
    const level = Buffer.alloc(count * 32);
    for (let index = 0; index < count; index += 1) {
      const left = below.subarray(index * 64, index * 64 + 32);
      const right =
        2 * index + 1 < belowCount
          ? below.subarray(index * 64 + 32, index * 64 + 64)
          : padding.root(height - 1);
      midgardValidationTraceBranchHash(left, right).copy(level, index * 32);
    }
    levels.push(level);
  }
  const node = (height: number, index: number): Buffer => {
    const level = levels[height]!;
    return index * 32 < level.length
      ? Buffer.from(level.subarray(index * 32, index * 32 + 32))
      : padding.root(height);
  };
  const assertIndex = (index: number): void => {
    if (!Number.isSafeInteger(index) || index < 0 || index > stepCount) {
      throw new Error("trace index is out of range");
    }
  };
  const honestIndex = (index: number): number => {
    assertIndex(index);
    return Math.min(index, lastHonestIndex);
  };
  const stateHash = (index: number): Buffer =>
    Buffer.from(validationTraceStateHashV1(leaves, honestIndex(index)));
  return {
    descriptor: {
      schemaVersion: MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
      machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
      traceRoot: node(depth, 0),
      stepCount,
      initialStateHash: stateHash(0),
      terminalStateHash: stateHash(stepCount),
      verdict: leaves.descriptor.verdict,
      rejectionCodeHash: leaves.descriptor.rejectionCodeHash,
    },
    lastHonestIndex,
    honestIndex,
    stateHash,
    proof: (index) => {
      assertIndex(index);
      const siblings: Buffer[] = [];
      let position = index;
      for (let height = 0; height < depth; height += 1) {
        siblings.push(
          node(height, position % 2 === 0 ? position + 1 : position - 1),
        );
        position = Math.floor(position / 2);
      }
      return { stateIndex: index, stateHash: stateHash(index), siblings };
    },
  };
};

/**
 * What a challenger does about an operator's descriptor, decided in the order
 * VerifySource checks it.
 *
 * - `malformed`: not a well-formed descriptor of this machine; it is not
 *   disputed by a game.
 * - `descriptorVerdict`: a normal transaction's descriptor that is not
 *   Accepted with no rejection; check 3 decides it from the descriptor.
 * - `agree`: the operator's terminal hash is the challenger's and is coherent
 *   with its step count, verdict and rejection. No game opens, whatever the
 *   operator's trace root: a trace-root-only difference is never disputed.
 * - `terminalIncoherent`: equal terminal hashes, but the state behind them
 *   is not the coherent terminal; the evidence is honest state
 *   `terminalStateIndex`.
 * - `placeholderTarget`: the transaction does not take a real descriptor
 *   (rule R-DESC), so no game opens.
 * - `bisection`: the terminal hashes differ; the game opens with the
 *   challenger's trace.
 */
export type ValidationChallengeDecisionV1 =
  | { readonly kind: "malformed" }
  | { readonly kind: "descriptorVerdict" }
  | { readonly kind: "agree" }
  | { readonly kind: "terminalIncoherent"; readonly terminalStateIndex: number }
  | { readonly kind: "placeholderTarget" }
  | {
      readonly kind: "bisection";
      readonly challenger: ValidationTraceAtStepCountV1;
    };

export const decideValidationChallengeV1 = (
  leaves: Pick<
    ValidationTraceLeavesV1,
    | "stepCount"
    | "stateHashes"
    | "descriptor"
    | "sourceKind"
    | "requiresRealDescriptor"
  >,
  operator: MidgardValidationTraceDescriptor,
): ValidationChallengeDecisionV1 => {
  if (
    operator.schemaVersion !== MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION ||
    operator.machineVersion !== MIDGARD_VALIDATION_MACHINE_VERSION ||
    !Number.isSafeInteger(operator.stepCount) ||
    operator.stepCount < 1 ||
    operator.stepCount > MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount
  ) {
    return { kind: "malformed" };
  }
  if (
    leaves.sourceKind === "normal" &&
    (operator.verdict !== "accepted" ||
      !Buffer.from(operator.rejectionCodeHash).equals(
        MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
      ))
  ) {
    return { kind: "descriptorVerdict" };
  }
  const challengerTerminalIndex = Math.min(
    operator.stepCount,
    leaves.stepCount,
  );
  if (
    Buffer.from(operator.terminalStateHash).equals(
      validationTraceStateHashV1(leaves, challengerTerminalIndex),
    )
  ) {
    // The state behind the shared hash is honest state min(n, T). It is the
    // coherent terminal iff n == T and the descriptor's verdict and
    // rejection are the honest run's.
    const coherent =
      operator.stepCount === leaves.stepCount &&
      operator.verdict === leaves.descriptor.verdict &&
      Buffer.from(operator.rejectionCodeHash).equals(
        leaves.descriptor.rejectionCodeHash,
      );
    return coherent
      ? { kind: "agree" }
      : {
          kind: "terminalIncoherent",
          terminalStateIndex: challengerTerminalIndex,
        };
  }
  if (!leaves.requiresRealDescriptor) return { kind: "placeholderTarget" };
  return {
    kind: "bisection",
    challenger: buildTraceAtStepCountV1(leaves, operator.stepCount),
  };
};
