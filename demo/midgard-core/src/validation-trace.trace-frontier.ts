import { ensureHash32, type Hash32 } from "./codec/hash.js";
import { MIDGARD_CONSENSUS_LIMITS } from "./consensus-profile.js";
import {
  fail,
  hashDomain,
  TRACE_BRANCH_DOMAIN,
  TRACE_LEAF_DOMAIN,
} from "./validation-trace.encode-midgard-validation-machine-state.js";

/**
 * Streaming form of the validation trace tree. `buildMidgardValidationTraceTree`
 * holds every level; this folds state hashes into an O(log n) frontier and
 * yields the same root: leaves are the trace states in order, padded to the
 * next power of two with the last state's leaf.
 */

/** The trace leaf of one machine-state hash. */
export const midgardValidationTraceLeafHash = (stateHash: Uint8Array): Hash32 =>
  hashDomain(
    TRACE_LEAF_DOMAIN,
    ensureHash32(stateHash, "validation_trace.state_hash"),
  );

/** The trace branch over two child roots. */
export const midgardValidationTraceBranchHash = (
  left: Uint8Array,
  right: Uint8Array,
): Hash32 =>
  hashDomain(
    TRACE_BRANCH_DOMAIN,
    Buffer.concat([
      ensureHash32(left, "validation_trace.left"),
      ensureHash32(right, "validation_trace.right"),
    ]),
  );

/**
 * Roots of runs of one repeated state: entry `k` is the root of a subtree of
 * `2^k` copies of `stateHash`'s leaf, computed as `pad_k = branch(pad_{k-1},
 * pad_{k-1})` without materialising the run.
 */
export class MidgardValidationTracePadding {
  private readonly roots: Hash32[];

  constructor(stateHash: Uint8Array) {
    this.roots = [midgardValidationTraceLeafHash(stateHash)];
  }

  public root(level: number): Hash32 {
    while (this.roots.length <= level) {
      const below = this.roots[this.roots.length - 1]!;
      this.roots.push(midgardValidationTraceBranchHash(below, below));
    }
    return this.roots[level]!;
  }
}

const MAX_TRACE_STATES =
  MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount + 1;

/**
 * An append-only validation trace commitment in O(log n) memory. States are
 * appended one at a time, or as a run of one repeated state (the padding of a
 * trace extended past its terminal), and `root()` returns the root
 * `buildMidgardValidationTraceTree` computes over the same states.
 */
export class MidgardValidationTraceFrontier {
  /** `peaks[k]` is the pending left subtree of `2^k` leaves, if any. */
  private readonly peaks: (Hash32 | undefined)[] = [];
  private count = 0;
  private first: Hash32 | undefined;
  private last: Hash32 | undefined;

  /** The number of states appended. */
  public get stateCount(): number {
    return this.count;
  }

  /** The first state hash appended. */
  public get initialStateHash(): Hash32 {
    return this.first ?? fail("Validation trace frontier is empty");
  }

  /** The last state hash appended. */
  public get lastStateHash(): Hash32 {
    return this.last ?? fail("Validation trace frontier is empty");
  }

  public appendState(stateHash: Uint8Array): void {
    this.appendRun(stateHash, 1);
  }

  /** Appends `length` copies of one state in O(log length) hashes. */
  public appendRun(stateHash: Uint8Array, length: number): void {
    if (!Number.isSafeInteger(length) || length <= 0) {
      fail("Validation trace run length must be a positive safe integer");
    }
    if (length > MAX_TRACE_STATES - this.count) {
      fail("Validation trace exceeds the maximum step count");
    }
    const hash = ensureHash32(stateHash, "validation_trace.state_hash");
    this.first ??= hash;
    this.last = hash;
    if (length === 1) {
      this.insert(midgardValidationTraceLeafHash(hash), 0);
      this.count += 1;
      return;
    }
    const padding = new MidgardValidationTracePadding(hash);
    let remaining = length;
    while (remaining > 0) {
      let level = 0;
      let size = 1;
      while (size * 2 <= remaining && this.count % (size * 2) === 0) {
        size *= 2;
        level += 1;
      }
      this.insert(padding.root(level), level);
      this.count += size;
      remaining -= size;
    }
  }

  /**
   * The trace root: the appended states padded to the next power of two
   * with the last state's leaf. The frontier is unchanged.
   */
  public root(): Hash32 {
    if (this.count === 0) {
      return fail("Validation trace must contain at least its initial state");
    }
    const copy = new MidgardValidationTraceFrontier();
    copy.peaks.push(...this.peaks);
    copy.count = this.count;
    copy.first = this.first;
    copy.last = this.last;
    let paddedCount = 1;
    while (paddedCount < this.count) paddedCount *= 2;
    if (paddedCount > this.count) {
      copy.appendRun(this.last!, paddedCount - this.count);
    }
    const root = copy.peaks[copy.peaks.length - 1];
    return root ?? fail("Validation trace frontier has no root");
  }

  private insert(node: Hash32, level: number): void {
    let carry = node;
    let height = level;
    while (this.peaks[height] !== undefined) {
      carry = midgardValidationTraceBranchHash(this.peaks[height]!, carry);
      this.peaks[height] = undefined;
      height += 1;
    }
    this.peaks[height] = carry;
  }
}
