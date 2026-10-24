/** The off-chain model of `midgard/data_summary_walk_v1`'s walk state. */
import type {
  MidgardCekDataSequenceSummary,
  MidgardCekDataSummary,
} from "@al-ft/midgard-core";

export type DataSummaryWalkFrame =
  | { readonly kind: "root" }
  | { readonly kind: "list" }
  | { readonly kind: "constr"; readonly constructor: bigint }
  | {
      readonly kind: "largeConstr";
      readonly indexRoot: Uint8Array;
      readonly indexCborLength: bigint;
      readonly indexMemory: bigint;
    }
  | { readonly kind: "map"; readonly remaining: bigint }
  | {
      readonly kind: "folding";
      readonly open: DataSummaryWalkFrame;
      readonly folded: MidgardCekDataSequenceSummary;
    };

export type DataSummaryWalkEntry =
  | { readonly kind: "item"; readonly summary: MidgardCekDataSummary }
  | { readonly kind: "parent"; readonly frame: DataSummaryWalkFrame };

/**
 * A stack as an immutable list, top first. Each node caches the hash chain of
 * itself and everything below it, so a checkpoint's root costs one hash per
 * push.
 */
export type DataSummaryWalkStack = {
  readonly entry: DataSummaryWalkEntry;
  readonly below: DataSummaryWalkStack | null;
  readonly depth: number;
  readonly chain: Buffer;
} | null;

/** What one on-chain step did, for the transaction cost model. */
export type DataSummaryWalkStepKind =
  | "integer"
  | "bytes"
  | "open"
  | "close"
  | "pop"
  | "popPair"
  | "finish";

export type DataSummaryWalkStep = {
  readonly kind: DataSummaryWalkStepKind;
  /** Content bytes a leaf step hashed (byte string, bignum or index). */
  readonly hashedBytes: number;
};

/** The checkpoint between two steps. */
export type DataSummaryWalkPoint = {
  readonly at: number;
  readonly frame: DataSummaryWalkFrame;
  readonly stack: DataSummaryWalkStack;
};

export type DataSummaryWalkTrace = {
  /** `points[i]` is the checkpoint after `i` steps. */
  readonly points: readonly DataSummaryWalkPoint[];
  /** `steps[i]` leads from `points[i]` to `points[i + 1]` (or the end). */
  readonly steps: readonly DataSummaryWalkStep[];
  readonly outcome:
    | { readonly kind: "summarised"; readonly summary: MidgardCekDataSummary }
    | { readonly kind: "unreadable" };
};
