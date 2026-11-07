/** The off-chain model of `midgard/data_serialise_walk_v1`'s reader state. */
import type {
  MidgardCekBlobFrontier,
  MidgardCekDataSequenceSummary,
  MidgardCekDataSummary,
} from "@al-ft/midgard-core";

export type MidgardCekSerialiseFrame =
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
      readonly open: MidgardCekSerialiseFrame;
      readonly folded: MidgardCekDataSequenceSummary;
    }
  | {
      /**
       * An indefinite byte string (`tag` 0) or the magnitude of a tag 2/3
       * integer, read up to here: `leaf` holds the full chunks of its
       * committed bytes and `pending` the rest.
       */
      readonly kind: "leaf";
      readonly parent: MidgardCekSerialiseFrame;
      readonly tag: 0 | 2 | 3;
      readonly leaf: MidgardCekBlobFrontier;
      readonly pending: Buffer;
      readonly length: bigint;
      readonly cborLength: bigint;
      readonly allFf: boolean;
    };

export type MidgardCekSerialiseEntry =
  | { readonly kind: "item"; readonly summary: MidgardCekDataSummary }
  | { readonly kind: "parent"; readonly frame: MidgardCekSerialiseFrame };

/**
 * A stack as an immutable list, top first. Each node caches the hash chain of
 * itself and everything below it.
 */
export type MidgardCekSerialiseStack = {
  readonly entry: MidgardCekSerialiseEntry;
  readonly below: MidgardCekSerialiseStack;
  readonly depth: number;
  readonly chain: Buffer;
} | null;

/** What one step did, for the hop cost model. */
export type MidgardCekSerialiseStepKind =
  | "integer"
  | "bytes"
  | "open"
  | "close"
  | "pop"
  | "popPair"
  | "finish"
  | "leaf";

export type MidgardCekSerialiseStep = {
  readonly kind: MidgardCekSerialiseStepKind;
  /** Bytes the step read. */
  readonly read: number;
  /** Chunks the step hashed into a leaf frontier. */
  readonly chunks: number;
};

export type MidgardCekSerialiseProgress =
  | {
      readonly kind: "advanced";
      readonly at: number;
      readonly frame: MidgardCekSerialiseFrame;
      readonly stack: MidgardCekSerialiseStack;
    }
  | { readonly kind: "finished"; readonly summary: MidgardCekDataSummary }
  | { readonly kind: "refused" };
