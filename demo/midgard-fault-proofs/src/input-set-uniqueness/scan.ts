/**
 * Prover-side duplicate scan for the `input-set-uniqueness` family.
 *
 * Operates on the decoded §5.3 item lists of §2.5 fields 0 (spend inputs) and
 * 1 (reference inputs): each item is the canonical 38-byte out-ref encoding,
 * so hex byte equality of two items *is* out-ref equality — exactly the
 * predicate step-02 enforces on-chain. The scan therefore constructs claims
 * that are convictable verbatim, and refuses (returns nothing) whenever the
 * committed transaction is honest.
 *
 * Sub-variant priority is deterministic — duplicate spend inputs, then
 * duplicate reference inputs, then spend/reference overlap — and within a
 * variant the lexicographically-first index pair wins, so one committed
 * transaction always maps to one canonical claim.
 */
import { inputSetUniquenessSubmitError } from "./submit-common.js";

export const INPUT_SET_UNIQUENESS_VIOLATION_ID =
  "input-set-uniqueness" as const;

export type InputSetUniquenessClaim =
  | {
      readonly kind: "duplicateSpendInputs";
      readonly firstIndex: bigint;
      readonly secondIndex: bigint;
    }
  | {
      readonly kind: "duplicateReferenceInputs";
      readonly firstIndex: bigint;
      readonly secondIndex: bigint;
    }
  | {
      readonly kind: "spendReferenceOverlap";
      readonly spendIndex: bigint;
      readonly referenceIndex: bigint;
    };

const normalizeItems = (
  items: readonly string[],
  fieldLabel: string,
): readonly string[] =>
  items.map((item, index) => {
    const lowered = item.toLowerCase();
    // §5.3 fields 0/1 item: `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16` — fixed
    // 38 bytes (the `58 26` wrapper is §5.1 preimage assembly, not the item).
    if (!/^825820[0-9a-f]{64}19[0-9a-f]{4}$/u.test(lowered)) {
      throw inputSetUniquenessSubmitError(
        `${fieldLabel} item ${index.toString()} is not a canonical 38-byte §5.3 out-ref item.`,
      );
    }
    return lowered;
  });

/** First in-list duplicate pair, or undefined for a unique list. */
const firstDuplicatePair = (
  items: readonly string[],
):
  | { readonly firstIndex: bigint; readonly secondIndex: bigint }
  | undefined => {
  const seenAt = new Map<string, number>();
  for (const [index, item] of items.entries()) {
    const first = seenAt.get(item);
    if (first !== undefined) {
      return { firstIndex: BigInt(first), secondIndex: BigInt(index) };
    }
    seenAt.set(item, index);
  }
  return undefined;
};

/**
 * Scans one committed transaction's decoded input lists for every
 * intra-transaction input-set fault, in canonical claim order.
 */
export const scanInputSetUniqueness = ({
  spendInputItemCbors,
  referenceInputItemCbors,
}: {
  readonly spendInputItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
}): readonly InputSetUniquenessClaim[] => {
  const spends = normalizeItems(spendInputItemCbors, "spend-input");
  const references = normalizeItems(referenceInputItemCbors, "reference-input");
  const claims: InputSetUniquenessClaim[] = [];

  const spendDuplicate = firstDuplicatePair(spends);
  if (spendDuplicate !== undefined) {
    claims.push({ kind: "duplicateSpendInputs", ...spendDuplicate });
  }
  const referenceDuplicate = firstDuplicatePair(references);
  if (referenceDuplicate !== undefined) {
    claims.push({ kind: "duplicateReferenceInputs", ...referenceDuplicate });
  }

  const spendIndexByItem = new Map<string, number>();
  // Iterate backwards so the map keeps the *lowest* spend index per item.
  for (let index = spends.length - 1; index >= 0; index -= 1) {
    spendIndexByItem.set(spends[index] as string, index);
  }
  for (const [referenceIndex, item] of references.entries()) {
    const spendIndex = spendIndexByItem.get(item);
    if (spendIndex !== undefined) {
      claims.push({
        kind: "spendReferenceOverlap",
        spendIndex: BigInt(spendIndex),
        referenceIndex: BigInt(referenceIndex),
      });
      break;
    }
  }

  return claims;
};

/**
 * The single canonical claim for a faulty transaction; throws when the input
 * sets are honest (no duplicate, no overlap) — a prover must never open a
 * thread it cannot finish.
 */
export const requireInputSetUniquenessClaim = (args: {
  readonly spendInputItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
}): InputSetUniquenessClaim => {
  const [claim] = scanInputSetUniqueness(args);
  if (claim === undefined) {
    throw inputSetUniquenessSubmitError(
      "the transaction's input sets are unique and disjoint; there is no input-set fault to prove.",
    );
  }
  return claim;
};
