/**
 * `value-not-preserved` prover-side search (offchain plan §3).
 *
 * The chain verifies ONE single-asset conservation fold; finding which asset
 * to accuse is entirely offchain. This module takes the committed
 * transaction's structured facts — validity, fee, resolved spent-input
 * values, outputs, mint — and returns the first violated unit in the
 * deterministic search order (ADA first, then every token unit appearing
 * anywhere in the tx, in canonical §5.3 order).
 *
 * Fail-closed edges, straight from the plan:
 *
 * - a REJECTED commitment (`TxIsInvalid`) is never searchable: the honest
 *   machine records it without applying it, so no imbalance in its body is a
 *   fault (§1.4) — the search refuses rather than returns `balanced`;
 * - a spend input whose pre-state value cannot be resolved (created inside
 *   the same block, §7.4) makes every verdict unprovable under the v1
 *   pre-state-membership evidence model — the search says so explicitly
 *   instead of accusing on partial sums.
 */
import type {
  MidgardMintPolicyItemV1,
  MidgardValue,
} from "@al-ft/midgard-core";

import { VALUE_NOT_PRESERVED_CATEGORY_LABEL } from "./contracts-v1.js";
import type {
  ClaimedAssetV1,
  ClaimedImbalanceDirectionV1,
} from "./schemas-v1.js";

/** A spend input's resolved pre-state value, or the §7.4 gap. */
export type ResolvedSpentValueV1 =
  | { readonly kind: "resolved"; readonly value: MidgardValue }
  | { readonly kind: "unknown"; readonly reason: string };

/** The search's verdict over one committed transaction. */
export type ValueNotPreservedFindingV1 =
  | {
      readonly kind: "fault";
      readonly claimedAsset: ClaimedAssetV1;
      readonly claimedDirection: ClaimedImbalanceDirectionV1;
      /** The signed conservation delta (negative = inflated). */
      readonly delta: bigint;
    }
  | { readonly kind: "balanced" }
  | { readonly kind: "unprovable"; readonly reason: string };

type UnitTotals = {
  readonly policyIdHex: string;
  readonly assetNameHex: string;
  spentIn: bigint;
  minted: bigint;
  paidOut: bigint;
};

const unitOrder = (left: UnitTotals, right: UnitTotals): number => {
  const policyCompare = Buffer.compare(
    Buffer.from(left.policyIdHex, "hex"),
    Buffer.from(right.policyIdHex, "hex"),
  );
  if (policyCompare !== 0) return policyCompare;
  const leftName = Buffer.from(left.assetNameHex, "hex");
  const rightName = Buffer.from(right.assetNameHex, "hex");
  return (
    leftName.length - rightName.length || Buffer.compare(leftName, rightName)
  );
};

/**
 * Searches one committed, operator-accepted transaction for a violated unit.
 *
 * `spentValues` MUST be index-aligned with the tx's spend-inputs field —
 * one entry per input, resolved against the challenged header's pre-state
 * ledger (the same source the step-02 witnesses will authenticate).
 */
export const findValueNotPreservedV1 = ({
  validity,
  fee,
  spentValues,
  outputValues,
  mintItems,
}: {
  readonly validity: "TxIsValid" | "TxIsInvalid";
  readonly fee: bigint;
  readonly spentValues: readonly ResolvedSpentValueV1[];
  readonly outputValues: readonly MidgardValue[];
  readonly mintItems: readonly MidgardMintPolicyItemV1[] | null;
}): ValueNotPreservedFindingV1 => {
  if (validity !== "TxIsValid") {
    throw new Error(
      `${VALUE_NOT_PRESERVED_CATEGORY_LABEL} finding: refusing to search a rejected commitment — an honest no-op recording never convicts (§1.4)`,
    );
  }
  const gaps = spentValues.filter(
    (entry): entry is Extract<ResolvedSpentValueV1, { kind: "unknown" }> =>
      entry.kind === "unknown",
  );
  if (gaps.length > 0) {
    return {
      kind: "unprovable",
      reason: `unresolvable spend input(s) under the pre-state ledger (§7.4): ${gaps
        .map((gap) => gap.reason)
        .join("; ")}`,
    };
  }

  // --- ADA first ---------------------------------------------------------
  let adaIn = 0n;
  let adaOut = 0n;
  const units = new Map<string, UnitTotals>();
  const totalsFor = (policyIdHex: string, assetNameHex: string): UnitTotals => {
    const key = `${policyIdHex}.${assetNameHex}`;
    const existing = units.get(key);
    if (existing !== undefined) return existing;
    const fresh: UnitTotals = {
      policyIdHex,
      assetNameHex,
      spentIn: 0n,
      minted: 0n,
      paidOut: 0n,
    };
    units.set(key, fresh);
    return fresh;
  };

  for (const entry of spentValues) {
    if (entry.kind !== "resolved") continue;
    adaIn += entry.value.lovelace;
    for (const [policyIdHex, names] of entry.value.assets) {
      for (const [assetNameHex, quantity] of names) {
        if (quantity === 0n) continue;
        totalsFor(policyIdHex, assetNameHex).spentIn += quantity;
      }
    }
  }
  for (const value of outputValues) {
    adaOut += value.lovelace;
    for (const [policyIdHex, names] of value.assets) {
      for (const [assetNameHex, quantity] of names) {
        if (quantity === 0n) continue;
        totalsFor(policyIdHex, assetNameHex).paidOut += quantity;
      }
    }
  }
  for (const item of mintItems ?? []) {
    const policyIdHex = Buffer.from(item.policyId).toString("hex");
    for (const asset of item.assets) {
      if (asset.quantity === 0n) continue;
      totalsFor(
        policyIdHex,
        Buffer.from(asset.assetName).toString("hex"),
      ).minted += asset.quantity;
    }
  }

  const adaDelta = adaIn - adaOut - fee;
  if (adaDelta !== 0n) {
    return {
      kind: "fault",
      claimedAsset: "AdaAsset",
      claimedDirection:
        adaDelta < 0n ? "ClaimedAssetInflated" : "ClaimedAssetDeflated",
      delta: adaDelta,
    };
  }

  // --- then every token unit, canonical order ----------------------------
  for (const totals of [...units.values()].sort(unitOrder)) {
    const delta = totals.spentIn + totals.minted - totals.paidOut;
    if (delta === 0n) continue;
    return {
      kind: "fault",
      claimedAsset: {
        TokenAsset: {
          policy_id: totals.policyIdHex,
          asset_name: totals.assetNameHex,
        },
      },
      claimedDirection:
        delta < 0n ? "ClaimedAssetInflated" : "ClaimedAssetDeflated",
      delta,
    };
  }
  return { kind: "balanced" };
};
