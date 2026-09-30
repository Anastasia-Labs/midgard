import {
  decodeMidgardFieldPreimage,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import {
  RejectionReasonSchema,
  terminalVerdictContradiction,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  beginMintDeclaredPolicy,
  canonicalKeyPrecedes,
  classifyMintDeclaredAssetLimitFinding,
  decodeMintDeclaredPolicyHeader,
  exactIndex,
  fail,
  initialMintDeclaredFoldCursor,
  MINT_DECLARED_ASSET_LIMIT_FOLD_BUDGET,
  MINT_DECLARED_ASSET_LIMIT_FOLD_POLICY_COST,
  MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS,
  MINT_DECLARED_OUTCOME_CROSSING,
  MINT_DECLARED_OUTCOME_NON_CROSSING,
  MINT_DECLARED_OUTCOME_SCANNING,
  type MintDeclaredAssetLimitFinding,
  type MintDeclaredFoldCursor,
  type MintDeclaredFoldTarget,
  readCanonicalInt,
  readCanonicalLength,
} from "./family.begin-mint-declared-policy.js";

/** `consume_asset_v1`: one asset entry of the open item. */
export const consumeMintDeclaredAsset = ({
  cursor,
  itemIndex,
  item,
  target,
}: {
  readonly cursor: MintDeclaredFoldCursor;
  readonly itemIndex: number;
  readonly item: Uint8Array;
  readonly target: MintDeclaredFoldTarget;
}): MintDeclaredFoldCursor => {
  if (
    cursor.outcome !== MINT_DECLARED_OUTCOME_SCANNING ||
    cursor.activePolicy.length !== 56 ||
    cursor.assetsRemaining <= 0 ||
    cursor.policyAssetCursor < 0
  )
    return fail("fold cursor has no open policy to consume");
  const name = readCanonicalLength(item, cursor.itemCursor, 2, "asset name");
  const nameEnd = name.next + name.value;
  if (name.value > 32 || nameEnd > item.length)
    return fail("mint asset name is wider than 32 bytes or truncated");
  const assetName = item.subarray(name.next, nameEnd);
  const quantity = readCanonicalInt(item, nameEnd);
  const nextCount = cursor.accumulatedCount + 1;
  if (quantity.value === 0n) return fail("mint asset quantity is zero");
  if (nextCount > MINT_DECLARED_ASSET_LIMIT_MAX_ASSETS)
    return fail("mint asset count crossed inside a prior policy");
  if (
    cursor.policyAssetCursor !== 0 &&
    !canonicalKeyPrecedes(Buffer.from(cursor.previousAsset, "hex"), assetName)
  )
    return fail("mint asset order is not strictly ascending");
  const last = cursor.assetsRemaining === 1;
  if (last ? quantity.next !== item.length : quantity.next >= item.length)
    return fail("mint policy body length does not match its declared count");
  if (last)
    return Object.freeze({
      ...cursor,
      accumulatedCount: nextCount,
      previousPolicy: cursor.activePolicy,
      activePolicy: "",
      itemCursor: 0,
      assetsRemaining: 0,
      policyAssetCursor: 0,
      previousAsset: "",
      outcome:
        itemIndex === target.policyIndex
          ? MINT_DECLARED_OUTCOME_NON_CROSSING
          : MINT_DECLARED_OUTCOME_SCANNING,
    });
  return Object.freeze({
    ...cursor,
    accumulatedCount: nextCount,
    itemCursor: quantity.next,
    assetsRemaining: cursor.assetsRemaining - 1,
    policyAssetCursor: cursor.policyAssetCursor + 1,
    previousAsset: Buffer.from(assetName).toString("hex"),
  });
};

/**
 * The step-03 validator's `advance`: spends `budget` work units from
 * `nextItemIndex`, opening policies at `fold_policy_cost` and consuming
 * assets at one unit each. The returned `nextItemIndex` is the walk position
 * after the transaction (unchanged while a policy stays open).
 */
export const advanceMintDeclaredFold = ({
  cursor,
  nextItemIndex,
  items,
  target,
  budget = MINT_DECLARED_ASSET_LIMIT_FOLD_BUDGET,
}: {
  readonly cursor: MintDeclaredFoldCursor;
  readonly nextItemIndex: number;
  readonly items: readonly Uint8Array[];
  readonly target: MintDeclaredFoldTarget;
  readonly budget?: number;
}): Readonly<{ cursor: MintDeclaredFoldCursor; nextItemIndex: number }> => {
  if (
    !Number.isSafeInteger(budget) ||
    budget <= 0 ||
    budget > MINT_DECLARED_ASSET_LIMIT_FOLD_BUDGET
  )
    return fail("fold budget is outside 1..staged_fold_budget");
  let state = cursor;
  let index = nextItemIndex;
  let left = budget;
  for (;;) {
    if (state.outcome !== MINT_DECLARED_OUTCOME_SCANNING) break;
    if (
      state.activePolicy === "" &&
      left < MINT_DECLARED_ASSET_LIMIT_FOLD_POLICY_COST
    )
      break;
    const item = items[index];
    if (item === undefined) return fail("fold walked past field 5");
    let opened = state;
    let remaining = left;
    if (state.activePolicy === "") {
      opened = beginMintDeclaredPolicy({
        cursor: state,
        itemIndex: index,
        item,
        target,
      });
      remaining = left - MINT_DECLARED_ASSET_LIMIT_FOLD_POLICY_COST;
    }
    if (opened.outcome !== MINT_DECLARED_OUTCOME_SCANNING) {
      state = opened;
      break;
    }
    let consumed = opened;
    while (remaining > 0 && consumed.assetsRemaining > 0) {
      consumed = consumeMintDeclaredAsset({
        cursor: consumed,
        itemIndex: index,
        item,
        target,
      });
      remaining -= 1;
    }
    state = consumed;
    left = remaining;
    if (consumed.activePolicy !== "") break;
    index += 1;
  }
  return Object.freeze({ cursor: state, nextItemIndex: index });
};

export type MintDeclaredAssetLimitFoldResult = Readonly<{
  crossing: boolean;
  accumulatedCount: number;
  targetPolicyId: string;
  targetDeclaredCount: number;
}>;

/**
 * Complete deterministic twin of the policy-level machine order. Earlier
 * items must decode fully; the target crossing is decided from its map header
 * before target-body decoding.
 */
export const foldMintDeclaredAssetLimit = (
  items: readonly Uint8Array[],
  policyIndex: number,
): MintDeclaredAssetLimitFoldResult => {
  exactIndex(policyIndex, "policy index");
  const targetItem = items[policyIndex];
  if (targetItem === undefined)
    return fail("policy coordinate is outside field 5");
  const header = decodeMintDeclaredPolicyHeader(targetItem);
  const target: MintDeclaredFoldTarget = {
    policyIndex,
    targetPolicyId: header.policyId.toString("hex"),
    targetDeclaredCount: header.declaredCount,
  };
  let cursor = initialMintDeclaredFoldCursor();
  let nextItemIndex = 0;
  while (cursor.outcome === MINT_DECLARED_OUTCOME_SCANNING) {
    const advanced = advanceMintDeclaredFold({
      cursor,
      nextItemIndex,
      items,
      target,
    });
    cursor = advanced.cursor;
    nextItemIndex = advanced.nextItemIndex;
  }
  return Object.freeze({
    crossing: cursor.outcome === MINT_DECLARED_OUTCOME_CROSSING,
    accumulatedCount: cursor.accumulatedCount,
    targetPolicyId: target.targetPolicyId,
    targetDeclaredCount: target.targetDeclaredCount,
  });
};

export type MintDeclaredAssetLimitEvidence = MintDeclaredAssetLimitFinding &
  MintDeclaredAssetLimitFoldResult &
  Readonly<{
    fieldPreimageHex: string;
    fieldCommitmentHex: string;
    targetItemHex: string;
    carriage: "Inline" | "RawUtxo" | "Certified";
  }>;

export const prepareMintDeclaredAssetLimitEvidence = ({
  finding: rawFinding,
  fieldPreimage,
  committedFieldHashHex,
}: {
  readonly finding: MintDeclaredAssetLimitFinding;
  readonly fieldPreimage: Uint8Array;
  readonly committedFieldHashHex: string;
}): MintDeclaredAssetLimitEvidence => {
  const finding = classifyMintDeclaredAssetLimitFinding(rawFinding);
  if (!/^[0-9a-f]{64}$/u.test(committedFieldHashHex))
    return fail("field commitment is not 32-byte lowercase hex");
  const actual = midgardFieldCommitment(fieldPreimage).toString("hex");
  if (actual !== committedFieldHashHex)
    return fail("retained field-5 bytes do not match the compact commitment");
  const items = decodeMidgardFieldPreimage(fieldPreimage);
  const target = items[finding.policyIndex];
  if (target === undefined) return fail("policy coordinate is outside field 5");
  return Object.freeze({
    ...finding,
    ...foldMintDeclaredAssetLimit(items, finding.policyIndex),
    fieldPreimageHex: Buffer.from(fieldPreimage).toString("hex"),
    fieldCommitmentHex: actual,
    targetItemHex: target.toString("hex"),
    carriage: selectMidgardFieldCarriageTier(fieldPreimage.length),
  });
};

export const mintDeclaredAssetLimitEvidenceCloses = (
  evidence: MintDeclaredAssetLimitEvidence,
): boolean => terminalVerdictContradiction(evidence.subject, evidence.crossing);

export const MintDeclaredAssetLimitVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});

export const MintDeclaredAssetLimitBoundPolicySchema = Data.Object({
  subject: MintDeclaredAssetLimitVerdictSubjectSchema,
  policy_index: Data.Integer(),
});

export const MintDeclaredAssetLimitAuthenticationStateSchema = Data.Enum([
  Data.Object({
    Bound: Data.Object({ bound: MintDeclaredAssetLimitBoundPolicySchema }),
  }),
  Data.Object({
    Grammar: Data.Object({
      bound: MintDeclaredAssetLimitBoundPolicySchema,
      checkpoint_hash: Data.Bytes(),
    }),
  }),
]);

export const MintDeclaredAssetLimitFoldStateSchema = Data.Object({
  subject: MintDeclaredAssetLimitVerdictSubjectSchema,
  policy_index: Data.Integer(),
  target_policy_id: Data.Bytes(),
  target_declared_count: Data.Integer(),
  checkpoint_hash: Data.Bytes(),
  accumulated_count: Data.Integer(),
  previous_policy: Data.Bytes(),
  active_policy: Data.Bytes(),
  item_cursor: Data.Integer(),
  assets_remaining: Data.Integer(),
  policy_asset_cursor: Data.Integer(),
  previous_asset: Data.Bytes(),
  outcome: Data.Integer(),
});

export type MintDeclaredAssetLimitFoldStateData = Readonly<{
  subject: VerdictSubject;
  policy_index: bigint;
  target_policy_id: string;
  target_declared_count: bigint;
  checkpoint_hash: string;
  accumulated_count: bigint;
  previous_policy: string;
  active_policy: string;
  item_cursor: bigint;
  assets_remaining: bigint;
  policy_asset_cursor: bigint;
  previous_asset: string;
  outcome: bigint;
}>;
