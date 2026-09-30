import { type VerdictSubject } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type MintDeclaredAssetLimitFoldStateData,
  MintDeclaredAssetLimitVerdictSubjectSchema,
} from "./family.advance-mint-declared-fold.js";
import {
  type MintDeclaredFoldCursor,
  type MintDeclaredFoldTarget,
} from "./family.begin-mint-declared-policy.js";

/** The wire fields of a step-03 datum for one fold snapshot. */
export const mintDeclaredFoldStateData = ({
  subject,
  target,
  cursor,
  checkpointHash,
}: {
  readonly subject: VerdictSubject;
  readonly target: MintDeclaredFoldTarget;
  readonly cursor: MintDeclaredFoldCursor;
  readonly checkpointHash: string;
}): MintDeclaredAssetLimitFoldStateData =>
  Object.freeze({
    subject,
    policy_index: BigInt(target.policyIndex),
    target_policy_id: target.targetPolicyId,
    target_declared_count: BigInt(target.targetDeclaredCount),
    checkpoint_hash: checkpointHash,
    accumulated_count: BigInt(cursor.accumulatedCount),
    previous_policy: cursor.previousPolicy,
    active_policy: cursor.activePolicy,
    item_cursor: BigInt(cursor.itemCursor),
    assets_remaining: BigInt(cursor.assetsRemaining),
    policy_asset_cursor: BigInt(cursor.policyAssetCursor),
    previous_asset: cursor.previousAsset,
    outcome: BigInt(cursor.outcome),
  });

/** Field-wise equality of two fold datums, the subject aside. */
export const mintDeclaredFoldDataMatches = (
  observed: MintDeclaredAssetLimitFoldStateData,
  expected: MintDeclaredAssetLimitFoldStateData,
): boolean =>
  (Object.keys(expected) as (keyof MintDeclaredAssetLimitFoldStateData)[])
    .filter((key) => key !== "subject")
    .every((key) => observed[key] === expected[key]);

export const MintDeclaredAssetLimitDecisionStateSchema = Data.Object({
  subject: MintDeclaredAssetLimitVerdictSubjectSchema,
  policy_index: Data.Integer(),
  crossing: Data.Boolean(),
});
