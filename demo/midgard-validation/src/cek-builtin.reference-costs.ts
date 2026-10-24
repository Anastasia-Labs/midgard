/**
 * The builtin cost functions the pinned reference evaluator is constructed
 * with: HarmonicLabs' Plutus V3 costing over the pinned cost model. The
 * evaluator charges them to a budget nothing reads; Midgard's budgets come
 * from `computeMidgardCekBuiltinBudget` (`cek-cost.ts`).
 */
import {
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW_LENGTH,
} from "@al-ft/midgard-core";
import { toCostModelV3 } from "@harmoniclabs/cardano-costmodels-ts";
import { costModelV3ToBuiltinCosts } from "@harmoniclabs/plutus-machine";

if (
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.length !==
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW_LENGTH
) {
  throw new Error("V1 pinned Plutus V3 cost model must have 350 entries");
}

export const MIDGARD_CEK_REFERENCE_BUILTIN_COSTS = costModelV3ToBuiltinCosts(
  toCostModelV3([
    ...PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
  ] as unknown as Parameters<typeof toCostModelV3>[0]),
);
