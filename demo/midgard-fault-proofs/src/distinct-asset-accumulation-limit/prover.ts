/** Plans a whole `distinctAssetAccumulationLimit` proof from the bound source
 * and predicts its conviction before any transaction is spent. An honest leaf
 * is refused.
 *
 * The fold counts asset entries in the producer's order: spent inputs at
 * their schedule positions, then outputs, then mint policies. An accepted
 * leaf, normal or forced, convicts on a crossing. A rejection recording an
 * accumulation-limit reason convicts unless it names the derived crossing; a
 * rejection recording another reason convicts when a crossing holds and that
 * reason ranks after it. */
import * as SDK from "@al-ft/midgard-sdk";

import { directPurposeBindTransitions } from "../direct-purpose-family/source-walk.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL,
  distinctAssetAccumulationLimitContractRoles,
  type DistinctAssetAccumulationLimitContracts,
} from "./contracts.js";
import {
  distinctAssetAccumulationLimitFieldPlans,
  type DistinctAssetAccumulationLimitTransition,
  sharedBindTransition,
} from "./custody.js";
import { distinctAssetUnits } from "./fold.js";
import {
  type DistinctAssetAccumulationLimitWalk,
  type DistinctAssetAccumulationLimitWalkBatch,
  distinctAssetAccumulationLimitWalkTransitions,
} from "./walk.js";

export class DistinctAssetAccumulationLimitHonestLeaf extends Error {
  constructor(message: string) {
    super(`${DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL}: ${message}`);
    this.name = "DistinctAssetAccumulationLimitHonestLeaf";
  }
}

const verdict = (
  material: DirectScriptMaterial,
  crossing: SDK.ValueFaultPosition | null,
) => ({
  crossing,
  convicts: SDK.distinctAssetAccumulationLimitConvicts(
    material.bound.subject,
    material.bound.network_order_context,
    crossing,
  ),
});

export type DistinctAssetAccumulationLimitProofPlan = {
  readonly plan: ReturnType<typeof distinctAssetAccumulationLimitFieldPlans>;
  /** Every custody transition from `bindEvent` to the terminal's input. */
  readonly transitions: readonly DistinctAssetAccumulationLimitTransition[];
  /** The terminal datum the fold writes. */
  readonly final: string;
  /** The fold, paged from any thread datum it holds. */
  readonly walk: DistinctAssetAccumulationLimitWalk;
  readonly crossing: SDK.ValueFaultPosition | null;
  readonly convicts: boolean;
};

export const prepareDistinctAssetAccumulationLimitProof = async ({
  material,
  contracts,
  prover,
  batch,
}: {
  readonly material: DirectScriptMaterial;
  readonly contracts: DistinctAssetAccumulationLimitContracts;
  readonly prover: string;
  readonly batch?: DistinctAssetAccumulationLimitWalkBatch;
}): Promise<DistinctAssetAccumulationLimitProofPlan> => {
  const roles = distinctAssetAccumulationLimitContractRoles(contracts);
  const plan = distinctAssetAccumulationLimitFieldPlans(material, prover);
  const bind = directPurposeBindTransitions(material, prover).map(
    sharedBindTransition,
  );
  const walk = await distinctAssetAccumulationLimitWalkTransitions({
    material,
    prover,
    plan,
    inputsScriptHash: roles.inputs.spendingScriptHash,
    ...(batch === undefined ? {} : { batch }),
  });
  return {
    plan,
    transitions: [...bind, ...walk.transitions],
    final: walk.final,
    walk,
    ...verdict(material, walk.crossing),
  };
};

/** The plan, refused before spending when the completed fold cannot
 * convict. */
export const requireConvictingDistinctAssetAccumulationLimitProof = async (
  params: Parameters<typeof prepareDistinctAssetAccumulationLimitProof>[0],
): Promise<DistinctAssetAccumulationLimitProofPlan> => {
  const prepared = await prepareDistinctAssetAccumulationLimitProof(params);
  if (!prepared.convicts)
    throw new DistinctAssetAccumulationLimitHonestLeaf(
      prepared.crossing === null
        ? "no asset entry exceeds the accumulation limit; nothing to prove"
        : "the leaf records the crossing or an earlier reason; nothing to prove",
    );
  return prepared;
};

/** Whether the bound leaf convicts, without planning custody; `null` when it
 * does not, or when a spent input has no descriptor in the event pre-state
 * and an earlier phase is the first fault. */
export const distinctAssetAccumulationLimitDetection = (
  material: DirectScriptMaterial,
): { readonly crossing: SDK.ValueFaultPosition | null } | null => {
  const units = distinctAssetUnits(material);
  if (units === null) return null;
  const result = verdict(material, SDK.distinctAssetCrossing(units));
  return result.convicts ? { crossing: result.crossing } : null;
};
