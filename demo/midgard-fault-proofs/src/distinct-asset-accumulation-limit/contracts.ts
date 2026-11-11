/** The `distinctAssetAccumulationLimit` direct walk family the submitters
 * and prover read. */
import * as SDK from "@al-ft/midgard-sdk";

import type {
  DirectPurposeContracts,
  DirectPurposeFamily,
} from "../direct-purpose-family/family.js";

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL =
  "distinct-asset-accumulation-limit";

/** Named roles; fails closed when the chain is not the 12-role graph. */
export const distinctAssetAccumulationLimitContractRoles = (
  contracts: DirectPurposeContracts,
): SDK.DistinctAssetAccumulationLimitRoles =>
  SDK.distinctAssetAccumulationLimitRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY: DirectPurposeFamily<SDK.DistinctAssetTerminal> =
  {
    label: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL,
    roleOrder: SDK.DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER,
    roles: distinctAssetAccumulationLimitContractRoles,
    verdict: {
      schema: SDK.DistinctAssetTerminalDatum,
      convicts: (state) =>
        SDK.distinctAssetAccumulationLimitConvicts(
          state.bound.subject,
          state.bound.network_order_context,
          SDK.distinctAssetCrossingPosition(state.crossing),
        ),
    },
  };

export type DistinctAssetAccumulationLimitRole =
  SDK.DistinctAssetAccumulationLimitRole;
