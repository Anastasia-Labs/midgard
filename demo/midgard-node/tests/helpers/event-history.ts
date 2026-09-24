import { readFileSync } from "node:fs";

import * as SDK from "@al-ft/midgard-sdk";

const blueprint = SDK.parseFaultProofBlueprint(
  JSON.parse(
    readFileSync(
      process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
        new URL("../../../../onchain/aiken/plutus.json", import.meta.url),
      "utf8",
    ),
  ),
);

/** Manifest shape fixtures retain unrelated stand-ins but carry real list
 * recipes and scripts. They do not prove deployed protocol behavior. */
export const withRealEventHistoryForTest = (
  contracts: SDK.MidgardValidators,
  nonce: { txHash: string; outputIndex: number },
): SDK.MidgardValidators => {
  const eventHistory = SDK.buildEventHistoryDeployments({
    blueprint,
    network: "Preprod",
    hubOraclePolicyId: contracts.hubOracle.policyId,
    initializationNonce: nonce,
    protectionDurationMs: 2_000n,
    bounds: {
      inlineLimitBytes: 512n,
      maxPayloadBytes: 5000n,
      maxPayloadNodes: 512n,
    },
  });
  return {
    ...contracts,
    eventHistory,
    deposit: eventHistory.deposit.list,
    withdrawal: eventHistory.withdrawal.list,
  };
};
