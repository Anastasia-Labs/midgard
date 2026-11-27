import * as SDK from "@al-ft/midgard-sdk/core";
import { expect } from "vitest";

/** A correction retires its own computation thread while other proofs may continue. */
export const verifyJourneyComputationThreadAbsent = async ({
  kupoUrl,
  computationThreadPolicyId,
  category,
  headerHash,
}: {
  kupoUrl: string;
  computationThreadPolicyId: string;
  category: SDK.FraudProofCatalogueCategoryName;
  headerHash: string;
}) => {
  const assetName =
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category] + headerHash;
  const threadResponse = await fetch(
    `${kupoUrl}/matches/${computationThreadPolicyId}.${assetName}?unspent`,
  );
  expect(threadResponse.ok).toBe(true);
  expect(await threadResponse.json()).toEqual([]);
};
