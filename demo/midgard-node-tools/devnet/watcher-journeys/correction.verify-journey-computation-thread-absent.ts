import { type FraudProofWorkflowJournalEntry } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { expect } from "vitest";

import { journeyIntentRecovery } from "./correction.verify-journey-corrected-scheduler.js";

/**
 * Compare the exact transactions the workflow confirmed against the last
 * intent journaled per action, excluding exact intents durably abandoned even
 * when a replacement binds a different action identity.
 */
export const journeyLatestIntentsByAction = (
  records: readonly FraudProofWorkflowJournalEntry[],
) => {
  return [...journeyIntentRecovery(records).latest.values()]
    .filter(({ outcome }) => outcome !== "not_found")
    .map(({ txHash }) => txHash);
};

/** Recovering an included attempt need not have emitted a confirmed event. */
export const verifyJourneyWorkflowTransactions = async (
  records: readonly FraudProofWorkflowJournalEntry[],
  authenticate: (txHash: string) => Promise<unknown>,
) => {
  const intents = journeyLatestIntentsByAction(records);
  const submitted = new Set(
    records.flatMap(({ event }) =>
      event.kind === "submission_intent" ? [event.txHash] : [],
    ),
  );
  for (const { event } of records)
    if (event.kind === "confirmed")
      expect(submitted.has(event.txHash)).toBe(true);
  for (const txHash of intents) await authenticate(txHash);
};

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
