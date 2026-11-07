/** The installed distinct-asset accumulation replay next to the item
 * canonicality families. An accepted leaf whose output or mint item is not in
 * the shape the accumulation fold reads has an earlier first fault: the
 * accumulation member finds nothing in it, and the union still returns the
 * canonicality family's finding for the block. */
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  type CompleteCanonicalReplay,
  createCompleteCanonicalReplayUnion,
  DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
  MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  type FixtureTransactionInput,
} from "./helpers/canonical-block-evidence-fixture.js";

const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/distinct-asset-accumulation-limit-replay-union",
  grade: "security",
} as const;

/** One accepted transaction with no inputs, committed at step 0 over the
 * empty pre-state. */
const blockOf = async (
  input: Omit<FixtureTransactionInput, "spendInputs" | "fee">,
) => {
  const tx = buildFixtureTransaction({ ...input, spendInputs: [], fee: 1n });
  const fixture = await buildCanonicalBlockFixture({
    transactions: [tx],
    transitionSteps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: { L2TransactionEventKey: { tx_id: tx.txId } },
        phase: "L2Transaction",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    ],
  });
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance,
  });
};

const violations = async (
  replayer: CompleteCanonicalReplay,
  input: Omit<FixtureTransactionInput, "spendInputs" | "fee">,
) =>
  (await replayer.replay(await blockOf(input), {})).detections.map(
    (detection) => detection.violationId,
  );

const DAAL = DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY;

describe("distinctAssetAccumulationLimit replay over an item it cannot read", () => {
  // Union members in catalogue order.
  it.each([
    [
      "an output",
      [TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY, DAAL],
      { outputs: [Buffer.alloc(64)] },
      "transaction-output-non-canonical",
    ],
    [
      "a mint item",
      [DAAL, MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY],
      { mintPolicyItems: [Buffer.from("00", "hex")] },
      "mint-item-non-canonical",
    ],
  ] as const)(
    "finds nothing in a malformed accepted %s and leaves the union its canonicality finding",
    async (_label, members, input, violationId) => {
      await expect(violations(DAAL, input)).resolves.toEqual([]);
      await expect(
        violations(createCompleteCanonicalReplayUnion([...members]), input),
      ).resolves.toEqual([violationId]);
    },
  );
});
