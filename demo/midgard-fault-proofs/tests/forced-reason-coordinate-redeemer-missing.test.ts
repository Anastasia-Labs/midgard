import { computeMidgardNativeTxId } from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { describe, expect, it } from "vitest";

import { makeHarness } from "./missing-redeemer-lifecycle.make-harness.js";
import { makeStage } from "./missing-redeemer-lifecycle.make-stage.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import {
  buildMissingRedeemerFixture,
  type MissingRedeemerFixture,
} from "./support/missing-redeemer-emulator.js";

/**
 * A forced RedeemerMissing reason names a purpose by kind and its index in
 * that kind's namespace, and missingRedeemer reopens exactly that purpose's
 * stage-10 selection and scans field 8 for its pointer. The fixture spends
 * two outputs of one script and redeems only the first. The verdict is the
 * one the node's classifier writes, so the suite fails if the writer and the
 * proof disagree on the namespace: one purpose early names the redeemed
 * spend and convicts; the written purpose is refused on chain.
 */

const fixtureAt = (purposeIndex: number) =>
  buildMissingRedeemerFixture({
    direction: "forced",
    purposeKind: 0,
    sourceLocation: "inline",
    unredeemedSecondSpend: true,
    purposeIndex,
  });

const writtenPurposeIndex = async (
  fixture: MissingRedeemerFixture,
): Promise<number> => {
  const forced = materializeMidgardForcedTxFromCanonical(
    fixture.transaction.tx,
  );
  const verdict = await nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(forced),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forced),
    ledger: fixture.ledgerWitnessEntries.map(({ outRef, output }) => [
      outRef,
      output,
    ]),
    programMaterialSidecarCbor: fixture.programMaterialSidecarCbor,
  });
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: {
      reason: { RedeemerMissing: { purpose_kind: 0n, purpose_index: 1n } },
    },
  });
  return 1;
};

/** The fixture committed under `RedeemerMissing { Spend, written + offset }`. */
const fixtureOffsetBy = async (offset: number) =>
  fixtureAt((await writtenPurposeIndex(await fixtureAt(0))) + offset);

describe("forced RedeemerMissing coordinate the node writes", () => {
  it("convicts a coordinate one purpose early, where the spend is redeemed", async () => {
    const fixture = await fixtureOffsetBy(-1);
    expect(fixture.material.evidence.redeemerMissing).toBe(false);
    const stage = await makeStage(await makeHarness(), fixture);
    const final = await stage.finalize(await stage.runToDecision());
    expect(final.fraudProofUnit).toBeTruthy();
    await stage.remove();
  }, 900_000);

  it("refuses the written coordinate on chain", async () => {
    const fixture = await fixtureOffsetBy(0);
    // The written spend carries no redeemer: the rejection holds.
    expect(fixture.material.evidence.redeemerMissing).toBe(true);
    const stage = await makeStage(await makeHarness(), fixture);
    const decision = await stage.runToDecision();
    await expect(stage.finalize(decision)).rejects.toThrow(
      /terminal decision differs/u,
    );
    // The scan found no pointer to the cited spend. Step 05 returns the
    // terminal rule, which convicts a forced rejection only when the pointer
    // is present, so it returns false.
    await expectOnchainRefusal(() => stage.finalizeDirectly(decision), {
      refusedBy: "fraud_proofs/missing_redeemer/step_05",
      check: /^Validator returned false$/u,
    });
  }, 900_000);
});
