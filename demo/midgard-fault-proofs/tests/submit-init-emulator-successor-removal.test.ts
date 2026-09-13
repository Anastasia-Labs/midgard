import { SCHEDULER_ASSET_NAME, SchedulerDatum } from "@al-ft/midgard-sdk";
import {
  Data,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { RegisteredOperatorActivationRequiredError } from "../src/remove-fraudulent-block.js";
import {
  buildProvedDoubleSpendFixture,
  expectStateQueueHeaderOrder,
  type ProvedDoubleSpendFixture,
  submitRemovalForFixture,
} from "./support/submit-init-emulator-fixtures.js";
import { onboardEmulatorOperator } from "./support/submit-init-emulator-shared.js";

/**
 * Removal on a queue whose only active operator is the faulty one: while an
 * eligible successor is merely registered the rewind is illegal and the
 * builder refuses; once the successor is activated the removal appoints it.
 * Active operators sort by key, so a successor below the faulty key becomes
 * the removed node's anchor (`GoToNextDueToOperatorRemoval`) and one above it
 * becomes the surviving tail of a rewind. Both must appoint the successor.
 */
const successorWallet = async (
  fixture: ProvedDoubleSpendFixture,
  polarity: "below" | "above",
) => {
  for (;;) {
    const account = generateEmulatorAccount({ lovelace: 0n });
    const keyHash = paymentCredentialOf(account.address).hash;
    const below = keyHash < fixture.fraudulentHeader.operatorVkey;
    if (below !== (polarity === "below")) continue;
    const funding = await fixture.funderLucid
      .newTx()
      .pay.ToAddress(account.address, { lovelace: 3_000_000_000n })
      .complete();
    await fixture.funderLucid.awaitTx(
      await (await funding.sign.withWallet().complete()).submit(),
    );
    const lucid = await Lucid(fixture.emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    return { lucid, keyHash };
  }
};

const schedulerDatum = async (fixture: ProvedDoubleSpendFixture) => {
  const [scheduler] = await fixture.funderLucid.utxosAtWithUnit(
    fixture.contracts.scheduler.spendingScriptAddress,
    toUnit(fixture.contracts.scheduler.policyId, SCHEDULER_ASSET_NAME),
  );
  if (scheduler === undefined) throw new Error("scheduler is missing");
  return Data.from(scheduler.datum!, SchedulerDatum);
};

describe("removal with a registered or activated successor", () => {
  for (const polarity of ["below", "above"] as const) {
    it(`refuses while the successor (${polarity} the faulty key) is only registered, then appoints it once active`, async () => {
      const fixture = await buildProvedDoubleSpendFixture();
      const successor = await successorWallet(fixture, polarity);
      expect(await schedulerDatum(fixture)).toEqual({
        ActiveOperator: expect.objectContaining({
          operator: fixture.fraudulentHeader.operatorVkey,
        }),
      });
      const { activeNodeUnit } = await onboardEmulatorOperator({
        lucid: successor.lucid,
        contracts: fixture.contracts,
        operatorKeyHash: successor.keyHash,
        registrationSlots: 2,
        awaitActivation: async (activationTime) => {
          // The registered tail's activation time precedes the removal's
          // upper bound, so the last-operator rewind must be refused.
          await expect(submitRemovalForFixture(fixture)).rejects.toBeInstanceOf(
            RegisteredOperatorActivationRequiredError,
          );
          while (
            BigInt(
              successor.lucid.slotToUnixTime(successor.lucid.currentSlot()),
            ) <= activationTime
          )
            fixture.emulator.awaitSlot(1);
        },
      });
      await expect(
        fixture.funderLucid.utxosAtWithUnit(
          fixture.contracts.activeOperators.spendingScriptAddress,
          activeNodeUnit,
        ),
      ).resolves.toHaveLength(1);

      const removeNow = BigInt(fixture.emulator.now());
      const result = await submitRemovalForFixture(fixture);
      expect(
        result.transactions.map(({ kind, slashingApproach }) => ({
          kind,
          slashingApproach,
        })),
      ).toEqual([
        { kind: "remove-target", slashingApproach: "SlashActiveOperator" },
      ]);
      // A lower successor anchors the removed node: GoToNext needs no
      // registered witness. A higher one leaves the root as anchor: the
      // rewind proves the registered list is empty through its root.
      if (polarity === "below")
        expect(result.registeredOperatorsElementOutRef).toBeNull();
      else expect(result.registeredOperatorsElementOutRef).not.toBeNull();

      const validTo = removeNow + 300_000n;
      const { funderLucid } = fixture;
      expect(await schedulerDatum(fixture)).toEqual({
        ActiveOperator: {
          operator: successor.keyHash,
          start_time:
            BigInt(
              funderLucid.slotToUnixTime(
                funderLucid.unixTimeToSlot(Number(validTo)),
              ),
            ) - 1n,
        },
      });
      await expectStateQueueHeaderOrder({
        lucid: funderLucid,
        contracts: fixture.contracts,
        expectedHeaderHashes: [],
      });
      await expect(
        funderLucid.utxosAtWithUnit(
          fixture.contracts.stateQueue.spendingScriptAddress,
          fixture.setup.stateQueueBlockUnit,
        ),
      ).resolves.toHaveLength(0);
      await expect(
        funderLucid.utxosAtWithUnit(
          fixture.contracts.activeOperators.spendingScriptAddress,
          fixture.setup.activeOperatorNodeUnit,
        ),
      ).resolves.toHaveLength(0);
      await expect(
        funderLucid.utxosAtWithUnit(
          fixture.contracts.activeOperators.spendingScriptAddress,
          activeNodeUnit,
        ),
      ).resolves.toHaveLength(1);
    }, 600_000);
  }
});
