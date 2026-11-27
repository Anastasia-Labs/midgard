import { USER_ROLES, type UserRole } from "./identities.js";
import type { Journey } from "./journey.js";
import { FatalJourneyError } from "./journey-cli.js";
import {
  drainResidue,
  expectedHoldings,
  jsonValue,
  type L2Utxo,
  sameValue,
  type Value,
} from "./journey-values.js";

const byLovelace = (utxos: readonly L2Utxo[]) =>
  [...utxos].sort((left, right) =>
    (right.value.lovelace ?? 0n) > (left.value.lovelace ?? 0n) ? 1 : -1,
  );

/** The scripted scenario. Every phase resumes from the journal. */
export const runScenario = async (
  journey: Journey,
  options: { idleMs: number },
) => {
  const v = journey.value.bind(journey);
  const holding = (label: string) => (utxos: readonly L2Utxo[]) =>
    byLovelace(
      utxos.filter((utxo) => (utxo.value[journey.unit(label)] ?? 0n) > 0n),
    )[0]?.outRef;
  const adaOnly = (utxos: readonly L2Utxo[]) =>
    byLovelace(utxos.filter((utxo) => Object.keys(utxo.value).length === 1))[0]
      ?.outRef;
  const largest = (utxos: readonly L2Utxo[]) => byLovelace(utxos)[0]?.outRef;

  await journey.phase("deposits-concurrent", async () => {
    await Promise.all([
      journey.deposit(
        "A1",
        "userA",
        v(2_000n, { tALPHA: 10_000n, tBETA: 5_000n }),
      ),
      journey.deposit("B1", "userB", v(1_500n, { tGAMMA: 8_000n })),
      journey.deposit("C1", "userC", v(1_000n)),
    ]);
  });
  await journey.phase("deposits-repeat", async () => {
    await Promise.all([
      journey.deposit("A2", "userA", v(500n, { tGAMMA: 3_000n })),
      journey.deposit("B2", "userB", v(400n, { tALPHA: 2_000n })),
    ]);
  });
  await journey.phase("deposits-credited", async () => {
    for (const deposit of journey.deposits())
      await journey.waitCredited(deposit);
  });
  await journey.phase("transfers-concurrent", async () => {
    const sent = await Promise.all([
      journey.transfer("T1", "userA", "userB", v(100n, { tALPHA: 1_000n })),
      journey.transfer("T2", "userB", "userC", v(50n, { tGAMMA: 500n })),
      journey.transfer("T3", "userC", "userA", v(30n)),
    ]);
    for (const transfer of sent)
      await journey.waitTransfer(transfer, "committed");
  });
  await journey.phase("transfers-sequential", async () => {
    // Each spends what the one before produced, as soon as it is accepted.
    const chain: [string, UserRole, UserRole, Value][] = [
      ["T4", "userA", "userC", v(20n, { tBETA: 300n })],
      ["T5", "userC", "userB", v(10n, { tBETA: 100n })],
      ["T6", "userB", "userA", v(5n, { tGAMMA: 50n })],
    ];
    for (const [id, from, to, amount] of chain) {
      const transfer = await journey.transfer(id, from, to, amount);
      await journey.waitTransfer(transfer, "accepted");
    }
    for (const transfer of journey.transfers())
      await journey.waitTransfer(transfer, "committed");
  });
  await journey.phase("idle", async () => {
    // A rerun idles only for what is left of the journaled period.
    let started = journey.journal.get<number>("idle:startedAt");
    if (started === undefined) {
      started = Date.now();
      journey.journal.set("idle:startedAt", started);
    }
    const remaining = Math.max(0, started + options.idleMs - Date.now());
    journey.log(
      `idle for ${Math.round(remaining / 1000)} s with no new activity`,
    );
    await journey.wait(remaining);
    await journey.waitReady();
  });
  await journey.phase("withdrawals-concurrent", async () => {
    await Promise.all([
      journey.withdraw("W1", "userA", 1, holding("tALPHA")),
      journey.withdraw("W2", "userB", 1, holding("tGAMMA")),
      journey.withdraw("W3", "userC", 1, adaOnly),
    ]);
  });
  await journey.phase("activity-after-withdrawals", async () => {
    await journey.withdraw("W4", "userA", 2, largest);
    await journey.deposit("B3", "userB", v(250n, { tBETA: 1_000n }));
    const transfer = await journey.transfer("T7", "userC", "userB", v(5n));
    await journey.waitTransfer(transfer, "committed");
  });
  await journey.phase("final-drain", async () => {
    for (const deposit of journey.deposits())
      await journey.waitCredited(deposit);
    for (const withdrawal of journey.withdrawals())
      await journey.waitPaidOut(withdrawal);
    await journey.until(
      "the node to drain all pending work",
      journey.deadlines.drainMs,
      async () => {
        const status = await journey.pipeline();
        // Thrown as "not yet", so the wait's reports and deadline name it.
        const residue = drainResidue(status);
        if (residue.length > 0)
          throw new Error(`not drained: ${residue.join("; ")}`);
        return status;
      },
    );
  });
  await journey.phase("holdings", async () => {
    const { holdings, feeBound } = expectedHoldings(
      journey.deposits(),
      journey.transfers(),
      journey.withdrawals(),
    );
    const problems: string[] = [];
    for (const user of USER_ROLES) {
      const actual = await journey.l2Totals(user);
      const { lovelace: actualAda = 0n, ...actualTokens } = actual;
      const { lovelace: expectedAda = 0n, ...expectedTokens } = holdings[user];
      if (!sameValue(actualTokens, expectedTokens))
        problems.push(
          `${user} tokens ${JSON.stringify(jsonValue(actualTokens))} != ${JSON.stringify(jsonValue(expectedTokens))}`,
        );
      if (actualAda > expectedAda || actualAda < expectedAda - feeBound[user])
        problems.push(
          `${user} lovelace ${actualAda} outside [${expectedAda - feeBound[user]}, ${expectedAda}]`,
        );
      journey.log(`${user} L2 holdings ${JSON.stringify(jsonValue(actual))}`);
    }
    if (problems.length > 0) throw new FatalJourneyError(problems.join("; "));
  });
};
