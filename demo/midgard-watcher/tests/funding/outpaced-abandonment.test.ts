import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { beginWorkflowFundingReservationAction } from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import {
  type Fixture,
  fundingOf,
  recordReplacement,
  signReplacement,
} from "./superseded-attempt-replacement.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

/** Clears the journal acknowledgement of the fixture's abandoned attempt, as
 * the rule that hid such rows under a newer pending transition left it. */
const clearAcknowledgement = (fixture: Fixture) => {
  const database = new DatabaseSync(
    join(fixture.journalRoot, "watcher.sqlite"),
  );
  try {
    const rows = database
      .prepare(
        "SELECT canonical_json FROM watcher_prover_funding_abandonment_v1 WHERE reservation_id = ?",
      )
      .all(fixture.plan.reservationId) as { canonical_json: string }[];
    expect(
      rows.map(
        (row) =>
          (
            JSON.parse(row.canonical_json) as {
              transition: { transactionHash: string };
            }
          ).transition.transactionHash,
      ),
    ).toEqual([fixture.transactionHash]);
    database
      .prepare(
        "UPDATE watcher_prover_funding_abandonment_v1 SET acknowledged_revision = NULL WHERE reservation_id = ?",
      )
      .run(fixture.plan.reservationId);
  } finally {
    database.close();
  }
};

const acknowledgedRevisions = (fixture: Fixture) => {
  const database = new DatabaseSync(
    join(fixture.journalRoot, "watcher.sqlite"),
  );
  try {
    return (
      database
        .prepare(
          "SELECT acknowledged_revision FROM watcher_prover_funding_abandonment_v1 WHERE reservation_id = ?",
        )
        .all(fixture.plan.reservationId) as {
        acknowledged_revision: string | null;
      }[]
    ).map((row) => row.acknowledged_revision);
  } finally {
    database.close();
  }
};

/** The fixture's attempt expires at the tip and is superseded. */
const supersedeAtTip = async (fixture: Fixture) => {
  fixture.useUnspentPendingInputs();
  vi.mocked(fixture.adapter.reconcile).mockResolvedValue({ kind: "not_found" });
  const journal = await fixture.recover();
  await fixture.run(journal);
  return journal;
};

it("treats an unacknowledged superseded row that a later confirmed submission outpaced as acknowledged, so the workflow goes on", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const funding = fundingOf(fixture);
  let journal = await supersedeAtTip(fixture);
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: "init", input: { actionKind: "proof.init" } },
  });
  const replacement = signReplacement(
    funding.outRef,
    BigInt(funding.lovelace),
    300n,
    (await fixture.records())[0]!.activeInputs
      .filter(({ role }) => role === "collateral")
      .map(({ outRef }) => outRef),
  );
  await recordReplacement(fixture, journal, replacement, 2);
  journal = await fixture.recover();
  vi.mocked(fixture.adapter.reconcile).mockImplementation(async ({ txHash }) =>
    txHash === replacement.transactionHash
      ? { kind: "confirmed", txHash }
      : { kind: "not_found" },
  );
  await fixture.run(journal);
  expect(
    (await journal.load(fixture.initial.workflowId)).at(-1)!.event,
  ).toEqual({
    kind: "confirmed",
    actionId: "init",
    txHash: replacement.transactionHash,
  });
  expect((await fixture.records())[0]!.pendingTransition).toBeNull();

  // The legacy state: the superseded row was never acknowledged, the
  // replacement's handoff and journal entries follow it, and nothing is
  // pending, so the row would resurface against an unrelated journal suffix.
  clearAcknowledgement(fixture);
  await fixture.restartStore();
  journal = await fixture.recover();
  const [record] = await fixture.records();
  expect(acknowledgedRevisions(fixture)).toEqual([record!.revision]);
  expect(
    await fixture.store.readAbandonmentHandoff({
      reservationId: fixture.plan.reservationId,
    }),
  ).toBeNull();
  // It stays a superseded attempt, covered by the replacement that spent
  // its funding input.
  expect(
    await fixture.store.readLegacyAbandonedTransactions!({
      reservationId: fixture.plan.reservationId,
    }),
  ).toMatchObject([
    { transition: { transactionHash: fixture.transactionHash } },
  ]);
  expect(
    await fixture.store.readSupersededAttemptFundingOutRefs!({
      reservationId: fixture.plan.reservationId,
    }),
  ).toEqual([]);

  // The workflow goes on to its next action.
  await expect(fixture.run(journal)).resolves.not.toMatchObject({
    kind: "stalled",
  });
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: "next", input: { actionKind: "step-one" } },
  });
  expect((await fixture.records())[0]!.activeInputs.length).toBeGreaterThan(0);
});

it("leaves a superseded row that nothing outpaced awaiting its journal acknowledgement", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  await supersedeAtTip(fixture);
  const before = acknowledgedRevisions(fixture);
  expect(before).toHaveLength(1);
  expect(before[0]).not.toBeNull();

  clearAcknowledgement(fixture);
  await fixture.restartStore();
  expect(acknowledgedRevisions(fixture)).toEqual([null]);
  expect(
    await fixture.store.readAbandonmentHandoff({
      reservationId: fixture.plan.reservationId,
    }),
  ).not.toBeNull();
  // The workflow acknowledges it from its own journal, as it would have.
  await fixture.run(await fixture.recover());
  const [record] = await fixture.records();
  expect(acknowledgedRevisions(fixture)).toEqual([record!.revision]);
});
