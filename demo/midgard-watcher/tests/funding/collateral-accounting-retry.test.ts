/**
 * A failed collateral accounting of the workflow's pending attempt holds
 * nothing: the journal names it once, the next run reads the attempt again
 * and retries, and a read that leaves nothing to account clears it.
 */
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import {
  FraudProofL1CheckpointChangedError,
  FraudProofL1RefusedError,
  FraudProofL1UnavailableError,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import {
  type Fixture,
  forfeitureOf,
  heldOutRefs,
  showLanding,
} from "../support/collateral-landing.js";
import {
  cleanupFundingRecoveryFixtures,
  FixtureProcessStop,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

/** The attempt lands failing phase 2 and the process stops once the
 * forfeiture commits, before the attempt is superseded. A rollback then drops
 * the landing, and reading the collateral fails with `failure`. */
const forfeitPendingAttemptUnreadably = async (
  fixture: Fixture,
  failure: Error = new Error("L1 provider did not answer"),
) => {
  const collateral = fixture.collateralInput.outRef;
  const before = await heldOutRefs(fixture);
  showLanding(fixture, true);
  fixture.chain.mockResolvedValue({ kind: "not_found", landedInvalid: true });
  fixture.stopAt("abandonPendingTransition", "before");
  await expect(fixture.run(await fixture.recover())).rejects.toBeInstanceOf(
    FixtureProcessStop,
  );
  const forfeited = (
    await fixture.journal.load(fixture.initial.workflowId)
  ).map(({ event }) => event);
  expect(forfeited.at(-1)).toEqual(forfeitureOf(fixture));
  await fixture.restartStore();
  const journal = await fixture.recover();
  showLanding(fixture, false);
  const readable = fixture.resolveInputs.getMockImplementation()!;
  fixture.resolveInputs.mockImplementation(async (refs) => {
    if (refs.includes(collateral)) throw failure;
    return await readable(refs);
  });
  return {
    before,
    forfeited,
    journal,
    readable: () => fixture.resolveInputs.mockImplementation(readable),
    events: async () =>
      (await fixture.journal.load(fixture.initial.workflowId)).map(
        ({ event }) => event,
      ),
  };
};

const accountingFailed = (fixture: Fixture) => ({
  kind: "stalled",
  reason: expect.stringMatching(
    new RegExp(
      `^collateral accounting failed for attempt ${fixture.transactionHash}: .*L1 provider did not answer$`,
      "u",
    ),
  ),
});

it("retries a failed reversal of the pending attempt's forfeiture on the next run, naming the failure once and holding nothing", async () => {
  const fixture = await setupFundingRecoveryFixture(
    false,
    true,
    false,
    20n,
    44,
    3,
    100n,
    true,
  );
  const { before, forfeited, journal, readable, events } =
    await forfeitPendingAttemptUnreadably(fixture);
  fixture.chain.mockResolvedValue({
    kind: "pending",
    txHash: fixture.transactionHash,
  });
  // The attempt is still pending: the run waits on it, not on the failure.
  await expect(fixture.run(journal)).resolves.toMatchObject({
    kind: "pending",
    reason: "transaction for init is pending",
  });
  const failed = await events();
  expect(failed.slice(forfeited.length)).toEqual([accountingFailed(fixture)]);
  // The next run reads the attempt again; the same failure is not named again.
  await expect(fixture.run(journal)).resolves.toMatchObject({
    kind: "pending",
  });
  expect(await events()).toEqual(failed);

  readable();
  await fixture.run(journal);
  expect((await events()).slice(failed.length)).toEqual([
    {
      kind: "collateral_forfeiture_reversed",
      actionId: "init",
      txHash: fixture.transactionHash,
    },
  ]);
  expect(await heldOutRefs(fixture)).toEqual(before);
  expect(fixture.adapter.submit).not.toHaveBeenCalled();
});

it("clears a failed reversal once a read shows the attempt landed failing phase 2 again", async () => {
  const fixture = await setupFundingRecoveryFixture(
    false,
    true,
    false,
    20n,
    44,
    3,
    100n,
    true,
  );
  const { forfeited, journal, events } =
    await forfeitPendingAttemptUnreadably(fixture);
  fixture.chain.mockResolvedValue({
    kind: "pending",
    txHash: fixture.transactionHash,
  });
  await expect(fixture.run(journal)).resolves.toMatchObject({
    kind: "pending",
  });
  expect((await events()).slice(forfeited.length)).toEqual([
    accountingFailed(fixture),
  ]);

  // The attempt lands failing phase 2 again: the forfeiture stands, nothing
  // is left to reverse, and the attempt is superseded.
  showLanding(fixture, true);
  fixture.chain.mockResolvedValue({ kind: "not_found", landedInvalid: true });
  await fixture.run(journal);
  expect((await events()).slice(forfeited.length + 1)).toEqual([
    {
      kind: "stalled",
      reason: `collateral accounting no longer needed for attempt ${fixture.transactionHash}`,
    },
    {
      kind: "reconciled",
      actionId: "init",
      outcome: "not_found",
      txHash: fixture.transactionHash,
    },
  ]);
});

const withCollateralReturn = () =>
  setupFundingRecoveryFixture(false, true, false, 20n, 44, 3, 100n, true);

it.each([
  ["the L1 source cannot answer", new FraudProofL1UnavailableError("down")],
  [
    "the L1 source refuses the read",
    new FraudProofL1RefusedError("pruned", "x"),
  ],
  [
    "the chain moved under the read",
    new FraudProofL1CheckpointChangedError("moved"),
  ],
])(
  "hands the run's caller a reversal that failed because %s, naming no accounting failure",
  async (_, failure) => {
    const fixture = await withCollateralReturn();
    const { forfeited, journal, readable, events } =
      await forfeitPendingAttemptUnreadably(fixture, failure);
    fixture.chain.mockResolvedValue({
      kind: "pending",
      txHash: fixture.transactionHash,
    });
    await expect(fixture.run(journal)).rejects.toBe(failure);
    expect(await events()).toEqual(forfeited);

    readable();
    await fixture.run(journal);
    expect((await events()).slice(forfeited.length)).toEqual([
      {
        kind: "collateral_forfeiture_reversed",
        actionId: "init",
        txHash: fixture.transactionHash,
      },
    ]);
  },
);

it("hands the run's caller a reversal that met a busy database, after a failure it already named", async () => {
  const fixture = await withCollateralReturn();
  const { forfeited, journal, readable, events } =
    await forfeitPendingAttemptUnreadably(fixture);
  fixture.chain.mockResolvedValue({
    kind: "pending",
    txHash: fixture.transactionHash,
  });
  await fixture.run(journal);
  const failed = await events();
  expect(failed.slice(forfeited.length)).toEqual([accountingFailed(fixture)]);

  // Another connection holds the write lock past the busy timeout.
  readable();
  const holder = new DatabaseSync(join(fixture.journalRoot, "watcher.sqlite"));
  holder.exec("BEGIN IMMEDIATE");
  try {
    await expect(fixture.run(journal)).rejects.toMatchObject({
      code: "ERR_SQLITE_ERROR",
      errcode: 5,
    });
  } finally {
    holder.exec("ROLLBACK");
    holder.close();
  }
  expect(await events()).toEqual(failed);

  await fixture.run(journal);
  expect((await events()).slice(failed.length)).toEqual([
    {
      kind: "collateral_forfeiture_reversed",
      actionId: "init",
      txHash: fixture.transactionHash,
    },
  ]);
});
