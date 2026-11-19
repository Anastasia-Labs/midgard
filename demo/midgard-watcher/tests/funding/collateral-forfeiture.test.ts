/**
 * A transaction takes its collateral only when it lands failing phase 2,
 * which means local evaluation disagreed with the chain. The workflow
 * accounts for it at the tip: the journal records the collateral it took,
 * the reservation drops that coin and holds the collateral return instead,
 * and the objective raises `prover_collateral_forfeited`. A rollback that
 * drops the landing undoes all of it.
 */
import { beginWorkflowFundingReservationAction } from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import {
  forfeitureOf,
  heldOutRefs,
  showLanding,
} from "../support/collateral-landing.js";
import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";
import { FixtureProcessStop } from "../support/fault-proof-funding-fixture.process-stop.js";
import { setupFundingRecoveryFixture } from "../support/fault-proof-funding-fixture.setup-funding-recovery-fixture.js";
import { cleanupFundingRecoveryFixtures } from "../support/fault-proof-funding-fixture.sources-for.js";

afterEach(async () => {
  vi.useRealTimers();
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

it("swaps the collateral for its return in the reservation when the attempt lands failing phase 2, and swaps back when a rollback drops the landing", async () => {
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
  const collateral = fixture.collateralInput.outRef;
  const back = fixture.collateralReturn!;
  const before = await heldOutRefs(fixture);
  expect(before).toContain(collateral);
  expect(before).not.toContain(back.outRef);

  showLanding(fixture, true);
  fixture.chain.mockResolvedValue({ kind: "not_found", landedInvalid: true });
  // The process stops once the forfeiture commits, before the attempt is
  // superseded, so the reservation shows the forfeiture alone.
  fixture.stopAt("abandonPendingTransition", "before");
  await expect(fixture.run(await fixture.recover())).rejects.toBeInstanceOf(
    FixtureProcessStop,
  );
  const forfeited = await fixture.journal.load(fixture.initial.workflowId);
  expect(forfeited.at(-1)!.event).toEqual(forfeitureOf(fixture));
  const [record] = await fixture.records();
  expect(record!.pendingTransition?.transactionHash).toBe(
    fixture.transactionHash,
  );
  expect(await heldOutRefs(fixture)).toEqual(
    [...before.filter((outRef) => outRef !== collateral), back.outRef].sort(),
  );
  expect(
    record!.activeInputs.find(({ outRef }) => outRef === back.outRef),
  ).toEqual({ outRef: back.outRef, lovelace: back.lovelace, assets: [] });

  // A rollback drops the landing before the restarted process reads the
  // attempt again: its collateral is unspent, its return gone, and the exact
  // bytes wait in the mempool.
  await fixture.restartStore();
  showLanding(fixture, false);
  fixture.chain.mockResolvedValue({
    kind: "pending",
    txHash: fixture.transactionHash,
  });
  await fixture.run(await fixture.recover());
  const reversed = await fixture.journal.load(fixture.initial.workflowId);
  expect(reversed.slice(0, forfeited.length)).toEqual(forfeited);
  expect(reversed.slice(forfeited.length).map(({ event }) => event)).toEqual([
    {
      kind: "collateral_forfeiture_reversed",
      actionId: "init",
      txHash: fixture.transactionHash,
    },
  ]);
  expect(await heldOutRefs(fixture)).toEqual(before);
  expect((await fixture.records())[0]!.pendingTransition?.transactionHash).toBe(
    fixture.transactionHash,
  );
  expect(fixture.adapter.submit).not.toHaveBeenCalled();
});

it("raises prover_collateral_forfeited for an attempt that landed failing phase 2, until a rollback drops the landing", async () => {
  vi.useFakeTimers({ toFake: ["Date"] });
  const test = await setupFaultProofExecution(false, {
    collateralReturn: true,
  });
  const { fixture } = test;
  test.getUtxosByOutRef.mockImplementation(async (...args: unknown[]) => {
    const refs = (
      args[0] as readonly { txHash: string; outputIndex: number }[]
    ).map(({ txHash, outputIndex }) => `${txHash}#${outputIndex}`);
    return fixture.walletUtxos.filter(({ txHash, outputIndex }) =>
      refs.includes(`${txHash}#${outputIndex}`),
    );
  });
  const objective = `doubleSpend/${test.input.job.headerHash}`;
  const forfeitureAlerts = () =>
    test.setAlert.mock.calls
      .map(
        ([alert]) =>
          alert as { code: string; subjectDigest: string; active: boolean },
      )
      .filter(({ code }) => code === "prover_collateral_forfeited");

  showLanding(fixture, true);
  fixture.chain.mockResolvedValue({ kind: "not_found", landedInvalid: true });
  const logged: string[] = [];
  const stderr = vi
    .spyOn(process.stderr, "write")
    .mockImplementation((line) => logged.push(String(line)) > 0);
  try {
    expect(await test.execution.execute(test.input)).toMatchObject({
      kind: "pending",
    });
  } finally {
    stderr.mockRestore();
  }
  const events = (await fixture.journal.load(fixture.initial.workflowId)).map(
    ({ event }) => event,
  );
  expect(events.slice(fixture.originalEntries.length)).toEqual([
    forfeitureOf(fixture),
    {
      kind: "reconciled",
      actionId: "init",
      outcome: "not_found",
      txHash: fixture.transactionHash,
    },
  ]);
  expect(forfeitureAlerts()).toEqual([
    {
      code: "prover_collateral_forfeited",
      subjectDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
      active: true,
      observedAtMs: expect.any(String),
    },
  ]);
  // One error line names the objective, the attempt and its collateral.
  expect(logged).toHaveLength(1);
  expect(JSON.parse(logged[0]!)).toMatchObject({
    level: "error",
    reason: "prover_collateral_forfeited",
    detail: expect.stringContaining(objective),
  });
  expect(logged[0]).toContain(fixture.transactionHash);
  expect(logged[0]).toContain(fixture.collateralInput.outRef);
  // Another run while the landing stands raises nothing again.
  expect(await test.execution.execute(test.input)).toMatchObject({
    kind: "pending",
  });
  expect(forfeitureAlerts()).toHaveLength(1);

  // A rollback drops the landing; the superseded attempt is absent with every
  // input live, so the next read of it finds the forfeiture undone.
  showLanding(fixture, false);
  fixture.chain.mockResolvedValue({
    kind: "unknown",
    reason: "Exact recorded transaction requires live rebroadcast authority",
    absent: true,
  });
  vi.setSystemTime(Date.now() + 15 * 60_000);
  expect(await test.execution.execute(test.input)).toMatchObject({
    kind: "pending",
  });
  expect(
    (await fixture.journal.load(fixture.initial.workflowId))
      .map(({ event }) => event)
      .slice(events.length),
  ).toEqual([
    {
      kind: "collateral_forfeiture_reversed",
      actionId: "init",
      txHash: fixture.transactionHash,
    },
  ]);
  const [raised, cleared] = forfeitureAlerts();
  expect(cleared).toMatchObject({
    code: "prover_collateral_forfeited",
    active: false,
  });
  expect(cleared).toHaveProperty("subjectDigest", raised!.subjectDigest);
});

it("reads a superseded attempt again when its collateral accounting fails, naming the failure once and holding nothing, until the accounting succeeds", async () => {
  vi.useFakeTimers({ toFake: ["Date"] });
  const test = await setupFaultProofExecution(false, {
    collateralReturn: true,
  });
  const { fixture } = test;
  const collateral = fixture.collateralInput.outRef;
  // The provider cannot answer for the collateral while `unreadable` holds.
  let unreadable = false;
  test.getUtxosByOutRef.mockImplementation(async (...args: unknown[]) => {
    const refs = (
      args[0] as readonly { txHash: string; outputIndex: number }[]
    ).map(({ txHash, outputIndex }) => `${txHash}#${outputIndex}`);
    if (unreadable && refs.includes(collateral))
      throw new Error("L1 provider did not answer");
    return fixture.walletUtxos.filter(({ txHash, outputIndex }) =>
      refs.includes(`${txHash}#${outputIndex}`),
    );
  });
  const alerts = (code: string) =>
    test.setAlert.mock.calls
      .map(([alert]) => alert as { code: string; active: boolean })
      .filter((alert) => alert.code === code);
  const events = async () =>
    (await fixture.journal.load(fixture.initial.workflowId)).map(
      ({ event }) => event,
    );
  const logged: string[] = [];
  vi.spyOn(process.stderr, "write").mockImplementation(
    (line) => logged.push(String(line)) > 0,
  );
  const later = async () => {
    vi.setSystemTime(Date.now() + 15 * 60_000);
    expect(await test.execution.execute(test.input)).toMatchObject({
      kind: "pending",
    });
  };

  showLanding(fixture, true);
  fixture.chain.mockResolvedValue({ kind: "not_found", landedInvalid: true });
  await later();
  const forfeited = await events();
  expect(forfeited.at(-2)).toEqual(forfeitureOf(fixture));

  // A rollback drops the landing, but the collateral cannot be read back.
  showLanding(fixture, false);
  fixture.chain.mockResolvedValue({
    kind: "unknown",
    reason: "Exact recorded transaction requires live rebroadcast authority",
    absent: true,
  });
  unreadable = true;
  await later();
  const failed = await events();
  expect(failed.slice(forfeited.length)).toEqual([
    {
      kind: "stalled",
      reason: expect.stringContaining(
        `collateral accounting failed for attempt ${fixture.transactionHash}`,
      ),
    },
  ]);
  expect(alerts("prover_collateral_accounting_failed")).toEqual([
    {
      code: "prover_collateral_accounting_failed",
      subjectDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
      active: true,
      observedAtMs: expect.any(String),
    },
  ]);
  expect(alerts("proof_submission_failure")).toEqual([]);
  const named = () =>
    logged.filter((line) =>
      line.includes('"prover_collateral_accounting_failed"'),
    );
  expect(named()).toHaveLength(1);
  expect(named()[0]).toContain("L1 provider did not answer");

  // The attempt is read again later; the same failure is not named again.
  const reads = () =>
    fixture.observeSignedTransaction.mock.calls.filter(
      ([{ transactionHash }]) => transactionHash === fixture.transactionHash,
    ).length;
  const before = reads();
  await later();
  expect(reads()).toBeGreaterThan(before);
  expect(await events()).toEqual(failed);
  expect(alerts("prover_collateral_accounting_failed")).toHaveLength(1);
  expect(named()).toHaveLength(1);
  expect(alerts("proof_submission_failure")).toEqual([]);

  // Once the collateral reads back, the reversal lands and the alert clears.
  unreadable = false;
  await later();
  expect((await events()).slice(failed.length)).toEqual([
    {
      kind: "collateral_forfeiture_reversed",
      actionId: "init",
      txHash: fixture.transactionHash,
    },
  ]);
  expect(alerts("prover_collateral_accounting_failed").at(-1)).toMatchObject({
    active: false,
  });
  expect(alerts("proof_submission_failure")).toEqual([]);
});

it("reverses a forfeiture whose collateral the reservation already holds again, after its pool was released and re-planned", async () => {
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
  const collateral = fixture.collateralInput.outRef;
  const back = fixture.collateralReturn!;
  // The attempt landed failing phase 2: its funding input is unspent, its
  // collateral taken and its return created.
  fixture.useUnspentPendingInputs();
  showLanding(fixture, true);
  fixture.chain.mockResolvedValue({ kind: "not_found", landedInvalid: true });
  const journal = await fixture.recover();
  await fixture.run(journal);
  const forfeited = await fixture.journal.load(fixture.initial.workflowId);
  expect(
    forfeited.slice(fixture.originalEntries.length).map(({ event }) => event),
  ).toEqual([
    forfeitureOf(fixture),
    {
      kind: "reconciled",
      actionId: "init",
      outcome: "not_found",
      txHash: fixture.transactionHash,
    },
  ]);
  // The attempt is superseded and nothing is unresolved: the pool is
  // released, the collateral return with it.
  expect(await heldOutRefs(fixture)).toEqual([]);

  // A rollback drops the landing, and the next action re-plans the pool from
  // the wallet as it is now: the collateral is live and unleased again.
  showLanding(fixture, false);
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: "init", input: { actionKind: "proof.init" } },
  });
  const replanned = await heldOutRefs(fixture);
  expect(replanned).toContain(collateral);
  expect(replanned).not.toContain(back.outRef);

  // The superseded attempt's next read finds it absent with every input live.
  fixture.chain.mockResolvedValue({
    kind: "unknown",
    reason: "Exact recorded transaction requires live rebroadcast authority",
    absent: true,
  });
  // The process stops once the reversal commits, before the idle pool is
  // released, so the reservation shows the reversal alone. The read is due
  // well after any earlier read of this attempt.
  fixture.stopAt("releaseIdle", "before");
  await expect(
    fixture.run(journal, () => new Date(Date.now() + 24 * 3_600_000)),
  ).rejects.toBeInstanceOf(FixtureProcessStop);
  const reversed = await fixture.journal.load(fixture.initial.workflowId);
  expect(reversed.slice(0, forfeited.length)).toEqual(forfeited);
  expect(reversed.slice(forfeited.length).map(({ event }) => event)).toEqual([
    {
      kind: "collateral_forfeiture_reversed",
      actionId: "init",
      txHash: fixture.transactionHash,
    },
  ]);
  // The collateral it already holds counts as given back, once.
  expect(await heldOutRefs(fixture)).toEqual(replanned);
});
