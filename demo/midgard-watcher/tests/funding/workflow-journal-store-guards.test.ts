import { join } from "node:path";

import {
  ConcurrentFraudProofWorkflowWriteError,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it } from "vitest";

import { parseWatcherProverFundingReservationRecord } from "../../src/funding/prover-funding-reservation.js";
import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture as setup,
} from "../support/fault-proof-funding-fixture.js";
import { openFixtureFundingStore } from "../support/fault-proof-funding-fixture.workflow-store.js";

afterEach(async () => {
  await cleanupFundingRecoveryFixtures();
});

it("refuses a workflow entry out of sequence as a concurrent write, keeping the rows", async () => {
  const fixture = await setup();
  const journal = fixture.workflowJournal.forObjective(fixture.objective);
  const before = fixture.workflowJournal.readExecution(
    fixture.objective,
  )!.entries;
  const entry = (sequence: number) => ({
    schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
    workflowId: fixture.initial.workflowId,
    identity: fixture.initial.identity,
    sequence,
    recordedAt: new Date().toISOString(),
    event: {
      kind: "reconciled" as const,
      actionId: "init",
      outcome: "pending" as const,
      txHash: fixture.transactionHash,
    },
  });
  // A writer whose fold is behind reuses a taken sequence; one that runs
  // ahead would leave a gap.
  for (const sequence of [before.length - 1, before.length + 1])
    await expect(
      journal.append(entry(sequence), sequence),
    ).rejects.toBeInstanceOf(ConcurrentFraudProofWorkflowWriteError);
  expect(
    fixture.workflowJournal.readExecution(fixture.objective)!.entries,
  ).toEqual(before);
  await journal.append(entry(before.length), before.length);
  expect(
    fixture.workflowJournal.readExecution(fixture.objective)!.entries,
  ).toHaveLength(before.length + 1);
});

it("reads a funding change another connection committed, never an audit it read before", async () => {
  const fixture = await setup(true);
  const other = await openFixtureFundingStore(
    join(fixture.journalRoot, "watcher.sqlite"),
  );
  try {
    // How many inputs the reservation holds, as the other connection reads it.
    const held = async () =>
      (await other.store.readAll()).map(
        (value) =>
          parseWatcherProverFundingReservationRecord(value).activeInputs.length,
      );
    const [before] = await held();
    expect(before).toBeGreaterThan(0);
    // Released through the fixture's own connection.
    expect(
      await fixture.store.releaseUnused!((await fixture.records())[0]!),
    ).toBe(true);
    expect(await held()).toEqual([0]);
  } finally {
    other.close();
  }
});
