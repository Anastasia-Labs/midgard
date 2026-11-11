import { mkdirSync, renameSync } from "node:fs";
import { join } from "node:path";

import { supersededAttemptReadSchedule } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.js";
import {
  closeWatcherJournalDatabase,
  WATCHER_JOURNAL_DATABASE_FILE,
} from "../../src/fault-proofs/watcher-journal-database.js";
import { WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION } from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import { openWatcherProverFundingRuntime } from "../../src/runtime/watcher-prover-funding-runtime.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { expiredNotFound } from "./prover-funding-recovery.retirement-fixture.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

type Fixture = Awaited<ReturnType<typeof setupFundingRecoveryFixture>>;

/** The README repair: the journals file and its WAL files are moved aside,
 * so the new journals start empty and hold no fault decision. */
const repairJournals = (journalRoot: string) => {
  closeWatcherJournalDatabase(journalRoot);
  const aside = join(journalRoot, "moved-aside");
  mkdirSync(aside);
  for (const suffix of ["", "-wal", "-shm"]) {
    const file = `${WATCHER_JOURNAL_DATABASE_FILE}${suffix}`;
    try {
      renameSync(join(journalRoot, file), join(aside, file));
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
    }
  }
};

/** Starts the watcher's funding runtime over the fixture's store. */
const start = (
  fixture: Fixture,
  observeSignedTransaction?: Fixture["observeSignedTransaction"],
) =>
  openWatcherProverFundingRuntime({
    deploymentIdentity,
    path: join(fixture.journalRoot, "watcher.sqlite"),
    authenticationKey: Buffer.alloc(32, 0x91),
    createProtocolParameters: async () => fixture.protocolParameters,
    launchScope: installedLaunchScope(fixture.old.launchScope),
    journalRoot: fixture.journalRoot,
    ...(observeSignedTransaction === undefined
      ? {}
      : { observeSignedTransaction }),
  });

const reservation = async (fixture: Fixture) =>
  (await fixture.records()).find(
    ({ reservationId }) => reservationId === fixture.plan.reservationId,
  );

describe("funding whose decision a journal repair dropped", () => {
  it("releases an unsigned reservation at startup, and only there", async () => {
    const fixture = await setupFundingRecoveryFixture(true);
    expect(await fixture.store.readReservedOutRefs({})).not.toEqual([]);
    repairJournals(fixture.journalRoot);
    const before = await fixture.records();

    // Sweeps that run beside dispatch leave it to the next startup.
    const beside = fixture.fundingFactory();
    await beside.releaseUnused();
    await beside.settleUnattended();
    expect(await fixture.records()).toEqual(before);
    closeWatcherJournalDatabase(fixture.journalRoot);

    const runtime = await start(fixture);
    runtime.store.close();
    expect(await reservation(fixture)).toMatchObject({ activeInputs: [] });
    expect(await fixture.store.readReservedOutRefs({})).toEqual([]);
  });

  it("holds a signed reservation at startup until its attempt is retired past release finality, then settles it", async () => {
    const fixture = await setupFundingRecoveryFixture(false, true);
    supersededAttemptReadSchedule.forget(fixture.transactionHash);
    repairJournals(fixture.journalRoot);
    fixture.chain.mockResolvedValue({ kind: "not_found" });
    const clock = vi.spyOn(Date, "now").mockReturnValue(1_000_000);
    const before = await fixture.records();

    const runtime = await start(fixture, fixture.observeSignedTransaction);
    try {
      expect(await fixture.records()).toEqual(before);
      expect(runtime.factory.decisionHolds()).toEqual([
        expect.objectContaining({
          reservationId: fixture.plan.reservationId,
          readiness: WATCHER_PROVER_FUNDING_ORPHANED_RESERVATION,
        }),
      ]);

      fixture.chain.mockImplementation(async (txHash) =>
        expiredNotFound(txHash!),
      );
      clock.mockReturnValue(1_060_000);
      await runtime.factory.recheckDecisionHolds();
      expect(await reservation(fixture)).toBeUndefined();
      expect(await fixture.store.readReservedOutRefs({})).toEqual([]);
      expect(runtime.factory.decisionHolds()).toEqual([]);
    } finally {
      runtime.store.close();
    }
  });
});
