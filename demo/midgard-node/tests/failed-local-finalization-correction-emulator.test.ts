import { expect, it } from "vitest";

import { assertStartupMutationJobsRecoverable } from "../src/commands/listen-startup.js";
import * as MutationJobs from "../src/database/mutationJobs.js";
import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  closeLifecycle,
  commitNextBlock,
  finalizeLocally,
  type Lifecycle,
  openCorrectionRewindScenario,
  read,
  readDeposits,
  readJournal,
  readLocalFinalizationJob,
  readObserver,
  readRecoveryPlans,
  readSqlLedgerRoot,
} from "./helpers/correction-rewind-scenario.js";

const C = Pending.Columns;
const J = MutationJobs.Columns;
const REWIND_DOMAIN = "midgard-history-correction-rewind-intent-v1";

const nativeRoot = async (handle: Pick<Lifecycle, "evidence">) => {
  const native = (await handle.evidence()).native;
  if (native === undefined) throw new Error("Native owner is not open");
  return native.durableRoot;
};

/**
 * The live preprod f5215638 state: a committed block whose local finalization
 * failed deterministically (a failed job row, its journal still waiting for
 * stability), removed on L1 by an accepted attestation-timeout correction,
 * then a node restart before the observer admitted the removal. Startup used
 * to refuse on the failed job, so the correction path that would close it
 * could never run.
 */
it("starts over a failed local finalization, then abandons the removed block's journal and job, rewinds, recommits and finalizes the reincluded deposit", async () => {
  const scenario = await openCorrectionRewindScenario({
    blocks: 1,
    localFinalization: "failed",
  });
  let h: Pick<Lifecycle, "close"> = scenario.h;
  try {
    const headerHash = scenario.headers[0]!;
    const journal = await readJournal(headerHash);
    expect(journal[C.STATUS]).toBe(Pending.Status.ObservedWaitingStability);
    const base = journal[C.BASE_UTXOS_ROOT];
    const failedJob = await readLocalFinalizationJob(headerHash);
    expect(failedJob?.[J.STATUS]).toBe(MutationJobs.Status.Failed);
    const failure = failedJob![J.LAST_ERROR];
    const deposits = journal.depositEventIds.map((id) => id.toString("hex"));
    expect(deposits).toHaveLength(1);

    // While the block is live the startup gate hands the failed job to the
    // runtime, which keeps retrying it: nothing closes it.
    await read(assertStartupMutationJobsRecoverable);
    expect(await readLocalFinalizationJob(headerHash)).toMatchObject({
      [J.STATUS]: MutationJobs.Status.Failed,
      [J.LAST_ERROR]: failure,
    });

    const removal = await scenario.removeTail(headerHash);
    await scenario.awaitRemovalFinality();
    // Removal final on L1 but not admitted; nothing has closed the job yet.
    expect((await readJournal(headerHash))[C.STATUS]).toBe(
      Pending.Status.ObservedWaitingStability,
    );
    // The process restarts before the observer admits the removal: startup
    // must not refuse.
    const restarted = await scenario.h.restartRuntime();
    h = restarted;
    expect(await readLocalFinalizationJob(headerHash)).toMatchObject({
      [J.STATUS]: MutationJobs.Status.Failed,
    });
    expect(
      (await scenario.tick(restarted.globals)).admittedTransactionHashes,
    ).toEqual([removal.accepted.transaction.txHash]);
    await scenario.nextSourceBlock(restarted);

    // Rewound to the removed block's base; the journal is abandoned under the
    // admitted correction and its moot job removed in the same transaction;
    // the abandoned journal is the durable record.
    expect(await nativeRoot(restarted)).toBe(base);
    expect((await readSqlLedgerRoot()).root_hex).toBe(base);
    const observer = await readObserver();
    const digest = observer.admitted.find(
      ({ transactionHash }) =>
        transactionHash === removal.accepted.transaction.txHash,
    )?.transitionDigest;
    expect(digest).toBeDefined();
    const abandoned = await readJournal(headerHash);
    expect(abandoned[C.STATUS]).toBe(Pending.Status.Abandoned);
    expect(abandoned[C.CORRECTION_TRANSITION_DIGEST]).toBe(digest);
    expect(await readLocalFinalizationJob(headerHash)).toBeUndefined();
    expect(await read(MutationJobs.countUnfinished)).toBe(0n);
    const plans = await readRecoveryPlans();
    expect(plans).toHaveLength(1);
    expect(plans[0]!.state).toBe("applied");
    expect(plans[0]!.intent.domain).toBe(REWIND_DOMAIN);
    expect(plans[0]!.intent.headerHash).toBe(headerHash);
    expect(plans[0]!.intent.targetRoot).toBe(base);
    expect(await readDeposits()).toEqual([
      { status: "projected", projectedHeader: null },
    ]);

    // The next block carries the reincluded deposit on the rewound base, and
    // its local finalization completes.
    const next = await commitNextBlock(restarted);
    const committed = await readJournal(next.submittedHeaderHash);
    expect(committed[C.BASE_UTXOS_ROOT]).toBe(base);
    expect(committed.depositEventIds.map((id) => id.toString("hex"))).toEqual(
      deposits,
    );
    expect((await scenario.readQueue()).at(-1)!.headerHash).toBe(
      next.submittedHeaderHash,
    );
    await finalizeLocally(restarted, next.submittedHeaderHash);
    expect((await readJournal(next.submittedHeaderHash))[C.STATUS]).toBe(
      Pending.Status.Finalized,
    );
    expect(
      (await readLocalFinalizationJob(next.submittedHeaderHash))?.[J.STATUS],
    ).toBe(MutationJobs.Status.Completed);
    expect(await nativeRoot(restarted)).toBe(next.submittedUtxosRoot);
    // The removed block's job stays gone.
    expect(await readLocalFinalizationJob(headerHash)).toBeUndefined();
  } finally {
    await closeLifecycle(h);
  }
}, 900_000);
