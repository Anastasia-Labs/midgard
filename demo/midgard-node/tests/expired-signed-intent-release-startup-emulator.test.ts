import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { expect, it, vi } from "vitest";

import * as MutationJobs from "../src/database/mutationJobs.js";
import * as Pending from "../src/database/pendingBlockFinalizations.js";
import { advanceEmulatorPastLatestBlockEndTime } from "./deposit-flow-emulator-shared.js";
import {
  closeLifecycle,
  commitNextBlock,
  finalizeLocally,
  outputOf,
  read,
  readJournal,
  readLocalFinalizationJob,
  submitUnlandedBlock,
} from "./helpers/correction-rewind-scenario.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import {
  admitTwoFundedTransfers,
  awaitOwnerReady,
  expectReplaced,
  type Handle,
  holdLeaseAsCrashed,
  readImmutableCounts,
  resetSharedRows,
  retireCrashedLease,
  signedTtl,
  UNLANDED,
} from "./helpers/signed-intent-replacement.js";

/**
 * The live startup shape (3b61adb6) of a signed commit that missed its
 * validity window: two L2 transfers, the TTL passes while the node is down,
 * and the node restarts with a FAILED local-finalization job row for the lost
 * block's journal, its crashed commit's lease still held, and no native MPF
 * owner open. Actual deployed validators, the production history owner and
 * Architecture G, the fixture's copy of listen's startup completion
 * (boundary seed, pending-finalization hydration,
 * assertStartupMutationJobsRecoverable, then the native owner only if none is
 * open), and emulator transactions. Only chain-point names and the history
 * transport are synthetic.
 */

// Every call of the signed-intent reconciliation's recovery preparation and
// every startup completion, in order, with the lost journal's state each one
// left or found.
const trace = vi.hoisted(() => ({
  header: undefined as string | undefined,
  events: [] as {
    kind: "reconciliation" | "completion";
    generation?: number;
    status: string | undefined;
    job: string | undefined;
    nativeOpen?: boolean;
  }[],
}));

const lostJournalState = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const header = trace.header!;
  const journal = yield* sql<{ status: string }>`SELECT status
    FROM pending_block_finalizations
    WHERE header_hash = ${Buffer.from(header, "hex")}`;
  const job = yield* sql<{ status: string }>`SELECT status
    FROM local_mutation_jobs
    WHERE job_id = ${MutationJobs.localBlockFinalizationJobId(header)}`;
  return { status: journal[0]?.status, job: job[0]?.status };
});

vi.mock(
  "../src/services/history-expired-intent-release.js",
  async (importOriginal) => {
    const { Effect } = await import("effect");
    const actual =
      await importOriginal<
        typeof import("../src/services/history-expired-intent-release.js")
      >();
    return {
      ...actual,
      prepareExpiredIntentRelease: (
        input: Parameters<typeof actual.prepareExpiredIntentRelease>[0],
      ) =>
        actual.prepareExpiredIntentRelease(input).pipe(
          Effect.tap(() =>
            trace.header === undefined
              ? Effect.void
              : lostJournalState.pipe(
                  Effect.tap((state) =>
                    Effect.sync(() =>
                      trace.events.push({ kind: "reconciliation", ...state }),
                    ),
                  ),
                  Effect.orDie,
                ),
          ),
        ),
    };
  },
);

const C = Pending.Columns;
const J = MutationJobs.Columns;

it("replaces a signed commit whose TTL passed while the node was down in its first startup convergence, before completion, removing its failed local-finalization job; the node reaches Ready and the recommit lands and finalizes", async () => {
  const lifecycle = await openHistoryProductionOwnerLifecycle({
    beforeCompletion: (generation, globals) =>
      trace.header === undefined
        ? Effect.void
        : lostJournalState.pipe(
            Effect.flatMap((state) =>
              Ref.get(globals.NATIVE_MPF_OWNER).pipe(
                Effect.map((owner) =>
                  trace.events.push({
                    kind: "completion",
                    generation,
                    ...state,
                    nativeOpen: owner !== undefined,
                  }),
                ),
              ),
            ),
          ),
  });
  let h: Handle = lifecycle;
  let crashedLease: string | undefined;
  try {
    await resetSharedRows();
    await advanceEmulatorPastLatestBlockEndTime(h.fixture);
    const { first, second, txIds } = await admitTwoFundedTransfers(lifecycle);

    // The commit is signed and handed to L1, which never includes it.
    const lost = await submitUnlandedBlock(
      lifecycle,
      h.fixture.emulator.now() - 1000,
    );
    const header = lost.submittedHeaderHash;
    const journal = await readJournal(header);
    expect(UNLANDED).toContain(journal[C.STATUS]);
    expect(journal[C.CORRECTION_TRANSITION_DIGEST]).toBeNull();
    expect(journal.mempoolTxIds.map((id) => id.toString("hex")).sort()).toEqual(
      txIds,
    );
    const ttl = signedTtl(journal[C.SIGNED_TX_CBOR]!);
    crashedLease = journal[C.STATE_QUEUE_LEASE_TOKEN];
    await holdLeaseAsCrashed(crashedLease);
    // Its local finalization failed and left a FAILED job row, which startup
    // hands to the runtime while the journal is live.
    const jobId = MutationJobs.localBlockFinalizationJobId(header);
    await read(
      Effect.gen(function* () {
        yield* MutationJobs.start({
          jobId,
          kind: MutationJobs.Kind.LocalBlockFinalization,
        });
        yield* MutationJobs.markFailed(
          jobId,
          "local finalization failed before the node went down",
        );
      }),
    );
    expect((await readLocalFinalizationJob(header))?.[J.STATUS]).toBe(
      MutationJobs.Status.Failed,
    );
    trace.header = header;
    trace.events.length = 0;

    // While the node is down, L1 passes the signed upper bound (the TTL)
    // and moves on; the source serves those points to the restarted owner.
    const restarted = await lifecycle.restartRuntime({
      synchronize: false,
      afterStop: async () => {
        const delta = ttl - h.fixture.emulator.slot;
        if (delta > 0) h.fixture.emulator.awaitSlot(delta);
        vi.setSystemTime(new Date(h.fixture.emulator.now()));
        await lifecycle.sealSourcePointWhileStopped();
        h.fixture.emulator.awaitBlock(1);
        vi.setSystemTime(new Date(h.fixture.emulator.now()));
        await lifecycle.sealSourcePointWhileStopped();
      },
    });
    h = restarted;
    expect(Effect.runSync(Ref.get(restarted.globals.NATIVE_MPF_OWNER))).toBe(
      undefined,
    );
    await awaitOwnerReady(restarted);

    // The first startup convergence replaced the block before completion:
    // the first reconciliation after the restart left the journal abandoned
    // and its job row removed, and the one completion before Ready found
    // exactly that, with the native owner the reconciliation opened.
    expect(trace.events[0]).toEqual({
      kind: "reconciliation",
      status: Pending.Status.Abandoned,
      job: undefined,
    });
    const completions = trace.events.filter(
      ({ kind }) => kind === "completion",
    );
    expect(completions).toEqual([
      {
        kind: "completion",
        generation: 1,
        status: Pending.Status.Abandoned,
        job: undefined,
        nativeOpen: true,
      },
    ]);
    expect(trace.events.indexOf(completions[0]!)).toBeGreaterThan(0);

    await expectReplaced(journal, { handle: restarted });
    const base = journal[C.BASE_UTXOS_ROOT];
    trace.header = undefined;

    // The reopened transfers recommit on the restored base, land and are
    // locally finalized, each once.
    const next = await commitNextBlock(restarted);
    expect(next.submittedHeaderHash).not.toBe(header);
    const recommitted = await readJournal(next.submittedHeaderHash);
    expect(recommitted[C.BASE_UTXOS_ROOT]).toBe(base);
    expect(
      recommitted.mempoolTxIds.map((id) => id.toString("hex")).sort(),
    ).toEqual(txIds);
    await restarted.synchronize();
    await finalizeLocally(restarted, next.submittedHeaderHash);
    expect(
      (await readJournal(next.submittedHeaderHash))[C.EXPECTED_UTXOS_ROOT],
    ).toBe((await restarted.evidence()).native?.durableRoot);
    expect(await readImmutableCounts(txIds)).toEqual(
      Object.fromEntries(txIds.map((id) => [id, 1])),
    );
    await outputOf(restarted, first, 5_000_000n);
    await outputOf(restarted, second, 4_000_000n);
  } finally {
    trace.header = undefined;
    if (crashedLease !== undefined) await retireCrashedLease(crashedLease);
    await closeLifecycle(h);
  }
}, 900_000);
