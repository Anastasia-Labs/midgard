import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { Level } from "level";
import { expect } from "vitest";

import * as Pending from "../../src/database/pendingBlockFinalizations.js";
import {
  activeLivenessReasons,
  CORRECTION_REWIND_JOURNAL_UNBOUND,
  HISTORY_CORRECTION_REWIND_SOURCE,
} from "../../src/services/liveness-halt.js";
import {
  C,
  captureRemoved,
  expectOwedAndUnreincluded,
  expectUnobservedRemoval,
  failureText,
  nativeRoot,
  openRemovedTailOverRetainedBlock,
  restartAfter,
  type Scenario,
} from "../attestation-timeout-reinclusion-emulator.expect-rewound-and-recommitted.js";
import {
  inspectObligation,
  journalUpdate,
  updateJournal,
  writeAndInspect,
} from "../attestation-timeout-reinclusion-hardening-emulator.inspect-with-foreign-submission.js";
import {
  closeLifecycle,
  type Lifecycle,
  openCorrectionRewindScenario,
  read,
  readJournal,
  readRecoveryPlans,
  readSqlLedgerRoot,
} from "./correction-rewind-scenario.js";

export const assertClosedCorrectionGate = async (
  h: Pick<Scenario["h"], "production">,
) => {
  expect((await Effect.runPromise(h.production.owner.frontier)).ready).toBe(
    false,
  );
  let entered = false;
  await expect(
    read(
      h.production.owner.runProducer(() => {
        entered = true;
        return Effect.succeed(undefined);
      }),
    ),
  ).rejects.toBeDefined();
  expect(entered).toBe(false);
  const state = await read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{
        state: string;
      }>`SELECT state FROM event_history_authority`;
    }),
  );
  expect(state).toHaveLength(1);
  expect(state[0]!.state).not.toBe("ready");
};

/** The raised liveness reason of the correction rewind's source, if any. */
const rewindLivenessReason = async (h: Pick<Scenario["h"], "globals">) =>
  (await Effect.runPromise(activeLivenessReasons(h.globals))).find(
    ({ source }) => source === HISTORY_CORRECTION_REWIND_SOURCE,
  )?.reason;

/** Polls (real time) until the running owner's rewind evaluation raised
 * `reason`; the owner retries a held rewind on its own timer. */
const awaitRewindLivenessReason = async (
  h: Pick<Scenario["h"], "globals">,
  reason: string,
) => {
  const deadline = performance.now() + 30_000;
  while ((await rewindLivenessReason(h)) !== reason) {
    if (performance.now() >= deadline)
      throw new Error(
        `Rewind liveness reason is ${String(await rewindLivenessReason(h))}, expected ${reason}`,
      );
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
};

/**
 * A removed own block whose journal no longer binds to the header its
 * admitted correction removed: its base tail header hash is pointed at a hash
 * no header has, and no retained journal of that hash exists. The rewind
 * target is that journal's base root, so the rewind holds on it: native MPF,
 * the SQL root and the journal stay unchanged, the history gate stays closed,
 * and readiness names `correction_rewind_journal_unbound`. Restoring the
 * column clears the hold, and the same node rewinds.
 */
export const assertUnboundRemovedJournalHolds = async () => {
  const scenario = await openCorrectionRewindScenario({ blocks: 1 });
  let h: Pick<Lifecycle, "close"> = scenario.h;
  try {
    const [removed] = await captureRemoved(scenario.headers);
    const header = removed!.headerHash;
    const unbound = Buffer.from("ff".repeat(28), "hex");
    let bound: Readonly<Record<string, unknown>> = {};
    let removal: Awaited<ReturnType<typeof scenario.removeTail>> | undefined;
    const restarted = await restartAfter(scenario, async () => {
      removal = await scenario.removeTail(header, { observe: false });
      bound = await updateJournal(header, {
        [C.BASE_TAIL_HEADER_HASH]: unbound,
      });
    });
    h = restarted;
    const boundTail = bound[C.BASE_TAIL_HEADER_HASH] as Buffer;
    expect(boundTail.equals(unbound)).toBe(false);
    // Before the removal is final nothing is owed, exactly as with a bound
    // journal: native, SQL and the journal are untouched.
    await expectUnobservedRemoval([removed!], restarted);
    await scenario.awaitRemovalFinality(restarted);
    expect(
      (await scenario.tick(restarted.globals)).admittedTransactionHashes,
    ).toEqual([removal!.accepted.transaction.txHash]);
    expect(await inspectObligation(scenario)).toEqual({
      kind: "blocked",
      reason: `removed block ${header}'s journal replay base ${unbound.toString("hex")}/${removed!.base} and candidate root ${removed!.expected} are not its header's predecessor ${boundTail.toString("hex")}/${removed!.base} and root ${removed!.expected}`,
      journalUnbound: true,
    });
    await scenario.nextSourceBlockWhileRefused(restarted);
    await awaitRewindLivenessReason(
      restarted,
      CORRECTION_REWIND_JOURNAL_UNBOUND,
    );
    await scenario.nextSourceBlockWhileRefused(restarted);
    expect(await nativeRoot(restarted)).toBe(removed!.expected);
    expect((await readSqlLedgerRoot()).root_hex).toBe(removed!.expected);
    const journal = await readJournal(header);
    expect(journal[C.STATUS]).toBe(Pending.Status.LocallyApplied);
    expect(journal[C.CORRECTION_TRANSITION_DIGEST] ?? null).toBeNull();
    expect(await readRecoveryPlans()).toEqual([]);
    await assertClosedCorrectionGate(restarted);
    expect(await rewindLivenessReason(restarted)).toBe(
      CORRECTION_REWIND_JOURNAL_UNBOUND,
    );
    // The journal binds again: the same node rewinds to the removed block's
    // base, and the reason clears.
    expect(
      await writeAndInspect(
        scenario,
        journalUpdate(header, { [C.BASE_TAIL_HEADER_HASH]: boundTail }),
      ),
    ).toEqual({
      kind: "ready",
      members: [{ headerHash: header, kind: "removed" }],
    });
    await scenario.nextSourceBlock(restarted);
    expect(await nativeRoot(restarted)).toBe(removed!.base);
    expect((await readSqlLedgerRoot()).root_hex).toBe(removed!.base);
    expect((await readJournal(header))[C.STATUS]).toBe(
      Pending.Status.Abandoned,
    );
    expect(await rewindLivenessReason(restarted)).toBeUndefined();
  } finally {
    await closeLifecycle(h);
  }
};

export const assertUnretainedCorrectionRoot = async () => {
  const { scenario, removed } = await openRemovedTailOverRetainedBlock();
  const levelPath = scenario.h.production.nodeConfig.LEDGER_MPF_DB_PATH;
  const target = removed[0]!.base;
  const readMarker = async () => {
    const db = new Level<string, unknown>(levelPath, { valueEncoding: "json" });
    await db.open();
    try {
      return await db.get("__root__");
    } finally {
      await db.close();
    }
  };
  try {
    // Drop the base root's own record while no service holds the store.
    const failure = await failureText(
      scenario.h.restartRuntime({
        afterStop: async () => {
          const db = new Level<string, unknown>(levelPath, {
            valueEncoding: "json",
          });
          await db.open();
          try {
            expect(await db.get(target)).toBeDefined();
            await db.del(target);
          } finally {
            await db.close();
          }
        },
      }),
    );
    expect(failure).toContain(
      `Native MPF canonical recovery target root ${target} is not retained in full; refusing to restore`,
    );
    expect(await readMarker()).toBe(removed[0]!.expected);
    expect((await readSqlLedgerRoot()).root_hex).not.toBe(target);
    await expectOwedAndUnreincluded(removed);
  } finally {
    await closeLifecycle(scenario.h);
  }
};
