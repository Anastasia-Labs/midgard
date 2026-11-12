/**
 * Migration 0036 on this node's landed block a rollback removed, as block
 * confirmation left it (`landed-blocks-own-migration.fixture.ts`), then the
 * working-ledger rebase (`landed-blocks-rebase.fixture.ts`):
 *
 * - a removed row whose journal block confirmation had not applied goes,
 *   its inclusion marks with it, as a rollback takes an unapplied row.
 *   Should the block land again, it is processed as a fresh own landing and
 *   the rebase applies it once (its transaction, events, deposit output and
 *   DA payload once, then folded once, and a later rebase changes nothing);
 *   should it not, its journal waits as a submitted commit that has not
 *   landed does, until the existing disposition takes it (here a block
 *   taking the slot after its base), which makes its members pending again
 *   with nothing to revert;
 * - one whose journal is locally applied is left as it is, and the rebase
 *   reverts it.
 */
import type { View } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";
import { describe, expect, it } from "vitest";

import { DepositsDB, WithdrawalsDB } from "../src/database/index.js";
import { foldToRoot } from "../src/landed-blocks/fold.js";
import {
  activeJournal,
  ownJournal as journalOf,
} from "../src/landed-blocks/journal.js";
import type { LandedBlockPorts } from "../src/landed-blocks/ports.js";
import { rebasePlan } from "../src/landed-blocks/rebase-target.js";
import { processRow, rollBackRows } from "../src/landed-blocks/settlements.js";
import { retrieveRows } from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import type { Globals } from "../src/services/globals.js";
import { testWrite } from "./helpers/driver-recompute.js";
import {
  appliedOnce,
  DEPOSIT_ID,
  DEPOSITED,
  FORCED,
  HEADER,
  landPreA,
  migrate,
  OWN,
  shown,
  Status,
  TX,
  useOwnMigration,
  WITHDRAWAL_ID,
} from "./landed-blocks-own-migration.fixture.js";
import {
  attempt,
  E0,
  E1,
  expectRebased,
  freshNative,
  hex,
  land,
  landedRow,
  R0,
  R1,
  run,
} from "./landed-blocks-rebase.fixture.js";
import {
  runningLocalJob,
  startupGate,
} from "./local-mutation-job-abandonment.journal-fixture.js";

useOwnMigration();

/**
 * `OWN` lands again where it was, as landed-block processing takes it: a
 * removed row is processed again (`rollBackRows`' relands); without one the
 * block is new processing, the unapplied own row its journal gives
 * (`process.ts` `newRow`) on the seeded block's delta.
 */
const landAgain = (globals: Globals) =>
  run(
    globals,
    testWrite(
      Effect.gen(function* () {
        const row = (yield* retrieveRows).find((row) => row.headerHash === OWN);
        if (row !== undefined) return yield* rollBackRows([], [row]);
        const own = (yield* journalOf(OWN))!;
        yield* processRow(
          landedRow({
            headerHash: OWN,
            kind: "own",
            applied: false,
            produced: [E1, DEPOSITED],
            depositIds: own.depositIds,
            withdrawals: own.withdrawals,
            forcedIds: own.forcedIds,
            txIds: own.txIds,
          }),
        );
      }),
    ),
  );

/** The production fold's ports with the view held. */
const foldPorts = {
  confirmView: () => Effect.succeed(true),
  write: <A, E, R>(work: Effect.Effect<A, E, R>) => withFollowerWrite(work),
} as unknown as LandedBlockPorts<never>;

/** The merge of `OWN` folds it into `confirmed_ledger`. */
const foldOwn = (globals: Globals) =>
  run(
    globals,
    testWrite(
      foldToRoot(
        foldPorts,
        {} as View,
        { headerHash: OWN, utxosRoot: R1 },
        () => Effect.succeed(null),
      ),
    ),
  );

/** `confirmed_ledger`'s outrefs and the events the fold of `OWN` moved. */
const folded = (globals: Globals) =>
  run(
    globals,
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const ledger = yield* sql<{ outref: Buffer }>`
        SELECT outref FROM confirmed_ledger ORDER BY outref`;
      const merges = yield* sql<{
        consumed_deposit_ids: Buffer[];
        finalized_withdrawal_ids: Buffer[];
        finalized_forced_ids: Buffer[];
      }>`SELECT consumed_deposit_ids, finalized_withdrawal_ids,
          finalized_forced_ids
        FROM node_confirmed_merges WHERE header_hash = ${HEADER}`;
      return {
        ledger: ledger.map((row) => hex(row.outref)),
        merges: merges.map((merge) => ({
          deposits: merge.consumed_deposit_ids.map(hex),
          withdrawals: merge.finalized_withdrawal_ids.map(hex),
          forced: merge.finalized_forced_ids.map(hex),
        })),
      };
    }),
  );

const sorted = (values: readonly string[]) => [...values].sort();

describe(
  "migration 0036 on an own landed block a rollback removed",
  { concurrent: false },
  () => {
    it.each([
      ["no local effects"],
      ["the old local finalization's SQL commit"],
    ] as const)(
      "deletes a block a rollback removed whose journal it had not applied (%s); landed again, the rebase applies it once and counts nothing twice",
      async (stored) => {
        const sqlStored = stored !== "no local effects";
        const native = { ...freshNative(), durableRoot: R1 };
        const globals = await landPreA(native, {
          status: Status.ObservedWaitingStability,
          withdrawal: "projected",
          forced: true,
          removed: true,
          stored: sqlStored ? "sql" : undefined,
        });
        if (sqlStored) await run(globals, testWrite(runningLocalJob(HEADER)));
        await migrate(globals);
        expect((await shown(globals)).row).toBeUndefined();
        if (sqlStored) expect(await run(globals, startupGate)).toBeUndefined();

        await landAgain(globals);
        const rebased = await attempt(globals);
        expectRebased(rebased);
        expect(sorted(rebased.working)).toEqual(
          sorted([hex(E1.outref), hex(DEPOSITED.outref)]),
        );
        expect(native.durableRoot).toBe(R1);
        const applied = await shown(globals);
        expect(applied).toMatchObject({
          ...appliedOnce(),
          forced: ["projected"],
        });
        // A second rebase changes nothing.
        expect(await attempt(globals)).toEqual(rebased);
        expect(await shown(globals)).toEqual(applied);

        // Its merge folds it once: the deposit consumed, the withdrawal and
        // the forced transaction finalized, its outputs confirmed.
        await foldOwn(globals);
        const merged = await folded(globals);
        expect(merged).toEqual({
          ledger: sorted([hex(E1.outref), hex(DEPOSITED.outref)]),
          merges: [
            {
              deposits: [hex(DEPOSIT_ID)],
              withdrawals: [hex(WITHDRAWAL_ID)],
              forced: [hex(FORCED)],
            },
          ],
        });
        const final = await shown(globals);
        expect(final).toMatchObject({
          row: undefined,
          journal: Option.some(Status.LocallyApplied),
          immutable: [TX.cbor.toString("hex")],
          blocks: [OWN],
          withdrawal: { status: WithdrawalsDB.Status.Finalized, header: OWN },
          forced: ["finalized"],
          deposit: { status: DepositsDB.Status.Consumed, header: OWN },
        });
        const after = await attempt(globals);
        expect(after.hold).toBeUndefined();
        expect(after.due).toBe("none");
        expect(await shown(globals)).toEqual(final);
        expect(await folded(globals)).toEqual(merged);
      },
    );

    it.each([
      ["no local effects"],
      ["the old local finalization's SQL commit"],
    ] as const)(
      "deletes a block a rollback removed whose journal it had not applied (%s); not landing again, its journal waits until a block takes its slot, and its members are pending again",
      async (stored) => {
        const sqlStored = stored !== "no local effects";
        const native = { ...freshNative(), durableRoot: R1 };
        const globals = await landPreA(native, {
          status: Status.ObservedWaitingStability,
          withdrawal: "projected",
          forced: true,
          removed: true,
          stored: sqlStored ? "sql" : undefined,
        });
        await migrate(globals);
        // The row and its marks are gone: nothing is left to revert. The
        // journal is a submitted commit that has not landed: no rebase is
        // due, and the next commit waits on it.
        expect(await shown(globals)).toMatchObject({
          row: undefined,
          journal: Option.some(Status.ObservedWaitingStability),
          pending: [null],
        });
        expect((await run(globals, rebasePlan)).kind).toBe("none");
        expect(await run(globals, activeJournal)).toBeDefined();

        // A foreign block takes the slot after the journal's base: the
        // rebase disposes of the journal, and its members are pending again.
        await land(globals, {});
        const rebased = await attempt(globals);
        expectRebased(rebased);
        expect(rebased.working).toEqual([hex(E1.outref)]);
        expect(await shown(globals)).toMatchObject({
          row: undefined,
          journal: Option.some(Status.Abandoned),
          immutable: [],
          blocks: [],
          withdrawal: { status: WithdrawalsDB.Status.Awaiting, header: null },
          forced: ["awaiting"],
          deposit: { status: DepositsDB.Status.Awaiting, header: null },
          pending: [null],
        });
        expect(await run(globals, activeJournal)).toBeUndefined();
      },
    );

    it("leaves a block a rollback removed whose journal is locally applied as it is; the rebase reverts it", async () => {
      const native = { ...freshNative(), durableRoot: R1 };
      const globals = await landPreA(native, {
        status: Status.LocallyApplied,
        withdrawal: "finalized",
        stored: "all",
        forced: true,
        removed: true,
      });
      const before = await shown(globals);
      expect(before).toMatchObject({
        row: { state: "removed", applied: true },
        journal: Option.some(Status.LocallyApplied),
      });

      await migrate(globals);
      expect(await shown(globals)).toEqual(before);

      const rebased = await attempt(globals);
      expect(rebased.hold).toBeUndefined();
      expect(rebased.due).toBe("none");
      expect(rebased.applied).toEqual([]);
      expect(rebased.working).toEqual([hex(E0.outref)]);
      expect(native.durableRoot).toBe(R0);
      expect(await shown(globals)).toMatchObject({
        row: undefined,
        journal: Option.some(Status.Abandoned),
        immutable: [],
        blocks: [],
        withdrawal: { status: WithdrawalsDB.Status.Awaiting, header: null },
        forced: ["awaiting"],
        deposit: { status: DepositsDB.Status.Awaiting, header: null },
        pending: [null],
      });
    });
  },
);
