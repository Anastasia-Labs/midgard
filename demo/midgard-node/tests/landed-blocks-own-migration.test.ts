/**
 * Migration 0036 on a node database holding this node's landed blocks as
 * block confirmation left them (a row it did not abandon stored applied,
 * with no withdrawals), then the working-ledger rebase (the follower-change
 * driver's `rebaseIfDue`, with the modelled native MPF owner of
 * `landed-blocks-rebase.fixture.ts`):
 *
 * - a row whose journal block confirmation had not applied (pending
 *   submission, submitted, local finalization pending, observed waiting
 *   stability) is never applied without the migration: no rebase is due
 *   and the next commit waits on its journal. The migration makes it
 *   unapplied, with its journal's withdrawals; the next rebase applies it
 *   once (its transaction in `immutable` and `blocks` once, its DA payload
 *   and outbox, its withdrawal projected, its journal locally applied) and
 *   the next commit no longer waits;
 * - so too after a crash past the old local finalization's SQL commit
 *   (its transaction, block rows and DA payload stored, no outbox): startup
 *   completes the unfinished job, and the rebase writes the rest once;
 * - a row whose journal is locally applied is left as it is and not
 *   applied again by a later rebase, and the events it finalized stay
 *   finalized; one whose journal was abandoned before it landed is left as
 *   it is, and the rebase revives and applies it.
 *
 * A row a rollback removed: `landed-blocks-own-migration-removed.test.ts`.
 */
import { Option } from "effect";
import { describe, expect, it } from "vitest";

import { WithdrawalsDB } from "../src/database/index.js";
import * as MutationJobsDB from "../src/database/mutationJobs.js";
import { activeJournal } from "../src/landed-blocks/journal.js";
import { rebasePlan } from "../src/landed-blocks/rebase-target.js";
import { testWrite } from "./helpers/driver-recompute.js";
import {
  appliedOnce,
  COMMITTEE_PEERS,
  FORCED,
  HEADER,
  landPreA,
  MEMBERSHIP,
  migrate,
  NEXT,
  OWN,
  shown,
  Status,
  TX,
  useOwnMigration,
} from "./landed-blocks-own-migration.fixture.js";
import {
  attempt,
  expectRebased,
  freshNative,
  land,
  R1,
  run,
} from "./landed-blocks-rebase.fixture.js";
import {
  findJob,
  localJobId,
  runningLocalJob,
  startupGate,
} from "./local-mutation-job-abandonment.journal-fixture.js";

useOwnMigration();

describe(
  "migration 0036 on own landed blocks block confirmation left",
  { concurrent: false },
  () => {
    it.each([
      [Status.PendingSubmission, "awaiting"],
      [Status.SubmittedLocalFinalizationPending, "projected"],
      [Status.SubmittedUnconfirmed, "awaiting"],
      [Status.ObservedWaitingStability, "projected"],
    ] as const)(
      "makes a block whose journal is %s unapplied, and the rebase applies it once",
      async (status, withdrawal) => {
        const native = freshNative();
        const globals = await landPreA(native, { status, withdrawal });
        // Before the migration the block is never applied: no rebase is
        // due, and the next commit waits on its journal.
        expect((await run(globals, rebasePlan)).kind).toBe("none");
        expect(await run(globals, activeJournal)).toBeDefined();

        await migrate(globals);
        expect((await shown(globals)).row).toMatchObject({
          applied: false,
          withdrawals: [MEMBERSHIP],
        });

        expectRebased(await attempt(globals));
        expect(native.durableRoot).toBe(R1);
        const applied = await shown(globals);
        expect(applied).toMatchObject(appliedOnce());
        expect(await run(globals, activeJournal)).toBeUndefined();

        // The migration again changes nothing; no rebase is due.
        await migrate(globals);
        expect((await run(globals, rebasePlan)).kind).toBe("none");
        expect(await shown(globals)).toEqual(applied);
      },
    );

    it("applies the rest once after a crash past the old local finalization's SQL commit", async () => {
      const globals = await landPreA(freshNative(), {
        status: Status.SubmittedLocalFinalizationPending,
        withdrawal: "projected",
        stored: "sql",
      });
      await run(globals, testWrite(runningLocalJob(HEADER)));
      await migrate(globals);

      // Startup completes the unfinished job: the rebase applies the block.
      expect(await run(globals, startupGate)).toBeUndefined();
      expect(
        (await run(globals, findJob(localJobId(HEADER))))?.[
          MutationJobsDB.Columns.STATUS
        ],
      ).toBe(MutationJobsDB.Status.Completed);

      expectRebased(await attempt(globals));
      expect(await shown(globals)).toMatchObject(appliedOnce());
      expect(await run(globals, activeJournal)).toBeUndefined();
    });

    it("leaves a block whose journal is locally applied as it is, not applied again, its finalized events kept", async () => {
      const globals = await landPreA(
        { ...freshNative(), durableRoot: R1 },
        {
          status: Status.LocallyApplied,
          withdrawal: "finalized",
          stored: "all",
          forced: true,
        },
      );
      // A foreign block landed on it, so a rebase is due.
      await land(globals, {
        headerHash: NEXT,
        parentHeaderHash: OWN,
        spent: [],
        produced: [],
        parentUtxosRoot: R1,
        utxosRoot: R1,
      });
      const before = await shown(globals);
      expect(before).toMatchObject({
        row: { applied: true, withdrawals: [], forcedIds: [FORCED] },
        journal: Option.some(Status.LocallyApplied),
        immutable: [TX.cbor.toString("hex")],
        blocks: [OWN],
        outbox: { peers: COMMITTEE_PEERS, announcements: 1 },
        withdrawal: { status: WithdrawalsDB.Status.Finalized, header: OWN },
        forced: ["finalized"],
      });

      await migrate(globals);
      expect(await shown(globals)).toEqual(before);

      const rebased = await attempt(globals);
      expect(rebased.hold).toBeUndefined();
      expect(rebased.due).toBe("none");
      expect(rebased.applied).toEqual([true, true]);
      expect(await shown(globals)).toEqual(before);
    });

    it("leaves a block whose journal was abandoned before it landed as it is; the rebase revives and applies it", async () => {
      const globals = await landPreA(freshNative(), {
        status: Status.Abandoned,
        withdrawal: "awaiting",
      });
      const before = await shown(globals);
      expect(before.row).toMatchObject({
        applied: false,
        withdrawals: [MEMBERSHIP],
      });

      await migrate(globals);
      expect(await shown(globals)).toEqual(before);

      expectRebased(await attempt(globals));
      expect(await shown(globals)).toMatchObject(appliedOnce());
    });
  },
);
