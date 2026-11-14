/**
 * Commit-stage rejection with the working ledger's rejection closure, on the
 * node database (with the landed-block rebase fixture):
 *
 * - a commit-stage rejection rejects the transaction and every pending
 *   transaction that spends its outputs ("dependent"), reverts their ledger
 *   effects and records what each dependent follows from, in one
 *   transaction; a pending transaction outside the closure stays;
 * - a later rebase then runs over what is left;
 * - a block member the closure reaches leaves the block: Phase B runs again
 *   without it, and it is recorded rejected with the rest.
 */

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import * as TxRejectionsDB from "../src/database/txRejections.js";
import {
  COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT,
  COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT,
} from "../src/mpf/commit-rejection.js";
import {
  persistCommitStageRejectedTransactions,
  settleCommitStageRejections,
} from "../src/mpf/commit-rejection.persist-commit-stage-rejected-transactions.js";
import { admitPending } from "./helpers/landed-blocks-sim.mempool.js";
import {
  attempt,
  E0,
  expectRebased,
  freshNative,
  hex,
  pendingTx,
  processOf,
  rejectionCauses,
  rejections,
  run,
  seed,
} from "./landed-blocks-rebase.fixture.js";

type Globals = Awaited<ReturnType<typeof processOf>>;

const directRejection = (txId: Buffer) => ({
  [TxRejectionsDB.Columns.TX_ID]: txId,
  [TxRejectionsDB.Columns.REJECT_CODE]:
    COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT,
  [TxRejectionsDB.Columns.REJECT_DETAIL]: "withdrawn input",
});

const sorted = (pairs: readonly (readonly [Buffer, string])[]) =>
  pairs
    .map(([id, code]) => [hex(id), code])
    .sort(([left], [right]) => (left! < right! ? -1 : 1));

const pendingIds = (globals: Globals) =>
  run(
    globals,
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{ tx_id: Buffer }>`
        SELECT tx_id FROM mempool UNION SELECT tx_id FROM processed_mempool`,
    ),
  ).then((rows) => rows.map((row) => hex(row.tx_id)).sort());

const workingOutRefs = (globals: Globals) =>
  run(
    globals,
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{ outref: Buffer }>`SELECT outref FROM mempool_ledger`,
    ),
  ).then((rows) => rows.map((row) => hex(row.outref)).sort());

/**
 * The frontier holds `E0`, and a processed foreign block spends it. Pending:
 * `a` spends `E0`, `b` spends nothing, `c` spends `b`'s output.
 */
const threePending = async () => {
  const globals = await processOf(freshNative());
  await seed(globals);
  const a = pendingTx("a", [E0.outref], 1);
  const b = pendingTx("b", [], 2);
  const c = pendingTx("c", [b.produced[0]!.outref], 3);
  await run(globals, admitPending([a, b, c]));
  return { globals, a, b, c };
};

describe(
  "a commit-stage rejection closes over its dependents",
  { concurrent: false },
  () => {
    it("rejects the transaction and its dependent, reverts their ledger effects and leaves the rest pending", async () => {
      const { globals, a, b, c } = await threePending();
      const outcome = await run(
        globals,
        persistCommitStageRejectedTransactions({
          rejectionEntries: [directRejection(b.id)],
          resolveInputPostState: () => undefined,
        }),
      );
      expect(outcome).toMatchObject({ _tag: "Persisted", ledgerChanged: true });
      expect(await rejections(globals)).toEqual(
        sorted([
          [b.id, COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT],
          [c.id, COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT],
        ]),
      );
      // The dependent is traced to `b`; `b`'s own rejection is not traced
      // to another transaction.
      expect(await rejectionCauses(globals)).toEqual([[hex(c.id), hex(b.id)]]);
      expect(await pendingIds(globals)).toEqual([hex(a.id)]);
      // `a`'s output stays; no output of the closure is left.
      expect(await workingOutRefs(globals)).toEqual([
        hex(a.produced[0]!.outref),
      ]);
    });

    it("lets a later rebase reject what the landed block spent", async () => {
      const { globals, b } = await threePending();
      await run(
        globals,
        persistCommitStageRejectedTransactions({
          rejectionEntries: [directRejection(b.id)],
          resolveInputPostState: () => undefined,
        }),
      );
      // The foreign block spends `E0`, the input of `a`.
      expectRebased(await attempt(globals));
    });

    it("takes a block member the closure reaches out of the block and runs Phase B again", async () => {
      const { globals, a, b, c } = await threePending();
      const candidate = (txId: Buffer) => ({ ledgerTx: { txId } });
      const evaluated: string[][] = [];
      const settled = await run(
        globals,
        settleCommitStageRejections({
          candidates: [candidate(a.id), candidate(c.id)],
          evaluate: (candidates) =>
            Effect.sync(() => {
              evaluated.push(
                candidates.map(({ ledgerTx }) => hex(ledgerTx.txId)),
              );
              return { accepted: candidates, rejected: [] };
            }),
          rejectionEntries: [directRejection(b.id)],
          resolveInputPostState: () => () => undefined,
        }),
      );
      // `c` leaves; Phase B runs again over `a` alone.
      expect(evaluated).toEqual([[hex(a.id), hex(c.id)], [hex(a.id)]]);
      expect(
        settled.accepted.map(({ ledgerTx }) => hex(ledgerTx.txId)),
      ).toEqual([hex(a.id)]);
      expect(
        sorted(
          settled.rejectionEntries.map((entry) => [
            entry[TxRejectionsDB.Columns.TX_ID],
            entry[TxRejectionsDB.Columns.REJECT_CODE],
          ]),
        ),
      ).toEqual(sorted([[c.id, COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT]]));
      expect(await rejections(globals)).toEqual(
        sorted([
          [b.id, COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT],
          [c.id, COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT],
        ]),
      );
    });
  },
);
