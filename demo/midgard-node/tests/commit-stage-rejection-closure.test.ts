/**
 * Commit-stage rejection with the working ledger's rejection closure, on the
 * node database (with the landed-block rebase fixture):
 *
 * - a commit-stage rejection of one member of a batch rejects every pending
 *   co-member ("batch"), reverts their ledger effects and reverses the
 *   receipt, in one transaction;
 * - a later rebuild then meets no unreversed receipt holding the rejected
 *   member;
 * - a block member the closure reaches leaves the block: Phase B runs again
 *   without it, and it is recorded rejected with the rest.
 */

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import * as TxRejectionsDB from "../src/database/txRejections.js";
import {
  COMMIT_REJECT_CODE_BATCH_MEMBER,
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
  receipt,
  rejections,
  run,
  seed,
  unreversedReceipts,
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
 * `a` spends `E0`, `b` spends nothing, `c` spends `b`'s output; `a` and `b`
 * were accepted in one batch.
 */
const batchOfTwo = async () => {
  const globals = await processOf(freshNative());
  await seed(globals);
  const a = pendingTx("a", [E0.outref], 1);
  const b = pendingTx("b", [], 2);
  const c = pendingTx("c", [b.produced[0]!.outref], 3);
  await run(globals, admitPending([a, b, c]));
  await receipt(globals, [a.id, b.id]);
  return { globals, a, b, c };
};

describe(
  "a commit-stage rejection closes over the batch",
  { concurrent: false },
  () => {
    it("rejects every co-member of a rejected member's batch, reverts their ledger effects and reverses the receipt", async () => {
      const { globals, a, b, c } = await batchOfTwo();
      const outcome = await run(
        globals,
        persistCommitStageRejectedTransactions({
          rejectionEntries: [directRejection(b.id)],
          // The block leaves `E0` unspent.
          resolveInputPostState: (outRef) =>
            outRef === hex(E0.outref) ? E0.output : undefined,
        }),
      );
      expect(outcome).toMatchObject({ _tag: "Persisted", ledgerChanged: true });
      expect(await rejections(globals)).toEqual(
        sorted([
          [a.id, COMMIT_REJECT_CODE_BATCH_MEMBER],
          [b.id, COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT],
          [c.id, COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT],
        ]),
      );
      expect(await unreversedReceipts(globals)).toBe(0);
      expect(await pendingIds(globals)).toEqual([]);
      // `a`'s input is back; no output of the closure is left.
      expect(await workingOutRefs(globals)).toEqual([hex(E0.outref)]);
    });

    it("lets a later rebuild that rejects the other member rebase", async () => {
      const { globals, b } = await batchOfTwo();
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
      const { globals, a, b, c } = await batchOfTwo();
      const candidate = (txId: Buffer) => ({ ledgerTx: { txId } });
      const evaluated: string[][] = [];
      const settled = await run(
        globals,
        settleCommitStageRejections({
          candidates: [candidate(b.id), candidate(c.id)],
          evaluate: (candidates) =>
            Effect.sync(() => {
              evaluated.push(
                candidates.map(({ ledgerTx }) => hex(ledgerTx.txId)),
              );
              return { accepted: candidates, rejected: [] };
            }),
          rejectionEntries: [directRejection(a.id)],
          resolveInputPostState: () => () => undefined,
        }),
      );
      // Both candidates leave; no candidate is left to evaluate.
      expect(evaluated).toEqual([[hex(b.id), hex(c.id)]]);
      expect(settled.accepted).toEqual([]);
      expect(
        sorted(
          settled.rejectionEntries.map((entry) => [
            entry[TxRejectionsDB.Columns.TX_ID],
            entry[TxRejectionsDB.Columns.REJECT_CODE],
          ]),
        ),
      ).toEqual(
        sorted([
          [b.id, COMMIT_REJECT_CODE_BATCH_MEMBER],
          [c.id, COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT],
        ]),
      );
      expect(await rejections(globals)).toEqual(
        sorted([
          [a.id, COMMIT_REJECT_CODE_WITHDRAWN_REFERENCE_INPUT],
          [b.id, COMMIT_REJECT_CODE_BATCH_MEMBER],
          [c.id, COMMIT_REJECT_CODE_SPENDS_REJECTED_OUTPUT],
        ]),
      );
      expect(await unreversedReceipts(globals)).toBe(0);
    });
  },
);
