/**
 * An intent-journal refusal hold (I1-fix F8c) is a named `/readyz` reason
 * that clears only on that family's next successful record, or once a landed
 * transaction other than the refused one spends one of its inputs (a valid
 * one's input, or a phase-2-failed one's collateral). Nothing else clears it:
 * not an unrelated landed transaction, not the passing of time. Bytes the
 * follower's decoder refuses still name their inputs (the ledger library's
 * decoder reads them); bytes no decoder reads name none.
 *
 * The journal runs over a follower store in the node's own test database, so
 * the refused inputs and the landed spends are the follower's facts. A
 * second journal over the same database plays the main process: it sees a
 * hold only through the table, after `refresh`.
 */
import {
  decodeTransaction,
  type FactStore,
  intentJournalProjection,
  type OutRef,
} from "@al-ft/midgard-l1-follower";
import { encodeSimTx, type SimTx } from "@al-ft/midgard-l1-follower/testing";
import { SqlClient } from "@effect/sql";
import { Effect, Either } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import {
  stateQueueProjection,
  stateQueueTrackedSet,
} from "../src/l1-state-queue/index.js";
import {
  INTENT_CONTENT_REF_MISSING,
  INTENT_UNDECODABLE,
  intentJournalOver,
  type IntentJournalService,
  journaledIntent,
  type NodeIntentFamily,
} from "../src/services/intent-journal.js";
import {
  db,
  openNodeFollowerStore,
} from "./helpers/forced-orders-node-store.js";
import { ChainDriver } from "./helpers/l1-events-store.js";
import {
  QUEUE_ADDRESS,
  SIM_QUEUE_CONFIG,
} from "./helpers/state-queue-sim.fixtures.js";
import { resetApplicationTables } from "./utils.js";

const SPARES = 6;
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

/**
 * A follower store with `SPARES` tracked outputs, and two journals over the
 * node database: `worker` records, `main` only reads the holds.
 */
const open = async () => {
  await db(resetApplicationTables);
  const store = await openNodeFollowerStore(
    [stateQueueProjection(SIM_QUEUE_CONFIG), intentJournalProjection],
    6,
  );
  opened.push(store);
  const driver = new ChainDriver(store, stateQueueTrackedSet(SIM_QUEUE_CONFIG));
  await driver.init();
  const plain = { address: QUEUE_ADDRESS, lovelace: 3_000_000n };
  const [funding] = await driver.forward([
    {
      inputs: [driver.chain.outsideInput()],
      outputs: Array.from({ length: SPARES }, () => plain),
      nonce: driver.chain.nonce(),
    },
  ]);
  const spare = (i: number): OutRef => ({ txHash: funding!, index: i });
  const spend = (inputs: readonly OutRef[], extra: Partial<SimTx> = {}) => {
    const cbor = encodeSimTx({
      inputs,
      outputs: [plain],
      nonce: driver.chain.nonce(),
      ...extra,
    });
    return {
      cbor: Buffer.from(cbor).toString("hex"),
      txHash: decodeTransaction(cbor).hash.toString("hex"),
    };
  };
  const land = (inputs: readonly OutRef[], extra: Partial<SimTx> = {}) =>
    driver.forward([
      { inputs, outputs: [plain], nonce: driver.chain.nonce(), ...extra },
    ]);
  return { spare, spend, land };
};

/** Runs `body` with a recording journal and a main-process journal over the node database. */
const withJournals = <A>(
  body: (
    journals: Readonly<{
      worker: IntentJournalService;
      main: IntentJournalService;
      sql: SqlClient.SqlClient;
    }>,
  ) => Promise<A>,
): Promise<A> =>
  db(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* Effect.promise(() =>
        body({
          worker: intentJournalOver(sql, () => false),
          main: intentJournalOver(sql, () => false),
          sql,
        }),
      );
    }),
  );

/** Records `tx` for `family`; a missing content reference is refused. */
const record = (
  journal: IntentJournalService,
  family: NodeIntentFamily,
  tx: Readonly<{ cbor: string; txHash: string }>,
  contentRef?: Buffer,
) =>
  Effect.runPromise(
    Effect.either(
      Effect.flatMap(journal.openPlan, (plan) =>
        journal.record(
          journaledIntent(family, `${family}:${tx.txHash}`, plan, contentRef),
          tx.cbor,
          tx.txHash,
          { kind: "record_only" },
        ),
      ),
    ),
  );

const reasons = async (main: IntentJournalService) => {
  await Effect.runPromise(main.refresh());
  return main.holds().map(({ reason }) => reason);
};

describe("intent-journal refusal holds", () => {
  it("a family's hold clears on that family's next successful record, and not on another family's", async () => {
    const s = await open();
    await withJournals(async ({ worker, main }) => {
      const refused = await record(worker, "commit", s.spend([s.spare(0)]));
      expect(Either.isLeft(refused) && refused.left).toMatchObject({
        reason: INTENT_CONTENT_REF_MISSING,
      });
      expect(worker.holds().map(({ reason }) => reason)).toEqual([
        INTENT_CONTENT_REF_MISSING,
      ]);
      expect(await reasons(main)).toEqual([INTENT_CONTENT_REF_MISSING]);
      expect(main.holds()[0]?.detail).toContain("commit commit:");

      // Another family's success leaves the commit hold standing.
      const ref = Buffer.alloc(28, 7);
      expect(
        Either.isRight(
          await record(worker, "merge", s.spend([s.spare(1)]), ref),
        ),
      ).toBe(true);
      expect(await reasons(main)).toEqual([INTENT_CONTENT_REF_MISSING]);

      // The commit family's next success clears it, in its own process at
      // once and in the database for the main process.
      expect(
        Either.isRight(
          await record(worker, "commit", s.spend([s.spare(2)]), ref),
        ),
      ).toBe(true);
      expect(worker.holds()).toEqual([]);
      expect(await reasons(main)).toEqual([]);
    });
  });

  it("a hold clears once another landed tx spends the refused tx's input; an unrelated landing and elapsed time do not", async () => {
    const s = await open();
    await withJournals(async ({ worker, main, sql }) => {
      await record(worker, "attest", s.spend([s.spare(0), s.spare(1)]));
      expect(await reasons(main)).toEqual([INTENT_CONTENT_REF_MISSING]);

      // An unrelated landed tx, and a hold a day old: it stands.
      await s.land([s.spare(2)]);
      await Effect.runPromise(
        sql`UPDATE intent_refusal_holds SET raised_at = NOW() - interval '1 day'`,
      );
      expect(await reasons(main)).toEqual([INTENT_CONTENT_REF_MISSING]);
      expect(worker.holds().map(({ reason }) => reason)).toEqual([
        INTENT_CONTENT_REF_MISSING,
      ]);

      // Another tx spends one of the refused tx's inputs and lands.
      await s.land([s.spare(1)]);
      expect(await reasons(main)).toEqual([]);
    });
  });

  it("an undecodable refusal names the inputs the ledger decoder reads, and a landed spend of one clears it", async () => {
    const s = await open();
    await withJournals(async ({ worker, main }) => {
      const tx = s.spend([s.spare(0)]);
      // Trailing bytes: the follower's decoder refuses them, CML reads the tx.
      const refused = await record(
        worker,
        "attest",
        { cbor: `${tx.cbor}00`, txHash: tx.txHash },
        Buffer.alloc(28, 7),
      );
      expect(Either.isLeft(refused) && refused.left).toMatchObject({
        reason: INTENT_UNDECODABLE,
      });
      expect(await reasons(main)).toEqual([INTENT_UNDECODABLE]);
      await s.land([s.spare(1)]);
      expect(await reasons(main)).toEqual([INTENT_UNDECODABLE]);
      await s.land([s.spare(0)]);
      expect(await reasons(main)).toEqual([]);
    });
  });

  it("bytes no decoder reads hold no inputs: only the family's next success clears the hold", async () => {
    const s = await open();
    await withJournals(async ({ worker, main }) => {
      const ref = Buffer.alloc(28, 7);
      const refused = await record(
        worker,
        "attest",
        { cbor: "ff", txHash: "00".repeat(32) },
        ref,
      );
      expect(Either.isLeft(refused) && refused.left).toMatchObject({
        reason: INTENT_UNDECODABLE,
      });
      await s.land([s.spare(0)]);
      expect(await reasons(main)).toEqual([INTENT_UNDECODABLE]);
      expect(
        Either.isRight(
          await record(worker, "attest", s.spend([s.spare(1)]), ref),
        ),
      ).toBe(true);
      expect(await reasons(main)).toEqual([]);
    });
  });

  it("a phase-2-failed tx that lands with the refused tx's input as collateral clears the hold", async () => {
    const s = await open();
    await withJournals(async ({ worker, main }) => {
      await record(worker, "settlement", s.spend([s.spare(0)]));
      expect(await reasons(main)).toEqual([INTENT_CONTENT_REF_MISSING]);
      // A valid tx naming it only as collateral consumes nothing.
      await s.land([s.spare(1)], { collaterals: [s.spare(0)] });
      expect(await reasons(main)).toEqual([INTENT_CONTENT_REF_MISSING]);
      await s.land([s.spare(2)], {
        collaterals: [s.spare(0)],
        isValid: false,
      });
      expect(await reasons(main)).toEqual([]);
    });
  });
});
