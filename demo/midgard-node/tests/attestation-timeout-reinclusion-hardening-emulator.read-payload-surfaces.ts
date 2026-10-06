import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { expect } from "vitest";

import * as Pending from "../src/database/pendingBlockFinalizations.js";
import {
  C,
  captureStoredState,
  nativeRoot,
  type Scenario,
} from "./attestation-timeout-reinclusion-hardening-emulator.inspect-with-foreign-submission.js";
import {
  read,
  readRecoveryPlans,
} from "./helpers/correction-rewind-scenario.js";

/** Nothing moved: no plan, the same journals, native root and deposits. */
export const expectNoRewind = async (
  scenario: Scenario,
  snapshot: Awaited<ReturnType<typeof captureState>>,
) => {
  expect(await readRecoveryPlans()).toEqual([]);
  expect(await captureState(scenario)).toEqual(snapshot);
};

export const captureState = async (scenario: Scenario) => ({
  native: await nativeRoot(scenario.h),
  ...(await captureStoredState(scenario)),
});

export const hex = (value: Buffer) => value.toString("hex");

/** Every local surface a removed block's payloads live on. */
export const readPayloadSurfaces = (input: {
  readonly headerHash: string;
  readonly txIds: readonly Buffer[];
  readonly outRefs: readonly Buffer[];
  readonly forcedEventId: Buffer;
  readonly withdrawalEventId: Buffer;
}) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const header = Buffer.from(input.headerHash, "hex");
      const blockTxs = yield* sql<{ tx_id: Buffer }>`
        SELECT tx_id FROM blocks WHERE header_hash = ${header}
        ORDER BY tx_id`;
      const immutable = yield* sql<{ tx_id: Buffer }>`
        SELECT tx_id FROM immutable WHERE tx_id IN ${sql.in([...input.txIds])}`;
      const mempool = yield* sql<{ tx_id: Buffer }>`
        SELECT tx_id FROM mempool`;
      const processed = yield* sql<{ tx_id: Buffer }>`
        SELECT tx_id FROM processed_mempool`;
      const rejections = yield* sql<{ tx_id: Buffer; reject_code: string }>`
        SELECT tx_id, reject_code FROM tx_rejections
        WHERE tx_id IN ${sql.in([...input.txIds])}`;
      const ledger = yield* sql<{ outref: Buffer }>`
        SELECT outref FROM mempool_ledger
        WHERE outref IN ${sql.in([...input.outRefs])}`;
      const forced = yield* sql<{
        status: string;
        projected_header_hash: Buffer | null;
      }>`SELECT status, projected_header_hash FROM forced_transaction_utxos
        WHERE tx_order_id = ${input.forcedEventId}`;
      const withdrawal = yield* sql<{
        status: string;
        projected_header_hash: Buffer | null;
      }>`SELECT status, projected_header_hash FROM withdrawal_utxos
        WHERE event_id = ${input.withdrawalEventId}`;
      return {
        blockTxs: blockTxs.map((row) => hex(row.tx_id)),
        immutable: immutable.map((row) => hex(row.tx_id)).sort(),
        mempool: new Set(mempool.map((row) => hex(row.tx_id))),
        processed: new Set(processed.map((row) => hex(row.tx_id))),
        rejections: Object.fromEntries(
          rejections.map((row) => [hex(row.tx_id), row.reject_code]),
        ),
        ledger: new Set(ledger.map((row) => hex(row.outref))),
        forced: forced.map((row) => ({
          status: row.status,
          header: row.projected_header_hash?.toString("hex") ?? null,
        })),
        withdrawal: withdrawal.map((row) => ({
          status: row.status,
          header: row.projected_header_hash?.toString("hex") ?? null,
        })),
      };
    }),
  );

/** Another deployment's manifest id of the same shape. */
export const foreignManifest = (record: Pending.Record) =>
  (record[C.DEPLOYMENT_MANIFEST_ID].startsWith("0") ? "1" : "0").concat(
    record[C.DEPLOYMENT_MANIFEST_ID].slice(1),
  );

/** Admit the removal of a one-block scenario's only block at release depth. */
export const admitOnlyRemoval = async (scenario: Scenario) => {
  const [removedHeader] = scenario.headers as [string];
  const removal = await scenario.removeTail(removedHeader);
  await scenario.awaitRemovalFinality();
  expect(
    (await scenario.tick(scenario.h.globals)).admittedTransactionHashes,
  ).toEqual([removal.accepted.transaction.txHash]);
  return removedHeader;
};

export const openNativeOwner = async (handle: Scenario["h"]) => {
  const owner = await Effect.runPromise(
    Ref.get(handle.globals.NATIVE_MPF_OWNER),
  );
  if (owner === undefined) throw new Error("Native owner is not open");
  return owner;
};

export const INJECTED_PLAN_FAILURE =
  "injected crash while marking the rewind applied";

/** A database fault at the plan's final state change, inside the repair's own
 * transaction. */
export const refusePlanApplication = (refuse: boolean) =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      if (!refuse) {
        yield* sql`DROP TRIGGER IF EXISTS midgard_test_refuse_plan_applied
          ON event_history_recovery_plans`;
        yield* sql`DROP FUNCTION IF EXISTS midgard_test_refuse_plan_applied()`;
        return;
      }
      yield* sql.unsafe(`CREATE OR REPLACE FUNCTION midgard_test_refuse_plan_applied()
        RETURNS trigger LANGUAGE plpgsql AS $$
        BEGIN RAISE EXCEPTION '${INJECTED_PLAN_FAILURE}'; END $$`);
      yield* sql.unsafe(`CREATE TRIGGER midgard_test_refuse_plan_applied
        BEFORE UPDATE ON event_history_recovery_plans FOR EACH ROW
        WHEN (NEW.state = 'applied' AND OLD.state = 'prepared')
        EXECUTE FUNCTION midgard_test_refuse_plan_applied()`);
    }),
  );
