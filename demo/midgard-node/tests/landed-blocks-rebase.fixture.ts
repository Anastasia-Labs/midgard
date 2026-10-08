/**
 * The landed-block rebase test's fixture (plan §7.3, N3): a frontier
 * ledger and one processed foreign block on it, a modelled native MPF owner
 * and its faults, one node process's globals, the owner's recovery
 * authority, one rebase attempt and what the node shows after it.
 */
import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect, Exit, Ref } from "effect";
import { expect } from "vitest";

import { ConfirmedLedgerDB } from "../src/database/index.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import {
  LANDED_BLOCK_BATCH_UNDECIDED,
  LANDED_BLOCK_REBASE_FAILED,
} from "../src/landed-blocks/holds.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import {
  landedBlockRebaseDisposition,
  prepareLandedBlockRebase,
} from "../src/landed-blocks/rebase.js";
import {
  Frontier,
  insertRow,
  type LandedBlockRow,
  retrieveRows,
} from "../src/landed-blocks/store.js";
import type { NodeConfig } from "../src/services/config.js";
import type { Database } from "../src/services/database.js";
import { withHistoryWrite } from "../src/services/event-history-producer.js";
import type { HistoryRecoveryPreparation } from "../src/services/event-history-recovery.js";
import { Globals } from "../src/services/globals.js";
import { currentLivenessReasons } from "../src/services/globals.liveness-reasons.js";
import { Lucid } from "../src/services/lucid.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import {
  type NativeMpfOwnerService,
  NativeMpfRootNotRetained,
} from "../src/services/mpf-native-owner/protocol.js";
import { simDigest, simOutput } from "./helpers/landed-blocks-sim.universe.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

export const entry = (
  label: string,
  lovelace: bigint,
): Ledger.MinimalEntry => ({
  outref: makeOutRefCbor(simDigest(`rebase:${label}`), 0),
  output: simOutput(lovelace),
});

export const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");
export const root = (n: number) => n.toString(16).padStart(2, "0").repeat(32);

export const E0 = entry("e0", 2_000_000n);
export const E1 = entry("e1", 3_000_000n);
export const FRONTIER = "f0".repeat(28);
export const BLOCK = "b1".repeat(28);
export const R0 = root(0x10);
export const R1 = root(0x11);

/** The modelled native MPF owner: a durable root, and the faults it shows. */
export type Native = {
  durableRoot: string;
  /** Restores refuse every root as not retained. */
  retainsNothing: boolean;
  /** The root an applied delta reaches, if not the expected one. */
  reaches: string | undefined;
};

export const nativeOwner = (native: Native) =>
  ({
    diagnostics: async () => ({ durableRoot: native.durableRoot }),
    restoreCanonicalRoot: async ({ targetRoot }: { targetRoot: string }) => {
      if (native.retainsNothing) throw new NativeMpfRootNotRetained(targetRoot);
      native.durableRoot = targetRoot;
    },
    fork: async (base: string) => ({ base }),
    applyEvents: async () => ({ candidateRoot: native.reaches ?? R1 }),
    promote: async () => {
      native.durableRoot = native.reaches ?? R1;
    },
    discard: async () => undefined,
  }) as unknown as NativeMpfOwnerService;

/** One node process: its globals, with the modelled native owner. */
export const processOf = async (native: Native) => {
  const globals = await Effect.runPromise(
    Effect.provide(Globals, Globals.Default),
  );
  await Effect.runPromise(
    Ref.set(globals.NATIVE_MPF_OWNER, nativeOwner(native)),
  );
  return globals;
};

export const run = <A, E>(
  globals: Globals,
  effect: Effect.Effect<A, E, Database | NodeConfig | Globals>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(effect.pipe(Effect.provideService(Globals, globals))),
  );

export const DEPLOYMENT = "de".repeat(32);

/** The owner's recovery authority, as its preparation holds it. */
export const recovering = async (globals: Globals) => {
  const token = {
    deploymentIdentity: DEPLOYMENT,
    ownerToken: randomUUID(),
    generation: "0",
  };
  await run(
    globals,
    Effect.flatMap(SqlClient.SqlClient, (sql) =>
      sql`DELETE FROM event_history_authority`.pipe(
        Effect.zipRight(sql`INSERT INTO event_history_authority
          (deployment_identity, owner_token, generation, state, reason, lease_until)
          VALUES (${Buffer.from(DEPLOYMENT, "hex")}, ${token.ownerToken}::uuid, 0,
            'recovering', 'landed-block rebase test',
            clock_timestamp() + interval '1 hour')`),
      ),
    ),
  );
  return {
    token,
    assertCurrent: Effect.void,
  } satisfies HistoryRecoveryPreparation;
};

/**
 * The test's own writes go through the unowned-history fixture gate, which
 * refuses while an owner holds the authority: the owner steps away first.
 */
export const released = (globals: Globals) =>
  run(
    globals,
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql`DELETE FROM event_history_authority`,
    ),
  );

/** `confirmed_ledger` at the frontier holds `E0`; the processed foreign block spends it for `E1`. */
export const seed = async (
  globals: Globals,
  row: Partial<LandedBlockRow> = {},
) => {
  await released(globals);
  await run(
    globals,
    withHistoryWrite(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        yield* ConfirmedLedgerDB.insertMultiple([
          ...(yield* ledgerRows([E0], new Map())),
        ]);
        yield* Frontier.upsert({ headerHash: FRONTIER, utxosRoot: R0 });
        yield* insertRow({
          headerHash: BLOCK,
          parentHeaderHash: FRONTIER,
          parentUtxosRoot: R0,
          utxosRoot: R1,
          kind: "foreign",
          state: "processed",
          applied: false,
          spent: [E0.outref],
          produced: [E1],
          depositIds: [],
          withdrawals: [],
          forcedIds: [],
          txIds: [],
          ...row,
        });
      }),
    ),
  );
};

export const sqlRun = async (
  globals: Globals,
  work: (
    sql: SqlClient.SqlClient,
  ) => Effect.Effect<unknown, unknown, Database | NodeConfig | Globals>,
) => {
  await released(globals);
  await run(
    globals,
    withHistoryWrite(Effect.flatMap(SqlClient.SqlClient, work)),
  );
};

/** What the node shows after one rebase attempt. */
export const attempt = async (globals: Globals) => {
  const preparation = await recovering(globals);
  const exit = await Effect.runPromiseExit(
    provideDatabaseLayers(
      prepareLandedBlockRebase(preparation).pipe(
        Effect.provideService(Globals, globals),
        // Reached only to start a native owner; the modelled one is running.
        Effect.provideService(Lucid, {} as Lucid),
        Effect.provideService(MidgardContracts, {} as MidgardContracts),
      ),
    ),
  );
  const held = await Effect.runPromise(
    Ref.get(globals.LANDED_BLOCK_REBASE_FAILURE),
  );
  return {
    exit,
    held: held?.reason,
    failure: held?.detail,
    reasons: await Effect.runPromise(currentLivenessReasons(globals)),
    disposition: await run(globals, landedBlockRebaseDisposition),
    applied: (await run(globals, retrieveRows)).map((row) => row.applied),
    working: (
      await run(
        globals,
        Effect.flatMap(
          SqlClient.SqlClient,
          (sql) => sql<{ outref: Buffer }>`SELECT outref FROM mempool_ledger`,
        ),
      )
    ).map((row) => hex(row.outref)),
  };
};

export const expectHeld = (
  shown: Awaited<ReturnType<typeof attempt>>,
  detail: RegExp,
  reason: string = LANDED_BLOCK_REBASE_FAILED,
) => {
  // The preparation returns: the owner is not failed by it.
  expect(Exit.isSuccess(shown.exit)).toBe(true);
  expect(shown.failure).toMatch(detail);
  expect(shown.held).toBe(reason);
  expect(shown.reasons).toContain(reason);
  expect(shown.disposition?.status).toBe("pending");
  expect(shown.applied).toEqual([false]);
};

export const expectRebased = (shown: Awaited<ReturnType<typeof attempt>>) => {
  expect(Exit.isSuccess(shown.exit)).toBe(true);
  expect(shown.failure).toBeUndefined();
  expect(shown.reasons).not.toContain(LANDED_BLOCK_REBASE_FAILED);
  expect(shown.reasons).not.toContain(LANDED_BLOCK_BATCH_UNDECIDED);
  expect(shown.disposition).toBeUndefined();
  expect(shown.applied).toEqual([true]);
  expect(shown.working).toContain(hex(E1.outref));
};

export const freshNative = (): Native => ({
  durableRoot: R0,
  retainsNothing: false,
  reaches: undefined,
});

export const BINDING = Buffer.alloc(32, 0x42);

/** One unreversed acceptance receipt holding `txIds`. */
export const receipt = (globals: Globals, txIds: readonly Buffer[]) =>
  sqlRun(globals, (sql) =>
    Effect.gen(function* () {
      const digest = Buffer.alloc(32, 0x43);
      yield* sql`INSERT INTO event_history_cursor (binding_digest, manifest_id,
          origin_receipt, origin_receipt_digest, anchor_hash, anchor_slot,
          anchor_height, anchor_snapshot_digest, head_hash, head_slot,
          head_height, head_application_revision, snapshot_digest, revision,
          addresses)
        VALUES (${BINDING}, ${Buffer.from(DEPLOYMENT, "hex")}, 'origin',
          ${digest}, ${digest}, 0, 0, ${digest}, ${digest}, 0, 0, NULL,
          ${digest}, 0, '[]'::jsonb)
        ON CONFLICT (binding_digest) DO NOTHING`;
      yield* sql`INSERT INTO event_history_l2_ledger_receipts (binding_digest,
          owner_generation, checkpoint_revision, head_hash, snapshot_digest,
          tx_ids, reference_outrefs, ledger_before, reference_before,
          deposits_before, payloads_before)
        VALUES (${BINDING}, 0, 0, ${digest}, ${digest},
          ${(sql as unknown as { array: (v: string[]) => unknown }).array(txIds.map((id) => `\\x${hex(id)}`)) as never}::bytea[],
          '{}'::bytea[], '[]'::jsonb, '[]'::jsonb, '[]'::jsonb, '[]'::jsonb)`;
    }),
  );

/** A pending transaction spending `spent`, admitted at `at` seconds. */
export const pendingTx = (
  label: string,
  spent: readonly Buffer[],
  at: number,
) => ({
  id: simDigest(`rebase:tx:${label}`),
  spent,
  produced: [
    {
      outref: makeOutRefCbor(simDigest(`rebase:tx:${label}`), 0),
      output: simOutput(5_000_000n + BigInt(at)),
    },
  ],
  at: new Date(Date.parse("2026-10-01T00:00:00.000Z") + at * 1_000),
});

export const rejections = (globals: Globals) =>
  run(
    globals,
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{ tx_id: Buffer; reject_code: string }>`
        SELECT tx_id, reject_code FROM tx_rejections ORDER BY tx_id`,
    ),
  ).then((rows) => rows.map((row) => [hex(row.tx_id), row.reject_code]));

/** The receipt settlements on record: `[tx id, settling header]` pairs. */
export const settlements = (globals: Globals) =>
  run(
    globals,
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{ tx_id: Buffer; settled_by: Buffer }>`
        SELECT tx_id, settled_by
        FROM event_history_l2_ledger_receipt_settlements ORDER BY tx_id`,
    ),
  ).then((rows) => rows.map((row) => [hex(row.tx_id), hex(row.settled_by)]));

/** The receipts no rejection has reversed. */
export const unreversedReceipts = (globals: Globals) =>
  run(
    globals,
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql`SELECT 1 FROM event_history_l2_ledger_receipts
        WHERE reversed_at_revision IS NULL`,
    ),
  ).then((rows) => rows.length);

/** A processed foreign row, the seeded block's fields under `row`. */
export const landedRow = (row: Partial<LandedBlockRow>): LandedBlockRow => ({
  headerHash: BLOCK,
  parentHeaderHash: FRONTIER,
  parentUtxosRoot: R0,
  utxosRoot: R1,
  kind: "foreign",
  state: "processed",
  applied: false,
  spent: [E0.outref],
  produced: [E1],
  depositIds: [],
  withdrawals: [],
  forcedIds: [],
  txIds: [],
  ...row,
});

/** Inserts the processed row `row` (the seeded block's fields by default). */
export const land = (globals: Globals, row: Partial<LandedBlockRow>) =>
  sqlRun(globals, () => insertRow(landedRow(row)));
