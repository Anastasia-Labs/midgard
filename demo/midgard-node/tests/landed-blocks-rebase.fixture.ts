/**
 * The landed-block rebase test's fixture (plan §7.3, N3): a frontier
 * ledger and one processed foreign block on it, a modelled native MPF owner
 * and its faults, one node process's globals, one rebase attempt as the
 * follower-change driver runs it (`rebaseIfDue` of its recompute) and what
 * the node shows after it.
 */
import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { expect } from "vitest";

import { ConfirmedLedgerDB } from "../src/database/index.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import { LANDED_BLOCK_REBASE_FAILED } from "../src/landed-blocks/holds.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import { rebasePlan } from "../src/landed-blocks/rebase-target.js";
import {
  Frontier,
  insertRow,
  type LandedBlockRow,
  retrieveRows,
} from "../src/landed-blocks/store.js";
import type { NodeConfig } from "../src/services/config.js";
import type { Database } from "../src/services/database.js";
import { Globals } from "../src/services/globals.js";
import { currentLivenessReasons } from "../src/services/globals.liveness-reasons.js";
import {
  type NativeMpfOwnerService,
  NativeMpfRootNotRetained,
} from "../src/services/mpf-native-owner/protocol.js";
import type { WriteBehind } from "../src/services/write-behind.js";
import { testDriverRecompute, testWrite } from "./helpers/driver-recompute.js";
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
  effect: Effect.Effect<A, E, Database | NodeConfig | Globals | WriteBehind>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(effect.pipe(Effect.provideService(Globals, globals))),
  );

/** `confirmed_ledger` at the frontier holds `E0`; the processed foreign block spends it for `E1`. */
export const seed = async (
  globals: Globals,
  row: Partial<LandedBlockRow> = {},
) => {
  // The reset leaves the gate unapplied, so the seed runs as a fixture.
  await run(globals, resetApplicationTables);
  await run(
    globals,
    testWrite(
      Effect.gen(function* () {
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
  await run(globals, testWrite(Effect.flatMap(SqlClient.SqlClient, work)));
};

/** What the node shows after one rebase attempt of the driver. */
export const attempt = async (globals: Globals) => {
  const hold = await run(
    globals,
    Effect.flatMap(testDriverRecompute(), (recompute) =>
      recompute.rebaseIfDue("landed-block rebase test"),
    ),
  );
  return {
    hold,
    held: hold?.reason,
    failure: hold?.detail,
    reasons: await Effect.runPromise(currentLivenessReasons(globals)),
    due: (await run(globals, rebasePlan)).kind,
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
  // The driver run returns its hold; nothing fails the process.
  expect(shown.failure).toMatch(detail);
  expect(shown.held).toBe(reason);
  expect(shown.reasons).toContain(reason);
  expect(shown.due).not.toBe("none");
  expect(shown.applied).toEqual([false]);
};

export const expectRebased = (shown: Awaited<ReturnType<typeof attempt>>) => {
  expect(shown.hold).toBeUndefined();
  expect(shown.reasons).not.toContain(LANDED_BLOCK_REBASE_FAILED);
  expect(shown.due).toBe("none");
  expect(shown.applied).toEqual([true]);
  expect(shown.working).toContain(hex(E1.outref));
};

export const freshNative = (): Native => ({
  durableRoot: R0,
  retainsNothing: false,
  reaches: undefined,
});

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

/** The recorded rejection causes: sorted `[rejected tx id, cause tx id]` pairs. */
export const rejectionCauses = (globals: Globals) =>
  run(
    globals,
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{ tx_id: Buffer; cause_tx_id: Buffer }>`
        SELECT tx_id, cause_tx_id FROM tx_rejection_causes`,
    ),
  ).then((rows) =>
    rows.map((row) => [hex(row.tx_id), hex(row.cause_tx_id)]).sort(),
  );

/**
 * A processed row, the seeded block's fields under `row`: foreign by default;
 * an own one is applied, as an own block processed with its journal live.
 */
export const landedRow = (row: Partial<LandedBlockRow>): LandedBlockRow => ({
  headerHash: BLOCK,
  parentHeaderHash: FRONTIER,
  parentUtxosRoot: R0,
  utxosRoot: R1,
  kind: "foreign",
  state: "processed",
  applied: row.kind === "own",
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
