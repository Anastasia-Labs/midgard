import { SqlClient } from "@effect/sql";
import { type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import * as MempoolLedgerDB from "midgard-node/database/mempoolLedger";
import {
  type DatabaseError,
  sqlErrorToDatabaseError,
} from "midgard-node/database/utils/common";
import { Database, NodeConfig } from "midgard-node/services/index";

import {
  assertPhase4GenesisLedgerGate,
  decodePhase4GenesisLedgerReport,
  fail,
  PHASE4_GENESIS_LEDGER_SCHEMA,
  PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE,
  Phase4GenesisLedgerError,
  type Phase4GenesisLedgerPlan,
  type Phase4GenesisLedgerReport,
  type Phase4GenesisLedgerRow,
  type Phase4GenesisWalletSummary,
  type Phase4WalletLabel,
  toLedgerRow,
  walletAddress,
} from "./phase4-genesis-ledger.assert-phase4-genesis-ledger-gate.js";

const rowMatches = (
  left: Phase4GenesisLedgerRow,
  right: Phase4GenesisLedgerRow,
): boolean =>
  left.tx_id.equals(right.tx_id) &&
  left.outref.equals(right.outref) &&
  left.output.equals(right.output) &&
  left.address === right.address &&
  left.source_event_id === null &&
  right.source_event_id === null;

/**
 * Seeds the complete configured set used by commit fallback while separately
 * proving that the process gate's A/B wallets are funded.
 */
export const makePhase4GenesisLedgerPlan = ({
  env,
  genesisUtxos,
  genesisUtxosByWallet,
}: {
  readonly env: Readonly<NodeJS.ProcessEnv>;
  readonly genesisUtxos: readonly UTxO[];
  readonly genesisUtxosByWallet: Readonly<{
    A: readonly UTxO[];
    B: readonly UTxO[];
    C: readonly UTxO[];
  }>;
}): Phase4GenesisLedgerPlan => {
  const addresses = {
    A: walletAddress(env, "A"),
    B: walletAddress(env, "B"),
  } as const;
  if (addresses.A === addresses.B) {
    fail("Phase 4 genesis wallets A and B must be distinct");
  }
  for (const label of ["A", "B"] as const) {
    const entries = genesisUtxosByWallet[label];
    if (entries.some((utxo) => utxo.address !== addresses[label])) {
      fail(`Phase 4 configured genesis wallet ${label} has a foreign address`);
    }
  }
  const rows = genesisUtxos.map(toLedgerRow);
  if (
    new Set(rows.map((row) => row.outref.toString("hex"))).size !== rows.length
  ) {
    fail("Phase 4 configured genesis UTxOs contain duplicate outrefs");
  }
  const groupedRows = [
    ...genesisUtxosByWallet.A,
    ...genesisUtxosByWallet.B,
    ...genesisUtxosByWallet.C,
  ].map(toLedgerRow);
  const groupedByOutref = new Map(
    groupedRows.map((row) => [row.outref.toString("hex"), row] as const),
  );
  if (
    groupedRows.length !== rows.length ||
    rows.some((row) => {
      const grouped = groupedByOutref.get(row.outref.toString("hex"));
      return grouped === undefined || !rowMatches(row, grouped);
    })
  ) {
    fail(
      "Phase 4 grouped genesis wallet identity does not match GENESIS_UTXOS",
    );
  }
  const wallets = Object.fromEntries(
    (["A", "B"] as const).map((label) => {
      const entries = genesisUtxosByWallet[label];
      const totalLovelace = entries.reduce(
        (total, utxo) => total + (utxo.assets.lovelace ?? 0n),
        0n,
      );
      if (entries.length === 0) {
        fail(`Phase 4 genesis wallet ${label} has no configured L2 UTxO`);
      }
      if (totalLovelace < PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE) {
        fail(
          `Phase 4 genesis wallet ${label} cannot fund the fixed process-gate transfer`,
        );
      }
      return [
        label,
        { utxoCount: entries.length, totalLovelace: totalLovelace.toString() },
      ];
    }),
  ) as Record<Phase4WalletLabel, Phase4GenesisWalletSummary>;
  return {
    rows,
    wallets,
    supplementalWalletRowCount: genesisUtxosByWallet.C.length,
  };
};

/** Empty may be seeded; only a byte-identical complete set is idempotent. */
export const classifyPhase4GenesisLedgerState = ({
  expected,
  existing,
}: {
  readonly expected: readonly Phase4GenesisLedgerRow[];
  readonly existing: readonly Phase4GenesisLedgerRow[];
}): "seed" | "already_present" => {
  if (expected.length === 0) {
    return fail("Phase 4 genesis ledger plan must not be empty");
  }
  if (existing.length === 0) return "seed";
  if (existing.length !== expected.length) {
    return fail(
      "Phase 4 mempool ledger is partial or contains non-genesis state; refusing bootstrap",
    );
  }
  const expectedByOutref = new Map(
    expected.map((row) => [row.outref.toString("hex"), row] as const),
  );
  if (
    existing.some((row) => {
      const expectedRow = expectedByOutref.get(row.outref.toString("hex"));
      return expectedRow === undefined || !rowMatches(row, expectedRow);
    })
  ) {
    return fail(
      "Phase 4 mempool ledger does not exactly match the complete configured genesis state",
    );
  }
  return "already_present";
};

const attempt = <A>(
  operation: () => A,
): Effect.Effect<A, Phase4GenesisLedgerError> =>
  Effect.try({
    try: operation,
    catch: (cause) =>
      cause instanceof Phase4GenesisLedgerError
        ? cause
        : new Phase4GenesisLedgerError({
            message: "Phase 4 genesis ledger validation failed",
            cause,
          }),
  });

const readLedgerRows = (sql: SqlClient.SqlClient) =>
  sql<Phase4GenesisLedgerRow>`SELECT
    ${sql(MempoolLedgerDB.Columns.TX_ID)},
    ${sql(MempoolLedgerDB.Columns.OUTREF)},
    ${sql(MempoolLedgerDB.Columns.OUTPUT)},
    ${sql(MempoolLedgerDB.Columns.ADDRESS)},
    ${sql(MempoolLedgerDB.Columns.SOURCE_EVENT_ID)}
    FROM ${sql(MempoolLedgerDB.tableName)}
    ORDER BY ${sql(MempoolLedgerDB.Columns.OUTREF)}`;

export const phase4GenesisLedgerProgram = ({
  mode,
  env = process.env,
}: {
  readonly mode: "seed" | "verify";
  readonly env?: Readonly<NodeJS.ProcessEnv>;
}): Effect.Effect<
  Phase4GenesisLedgerReport,
  Phase4GenesisLedgerError | DatabaseError,
  NodeConfig | Database
> =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    yield* attempt(() => assertPhase4GenesisLedgerGate({ env, config }));
    const genesisUtxosByWallet = yield* attempt(() => {
      const configured = config.GENESIS_UTXOS_BY_WALLET;
      if (configured === undefined) {
        return fail(
          "Phase 4 node configuration does not preserve genesis wallet identity",
        );
      }
      return configured;
    });
    const plan = yield* attempt(() =>
      makePhase4GenesisLedgerPlan({
        env,
        genesisUtxos: config.GENESIS_UTXOS,
        genesisUtxosByWallet,
      }),
    );
    const sql = yield* SqlClient.SqlClient;
    const status = yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`LOCK TABLE ${sql(
          MempoolLedgerDB.tableName,
        )} IN ACCESS EXCLUSIVE MODE`;
        const before = yield* readLedgerRows(sql);
        const disposition = yield* attempt(() =>
          classifyPhase4GenesisLedgerState({
            expected: plan.rows,
            existing: before,
          }),
        );
        if (mode === "verify" && disposition === "seed") {
          return yield* Effect.fail(
            new Phase4GenesisLedgerError({
              message:
                "Phase 4 configured genesis ledger is absent from the matched snapshot",
            }),
          );
        }
        if (disposition === "seed") {
          yield* sql`INSERT INTO ${sql(MempoolLedgerDB.tableName)} ${sql.insert(
            plan.rows,
          )}`;
        }
        const after = yield* readLedgerRows(sql);
        yield* attempt(() =>
          classifyPhase4GenesisLedgerState({
            expected: plan.rows,
            existing: after,
          }),
        );
        return disposition === "seed" ? "seeded" : "already_present";
      }),
    );
    const report = {
      schemaVersion: PHASE4_GENESIS_LEDGER_SCHEMA,
      satisfied: true,
      mode,
      status,
      rowCount: plan.rows.length,
      wallets: plan.wallets,
      supplementalWalletRowCount: plan.supplementalWalletRowCount,
      minimumTransferLovelace:
        PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE.toString(),
    } satisfies Phase4GenesisLedgerReport;
    return yield* attempt(() => decodePhase4GenesisLedgerReport(report));
  }).pipe(
    sqlErrorToDatabaseError(
      MempoolLedgerDB.tableName,
      "Failed to seed or verify the Phase 4 genesis ledger",
    ),
  );
