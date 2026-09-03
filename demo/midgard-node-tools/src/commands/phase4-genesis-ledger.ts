import { isAbsolute } from "node:path";

import { outRefToCbor } from "@al-ft/lucid-midgard";
import { SqlClient } from "@effect/sql";
import {
  type Address,
  type UTxO,
  utxoToCore,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Data, Effect } from "effect";
import * as MempoolLedgerDB from "midgard-node/database/mempoolLedger";
import {
  type DatabaseError,
  sqlErrorToDatabaseError,
} from "midgard-node/database/utils/common";
import * as Ledger from "midgard-node/database/utils/ledger";
import { exactObjectKeys } from "midgard-node/exact-object-keys";
import {
  Database,
  NodeConfig,
  type NodeConfigDep,
} from "midgard-node/services/index";

export const PHASE4_GENESIS_BOOTSTRAP_TOKEN =
  "phase4-local-devnet-l2-genesis-v1";
export const PHASE4_GENESIS_BOOTSTRAP_ENV = "MIDGARD_PHASE4_GENESIS_BOOTSTRAP";
export const PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE = 50_000n;
export const PHASE4_GENESIS_LEDGER_SCHEMA =
  "midgard-phase4-local-genesis-ledger-v1";

type Phase4WalletLabel = "A" | "B";

export class Phase4GenesisLedgerError extends Data.TaggedError(
  "Phase4GenesisLedgerError",
)<{
  readonly message: string;
  readonly cause?: unknown;
}> {}

export type Phase4GenesisLedgerRow = {
  readonly [Ledger.Columns.TX_ID]: Buffer;
  readonly [Ledger.Columns.OUTREF]: Buffer;
  readonly [Ledger.Columns.OUTPUT]: Buffer;
  readonly [Ledger.Columns.ADDRESS]: Address;
  readonly [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: Buffer | null;
};

export type Phase4GenesisWalletSummary = {
  readonly utxoCount: number;
  readonly totalLovelace: string;
};

export type Phase4GenesisLedgerPlan = {
  readonly rows: readonly Phase4GenesisLedgerRow[];
  readonly wallets: Readonly<
    Record<Phase4WalletLabel, Phase4GenesisWalletSummary>
  >;
  readonly supplementalWalletRowCount: number;
};

export type Phase4GenesisLedgerReport = {
  readonly schemaVersion: typeof PHASE4_GENESIS_LEDGER_SCHEMA;
  readonly satisfied: true;
  readonly mode: "seed" | "verify";
  readonly status: "seeded" | "already_present";
  readonly rowCount: number;
  readonly wallets: Phase4GenesisLedgerPlan["wallets"];
  readonly supplementalWalletRowCount: number;
  readonly minimumTransferLovelace: string;
};

const canonicalNatural = (value: unknown): value is string =>
  typeof value === "string" && /^(?:0|[1-9][0-9]*)$/u.test(value);

export const decodePhase4GenesisLedgerReportV1 = (
  value: unknown,
): Phase4GenesisLedgerReport => {
  if (
    !exactObjectKeys(value, [
      "schemaVersion",
      "satisfied",
      "mode",
      "status",
      "rowCount",
      "wallets",
      "supplementalWalletRowCount",
      "minimumTransferLovelace",
    ])
  ) {
    return fail(
      "Phase 4 genesis ledger report fields do not match the exact V1 schema",
    );
  }
  if (
    !exactObjectKeys(value.wallets, ["A", "B"]) ||
    !exactObjectKeys(value.wallets.A, ["utxoCount", "totalLovelace"]) ||
    !exactObjectKeys(value.wallets.B, ["utxoCount", "totalLovelace"])
  ) {
    return fail(
      "Phase 4 genesis ledger wallet fields do not match the exact V1 schema",
    );
  }
  const walletA = value.wallets.A;
  const walletB = value.wallets.B;
  if (
    value.schemaVersion !== PHASE4_GENESIS_LEDGER_SCHEMA ||
    value.satisfied !== true ||
    (value.mode !== "seed" && value.mode !== "verify") ||
    (value.status !== "seeded" && value.status !== "already_present") ||
    (value.mode === "verify" && value.status !== "already_present") ||
    !Number.isSafeInteger(value.rowCount) ||
    (value.rowCount as number) <= 0 ||
    !Number.isSafeInteger(value.supplementalWalletRowCount) ||
    (value.supplementalWalletRowCount as number) < 0 ||
    !Number.isSafeInteger(walletA.utxoCount) ||
    (walletA.utxoCount as number) <= 0 ||
    !Number.isSafeInteger(walletB.utxoCount) ||
    (walletB.utxoCount as number) <= 0 ||
    !canonicalNatural(walletA.totalLovelace) ||
    walletA.totalLovelace === "0" ||
    !canonicalNatural(walletB.totalLovelace) ||
    walletB.totalLovelace === "0" ||
    value.minimumTransferLovelace !==
      PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE.toString() ||
    value.rowCount !==
      (walletA.utxoCount as number) +
        (walletB.utxoCount as number) +
        (value.supplementalWalletRowCount as number)
  ) {
    return fail("Phase 4 genesis ledger report contains a noncanonical value");
  }
  return value as Phase4GenesisLedgerReport;
};

const fail = (message: string, cause?: unknown): never => {
  throw new Phase4GenesisLedgerError({ message, cause });
};

const requiredEnv = (
  env: Readonly<NodeJS.ProcessEnv>,
  name: string,
): string => {
  const value = env[name]?.trim();
  if (value === undefined || value.length === 0) {
    return fail(`Phase 4 genesis ledger command requires ${name}`);
  }
  return value;
};

const requireLoopbackHttpEndpoint = (value: string, label: string): void => {
  let endpoint: URL;
  try {
    endpoint = new URL(value);
  } catch (cause) {
    return fail(`${label} must be an absolute loopback HTTP endpoint`, cause);
  }
  if (
    endpoint.protocol !== "http:" ||
    endpoint.hostname !== "127.0.0.1" ||
    endpoint.port.length === 0 ||
    endpoint.pathname !== "/" ||
    endpoint.search.length > 0 ||
    endpoint.hash.length > 0
  ) {
    fail(`${label} must be an exact 127.0.0.1 HTTP endpoint`);
  }
};

/** Enforces the dedicated local-devnet mutation boundary before touching SQL. */
export const assertPhase4GenesisLedgerGate = ({
  env,
  config,
}: {
  readonly env: Readonly<NodeJS.ProcessEnv>;
  readonly config: Pick<
    NodeConfigDep,
    | "NETWORK"
    | "L1_PROVIDER"
    | "L1_OGMIOS_KEY"
    | "L1_KUPO_KEY"
    | "MIN_FEE_A"
    | "MIN_FEE_B"
    | "RUN_GENESIS_ON_STARTUP"
    | "POSTGRES_HOST"
    | "POSTGRES_PORT"
    | "POSTGRES_DB"
  >;
}): void => {
  if (env[PHASE4_GENESIS_BOOTSTRAP_ENV] !== PHASE4_GENESIS_BOOTSTRAP_TOKEN) {
    fail(
      "Phase 4 genesis ledger command requires its dedicated authorization token",
    );
  }
  if (env.MIDGARD_PHASE4_PROCESS_TARGET !== "local-devnet") {
    fail(
      "Phase 4 genesis ledger command refuses every target except local-devnet",
    );
  }
  if (env.MIDGARD_DOTENV_MODE !== "disabled") {
    fail(
      "Phase 4 genesis ledger command requires checkout dotenv loading to be disabled",
    );
  }
  if (config.NETWORK !== "Custom" || env.NETWORK !== "Custom") {
    fail("Phase 4 genesis ledger command requires NETWORK=Custom");
  }
  if (config.RUN_GENESIS_ON_STARTUP || env.RUN_GENESIS_ON_STARTUP !== "false") {
    fail(
      "Phase 4 genesis ledger command requires RUN_GENESIS_ON_STARTUP=false",
    );
  }
  if (config.L1_PROVIDER !== "Kupmios" || env.L1_PROVIDER !== "Kupmios") {
    fail(
      "Phase 4 genesis ledger command requires the isolated Kupmios provider",
    );
  }
  requireLoopbackHttpEndpoint(config.L1_OGMIOS_KEY, "L1_OGMIOS_KEY");
  requireLoopbackHttpEndpoint(config.L1_KUPO_KEY, "L1_KUPO_KEY");
  if (config.POSTGRES_HOST !== "127.0.0.1") {
    fail("Phase 4 genesis ledger command requires loopback Postgres");
  }
  const runPostgresPortText = requiredEnv(env, "MIDGARD_PHASE4_POSTGRES_PORT");
  const postgresPortText = requiredEnv(env, "POSTGRES_PORT");
  if (
    !/^\d+$/u.test(runPostgresPortText) ||
    postgresPortText !== runPostgresPortText
  ) {
    fail(
      "Phase 4 genesis ledger command requires the exact run-scoped Postgres port",
    );
  }
  const runPostgresPort = Number(runPostgresPortText);
  if (
    !Number.isSafeInteger(runPostgresPort) ||
    runPostgresPort <= 0 ||
    runPostgresPort > 65_535 ||
    runPostgresPort === 5_432 ||
    runPostgresPort === 5_433 ||
    config.POSTGRES_PORT !== runPostgresPort
  ) {
    fail(
      "Phase 4 genesis ledger command refuses a mismatched, invalid, or protected Postgres port",
    );
  }
  const runDatabase = requiredEnv(env, "MIDGARD_PHASE4_POSTGRES_DATABASE");
  if (
    !runDatabase.startsWith("midgard_phase4_process_") ||
    config.POSTGRES_DB !== runDatabase ||
    env.POSTGRES_DB !== runDatabase
  ) {
    fail(
      "Phase 4 genesis ledger command requires the exact run-scoped database",
    );
  }
  if (
    config.MIN_FEE_A !== 0n ||
    config.MIN_FEE_B !== 0n ||
    env.MIN_FEE_A !== "0" ||
    env.MIN_FEE_B !== "0"
  ) {
    fail(
      "Phase 4 genesis ledger command requires pinned MIN_FEE_A=0 and MIN_FEE_B=0",
    );
  }
  const composeProject = requiredEnv(env, "MIDGARD_PHASE4_COMPOSE_PROJECT");
  if (!composeProject.startsWith("midgard_phase4_process_")) {
    fail(
      "Phase 4 genesis ledger command requires the run-scoped Compose project",
    );
  }
  const runDir = requiredEnv(env, "MIDGARD_PHASE4_RUN_DIR");
  if (!isAbsolute(runDir)) {
    fail("Phase 4 genesis ledger command requires an absolute run directory");
  }
  const networkMagicText = requiredEnv(env, "MIDGARD_PHASE4_NETWORK_MAGIC");
  if (!/^\d+$/u.test(networkMagicText)) {
    fail("Phase 4 genesis ledger network magic must be a natural number");
  }
  const networkMagic = Number(networkMagicText);
  if (
    !Number.isSafeInteger(networkMagic) ||
    networkMagic <= 0 ||
    networkMagic === 1 ||
    networkMagic === 2 ||
    networkMagic === 764_824_073
  ) {
    fail("Phase 4 genesis ledger command refuses public-network magic");
  }
};

const walletAddress = (
  env: Readonly<NodeJS.ProcessEnv>,
  label: Phase4WalletLabel,
): Address => {
  const seed = requiredEnv(env, `TESTNET_GENESIS_WALLET_SEED_PHRASE_${label}`);
  try {
    return walletFromSeed(seed, { network: "Custom" }).address;
  } catch {
    // Never attach the wallet-library error: an upstream diagnostic must not
    // gain an opportunity to echo the supplied seed phrase.
    return fail(`Phase 4 genesis wallet ${label} configuration is invalid`);
  }
};

const toLedgerRow = (utxo: UTxO): Phase4GenesisLedgerRow => {
  const core = utxoToCore(utxo);
  return {
    [Ledger.Columns.TX_ID]: Buffer.from(utxo.txHash, "hex"),
    // §5.3 field-0/1 item encoding — the ledger key on-chain
    // `ledger_outref_key` derives, not CML's minimal-index form.
    [Ledger.Columns.OUTREF]: outRefToCbor(utxo),
    [Ledger.Columns.OUTPUT]: Buffer.from(core.output().to_cbor_bytes()),
    [Ledger.Columns.ADDRESS]: utxo.address,
    [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
  };
};

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
    return yield* attempt(() => decodePhase4GenesisLedgerReportV1(report));
  }).pipe(
    sqlErrorToDatabaseError(
      MempoolLedgerDB.tableName,
      "Failed to seed or verify the Phase 4 genesis ledger",
    ),
  );
