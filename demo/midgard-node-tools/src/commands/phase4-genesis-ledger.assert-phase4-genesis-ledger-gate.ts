import { isAbsolute } from "node:path";

import { outRefToCbor } from "@al-ft/lucid-midgard";
import {
  type Address,
  type UTxO,
  utxoToCore,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Data } from "effect";
import * as MempoolLedgerDB from "midgard-node/database/mempoolLedger";
import * as Ledger from "midgard-node/database/utils/ledger";
import { exactObjectKeys } from "midgard-node/exact-object-keys";
import { type NodeConfigDep } from "midgard-node/services/index";

export const PHASE4_GENESIS_BOOTSTRAP_TOKEN =
  "phase4-local-devnet-l2-genesis-v1";

export const PHASE4_GENESIS_BOOTSTRAP_ENV = "MIDGARD_PHASE4_GENESIS_BOOTSTRAP";

export const PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE = 50_000n;

export const PHASE4_GENESIS_LEDGER_SCHEMA =
  "midgard-phase4-local-genesis-ledger-v1";

export type Phase4WalletLabel = "A" | "B";

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

export const decodePhase4GenesisLedgerReport = (
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

export const fail = (message: string, cause?: unknown): never => {
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

export const walletAddress = (
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

export const toLedgerRow = (utxo: UTxO): Phase4GenesisLedgerRow => {
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
