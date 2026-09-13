import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  type Address,
  type UTxO,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import * as MempoolLedgerDB from "midgard-node/database/mempoolLedger";
import * as Ledger from "midgard-node/database/utils/ledger";
import { loadRuntimeDotenv } from "midgard-node/runtime-env";
import { describe, expect, it } from "vitest";

import {
  assertPhase4GenesisLedgerGate,
  classifyPhase4GenesisLedgerState,
  decodePhase4GenesisLedgerReport,
  makePhase4GenesisLedgerPlan,
  PHASE4_GENESIS_BOOTSTRAP_ENV,
  PHASE4_GENESIS_BOOTSTRAP_TOKEN,
  PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE,
  type Phase4GenesisLedgerRow,
} from "../src/commands/phase4-genesis-ledger.js";

const SEED_A =
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail";
const SEED_B =
  "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee";
const gateEnv = (): NodeJS.ProcessEnv => ({
  [PHASE4_GENESIS_BOOTSTRAP_ENV]: PHASE4_GENESIS_BOOTSTRAP_TOKEN,
  MIDGARD_PHASE4_PROCESS_TARGET: "local-devnet",
  MIDGARD_DOTENV_MODE: "disabled",
  MIDGARD_PHASE4_POSTGRES_DATABASE: "midgard_phase4_process_test",
  MIDGARD_PHASE4_POSTGRES_PORT: "5544",
  MIDGARD_PHASE4_COMPOSE_PROJECT: "midgard_phase4_process_test",
  MIDGARD_PHASE4_RUN_DIR: "/tmp/midgard-phase4-test",
  MIDGARD_PHASE4_NETWORK_MAGIC: "424242",
  NETWORK: "Custom",
  L1_PROVIDER: "Kupmios",
  POSTGRES_DB: "midgard_phase4_process_test",
  POSTGRES_PORT: "5544",
  MIN_FEE_A: "0",
  MIN_FEE_B: "0",
  RUN_GENESIS_ON_STARTUP: "false",
  TESTNET_GENESIS_WALLET_SEED_PHRASE_A: SEED_A,
  TESTNET_GENESIS_WALLET_SEED_PHRASE_B: SEED_B,
});

const gateConfig = () =>
  ({
    NETWORK: "Custom",
    L1_PROVIDER: "Kupmios",
    L1_OGMIOS_KEY: "http://127.0.0.1:2337",
    L1_KUPO_KEY: "http://127.0.0.1:2442",
    MIN_FEE_A: 0n,
    MIN_FEE_B: 0n,
    RUN_GENESIS_ON_STARTUP: false,
    POSTGRES_HOST: "127.0.0.1",
    POSTGRES_PORT: 5544,
    POSTGRES_DB: "midgard_phase4_process_test",
  }) as const;

const configuredUtxo = ({
  txByte,
  outputIndex,
  address,
  lovelace,
}: {
  readonly txByte: string;
  readonly outputIndex: number;
  readonly address: Address;
  readonly lovelace: bigint;
}): UTxO => ({
  txHash: txByte.repeat(32),
  outputIndex,
  address,
  assets: { lovelace },
});

const row = (byte: number): Phase4GenesisLedgerRow => ({
  [Ledger.Columns.TX_ID]: Buffer.from([byte]),
  [Ledger.Columns.OUTREF]: Buffer.from([byte, 0]),
  [Ledger.Columns.OUTPUT]: Buffer.from([byte, 1]),
  [Ledger.Columns.ADDRESS]: `addr_test_${byte.toString()}` as Address,
  [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
});

describe("Phase 4 explicit genesis ledger gate", () => {
  it("accepts only the exact isolated local-devnet identity", () => {
    expect(() =>
      assertPhase4GenesisLedgerGate({ env: gateEnv(), config: gateConfig() }),
    ).not.toThrow();
  });

  it.each([
    [{ [PHASE4_GENESIS_BOOTSTRAP_ENV]: undefined }, {}, "authorization token"],
    [{ MIDGARD_PHASE4_PROCESS_TARGET: "preprod" }, {}, "local-devnet"],
    [{ MIDGARD_DOTENV_MODE: "enabled" }, {}, "dotenv"],
    [{ MIDGARD_PHASE4_NETWORK_MAGIC: "1" }, {}, "public-network magic"],
    [
      { POSTGRES_PORT: "5433", MIDGARD_PHASE4_POSTGRES_PORT: "5433" },
      { POSTGRES_PORT: 5433 },
      "protected Postgres port",
    ],
    [{ POSTGRES_PORT: "5545" }, {}, "run-scoped Postgres port"],
    [{}, { POSTGRES_PORT: 5545 }, "mismatched"],
    [{ POSTGRES_DB: "midgard" }, {}, "run-scoped database"],
    [{ MIN_FEE_A: "10" }, {}, "MIN_FEE_A=0"],
    [{}, { MIN_FEE_A: 10n }, "MIN_FEE_A=0"],
    [{ MIN_FEE_B: "10" }, {}, "MIN_FEE_A=0"],
    [{}, { MIN_FEE_B: 10n }, "MIN_FEE_A=0"],
    [{}, { RUN_GENESIS_ON_STARTUP: true }, "RUN_GENESIS_ON_STARTUP=false"],
    [{}, { L1_KUPO_KEY: "https://preprod.example.com" }, "127.0.0.1"],
  ])("rejects unsafe identity %#", (envOverride, configOverride, message) => {
    expect(() =>
      assertPhase4GenesisLedgerGate({
        env: { ...gateEnv(), ...envOverride },
        config: { ...gateConfig(), ...configOverride },
      }),
    ).toThrow(message);
  });
});

describe("Phase 4 genesis ledger V1 report decoder", () => {
  const report = () => ({
    schemaVersion: "midgard-phase4-local-genesis-ledger-v1",
    satisfied: true,
    mode: "seed",
    status: "seeded",
    rowCount: 3,
    wallets: {
      A: { utxoCount: 1, totalLovelace: "4000000000" },
      B: { utxoCount: 1, totalLovelace: "126943" },
    },
    supplementalWalletRowCount: 1,
    minimumTransferLovelace: "50000",
  });

  it("accepts only the exact canonical report", () => {
    expect(decodePhase4GenesisLedgerReport(report())).toEqual(report());
  });

  it.each([
    [
      { schemaVersion: "midgard-phase4-local-genesis-ledger-v2" },
      "noncanonical",
    ],
    [{ unexpected: true }, "fields"],
    [{ wallets: { ...report().wallets, C: report().wallets.A } }, "wallet"],
    [
      {
        wallets: {
          ...report().wallets,
          A: { ...report().wallets.A, totalLovelace: "04000000000" },
        },
      },
      "noncanonical",
    ],
    [{ status: "complete" }, "noncanonical"],
    [
      {
        schemaVersion: "midgard-phase4-t1-recovery-attestation-v1",
      },
      "noncanonical",
    ],
  ])("rejects adversarial report mutation %#", (override, message) => {
    expect(() =>
      decodePhase4GenesisLedgerReport({ ...report(), ...override }),
    ).toThrow(message);
  });

  it("rejects a missing required key", () => {
    const { rowCount: _rowCount, ...missing } = report();
    expect(() => decodePhase4GenesisLedgerReport(missing)).toThrow("fields");
  });
});

describe("Phase 4 configured genesis plan", () => {
  it("seeds the complete fallback set while proving A/B funding and reporting supplemental wallet C", () => {
    const addressA = walletFromSeed(SEED_A, { network: "Custom" }).address;
    const addressB = walletFromSeed(SEED_B, { network: "Custom" }).address;
    const a = configuredUtxo({
      txByte: "11",
      outputIndex: 0,
      address: addressA,
      lovelace: 4_000_000_000n,
    });
    const b = configuredUtxo({
      txByte: "22",
      outputIndex: 0,
      address: addressB,
      lovelace: 126_943n,
    });
    // The isolated Phase 4 harness maps its unused supplemental wallet C to A.
    const c = configuredUtxo({
      txByte: "33",
      outputIndex: 0,
      address: addressA,
      lovelace: 300n,
    });
    const plan = makePhase4GenesisLedgerPlan({
      env: gateEnv(),
      genesisUtxos: [a, b, c],
      genesisUtxosByWallet: {
        A: [a],
        B: [b],
        C: [c],
      },
    });

    expect(plan.rows).toHaveLength(3);
    expect(plan.rows.map((entry) => entry.address)).toEqual([
      addressA,
      addressB,
      addressA,
    ]);
    expect(plan.wallets).toEqual({
      A: { utxoCount: 1, totalLovelace: "4000000000" },
      B: { utxoCount: 1, totalLovelace: "126943" },
    });
    expect(plan.supplementalWalletRowCount).toBe(1);
    expect(PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE).toBe(50_000n);
  });

  it("fails when either configured wallet cannot fund the fixed transfer", () => {
    const addressA = walletFromSeed(SEED_A, { network: "Custom" }).address;
    const addressB = walletFromSeed(SEED_B, { network: "Custom" }).address;
    const a = configuredUtxo({
      txByte: "11",
      outputIndex: 0,
      address: addressA,
      lovelace: 50_000n,
    });
    const b = configuredUtxo({
      txByte: "22",
      outputIndex: 0,
      address: addressB,
      lovelace: 49_999n,
    });
    const c = configuredUtxo({
      txByte: "33",
      outputIndex: 0,
      address: addressA,
      lovelace: 300n,
    });
    expect(() =>
      makePhase4GenesisLedgerPlan({
        env: gateEnv(),
        genesisUtxos: [a, b, c],
        genesisUtxosByWallet: {
          A: [a],
          B: [b],
          C: [c],
        },
      }),
    ).toThrow("wallet B cannot fund");
  });

  it("fails when grouped wallet identity and the commit fallback set diverge", () => {
    const addressA = walletFromSeed(SEED_A, { network: "Custom" }).address;
    const addressB = walletFromSeed(SEED_B, { network: "Custom" }).address;
    const a = configuredUtxo({
      txByte: "11",
      outputIndex: 0,
      address: addressA,
      lovelace: 50_000n,
    });
    const b = configuredUtxo({
      txByte: "22",
      outputIndex: 0,
      address: addressB,
      lovelace: 126_943n,
    });
    expect(() =>
      makePhase4GenesisLedgerPlan({
        env: gateEnv(),
        genesisUtxos: [a, b],
        genesisUtxosByWallet: {
          A: [a],
          B: [b],
          C: [
            configuredUtxo({
              txByte: "33",
              outputIndex: 0,
              address: addressA,
              lovelace: 300n,
            }),
          ],
        },
      }),
    ).toThrow("does not match GENESIS_UTXOS");
  });
});

describe("Phase 4 genesis ledger idempotency", () => {
  it("allows only empty seed or a complete byte-identical set", () => {
    const expected = [row(1), row(2)];
    expect(classifyPhase4GenesisLedgerState({ expected, existing: [] })).toBe(
      "seed",
    );
    expect(
      classifyPhase4GenesisLedgerState({
        expected,
        existing: expected.map((entry) => ({
          ...entry,
          tx_id: Buffer.from(entry.tx_id),
          outref: Buffer.from(entry.outref),
          output: Buffer.from(entry.output),
        })),
      }),
    ).toBe("already_present");
  });

  it("refuses partial, extra, or payload-mismatched state", () => {
    const expected = [row(1), row(2)];
    expect(() =>
      classifyPhase4GenesisLedgerState({
        expected,
        existing: [expected[0]],
      }),
    ).toThrow("partial or contains non-genesis state");
    expect(() =>
      classifyPhase4GenesisLedgerState({
        expected,
        existing: [...expected, row(3)],
      }),
    ).toThrow("partial or contains non-genesis state");
    expect(() =>
      classifyPhase4GenesisLedgerState({
        expected,
        existing: [expected[0], { ...expected[1], output: Buffer.from([9]) }],
      }),
    ).toThrow("does not exactly match");
  });
});

describe("isolated runtime dotenv policy", () => {
  it("does not backfill a missing child key from the checkout .env when disabled", () => {
    const cwd = mkdtempSync(join(tmpdir(), "midgard-phase4-dotenv-"));
    try {
      writeFileSync(join(cwd, ".env"), "CHECKOUT_ONLY_SECRET=must-not-load\n", {
        mode: 0o600,
      });
      const env: NodeJS.ProcessEnv = { MIDGARD_DOTENV_MODE: "disabled" };
      loadRuntimeDotenv({ env, cwd });
      expect(env.CHECKOUT_ONLY_SECRET).toBeUndefined();
    } finally {
      rmSync(cwd, { recursive: true, force: true });
    }
  });

  it("retains explicit enabled-mode behavior for ordinary interactive commands", () => {
    const cwd = mkdtempSync(join(tmpdir(), "midgard-dotenv-"));
    try {
      writeFileSync(join(cwd, ".env"), "LOCAL_ONLY_VALUE=loaded\n", {
        mode: 0o600,
      });
      const env: NodeJS.ProcessEnv = { MIDGARD_DOTENV_MODE: "enabled" };
      loadRuntimeDotenv({ env, cwd });
      expect(env.LOCAL_ONLY_VALUE).toBe("loaded");
    } finally {
      rmSync(cwd, { recursive: true, force: true });
    }
  });
});
