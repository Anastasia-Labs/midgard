import { writeFile } from "node:fs/promises";

import {
  computeMidgardNativeTxId,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec";
import { createTrackedTempDirFactory } from "@al-ft/midgard-test-support/temp-files";
import { walletFromSeed } from "@lucid-evolution/lucid";
import type { NodeUtxo } from "midgard-node/commands/command-utils";
import { sha256Hex } from "midgard-node/sha256";
import { afterEach, describe, expect, it, vi } from "vitest";

import { parseE2EL2StressConfig } from "../src/commands/e2e-stress-l2-throughput/index.js";
import { type OpenLoopCorpusRow } from "../src/commands/stress-open-loop.js";

export const TEST_SEED =
  "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart";

export const OTHER_TEST_SEED =
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail";

export const THIRD_TEST_SEED =
  "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee";

export const txHashForIndex = (index: number): string =>
  index.toString(16).padStart(64, "0");

export const makeTempDir = createTrackedTempDirFactory("midgard-e2e-stress-");

afterEach(async () => {
  vi.restoreAllMocks();
});

export const responseJson = (body: unknown, status = 200): Response =>
  ({
    status,
    text: async () => JSON.stringify(body),
    json: async () => body,
  }) as Response;

export const jsonClone = <Value>(value: Value): Value =>
  JSON.parse(JSON.stringify(value)) as Value;

export const corpusRow = (index: number): OpenLoopCorpusRow => {
  const nativeTransaction = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: encodeCbor([
        encodeMidgardTxOutput({
          address: Buffer.concat([
            Buffer.from([0x60]),
            Buffer.alloc(28, index + 1),
          ]),
          value: { lovelace: 2_000_000n, assets: new Map() },
        }),
      ]),
      fee: BigInt(index),
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
  const canonicalCbor = encodeMidgardNativeTxCanonical(nativeTransaction);
  const txHash = computeMidgardNativeTxId(nativeTransaction).toString("hex");
  return {
    txHash,
    canonicalCborHex: canonicalCbor.toString("hex"),
    canonicalCborSha256: sha256Hex(canonicalCbor),
    canonicalCborByteLength: canonicalCbor.length,
    senderWalletId: `wallet-${index.toString()}`,
    selectedInputOutref: `${txHashForIndex(index + 200)}#0`,
    outputOutrefs: [`${txHash}#0`],
    planShape: "fanout",
    parentTxHash: null,
    corpusSliceId: "slice-a",
  };
};

export const writeCorpus = async (
  outDir: string,
  rows: readonly OpenLoopCorpusRow[],
): Promise<string> => {
  const path = `${outDir}/tx-corpus.ndjson`;
  await writeFile(
    path,
    `${rows.map((row) => JSON.stringify(row)).join("\n")}\n`,
    "utf8",
  );
  return path;
};

export const makeClock = (stepMs = 1_000) => {
  let time = Date.parse("2026-01-01T00:00:00.000Z");
  return {
    now: () => {
      time += stepMs;
      return new Date(time);
    },
    sleep: async (ms: number) => {
      time += ms;
    },
  };
};

export const fakeUtxo = {
  txHash: "11".repeat(32),
  outputIndex: 0,
  outrefCbor: Buffer.from("00", "hex"),
  outputCbor: Buffer.from("00", "hex"),
  address: walletFromSeed(TEST_SEED, { network: "Preprod" }).address,
  assets: { lovelace: 10_000_000n },
} satisfies NodeUtxo;

describe("e2e-stress-l2-throughput config", () => {
  it("rejects non-positive and unbounded parameters by default", () => {
    expect(() =>
      parseE2EL2StressConfig({
        walletSeedPhrase: TEST_SEED,
        count: "0",
      }),
    ).toThrow("--count must be a safe positive integer");

    expect(() =>
      parseE2EL2StressConfig({
        walletSeedPhrase: TEST_SEED,
        count: "501",
      }),
    ).toThrow("exceeds the default cap");
  });

  it("rejects unsafe shared-wallet concurrency", () => {
    expect(() =>
      parseE2EL2StressConfig({
        walletSeedPhrase: TEST_SEED,
        count: "2",
        concurrency: "2",
      }),
    ).toThrow("--concurrency > 1 requires --mode parallel-fanout");

    expect(() =>
      parseE2EL2StressConfig({
        walletSeedPhrase: TEST_SEED,
        mode: "parallel-fanout",
        count: "2",
        concurrency: "2",
        stressWalletSeedPhraseEnvs: ["STRESS_A"],
        env: {
          STRESS_A: OTHER_TEST_SEED,
        },
      }),
    ).toThrow("requires at least 2 independent");
  });

  it("accepts pre-funded independent wallet seeds for bounded fanout", () => {
    const config = parseE2EL2StressConfig({
      mode: "parallel-fanout",
      count: "4",
      concurrency: "2",
      stressWalletSeedPhraseEnvs: ["STRESS_A", "STRESS_B"],
      env: {
        STRESS_A: OTHER_TEST_SEED,
        STRESS_B: THIRD_TEST_SEED,
      },
    });

    expect(config.mode).toBe("parallel-fanout");
    expect(config.primaryWallet).toBeUndefined();
    expect(config.stressWallets).toHaveLength(2);
    expect(
      new Set(config.stressWallets.map((wallet) => wallet.address)).size,
    ).toBe(2);
  });

  it("resolves unpadded stress wallet env names through canonical padded names", () => {
    const config = parseE2EL2StressConfig({
      mode: "parallel-fanout",
      count: "1",
      concurrency: "1",
      stressWalletSeedPhraseEnvs: ["STRESS_A_1"],
      env: {
        STRESS_A_0001: OTHER_TEST_SEED,
      },
    });

    expect(config.stressWallets[0]?.resolvedWalletSeedPhrase.resolvedFrom).toBe(
      "STRESS_A_0001",
    );
  });

  it("reports every unresolvable stress wallet env var at once", () => {
    expect(() =>
      parseE2EL2StressConfig({
        mode: "parallel-fanout",
        count: "2",
        concurrency: "2",
        stressWalletSeedPhraseEnvs: ["MISSING_A", "MISSING_B_01"],
        env: {},
      }),
    ).toThrow(
      /2\/2 stress wallet env vars are unresolvable:[\s\S]*MISSING_A[\s\S]*MISSING_B_01[\s\S]*MISSING_B_0001/,
    );
  });

  it("keeps duplicate detection when malformed names resolve to the same padded env", () => {
    expect(() =>
      parseE2EL2StressConfig({
        mode: "parallel-fanout",
        count: "2",
        concurrency: "2",
        stressWalletSeedPhraseEnvs: ["STRESS_A_1", "STRESS_A_01"],
        env: {
          STRESS_A_0001: OTHER_TEST_SEED,
        },
      }),
    ).toThrow("Duplicate stress wallet seed source STRESS_A_0001");
  });

  it("requires a prebuilt corpus for open-loop upper-bound runs", () => {
    expect(() =>
      parseE2EL2StressConfig({
        loadModel: "open-loop-upper-bound",
        targetRateTps: "2",
        openLoopDurationMs: "1000",
      }),
    ).toThrow("requires --tx-corpus");
  });
});
