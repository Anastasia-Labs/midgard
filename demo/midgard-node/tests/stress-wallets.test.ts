import { access, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { assetsToValue, CML, walletFromSeed } from "@lucid-evolution/lucid";
import { Context, Effect, Layer } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  fetchNodeTxStatus,
  fetchNodeUtxosByAddress,
  formatJson,
  type NodeUtxo,
} from "@/commands/command-utils.js";
import {
  consolidateStressWallets,
  createL2Wallets,
  fanoutStressWallets,
  parseConsolidationReadiness,
  parseStressWalletConsolidationJournal,
  parseStressWalletConsolidationReadinessEvidence,
  parseStressWalletConsolidationReport,
  parseStressWalletConsolidationResult,
  parseStressWalletCreateResult,
  parseStressWalletFanoutReport,
  parseStressWalletFanoutResult,
  parseStressWalletPrepareResult,
  parseStressWalletRecord,
  parseStressWalletTerminalDrainJournal,
  parseStressWalletTerminalDrainReport,
  parseStressWalletTerminalDrainResult,
  prepareStressWallets,
  runBounded,
  runWithSharedFanoutContext,
  stressWalletFileName,
  terminalDrainStressWallets,
} from "@/commands/stress-wallets.js";
import {
  buildTerminalDrainTx,
  buildTransferTxWithMinFee,
} from "@/commands/submit-l2-transfer.js";

import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";

class FanoutAcquisitionProbe extends Context.Tag("FanoutAcquisitionProbe")<
  FanoutAcquisitionProbe,
  { readonly acquisition: number }
>() {}

const TEST_SEEDS = [
  "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart",
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
  "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee",
];

const makeTempDir = async (): Promise<string> =>
  mkdtemp(join(tmpdir(), "midgard-stress-wallets-"));

const seedGenerator = () => {
  let index = 0;
  return () => TEST_SEEDS[index++]!;
};

const nodeUtxo = ({
  txHashByte,
  outputIndex = 0,
  address,
  lovelace,
}: {
  readonly txHashByte: string;
  readonly outputIndex?: number;
  readonly address: string;
  readonly lovelace: bigint;
}): NodeUtxo => ({
  txHash: txHashByte.repeat(32),
  outputIndex,
  outrefCbor: makeOutRefCbor(txHashByte.repeat(32), outputIndex),
  outputCbor: Buffer.from("00", "hex"),
  address,
  assets: { lovelace },
});

const prepareCanonicalNativeTransfer = async ({
  sourceSeedPhrase,
  sourceAddress,
  destinationAddress,
  sourceLovelace,
  requestedLovelace,
  txHashByte,
}: {
  readonly sourceSeedPhrase: string;
  readonly sourceAddress: string;
  readonly destinationAddress: string;
  readonly sourceLovelace: bigint;
  readonly requestedLovelace: bigint;
  readonly txHashByte: string;
}) => {
  const wallet = walletFromSeed(sourceSeedPhrase, { network: "Preprod" });
  const txHash = txHashByte.repeat(32);
  const outrefCbor = makeOutRefCbor(txHash, 0);
  const outputCbor = Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(sourceAddress),
      assetsToValue({ lovelace: sourceLovelace }),
    ).to_cbor_bytes(),
  );
  const built = await buildTransferTxWithMinFee({
    senderAddress: sourceAddress,
    destinationAddress,
    signer: CML.PrivateKey.from_bech32(wallet.paymentKey),
    availableUtxos: [
      {
        txHash,
        outputIndex: 0,
        outrefCbor,
        outputCbor,
        address: sourceAddress,
        assets: { lovelace: sourceLovelace },
      },
    ],
    requestedAssets: { lovelace: requestedLovelace },
    network: "Preprod",
    networkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
  });
  return {
    txHash: built.txIdHex,
    signedTxCbor: built.txHex,
    selectedInputs: built.selectedInputs.map(
      (input) => `${input.txHash}#${input.outputIndex.toString()}`,
    ),
  };
};

const prepareCanonicalTerminalDrain = async ({
  sourceSeedPhrase,
  sourceAddress,
  destinationAddress,
  utxos,
}: {
  readonly sourceSeedPhrase: string;
  readonly sourceAddress: string;
  readonly destinationAddress: string;
  readonly utxos: readonly NodeUtxo[];
}) => {
  const wallet = walletFromSeed(sourceSeedPhrase, { network: "Preprod" });
  const built = await buildTerminalDrainTx({
    senderAddress: sourceAddress,
    destinationAddress,
    signer: CML.PrivateKey.from_bech32(wallet.paymentKey),
    availableUtxos: utxos,
    network: "Preprod",
    networkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
  });
  return {
    txHash: built.txIdHex,
    signedTxCbor: built.txHex,
    selectedInputs: built.selectedInputs.map(
      (x) => x.txHash + "#" + x.outputIndex.toString(),
    ),
    requestedLovelace: built.requestedAssets.lovelace ?? 0n,
    feeLovelace: built.fee,
    signedTxBytes: built.txCbor.length,
  };
};

const fullConsolidationReadiness = () => ({
  httpStatus: 200,
  body: {
    ready: true,
    reasons: [],
    durableAdmissionBacklog: "0",
    mempoolTxCount: "0",
    unfinishedLocalMutationJobs: "0",
    unresolvedBlockSubmissionAgeMs: 0,
    providerQueryHealthy: true,
    stateQueueMutationLease: { status: "idle", pendingFinalizations: [] },
    blockCommitmentCoordination: {
      commitWorkerActive: false,
      commitPipelinePhase: "idle",
    },
  },
});

describe("stress wallet commands", () => {
  it("aborts a hanging UTxO request at the configured deadline", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(
        (
          _input: string | URL | Request,
          init?: RequestInit,
        ): Promise<Response> =>
          new Promise((_resolve, reject) => {
            init?.signal?.addEventListener(
              "abort",
              () => reject(init.signal?.reason),
              { once: true },
            );
          }),
      ),
    );
    try {
      await expect(
        fetchNodeUtxosByAddress("http://127.0.0.1:3000", "addr_test1deadline", {
          timeoutMs: 5,
        }),
      ).rejects.toBeDefined();
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("acquires and retains fanout services once across multiple submissions", async () => {
    let acquisitions = 0;
    let releases = 0;
    const layer = Layer.scoped(
      FanoutAcquisitionProbe,
      Effect.acquireRelease(
        Effect.sync(() => ({ acquisition: ++acquisitions })),
        () => Effect.sync(() => void (releases += 1)),
      ),
    );

    const observed = await Effect.runPromise(
      runWithSharedFanoutContext<
        { readonly acquisition: number }[],
        FanoutAcquisitionProbe
      >(async (runShared) => [
        await runShared(FanoutAcquisitionProbe),
        await runShared(FanoutAcquisitionProbe),
        await runShared(FanoutAcquisitionProbe),
      ]).pipe(Effect.provide(layer)),
    );

    expect(observed.map(({ acquisition }) => acquisition)).toEqual([1, 1, 1]);
    expect(acquisitions).toBe(1);
    expect(releases).toBe(1);
  });

  it("keeps shared fanout services alive until started siblings settle after a failure", async () => {
    let acquisitions = 0;
    let releases = 0;
    const layer = Layer.scoped(
      FanoutAcquisitionProbe,
      Effect.acquireRelease(
        Effect.sync(() => ({ acquisition: ++acquisitions })),
        () => Effect.sync(() => void (releases += 1)),
      ),
    );
    const started: number[] = [];
    let signalThreeStarted: (() => void) | undefined;
    const threeStarted = new Promise<void>((resolve) => {
      signalThreeStarted = resolve;
    });
    const controls = Array.from({ length: 3 }, () => {
      let resolve: (() => void) | undefined;
      let reject: ((error: unknown) => void) | undefined;
      const promise = new Promise<void>((resolvePromise, rejectPromise) => {
        resolve = resolvePromise;
        reject = rejectPromise;
      });
      return {
        promise,
        resolve: () => resolve?.(),
        reject: (error: unknown) => reject?.(error),
      };
    });
    const originalError = new Error("first fanout submission failed");
    let observedFailure: unknown;
    let settled = false;

    const outcome = Effect.runPromise(
      runWithSharedFanoutContext<void, FanoutAcquisitionProbe>(
        async (runShared) => {
          try {
            await runBounded([0, 1, 2, 3], 3, async (index) => {
              started.push(index);
              if (started.length === 3) {
                signalThreeStarted?.();
              }
              await runShared(FanoutAcquisitionProbe);
              await controls[index]!.promise;
            });
          } catch (error) {
            observedFailure = error;
            throw error;
          }
        },
      ).pipe(Effect.provide(layer)),
    ).then(
      () => ({ status: "resolved" as const }),
      (error: unknown) => ({ status: "rejected" as const, error }),
    );
    void outcome.finally(() => {
      settled = true;
    });

    await threeStarted;
    controls[0]!.reject(originalError);
    await Promise.resolve();
    await Promise.resolve();

    expect(started).toEqual([0, 1, 2]);
    expect(settled).toBe(false);
    expect(acquisitions).toBe(1);
    expect(releases).toBe(0);

    controls[1]!.resolve();
    await Promise.resolve();
    expect(settled).toBe(false);
    expect(releases).toBe(0);

    controls[2]!.resolve();
    await expect(outcome).resolves.toMatchObject({
      status: "rejected",
      error: { message: originalError.message },
    });
    expect(observedFailure).toBe(originalError);
    expect(settled).toBe(true);
    expect(started).toEqual([0, 1, 2]);
    expect(releases).toBe(1);
  });

  it("creates persisted L2 wallet records and redacted command output", async () => {
    const dir = await makeTempDir();
    try {
      const result = await createL2Wallets({
        count: 2,
        outDir: dir,
        network: "Preprod",
        now: () => new Date("2026-01-01T00:00:00.000Z"),
        generateSeedPhrase: seedGenerator(),
      });

      expect(result.createdCount).toBe(2);
      expect(result.reusedCount).toBe(0);
      expect(result.wallets).toHaveLength(2);
      expect(result.wallets[0]?.envName).toBe("STRESS_WALLET_SEED_PHRASE_0001");
      expect(formatJson(result)).not.toContain(TEST_SEEDS[0]);
      expect(parseStressWalletCreateResult(result).schemaVersion).toBe(
        "midgard-stress-wallet-create-result-v1",
      );
      expect(() =>
        parseStressWalletCreateResult({ ...result, extension: true }),
      ).toThrow("extra=[extension]");
      expect(() =>
        parseStressWalletCreateResult({
          ...result,
          createdCount: result.createdCount + 1,
        }),
      ).toThrow("cardinality binding");
      expect(() =>
        parseStressWalletCreateResult({
          ...result,
          wallets: [{ ...result.wallets[0], historical: true }],
        }),
      ).toThrow("extra=[historical]");

      const firstPath = join(dir, stressWalletFileName(1));
      const firstDocument = JSON.parse(
        await readFile(firstPath, "utf8"),
      ) as Record<string, unknown>;
      const firstRecord = parseStressWalletRecord(firstDocument);
      expect(firstRecord.seedPhrase).toBe(TEST_SEEDS[0]);
      expect(firstRecord.l2Address).toBe(result.wallets[0]?.l2Address);
      expect(() =>
        parseStressWalletRecord({ ...firstDocument, historical: true }),
      ).toThrow("extra=[historical]");
      const { seedPhrase: _seedPhrase, ...missingSeedPhrase } = firstDocument;
      expect(() => parseStressWalletRecord(missingSeedPhrase)).toThrow(
        "missing=[seedPhrase]",
      );
      expect(() =>
        parseStressWalletRecord({
          ...firstDocument,
          schemaVersion: "midgard-stress-wallet-v2",
        }),
      ).toThrow("Unsupported stress wallet schemaVersion");

      const envFile = await readFile(result.envFilePath, "utf8");
      const argsFile = await readFile(result.argsFilePath, "utf8");
      expect(envFile).toContain(TEST_SEEDS[0]);
      expect(argsFile).toContain(
        "--stress-wallet-seed-phrase-env STRESS_WALLET_SEED_PHRASE_0001",
      );
      expect(argsFile).toContain(
        "--stress-wallet-seed-phrase-env STRESS_WALLET_SEED_PHRASE_0002",
      );
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("prepares wallets by skipping funded records, depositing missing funds, projecting, and verifying /utxos", async () => {
    const dir = await makeTempDir();
    try {
      const created = await createL2Wallets({
        count: 2,
        outDir: dir,
        network: "Preprod",
        now: () => new Date("2026-01-01T00:00:00.000Z"),
        generateSeedPhrase: seedGenerator(),
      });
      const first = created.wallets[0]!;
      const second = created.wallets[1]!;
      const utxosByAddress = new Map<string, readonly NodeUtxo[]>([
        [
          first.l2Address,
          [
            nodeUtxo({
              txHashByte: "11",
              address: first.l2Address,
              lovelace: 10_000_000n,
            }),
          ],
        ],
        [second.l2Address, []],
      ]);
      const submitted: string[] = [];
      let projectCount = 0;

      const result = await prepareStressWallets(
        {
          count: 2,
          outDir: dir,
          network: "Preprod",
          lovelacePerWallet: 5_000_000n,
          nodeEndpoint: "http://127.0.0.1:3000",
          projectionWaitMs: 0,
          verifyTimeoutMs: 1_000,
          pollIntervalMs: 1,
          now: () => new Date("2026-01-01T00:01:00.000Z"),
        },
        {
          submitDeposit: async ({ wallet, lovelace }) => {
            submitted.push(wallet.envName);
            utxosByAddress.set(wallet.l2Address, [
              nodeUtxo({
                txHashByte: "22",
                address: wallet.l2Address,
                lovelace,
              }),
            ]);
            return {
              txHash: "aa".repeat(32),
              depositEventId: "bb".repeat(34),
            };
          },
          projectDeposits: async () => {
            projectCount += 1;
          },
          fetchUtxos: async (_endpoint, address) =>
            utxosByAddress.get(address) ?? [],
          sleep: async () => {},
        },
      );

      expect(submitted).toEqual(["STRESS_WALLET_SEED_PHRASE_0002"]);
      expect(projectCount).toBe(1);
      expect(result.submittedDepositCount).toBe(1);
      expect(result.alreadyFundedCount).toBe(1);
      expect(result.verifiedWalletCount).toBe(2);
      expect(result.wallets.map((entry) => entry.status)).toEqual([
        "already_funded",
        "submitted",
      ]);
      expect(parseStressWalletPrepareResult(result).schemaVersion).toBe(
        "midgard-stress-wallet-prepare-result-v1",
      );
      expect(() =>
        parseStressWalletPrepareResult({
          ...result,
          schemaVersion: "midgard-stress-wallet-prepare-result-v2",
        }),
      ).toThrow("schemaVersion must be exactly");
      const { wallets: _wallets, ...prepareWithoutWallets } = result;
      expect(() =>
        parseStressWalletPrepareResult(prepareWithoutWallets),
      ).toThrow("missing=[wallets]");
      expect(() =>
        parseStressWalletPrepareResult({
          ...result,
          wallets: [
            {
              ...result.wallets[0],
              wallet: { ...result.wallets[0]!.wallet, historical: true },
            },
            result.wallets[1],
          ],
        }),
      ).toThrow("extra=[historical]");

      const secondRecord = parseStressWalletRecord(
        JSON.parse(
          await readFile(join(dir, stressWalletFileName(2)), "utf8"),
        ) as unknown,
      );
      expect(secondRecord.latestFunding?.depositTxHash).toBe("aa".repeat(32));
      expect(secondRecord.latestFunding?.verifiedFundingUtxoCount).toBe(1);
      expect(formatJson(result)).not.toContain(TEST_SEEDS[1]);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("can create missing wallet records during prepare when explicitly requested", async () => {
    const dir = await makeTempDir();
    try {
      const utxosByAddress = new Map<string, readonly NodeUtxo[]>();
      const result = await prepareStressWallets(
        {
          count: 1,
          outDir: dir,
          network: "Preprod",
          createMissing: true,
          lovelacePerWallet: 4_000_000n,
          nodeEndpoint: "http://127.0.0.1:3000",
          projectionWaitMs: 0,
          verifyTimeoutMs: 1_000,
          pollIntervalMs: 1,
          now: () => new Date("2026-01-01T00:02:00.000Z"),
          generateSeedPhrase: seedGenerator(),
        },
        {
          submitDeposit: async ({ wallet, lovelace }) => {
            utxosByAddress.set(wallet.l2Address, [
              nodeUtxo({
                txHashByte: "33",
                address: wallet.l2Address,
                lovelace,
              }),
            ]);
            return { txHash: "cc".repeat(32) };
          },
          projectDeposits: async () => {},
          fetchUtxos: async (_endpoint, address) =>
            utxosByAddress.get(address) ?? [],
          sleep: async () => {},
        },
      );

      expect(result.generatedWalletCount).toBe(1);
      expect(result.submittedDepositCount).toBe(1);
      expect(result.wallets[0]?.wallet.envName).toBe(
        "STRESS_WALLET_SEED_PHRASE_0001",
      );
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("rejects existing wallet files that do not match the requested env prefix", async () => {
    const dir = await makeTempDir();
    try {
      await createL2Wallets({
        count: 1,
        outDir: dir,
        envPrefix: "OTHER_STRESS_WALLET",
        network: "Preprod",
        generateSeedPhrase: seedGenerator(),
      });

      await expect(
        prepareStressWallets(
          {
            count: 1,
            outDir: dir,
            network: "Preprod",
            lovelacePerWallet: 4_000_000n,
            nodeEndpoint: "http://127.0.0.1:3000",
            projectionWaitMs: 0,
            verifyTimeoutMs: 1,
            pollIntervalMs: 1,
          },
          {
            submitDeposit: async () => ({ txHash: "dd".repeat(32) }),
            projectDeposits: async () => {},
            fetchUtxos: async () => [],
            sleep: async () => {},
          },
        ),
      ).rejects.toThrow("records envName OTHER_STRESS_WALLET_0001");
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("fans out L2 funding level-by-level and records verified funding snapshots", async () => {
    const dir = await makeTempDir();
    try {
      const created = await createL2Wallets({
        count: 3,
        outDir: dir,
        network: "Preprod",
        now: () => new Date("2026-01-01T00:00:00.000Z"),
        generateSeedPhrase: seedGenerator(),
      });
      const balances = new Map<string, bigint>(
        created.wallets.map((wallet) => [wallet.l2Address, 0n] as const),
      );
      const submitted: Array<{
        readonly source: string;
        readonly destination: string;
        readonly lovelace: bigint;
        readonly level: number;
      }> = [];
      let txCounter = 0;

      const result = await fanoutStressWallets(
        {
          count: 3,
          outDir: dir,
          network: "Preprod",
          lovelacePerWallet: 1_000n,
          treasurySeedPhrase: TEST_SEEDS[0],
          nodeEndpoint: "http://127.0.0.1:3000",
          branchFactor: 2,
          maxInFlight: 2,
          feeHeadroomLovelace: 10n,
          createMissing: false,
          acceptanceTimeoutMs: 1_000,
          pollInitialIntervalMs: 1,
          pollMaxIntervalMs: 1,
          now: () => new Date("2026-01-01T00:03:00.000Z"),
        },
        {
          submitTransfer: async ({ source, destination, lovelace, level }) => {
            txCounter += 1;
            const sourceLabel =
              source.kind === "treasury" ? "treasury" : source.wallet.walletId;
            submitted.push({
              source: sourceLabel,
              destination: destination.walletId,
              lovelace,
              level,
            });
            if (source.kind === "wallet") {
              const current = balances.get(source.wallet.l2Address) ?? 0n;
              balances.set(source.wallet.l2Address, current - lovelace - 10n);
            }
            balances.set(
              destination.l2Address,
              (balances.get(destination.l2Address) ?? 0n) + lovelace,
            );
            return {
              txHash: txCounter.toString(16).padStart(64, "0"),
              status: "queued",
            };
          },
          fetchTxStatus: async () => "accepted",
          fetchUtxos: async (_endpoint, address) => {
            const lovelace = balances.get(address) ?? 0n;
            return lovelace > 0n
              ? [
                  nodeUtxo({
                    txHashByte: "44",
                    address,
                    lovelace,
                  }),
                ]
              : [];
          },
          sleep: async () => {},
        },
      );

      expect(result.submittedTransferCount).toBe(3);
      expect(result.alreadyFundedTransferCount).toBe(0);
      expect(result.verifiedWalletCount).toBe(3);
      expect(result.rootRequiredLovelace).toBe("3030");
      expect(result.levels).toEqual([
        { level: 1, transferCount: 2 },
        { level: 2, transferCount: 1 },
      ]);
      expect(
        submitted.map((entry) => [entry.source, entry.destination]),
      ).toEqual([
        ["treasury", "stress-wallet-0001"],
        ["treasury", "stress-wallet-0002"],
        ["stress-wallet-0001", "stress-wallet-0003"],
      ]);
      expect(submitted.map((entry) => entry.lovelace.toString(10))).toEqual([
        "2010",
        "1000",
        "1000",
      ]);

      const firstRecord = parseStressWalletRecord(
        JSON.parse(
          await readFile(join(dir, stressWalletFileName(1)), "utf8"),
        ) as unknown,
      );
      expect(firstRecord.latestFunding?.fundingUtxos?.[0]?.lovelace).toBe(
        "1000",
      );
      expect(parseStressWalletFanoutResult(result).schemaVersion).toBe(
        "midgard-stress-wallet-fanout-result-v1",
      );
      expect(() =>
        parseStressWalletFanoutResult({ ...result, extension: true }),
      ).toThrow("extra=[extension]");
      const fanoutReport = JSON.parse(
        await readFile(result.reportPath, "utf8"),
      ) as unknown;
      expect(parseStressWalletFanoutReport(fanoutReport).schemaVersion).toBe(
        "midgard-stress-wallet-fanout-report-v1",
      );
      expect(() =>
        parseStressWalletFanoutReport({
          ...(fanoutReport as Record<string, unknown>),
          edges: [
            {
              ...((fanoutReport as { readonly edges: readonly object[] })
                .edges[0] as object),
              historical: true,
            },
          ],
        }),
      ).toThrow("extra=[historical]");
      expect(formatJson(result)).not.toContain(TEST_SEEDS[0]);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("resumes fanout by skipping already funded edges and funding missing children", async () => {
    const dir = await makeTempDir();
    try {
      const created = await createL2Wallets({
        count: 3,
        outDir: dir,
        network: "Preprod",
        now: () => new Date("2026-01-01T00:00:00.000Z"),
        generateSeedPhrase: seedGenerator(),
      });
      const balances = new Map<string, bigint>(
        created.wallets.map((wallet, index) => [
          wallet.l2Address,
          index === 0 ? 2_010n : 0n,
        ]),
      );
      const submitted: Array<{
        readonly source: string;
        readonly destination: string;
        readonly lovelace: bigint;
      }> = [];
      let txCounter = 0;

      const result = await fanoutStressWallets(
        {
          count: 3,
          outDir: dir,
          network: "Preprod",
          lovelacePerWallet: 1_000n,
          treasurySeedPhrase: TEST_SEEDS[0],
          nodeEndpoint: "http://127.0.0.1:3000",
          branchFactor: 2,
          maxInFlight: 1,
          feeHeadroomLovelace: 10n,
          createMissing: false,
          acceptanceTimeoutMs: 1_000,
          pollInitialIntervalMs: 1,
          pollMaxIntervalMs: 1,
          now: () => new Date("2026-01-01T00:04:00.000Z"),
        },
        {
          submitTransfer: async ({ source, destination, lovelace }) => {
            txCounter += 1;
            const sourceLabel =
              source.kind === "treasury" ? "treasury" : source.wallet.walletId;
            submitted.push({
              source: sourceLabel,
              destination: destination.walletId,
              lovelace,
            });
            if (source.kind === "wallet") {
              const current = balances.get(source.wallet.l2Address) ?? 0n;
              balances.set(source.wallet.l2Address, current - lovelace - 10n);
            }
            balances.set(
              destination.l2Address,
              (balances.get(destination.l2Address) ?? 0n) + lovelace,
            );
            return {
              txHash: txCounter.toString(16).padStart(64, "0"),
              status: "queued",
            };
          },
          fetchTxStatus: async () => "accepted",
          fetchUtxos: async (_endpoint, address) => {
            const lovelace = balances.get(address) ?? 0n;
            return lovelace > 0n
              ? [
                  nodeUtxo({
                    txHashByte:
                      address === created.wallets[0]!.l2Address ? "55" : "66",
                    address,
                    lovelace,
                  }),
                ]
              : [];
          },
          sleep: async () => {},
        },
      );

      expect(result.submittedTransferCount).toBe(2);
      expect(result.alreadyFundedTransferCount).toBe(1);
      expect(result.verifiedWalletCount).toBe(3);
      expect(result.levels).toEqual([
        { level: 1, transferCount: 2 },
        { level: 2, transferCount: 1 },
      ]);
      expect(
        submitted.map((entry) => [entry.source, entry.destination]),
      ).toEqual([
        ["treasury", "stress-wallet-0002"],
        ["stress-wallet-0001", "stress-wallet-0003"],
      ]);

      const report = JSON.parse(await readFile(result.reportPath, "utf8")) as {
        readonly edges: readonly { readonly submitted: boolean }[];
      };
      expect(report.edges.map((edge) => edge.submitted)).toEqual([
        false,
        true,
        true,
      ]);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("consolidates with bounded reads, exact accounting, and a private evidence report", async () => {
    const dir = await makeTempDir();
    try {
      const created = await createL2Wallets({
        count: 2,
        outDir: dir,
        network: "Preprod",
        now: () => new Date("2026-01-01T00:00:00.000Z"),
        generateSeedPhrase: seedGenerator(),
      });
      const balances = new Map<string, bigint>([
        [created.wallets[0]!.l2Address, 1_000n],
        [created.wallets[1]!.l2Address, 2_000n],
      ]);
      const submitted: Array<{
        readonly walletId: string;
        readonly lovelace: bigint;
      }> = [];
      const preparedByTx = new Map<
        string,
        {
          readonly walletId: string;
          readonly sourceAddress: string;
          readonly treasuryAddress: string;
          readonly lovelace: bigint;
          readonly signedTxCbor: string;
        }
      >();
      let destinationAddress = "";
      let activeReads = 0;
      let maxActiveReads = 0;
      let statusReadCount = 0;
      let readinessReadCount = 0;

      const result = await consolidateStressWallets(
        {
          count: 2,
          outDir: dir,
          network: "Preprod",
          treasurySeedPhrase: TEST_SEEDS[2]!,
          nodeEndpoint: "http://127.0.0.1:3000",
          reserveLovelace: 100n,
          requiredTreasuryLovelace: 2_800n,
          maxInFlight: 1,
          acceptanceTimeoutMs: 1_000,
          verificationTimeoutMs: 1_000,
          pollInitialIntervalMs: 1,
          pollMaxIntervalMs: 1,
          now: () => new Date("2026-01-01T00:05:00.000Z"),
        },
        {
          prepareTransfer: async ({ source, treasuryAddress, lovelace }) => {
            destinationAddress = treasuryAddress;
            const prepared = await prepareCanonicalNativeTransfer({
              sourceSeedPhrase: source.seedPhrase,
              sourceAddress: source.l2Address,
              destinationAddress: treasuryAddress,
              sourceLovelace: balances.get(source.l2Address) ?? 0n,
              requestedLovelace: lovelace,
              txHashByte: "77",
            });
            preparedByTx.set(prepared.txHash, {
              walletId: source.walletId,
              sourceAddress: source.l2Address,
              treasuryAddress,
              lovelace,
              signedTxCbor: prepared.signedTxCbor,
            });
            return prepared;
          },
          submitPreparedTransfer: async ({ txHash, signedTxCbor }) => {
            const prepared = preparedByTx.get(txHash);
            if (
              prepared === undefined ||
              prepared.signedTxCbor !== signedTxCbor
            ) {
              throw new Error("unexpected prepared transfer");
            }
            submitted.push({
              walletId: prepared.walletId,
              lovelace: prepared.lovelace,
            });
            balances.set(prepared.sourceAddress, 100n);
            balances.set(
              prepared.treasuryAddress,
              (balances.get(prepared.treasuryAddress) ?? 0n) +
                prepared.lovelace,
            );
            return { txHash, status: "accepted" };
          },
          fetchReadiness: async () => {
            readinessReadCount += 1;
            if (readinessReadCount === 2) {
              const pending = fullConsolidationReadiness();
              return {
                ...pending,
                httpStatus: 503,
                body: {
                  ...pending.body,
                  ready: false,
                  reasons: ["local_finalization_pending"],
                  mempoolTxCount: "1",
                },
              };
            }
            return fullConsolidationReadiness();
          },
          fetchTxStatus: async () => {
            const statuses = [
              "not_found",
              "validating",
              "accepted",
              "pending_commit",
              "committed",
            ] as const;
            const status =
              statusReadCount < 10
                ? statuses[statusReadCount % statuses.length]!
                : "committed";
            statusReadCount += 1;
            return status;
          },
          fetchUtxos: async (_endpoint, address) => {
            activeReads += 1;
            maxActiveReads = Math.max(maxActiveReads, activeReads);
            await Promise.resolve();
            const lovelace = balances.get(address) ?? 0n;
            activeReads -= 1;
            return lovelace === 0n
              ? []
              : [nodeUtxo({ txHashByte: "77", address, lovelace })];
          },
          sleep: async () => {},
        },
      );

      expect(destinationAddress).toBe(result.treasuryAddress);
      expect(submitted).toEqual([
        { walletId: "stress-wallet-0001", lovelace: 900n },
        { walletId: "stress-wallet-0002", lovelace: 1_900n },
      ]);
      expect(maxActiveReads).toBe(1);
      expect(statusReadCount).toBe(10);
      expect(readinessReadCount).toBe(3);
      const readinessEvidence = await readFile(
        join(dir, "consolidation-readiness.jsonl"),
        "utf8",
      );
      expect(readinessEvidence.trim().split("\n")).toHaveLength(3);
      expect(readinessEvidence).toContain(`"fullReady":false`);
      expect(readinessEvidence).toContain(`"fullReady":true`);
      for (const line of readinessEvidence.trim().split("\n")) {
        expect(
          parseStressWalletConsolidationReadinessEvidence(
            JSON.parse(line) as unknown,
          ).schemaVersion,
        ).toBe("midgard-stress-wallet-consolidation-readiness-v1");
      }
      const exactReadiness = fullConsolidationReadiness();
      expect(parseConsolidationReadiness(exactReadiness).ready).toBe(true);
      expect(() =>
        parseConsolidationReadiness({
          ...exactReadiness,
          body: { ...exactReadiness.body, extension: true },
        }),
      ).toThrow("extra=[extension]");
      expect(result.treasuryDeltaLovelace).toBe("2800");
      expect(result.sourceBeforeLovelace).toBe("3000");
      expect(result.sourceAfterLovelace).toBe("200");
      expect(result.inferredFeesLovelace).toBe("0");
      expect(result.submittedTransferCount).toBe(2);
      expect(result.resumedTransferCount).toBe(0);
      expect(parseStressWalletConsolidationResult(result).schemaVersion).toBe(
        "midgard-stress-wallet-consolidation-result-v1",
      );
      const report = JSON.parse(
        await readFile(result.reportPath, "utf8"),
      ) as Record<string, unknown>;
      expect(parseStressWalletConsolidationReport(report).schemaVersion).toBe(
        "midgard-stress-wallet-consolidation-report-v1",
      );
      expect(JSON.stringify(report)).toContain("selectedInputs");
      expect(() =>
        parseStressWalletConsolidationReport({
          ...report,
          schemaVersion: "midgard-stress-wallet-consolidation-report-v2",
        }),
      ).toThrow("schemaVersion must be exactly");
      expect(() =>
        parseStressWalletConsolidationReport({
          ...report,
          upgradeVerification: true,
        }),
      ).toThrow("extra=[upgradeVerification]");
      expect(() =>
        parseStressWalletConsolidationReport({
          ...report,
          treasury: {
            ...(report.treasury as Record<string, unknown>),
            historical: true,
          },
        }),
      ).toThrow("extra=[historical]");
      const journal = JSON.parse(
        await readFile(join(dir, "consolidation-state.json"), "utf8"),
      ) as Record<string, unknown>;
      expect(parseStressWalletConsolidationJournal(journal).schemaVersion).toBe(
        "midgard-stress-wallet-consolidation-journal-v1",
      );
      expect(() =>
        parseStressWalletConsolidationJournal({
          ...journal,
          schemaVersion: "midgard-stress-wallet-consolidate-v1",
        }),
      ).toThrow("Unsupported consolidation state schema");
      expect(() =>
        parseStressWalletConsolidationJournal({
          ...journal,
          legacyVerification: {},
        }),
      ).toThrow("extra=[legacyVerification]");
      const { scope: _scope, ...journalWithoutScope } = journal;
      expect(() =>
        parseStressWalletConsolidationJournal(journalWithoutScope),
      ).toThrow("missing=[scope]");
      expect(formatJson(result)).not.toContain(TEST_SEEDS[2]);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("fails the required treasury gate before submitting any consolidation transfer", async () => {
    const dir = await makeTempDir();
    try {
      const created = await createL2Wallets({
        count: 1,
        outDir: dir,
        network: "Preprod",
        generateSeedPhrase: seedGenerator(),
      });
      let submitCount = 0;
      await expect(
        consolidateStressWallets(
          {
            count: 1,
            outDir: dir,
            network: "Preprod",
            treasurySeedPhrase: TEST_SEEDS[2]!,
            reserveLovelace: 100n,
            requiredTreasuryLovelace: 1_000n,
          },
          {
            prepareTransfer: async () => {
              submitCount += 1;
              return {
                txHash: "88".repeat(32),
                signedTxCbor: "a1",
                selectedInputs: [`${"88".repeat(32)}#0`],
              };
            },
            submitPreparedTransfer: async () => ({
              txHash: "88".repeat(32),
              status: "accepted",
            }),
            fetchTxStatus: async () => "accepted",
            fetchUtxos: async (_endpoint, address) =>
              address === created.wallets[0]!.l2Address
                ? [nodeUtxo({ txHashByte: "88", address, lovelace: 500n })]
                : [],
          },
        ),
      ).rejects.toThrow("no transfers submitted");
      expect(submitCount).toBe(0);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("fails closed and journals malformed consolidation readiness before submission", async () => {
    const dir = await makeTempDir();
    try {
      const created = await createL2Wallets({
        count: 1,
        outDir: dir,
        network: "Preprod",
        generateSeedPhrase: seedGenerator(),
      });
      let submitCount = 0;
      await expect(
        consolidateStressWallets(
          {
            count: 1,
            outDir: dir,
            network: "Preprod",
            treasurySeedPhrase: TEST_SEEDS[2]!,
            reserveLovelace: 100n,
          },
          {
            prepareTransfer: async () => {
              submitCount += 1;
              return {
                txHash: "ab".repeat(32),
                signedTxCbor: "a1",
                selectedInputs: [`${"ab".repeat(32)}#0`],
              };
            },
            submitPreparedTransfer: async () => ({
              txHash: "ab".repeat(32),
              status: "accepted",
            }),
            fetchReadiness: async () => ({
              httpStatus: 200,
              body: { ready: true },
            }),
            fetchTxStatus: async () => "committed",
            fetchUtxos: async (_endpoint, address) =>
              address === created.wallets[0]!.l2Address
                ? [nodeUtxo({ txHashByte: "ab", address, lovelace: 500n })]
                : [],
          },
        ),
      ).rejects.toThrow("Malformed consolidation readiness response");
      expect(submitCount).toBe(0);
      const evidence = await readFile(
        join(dir, "consolidation-readiness.jsonl"),
        "utf8",
      );
      expect(evidence).toContain(`"malformed":true`);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("rejects a mismatched V1 scope before network reads and preserves every journal byte", async () => {
    const dir = await makeTempDir();
    try {
      const created = await createL2Wallets({
        count: 1,
        outDir: dir,
        network: "Preprod",
        generateSeedPhrase: seedGenerator(),
      });
      const sourceAddress = created.wallets[0]!.l2Address;
      const options = {
        count: 1,
        outDir: dir,
        network: "Preprod" as const,
        treasurySeedPhrase: TEST_SEEDS[2]!,
        reserveLovelace: 100n,
        acceptanceTimeoutMs: 10,
        readinessTimeoutMs: 10,
        verificationTimeoutMs: 10,
        pollInitialIntervalMs: 1,
        pollMaxIntervalMs: 1,
      };
      const sourceUtxo = nodeUtxo({
        txHashByte: "bc",
        address: sourceAddress,
        lovelace: 500n,
      });
      await expect(
        consolidateStressWallets(options, {
          prepareTransfer: async () => {
            throw new Error("stop after intent checkpoint");
          },
          submitPreparedTransfer: async () => {
            throw new Error("must not submit");
          },
          fetchReadiness: async () => fullConsolidationReadiness(),
          fetchTxStatus: async () => "not_found",
          fetchUtxos: async (_endpoint, address) =>
            address === sourceAddress ? [sourceUtxo] : [],
        }),
      ).rejects.toThrow("stop after intent checkpoint");

      const statePath = join(dir, "consolidation-state.json");
      const mismatched = JSON.parse(await readFile(statePath, "utf8")) as {
        scope: { count: number };
      };
      mismatched.scope.count = 2;
      await writeFile(statePath, `${formatJson(mismatched)}\n`, "utf8");
      const bytesBefore = await readFile(statePath);
      let networkReadCount = 0;

      await expect(
        consolidateStressWallets(options, {
          prepareTransfer: async () => {
            throw new Error("must not prepare");
          },
          submitPreparedTransfer: async () => {
            throw new Error("must not submit");
          },
          fetchReadiness: async () => {
            networkReadCount += 1;
            return fullConsolidationReadiness();
          },
          fetchTxStatus: async () => {
            networkReadCount += 1;
            return "not_found";
          },
          fetchUtxos: async () => {
            networkReadCount += 1;
            return [];
          },
        }),
      ).rejects.toThrow("exact wallet scope");

      expect(networkReadCount).toBe(0);
      expect(await readFile(statePath)).toEqual(bytesBefore);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("normalizes only a tx-bound canonical not_found HTTP 404", async () => {
    const txHash = "ab".repeat(32);
    const response = (
      status: number,
      body: unknown,
      contentType = "application/json",
    ) =>
      new Response(typeof body === "string" ? body : JSON.stringify(body), {
        status,
        headers: { "content-type": contentType },
      });
    const readWith = async (value: Response) => {
      vi.stubGlobal(
        "fetch",
        vi.fn(async () => value),
      );
      return fetchNodeTxStatus("http://127.0.0.1:3000", txHash, 100);
    };
    try {
      await expect(
        readWith(response(404, { txId: txHash, status: "not_found" })),
      ).resolves.toBe("not_found");
      await expect(
        readWith(response(200, { txId: txHash, status: "accepted" })),
      ).resolves.toBe("accepted");
      await expect(
        readWith(response(404, { txId: "cd".repeat(32), status: "not_found" })),
      ).rejects.toThrow("invalid or mismatched status body");
      await expect(
        readWith(response(404, { txId: txHash, status: "accepted" })),
      ).rejects.toThrow("invalid or mismatched status body");
      await expect(
        readWith(response(404, "not-json", "text/plain")),
      ).rejects.toThrow("malformed JSON");
      await expect(
        readWith(response(500, { txId: txHash, status: "not_found" })),
      ).rejects.toThrow("invalid or mismatched status body");
      await expect(
        readWith(response(200, { txId: txHash, status: "not_found" })),
      ).rejects.toThrow("invalid or mismatched status body");
    } finally {
      vi.unstubAllGlobals();
    }
  });

  it("rejects tampered hash/CBOR pairs and duplicate entries before network reads without rewriting state", async () => {
    const dir = await makeTempDir();
    try {
      const created = await createL2Wallets({
        count: 1,
        outDir: dir,
        network: "Preprod",
        generateSeedPhrase: seedGenerator(),
      });
      const sourceAddress = created.wallets[0]!.l2Address;
      const treasuryAddress = walletFromSeed(TEST_SEEDS[2]!, {
        network: "Preprod",
      }).address;
      const options = {
        count: 1,
        outDir: dir,
        network: "Preprod" as const,
        treasurySeedPhrase: TEST_SEEDS[2]!,
        reserveLovelace: 100n,
        acceptanceTimeoutMs: 10,
        readinessTimeoutMs: 10,
        verificationTimeoutMs: 10,
        pollInitialIntervalMs: 1,
        pollMaxIntervalMs: 1,
      };
      const prepared = await prepareCanonicalNativeTransfer({
        sourceSeedPhrase: TEST_SEEDS[0]!,
        sourceAddress,
        destinationAddress: treasuryAddress,
        sourceLovelace: 1_000n,
        requestedLovelace: 900n,
        txHashByte: "dd",
      });
      const alternative = await prepareCanonicalNativeTransfer({
        sourceSeedPhrase: TEST_SEEDS[0]!,
        sourceAddress,
        destinationAddress: treasuryAddress,
        sourceLovelace: 1_000n,
        requestedLovelace: 800n,
        txHashByte: "dd",
      });
      const selectedValueMismatch = await prepareCanonicalNativeTransfer({
        sourceSeedPhrase: TEST_SEEDS[0]!,
        sourceAddress,
        destinationAddress: treasuryAddress,
        sourceLovelace: 1_100n,
        requestedLovelace: 900n,
        txHashByte: "dd",
      });
      const sourceUtxo = nodeUtxo({
        txHashByte: "dd",
        address: sourceAddress,
        lovelace: 1_000n,
      });
      await expect(
        consolidateStressWallets(options, {
          prepareTransfer: async () => prepared,
          submitPreparedTransfer: async () => {
            throw new Error("stop after prepared checkpoint");
          },
          fetchReadiness: async () => fullConsolidationReadiness(),
          fetchTxStatus: async () => "not_found",
          fetchUtxos: async (_endpoint, address) =>
            address === sourceAddress ? [sourceUtxo] : [],
        }),
      ).rejects.toThrow("stop after prepared checkpoint");

      const statePath = join(dir, "consolidation-state.json");
      const original = JSON.parse(await readFile(statePath, "utf8")) as {
        entries: Array<{
          txHash?: string;
          signedTxCbor?: string;
        }>;
      };
      const assertRejectedWithoutReadOrRewrite = async (
        state: typeof original,
        message: string,
      ) => {
        const bytes = Buffer.from(`${formatJson(state)}\n`, "utf8");
        await writeFile(statePath, bytes);
        let networkReads = 0;
        await expect(
          consolidateStressWallets(options, {
            prepareTransfer: async () => {
              networkReads += 1;
              throw new Error("must not prepare");
            },
            submitPreparedTransfer: async () => {
              networkReads += 1;
              throw new Error("must not submit");
            },
            fetchReadiness: async () => {
              networkReads += 1;
              return fullConsolidationReadiness();
            },
            fetchTxStatus: async () => {
              networkReads += 1;
              return "committed";
            },
            fetchUtxos: async () => {
              networkReads += 1;
              return [];
            },
          }),
        ).rejects.toThrow(message);
        expect(networkReads).toBe(0);
        expect(await readFile(statePath)).toEqual(bytes);
      };

      await assertRejectedWithoutReadOrRewrite(
        {
          ...original,
          entries: [
            {
              ...original.entries[0]!,
              txHash: "ff".repeat(32),
            },
          ],
        },
        "txHash/signedTxCbor mismatch",
      );
      await assertRejectedWithoutReadOrRewrite(
        {
          ...original,
          entries: [
            {
              ...original.entries[0]!,
              signedTxCbor: alternative.signedTxCbor,
            },
          ],
        },
        "txHash/signedTxCbor mismatch",
      );
      await assertRejectedWithoutReadOrRewrite(
        {
          ...original,
          entries: [
            {
              ...original.entries[0]!,
              txHash: alternative.txHash,
              signedTxCbor: alternative.signedTxCbor,
            },
          ],
        },
        "wrong treasury value",
      );
      const selectedValueMismatchState = {
        ...original,
        entries: [
          {
            ...original.entries[0]!,
            txHash: selectedValueMismatch.txHash,
            signedTxCbor: selectedValueMismatch.signedTxCbor,
            selectedInputLovelace: "1100",
          },
        ],
      };
      const selectedValueMismatchBytes = Buffer.from(
        formatJson(selectedValueMismatchState) + "\n",
        "utf8",
      );
      await writeFile(statePath, selectedValueMismatchBytes);
      let submitAttempts = 0;
      await expect(
        consolidateStressWallets(options, {
          prepareTransfer: async () => {
            throw new Error("must not prepare");
          },
          submitPreparedTransfer: async () => {
            submitAttempts += 1;
            throw new Error("must not submit");
          },
          fetchReadiness: async () => fullConsolidationReadiness(),
          fetchTxStatus: async () => {
            throw new Error("must not read status");
          },
          fetchUtxos: async (_endpoint, address) =>
            address === sourceAddress ? [sourceUtxo] : [],
        }),
      ).rejects.toThrow("selected-input value does not match");
      expect(submitAttempts).toBe(0);
      expect(await readFile(statePath)).toEqual(selectedValueMismatchBytes);
      await assertRejectedWithoutReadOrRewrite(
        {
          ...original,
          entries: [original.entries[0]!, original.entries[0]!],
        },
        "Duplicate walletId",
      );
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("serializes concurrent consolidation operators before any network or submission work", async () => {
    const dir = await makeTempDir();
    let releaseFetch: (() => void) | undefined;
    try {
      await createL2Wallets({
        count: 1,
        outDir: dir,
        network: "Preprod",
        generateSeedPhrase: seedGenerator(),
      });
      const options = {
        count: 1,
        outDir: dir,
        network: "Preprod" as const,
        treasurySeedPhrase: TEST_SEEDS[2]!,
        reserveLovelace: 100n,
        requiredTreasuryLovelace: 1n,
      };
      let enteredResolve: (() => void) | undefined;
      const entered = new Promise<void>((resolve) => {
        enteredResolve = resolve;
      });
      const fetchGate = new Promise<void>((resolve) => {
        releaseFetch = resolve;
      });
      const first = consolidateStressWallets(options, {
        prepareTransfer: async () => {
          throw new Error("must not prepare");
        },
        submitPreparedTransfer: async () => {
          throw new Error("must not submit");
        },
        fetchReadiness: async () => fullConsolidationReadiness(),
        fetchTxStatus: async () => "not_found",
        fetchUtxos: async () => {
          enteredResolve?.();
          await fetchGate;
          return [];
        },
      });
      await entered;
      let secondOperatorCalls = 0;
      await expect(
        consolidateStressWallets(options, {
          prepareTransfer: async () => {
            secondOperatorCalls += 1;
            throw new Error("must not prepare");
          },
          submitPreparedTransfer: async () => {
            secondOperatorCalls += 1;
            throw new Error("must not submit");
          },
          fetchReadiness: async () => {
            secondOperatorCalls += 1;
            return fullConsolidationReadiness();
          },
          fetchTxStatus: async () => {
            secondOperatorCalls += 1;
            return "not_found";
          },
          fetchUtxos: async () => {
            secondOperatorCalls += 1;
            return [];
          },
        }),
      ).rejects.toThrow("exclusively locked");
      expect(secondOperatorCalls).toBe(0);
      releaseFetch?.();
      await expect(first).rejects.toThrow("Projected treasury");
      await expect(
        access(join(dir, "consolidation-state.json.lock")),
      ).rejects.toThrow();
    } finally {
      releaseFetch?.();
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("never removes a replacement consolidation lock during cleanup", async () => {
    const dir = await makeTempDir();
    try {
      await createL2Wallets({
        count: 1,
        outDir: dir,
        network: "Preprod",
        generateSeedPhrase: seedGenerator(),
      });
      const lockPath = join(dir, "consolidation-state.json.lock");
      const replacement = formatJson({ token: "replacement" }) + "\n";
      let replaced = false;
      await expect(
        consolidateStressWallets(
          {
            count: 1,
            outDir: dir,
            network: "Preprod",
            treasurySeedPhrase: TEST_SEEDS[2]!,
            reserveLovelace: 100n,
            requiredTreasuryLovelace: 1n,
          },
          {
            prepareTransfer: async () => {
              throw new Error("must not prepare");
            },
            submitPreparedTransfer: async () => {
              throw new Error("must not submit");
            },
            fetchReadiness: async () => fullConsolidationReadiness(),
            fetchTxStatus: async () => "not_found",
            fetchUtxos: async () => {
              if (!replaced) {
                replaced = true;
                await rm(lockPath);
                await writeFile(lockPath, replacement, "utf8");
              }
              return [];
            },
          },
        ),
      ).rejects.toThrow("lock ownership changed");
      expect(await readFile(lockPath, "utf8")).toBe(replacement);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("journals signed CBOR before submission and resumes only the exact transaction", async () => {
    const dir = await makeTempDir();
    try {
      const created = await createL2Wallets({
        count: 1,
        outDir: dir,
        network: "Preprod",
        generateSeedPhrase: seedGenerator(),
      });
      const sourceAddress = created.wallets[0]!.l2Address;
      const balances = new Map<string, bigint>([[sourceAddress, 1_000n]]);
      const treasuryAddress = walletFromSeed(TEST_SEEDS[2]!, {
        network: "Preprod",
      }).address;
      const submitAttempts: Array<{
        readonly txHash: string;
        readonly signedTxCbor: string;
      }> = [];
      const commonOptions = {
        count: 1,
        outDir: dir,
        network: "Preprod" as const,
        treasurySeedPhrase: TEST_SEEDS[2]!,
        reserveLovelace: 100n,
        acceptanceTimeoutMs: 10,
        readinessTimeoutMs: 10,
        verificationTimeoutMs: 1_000,
        pollInitialIntervalMs: 1,
        pollMaxIntervalMs: 1,
      };
      const fetchUtxos = async (_endpoint: string, address: string) => {
        const lovelace = balances.get(address) ?? 0n;
        return lovelace === 0n
          ? []
          : [nodeUtxo({ txHashByte: "99", address, lovelace })];
      };
      const prepared = await prepareCanonicalNativeTransfer({
        sourceSeedPhrase: TEST_SEEDS[0]!,
        sourceAddress,
        destinationAddress: treasuryAddress,
        sourceLovelace: 1_000n,
        requestedLovelace: 900n,
        txHashByte: "99",
      });

      await expect(
        consolidateStressWallets(commonOptions, {
          prepareTransfer: async ({ treasuryAddress: destination }) => {
            expect(destination).toBe(treasuryAddress);
            return prepared;
          },
          submitPreparedTransfer: async ({ txHash, signedTxCbor }) => {
            const journal = JSON.parse(
              await readFile(join(dir, "consolidation-state.json"), "utf8"),
            ) as {
              readonly entries: readonly {
                readonly txHash?: string;
                readonly signedTxCbor?: string;
              }[];
            };
            expect(journal.entries[0]).toMatchObject({
              txHash,
              signedTxCbor,
            });
            submitAttempts.push({ txHash, signedTxCbor });
            throw new Error("simulated ambiguous interruption after submit");
          },
          fetchReadiness: async () => fullConsolidationReadiness(),
          fetchTxStatus: async () => "not_found",
          fetchUtxos,
        }),
      ).rejects.toThrow("simulated ambiguous interruption after submit");

      let statusReadCount = 0;
      const statusFetch = vi.fn(
        async () =>
          new Response(
            JSON.stringify({ txId: prepared.txHash, status: "not_found" }),
            { status: 404, headers: { "content-type": "application/json" } },
          ),
      );
      vi.stubGlobal("fetch", statusFetch);
      const result = await consolidateStressWallets(commonOptions, {
        prepareTransfer: async () => {
          throw new Error("resume must not rebuild the transaction");
        },
        submitPreparedTransfer: async ({ txHash, signedTxCbor }) => {
          submitAttempts.push({ txHash, signedTxCbor });
          balances.set(sourceAddress, 100n);
          balances.set(treasuryAddress, 900n);
          return { txHash, status: "accepted" };
        },
        fetchReadiness: async () => fullConsolidationReadiness(),
        fetchTxStatus: async (nodeEndpoint, txHash) => {
          statusReadCount += 1;
          return statusReadCount === 1
            ? fetchNodeTxStatus(nodeEndpoint, txHash, 100)
            : "committed";
        },
        fetchUtxos,
        sleep: async () => {},
      });

      expect(submitAttempts).toEqual([
        { txHash: prepared.txHash, signedTxCbor: prepared.signedTxCbor },
        { txHash: prepared.txHash, signedTxCbor: prepared.signedTxCbor },
      ]);
      expect(result.submittedTransferCount).toBe(1);
      expect(result.resumedTransferCount).toBe(1);
      expect(result.treasuryDeltaLovelace).toBe("900");
      expect(statusFetch).toHaveBeenCalledTimes(1);
    } finally {
      vi.unstubAllGlobals();
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("prepares every terminal drain before submission and resumes exact CBOR to zero with private conservation evidence", async () => {
    const dir = await mkdtemp(join("/tmp", "midgard-terminal-drain-"));
    try {
      const created = await createL2Wallets({
        count: 2,
        outDir: dir,
        network: "Preprod",
        now: () => new Date("2026-01-02T00:00:00.000Z"),
        generateSeedPhrase: seedGenerator(),
      });
      const treasuryAddress = walletFromSeed(TEST_SEEDS[2]!, {
        network: "Preprod",
      }).address;
      const sourceUtxos = new Map<string, readonly NodeUtxo[]>();
      for (let i = 0; i < created.wallets.length; i += 1) {
        const record = created.wallets[i]!;
        const hash = (i === 0 ? "81" : "82").repeat(32);
        sourceUtxos.set(record.l2Address, [
          {
            txHash: hash,
            outputIndex: 0,
            outrefCbor: makeOutRefCbor(hash, 0),
            outputCbor: Buffer.from(
              makeMidgardTxOutput(
                CML.Address.from_bech32(record.l2Address),
                assetsToValue({ lovelace: BigInt(1_000 + i * 500) }),
              ).to_cbor_bytes(),
            ),
            address: record.l2Address,
            assets: { lovelace: BigInt(1_000 + i * 500) },
          },
        ]);
      }
      let originalSourceUtxos = new Map<string, readonly NodeUtxo[]>();
      let treasury = 50n;
      const preparedByHash = new Map<
        string,
        { address: string; amount: bigint; cbor: string }
      >();
      let prepareCalls = 0;
      let submitCalls = 0;
      const submitted = new Set<string>();
      const fetchUtxos = async (
        _endpoint: string,
        address: string,
      ): Promise<readonly NodeUtxo[]> => {
        if (address === treasuryAddress)
          return treasury === 0n
            ? []
            : [nodeUtxo({ txHashByte: "90", address, lovelace: treasury })];
        return sourceUtxos.get(address) ?? [];
      };
      const common = {
        count: 2,
        outDir: dir,
        network: "Preprod" as const,
        treasurySeedPhrase: TEST_SEEDS[2]!,
        nodeEndpoint: "http://127.0.0.1:3000",
        minFeeA: 0n,
        minFeeB: 0n,
        maxInFlight: 2,
        acceptanceTimeoutMs: 1_000,
        verificationTimeoutMs: 1_000,
        pollInitialIntervalMs: 1,
        pollMaxIntervalMs: 1,
        now: () => new Date("2026-01-02T00:05:00.000Z"),
      };
      const prepared = await terminalDrainStressWallets(
        { ...common, prepareOnly: true },
        {
          prepareTransfer: async ({
            source,
            treasuryAddress: destinationAddress,
          }) => {
            prepareCalls += 1;
            const tx = await prepareCanonicalTerminalDrain({
              sourceSeedPhrase: source.seedPhrase,
              sourceAddress: source.l2Address,
              destinationAddress,
              utxos: sourceUtxos.get(source.l2Address)!,
            });
            preparedByHash.set(tx.txHash, {
              address: source.l2Address,
              amount: tx.requestedLovelace,
              cbor: tx.signedTxCbor,
            });
            return tx;
          },
          submitPreparedTransfer: async () => {
            submitCalls += 1;
            throw new Error("prepare-only crossed submission barrier");
          },
          fetchTxStatus: async () => "not_found",
          fetchUtxos,
        },
      );
      expect(prepared.phase).toBe("prepared");
      expect(prepareCalls).toBe(2);
      expect(submitCalls).toBe(0);
      expect(parseStressWalletTerminalDrainResult(prepared).schemaVersion).toBe(
        "midgard-stress-wallet-terminal-drain-result-v1",
      );
      expect(
        (await (await import("node:fs/promises")).stat(prepared.statePath))
          .mode & 0o777,
      ).toBe(0o600);
      const canonicalPreparedState = await readFile(prepared.statePath, "utf8");
      const canonicalPreparedDocument = JSON.parse(
        canonicalPreparedState,
      ) as Record<string, unknown>;
      expect(
        parseStressWalletTerminalDrainJournal(canonicalPreparedDocument)
          .schemaVersion,
      ).toBe("midgard-stress-wallet-terminal-drain-journal-v1");
      expect(() =>
        parseStressWalletTerminalDrainJournal({
          ...canonicalPreparedDocument,
          schemaVersion: "midgard-stress-wallet-terminal-drain-v1",
        }),
      ).toThrow("Unsupported terminal drain journal schema");
      expect(() =>
        parseStressWalletTerminalDrainJournal({
          ...canonicalPreparedDocument,
          legacyResult: true,
        }),
      ).toThrow("extra=[legacyResult]");
      const { entries: _entries, ...terminalWithoutEntries } =
        canonicalPreparedDocument;
      expect(() =>
        parseStressWalletTerminalDrainJournal(terminalWithoutEntries),
      ).toThrow("missing=[entries]");
      expect(() =>
        parseStressWalletTerminalDrainJournal({
          ...canonicalPreparedDocument,
          scopeSha256: "ff".repeat(32),
        }),
      ).toThrow("must bind the exact parsed scope");
      const preparedTamperers: readonly ((state: any) => void)[] = [
        (state) => {
          state.entries[0].txHash = "00".repeat(32);
        },
        (state) => {
          state.entries[0].signedTxCbor = "00";
        },
        (state) => {
          state.entries[0].selectedInputs = [];
        },
        (state) => {
          state.entries[0].requestedLovelace = (
            BigInt(state.entries[0].requestedLovelace) + 1n
          ).toString();
        },
        (state) => {
          state.entries[0].feeLovelace = "1";
        },
        (state) => {
          state.entries[0].signedTxBytes += 1;
        },
        (state) => {
          state.scopeSha256 = "ff".repeat(32);
        },
      ];
      for (const tamper of preparedTamperers) {
        const state = JSON.parse(canonicalPreparedState) as any;
        tamper(state);
        const tampered = formatJson(state) + "\n";
        await writeFile(prepared.statePath, tampered, "utf8");
        await expect(
          terminalDrainStressWallets(
            { ...common, prepareOnly: true },
            {
              prepareTransfer: async () => {
                throw new Error("tamper rebuilt");
              },
              submitPreparedTransfer: async () => {
                throw new Error("tamper submitted");
              },
              fetchTxStatus: async () => {
                throw new Error("tamper queried status");
              },
              fetchUtxos,
            },
          ),
        ).rejects.toThrow();
        expect(await readFile(prepared.statePath, "utf8")).toBe(tampered);
      }
      await writeFile(prepared.statePath, canonicalPreparedState, "utf8");
      originalSourceUtxos = new Map(sourceUtxos);
      await expect(
        terminalDrainStressWallets(common, {
          prepareTransfer: async () => {
            throw new Error("ambiguous resume rebuilt transaction");
          },
          submitPreparedTransfer: async ({ signedTxCbor }) => {
            submitCalls += 1;
            expect(
              [...preparedByHash.values()].some(
                (intent) => intent.cbor === signedTxCbor,
              ),
            ).toBe(true);
            throw new Error("simulated commit-ambiguous interruption");
          },
          fetchTxStatus: async () => "not_found",
          fetchUtxos,
        }),
      ).rejects.toThrow("simulated commit-ambiguous interruption");
      expect(submitCalls).toBe(1);
      const firstAddress = created.wallets[0]!.l2Address;
      sourceUtxos.set(firstAddress, []);
      await expect(
        terminalDrainStressWallets(common, {
          prepareTransfer: async () => {
            throw new Error("missing-input resume rebuilt transaction");
          },
          submitPreparedTransfer: async () => {
            throw new Error("missing-input resume submitted");
          },
          fetchTxStatus: async () => "not_found",
          fetchUtxos,
        }),
      ).rejects.toThrow("missing/changed inputs");
      sourceUtxos.set(firstAddress, originalSourceUtxos.get(firstAddress)!);
      const result = await terminalDrainStressWallets(common, {
        prepareTransfer: async () => {
          throw new Error("resume rebuilt a durable terminal drain");
        },
        submitPreparedTransfer: async ({ txHash, signedTxCbor }) => {
          submitCalls += 1;
          const intent = preparedByHash.get(txHash)!;
          expect(signedTxCbor).toBe(intent.cbor);
          sourceUtxos.set(intent.address, []);
          treasury += intent.amount;
          submitted.add(txHash);
          return { txHash, status: "accepted" };
        },
        fetchTxStatus: async (_endpoint, txHash) =>
          submitted.has(txHash) ? "committed" : "not_found",
        fetchUtxos,
        sleep: async () => {},
      });
      expect(result.phase).toBe("committed");
      expect(result.residualSourceLovelace).toBe("0");
      expect(parseStressWalletTerminalDrainResult(result).schemaVersion).toBe(
        "midgard-stress-wallet-terminal-drain-result-v1",
      );
      expect(result.grossSourceLovelace).toBe("2500");
      expect(result.treasuryDeltaLovelace).toBe("2500");
      expect(result.totalFeesLovelace).toBe("0");
      expect(submitCalls).toBe(3);
      expect(
        (await (await import("node:fs/promises")).stat(result.reportPath!))
          .mode & 0o777,
      ).toBe(0o600);
      const immutableReport = await readFile(result.reportPath!, "utf8");
      expect(
        parseStressWalletTerminalDrainReport(
          JSON.parse(immutableReport) as unknown,
        ).schemaVersion,
      ).toBe("midgard-stress-wallet-terminal-drain-report-v1");
      const parsedImmutableReport = JSON.parse(immutableReport) as Record<
        string,
        unknown
      >;
      expect(() =>
        parseStressWalletTerminalDrainReport({
          ...parsedImmutableReport,
          conservation: {
            ...(parsedImmutableReport.conservation as Record<string, unknown>),
            historical: true,
          },
        }),
      ).toThrow("extra=[historical]");
      await expect(
        terminalDrainStressWallets(common, {
          prepareTransfer: async () => {
            throw new Error("report replay rebuilt");
          },
          submitPreparedTransfer: async () => {
            throw new Error("report replay submitted");
          },
          fetchTxStatus: async () => "committed",
          fetchUtxos,
          sleep: async () => {},
        }),
      ).rejects.toThrow();
      expect(await readFile(result.reportPath!, "utf8")).toBe(immutableReport);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("rejects every non-canonical already_empty variant before status, submission, or journal rewrite", async () => {
    const dir = await mkdtemp(join("/tmp", "midgard-terminal-empty-"));
    try {
      await createL2Wallets({
        count: 1,
        outDir: dir,
        network: "Preprod",
        generateSeedPhrase: seedGenerator(),
      });
      const options = {
        count: 1,
        outDir: dir,
        network: "Preprod" as const,
        treasurySeedPhrase: TEST_SEEDS[2]!,
        nodeEndpoint: "http://127.0.0.1:3000",
        minFeeA: 0n,
        minFeeB: 0n,
        prepareOnly: true,
      };
      const runtime = {
        prepareTransfer: async () => {
          throw new Error("empty wallet prepared");
        },
        submitPreparedTransfer: async () => {
          throw new Error("empty wallet submitted");
        },
        fetchTxStatus: vi.fn(async () => "committed"),
        fetchUtxos: async () => [] as readonly NodeUtxo[],
      };
      const prepared = await terminalDrainStressWallets(options, runtime);
      const canonical = await readFile(prepared.statePath, "utf8");
      const variants: readonly ((state: any) => void)[] = [
        (state) => {
          state.entries[0].beforeValueSha256 = "00".repeat(32);
        },
        (state) => {
          state.entries[0].txHash = "11".repeat(32);
        },
        (state) => {
          state.entries[0].signedTxCbor = "00";
        },
        (state) => {
          state.entries[0].selectedInputs = [];
        },
        (state) => {
          state.entries[0].selectedInputLovelace = "0";
        },
        (state) => {
          state.entries[0].requestedLovelace = "0";
        },
        (state) => {
          state.entries[0].feeLovelace = "0";
        },
        (state) => {
          state.entries[0].signedTxBytes = 1;
        },
        (state) => {
          state.entries[0].acceptedStatus = "committed";
        },
      ];
      for (const mutate of variants) {
        const state = JSON.parse(canonical) as any;
        mutate(state);
        const tampered = formatJson(state) + "\n";
        await writeFile(prepared.statePath, tampered, "utf8");
        await expect(
          terminalDrainStressWallets(options, runtime),
        ).rejects.toThrow();
        expect(await readFile(prepared.statePath, "utf8")).toBe(tampered);
      }
      expect(runtime.fetchTxStatus).not.toHaveBeenCalled();
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("keeps the prepare-all barrier on partial failure and rejects a CAS mutation without submission", async () => {
    for (const mode of ["partial", "cas"] as const) {
      const dir = await mkdtemp(join("/tmp", "midgard-terminal-prepare-"));
      try {
        const created = await createL2Wallets({
          count: mode === "partial" ? 2 : 1,
          outDir: dir,
          network: "Preprod",
          generateSeedPhrase: seedGenerator(),
        });
        const utxos = new Map<string, readonly NodeUtxo[]>();
        for (let i = 0; i < created.wallets.length; i += 1) {
          const w = created.wallets[i]!,
            hash = (i === 0 ? "a1" : "a2").repeat(32);
          utxos.set(w.l2Address, [
            {
              txHash: hash,
              outputIndex: 0,
              outrefCbor: makeOutRefCbor(hash, 0),
              outputCbor: Buffer.from(
                makeMidgardTxOutput(
                  CML.Address.from_bech32(w.l2Address),
                  assetsToValue({ lovelace: 1000n }),
                ).to_cbor_bytes(),
              ),
              address: w.l2Address,
              assets: { lovelace: 1000n },
            },
          ]);
        }
        let prepares = 0,
          submits = 0;
        const statePath = join(dir, "terminal-drain-state.json");
        await expect(
          terminalDrainStressWallets(
            {
              count: created.wallets.length,
              outDir: dir,
              network: "Preprod",
              treasurySeedPhrase: TEST_SEEDS[2]!,
              nodeEndpoint: "http://127.0.0.1:3000",
              minFeeA: 0n,
              minFeeB: 0n,
            },
            {
              prepareTransfer: async ({ source, treasuryAddress }) => {
                prepares += 1;
                if (mode === "partial" && prepares === 2)
                  throw new Error("partial prepare failure");
                const tx = await prepareCanonicalTerminalDrain({
                  sourceSeedPhrase: source.seedPhrase,
                  sourceAddress: source.l2Address,
                  destinationAddress: treasuryAddress,
                  utxos: utxos.get(source.l2Address)!,
                });
                if (mode === "cas")
                  await writeFile(statePath, "external-cas-mutation\n", "utf8");
                return tx;
              },
              submitPreparedTransfer: async () => {
                submits += 1;
                throw new Error("barrier submitted");
              },
              fetchTxStatus: async () => "not_found",
              fetchUtxos: async (_e, address) => utxos.get(address) ?? [],
            },
          ),
        ).rejects.toThrow(
          mode === "partial" ? "partial prepare failure" : "generation changed",
        );
        expect(submits).toBe(0);
        if (mode === "partial")
          await expect(access(statePath)).rejects.toThrow();
        else
          expect(await readFile(statePath, "utf8")).toBe(
            "external-cas-mutation\n",
          );
      } finally {
        await rm(dir, { recursive: true, force: true });
      }
    }
  });

  it("serializes prepare deposits against terminal drain and never removes a replacement shared lock", async () => {
    const dir = await mkdtemp(join("/tmp", "midgard-funds-lock-"));
    try {
      await createL2Wallets({
        count: 1,
        outDir: dir,
        network: "Preprod",
        generateSeedPhrase: seedGenerator(),
      });
      let release!: () => void;
      const gate = new Promise<void>((resolve) => {
        release = resolve;
      });
      let depositStarted!: () => void;
      const started = new Promise<void>((resolve) => {
        depositStarted = resolve;
      });
      let funded = false;
      const preparation = prepareStressWallets(
        {
          count: 1,
          outDir: dir,
          network: "Preprod",
          lovelacePerWallet: 1000n,
          projectionWaitMs: 0,
          verifyTimeoutMs: 1000,
          pollIntervalMs: 0,
        },
        {
          submitDeposit: async () => {
            depositStarted();
            await gate;
            funded = true;
            return { txHash: "aa".repeat(32) };
          },
          projectDeposits: async () => {},
          fetchUtxos: async (_e, address) =>
            funded
              ? [nodeUtxo({ txHashByte: "ab", address, lovelace: 1000n })]
              : [],
          sleep: async () => {},
        },
      );
      await started;
      await expect(
        terminalDrainStressWallets(
          {
            count: 1,
            outDir: dir,
            network: "Preprod",
            treasurySeedPhrase: TEST_SEEDS[2]!,
            minFeeA: 0n,
            minFeeB: 0n,
            prepareOnly: true,
          },
          {
            prepareTransfer: async () => {
              throw new Error("cross-operation prepare");
            },
            submitPreparedTransfer: async () => {
              throw new Error("cross-operation submit");
            },
            fetchTxStatus: async () => "not_found",
            fetchUtxos: async () => [],
          },
        ),
      ).rejects.toThrow("funds operations are exclusively locked");
      const lockPath = join(dir, "stress-wallet-funds.lock");
      await rm(lockPath);
      await writeFile(
        lockPath,
        formatJson({ token: "replacement" }) + "\n",
        "utf8",
      );
      release();
      await expect(preparation).rejects.toThrow("lock ownership changed");
      expect(JSON.parse(await readFile(lockPath, "utf8"))).toEqual({
        token: "replacement",
      });
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("rejects a nonempty zero-value residual UTxO and a treasury conservation mismatch", async () => {
    for (const mode of ["zero-utxo", "conservation"] as const) {
      const dir = await mkdtemp(join("/tmp", "midgard-terminal-final-"));
      try {
        const created = await createL2Wallets({
          count: 1,
          outDir: dir,
          network: "Preprod",
          generateSeedPhrase: seedGenerator(),
        });
        const source = created.wallets[0]!;
        const initialHash = "b1".repeat(32);
        const canonicalSource: NodeUtxo = {
          txHash: initialHash,
          outputIndex: 0,
          outrefCbor: makeOutRefCbor(initialHash, 0),
          outputCbor: Buffer.from(
            makeMidgardTxOutput(
              CML.Address.from_bech32(source.l2Address),
              assetsToValue({ lovelace: 1000n }),
            ).to_cbor_bytes(),
          ),
          address: source.l2Address,
          assets: { lovelace: 1000n },
        };
        let sourceUtxos: readonly NodeUtxo[] =
          mode === "zero-utxo" ? [] : [canonicalSource];
        const options = {
          count: 1,
          outDir: dir,
          network: "Preprod" as const,
          treasurySeedPhrase: TEST_SEEDS[2]!,
          nodeEndpoint: "http://127.0.0.1:3000",
          minFeeA: 0n,
          minFeeB: 0n,
          verificationTimeoutMs: 0,
          pollInitialIntervalMs: 1,
        };
        const fetchUtxos = async (_e: string, address: string) =>
          address === source.l2Address ? sourceUtxos : [];
        await terminalDrainStressWallets(
          { ...options, prepareOnly: true },
          {
            prepareTransfer: async ({ source: record, treasuryAddress }) =>
              prepareCanonicalTerminalDrain({
                sourceSeedPhrase: record.seedPhrase,
                sourceAddress: record.l2Address,
                destinationAddress: treasuryAddress,
                utxos: sourceUtxos,
              }),
            submitPreparedTransfer: async () => {
              throw new Error("prepare-only submit");
            },
            fetchTxStatus: async () => "not_found",
            fetchUtxos,
          },
        );
        if (mode === "zero-utxo")
          sourceUtxos = [
            nodeUtxo({
              txHashByte: "b2",
              address: source.l2Address,
              lovelace: 0n,
            }),
          ];
        else sourceUtxos = [];
        await expect(
          terminalDrainStressWallets(options, {
            prepareTransfer: async () => {
              throw new Error("resume rebuild");
            },
            submitPreparedTransfer: async ({ txHash }) => {
              sourceUtxos = [];
              return { txHash, status: "accepted" };
            },
            fetchTxStatus: async () =>
              mode === "zero-utxo" ? "committed" : "committed",
            fetchUtxos,
            sleep: async () => {},
            monotonicNow: (() => {
              let n = 0;
              return () => n++;
            })(),
          }),
        ).rejects.toThrow("exact-zero conservation did not converge");
      } finally {
        await rm(dir, { recursive: true, force: true });
      }
    }
  });
});
