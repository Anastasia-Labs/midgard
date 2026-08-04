import "./utils.js";

import { createHash } from "node:crypto";
import { EventEmitter } from "node:events";
import {
  mkdir,
  mkdtemp,
  readdir,
  readFile,
  stat,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { basename, dirname, join } from "node:path";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import {
  assetsToValue,
  CML,
  getAddressDetails,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { formatJson } from "@/commands/command-utils.js";
import { assembleCorpusShards } from "@/commands/stress-corpus/assemble.js";
import {
  buildCorpusChain,
  nodeUtxoFromCorpusFunding,
} from "@/commands/stress-corpus/build-chain.js";
import {
  createHistoricalExtensionSchedule,
  parseCorpusIndexEntries,
  readVerifiedHistoricalRetainedTerminals,
  verifyHistoricalExtensionCorpus,
} from "@/commands/stress-corpus/historical-extension.js";
import {
  type CorpusWorkerFleetHandle,
  generateHistoricalCorpusExtension,
  HISTORICAL_EXTENSION_BINDING_SCHEMA_VERSION,
  HISTORICAL_EXTENSION_GENERATION_SCHEMA_VERSION,
  type HistoricalExtensionConfig,
  parseHistoricalExtensionConfig,
  runCorpusWorkerFleet,
  verifyHistoricalExtensionBaseEvidence,
} from "@/commands/stress-corpus/historical-extension-command.js";
import {
  generateStressCorpus,
  parseStressCorpusGenerateConfig,
} from "@/commands/stress-corpus-generate.js";
import { STRESS_WALLET_SCHEMA_VERSION } from "@/commands/stress-wallets.js";
import { makeTransferMidgard } from "@/commands/transfer-build-core.js";
import type { CorpusWorkerInput } from "@/workers/corpus-chain-builder.js";
import { runCorpusChainWorker } from "@/workers/corpus-chain-builder.js";

import { makeMidgardTxOutput } from "./midgard-output-helpers.js";

const SYNTHETIC_TEST_SEEDS = [
  "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart",
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
] as const;

const sha256File = async (path: string): Promise<string> =>
  createHash("sha256")
    .update(await readFile(path))
    .digest("hex");

const listRegularFiles = async (root: string): Promise<readonly string[]> => {
  const entries = await readdir(root, { withFileTypes: true });
  const nested = await Promise.all(
    entries.map(async (entry) => {
      const path = join(root, entry.name);
      if (entry.isDirectory()) {
        return listRegularFiles(path);
      }
      return entry.isFile() ? [path] : [];
    }),
  );
  return nested.flat();
};

const emptyFleetInput = (index: number): CorpusWorkerInput => ({
  shardPath: `/tmp/historical-extension-fleet-${index.toString()}.ndjson`,
  walletBatch: [],
  depth: 1,
  amountLovelace: "1",
  feeParams: { minFeeA: "0", minFeeB: "0" },
  network: "Preprod",
  networkId: "0",
  maxSubmitTxCborBytes: 32_768,
  planShape: "chain",
  terminalChangeFloorLovelace: "1",
});

class FakeCorpusWorker extends EventEmitter {
  public terminated = false;
  public joined = false;

  public async terminate(): Promise<number> {
    this.terminated = true;
    await Promise.resolve();
    this.joined = true;
    return 1;
  }
}

const fundingOutputCbor = (address: string, lovelace: bigint): string =>
  Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(address),
      assetsToValue({ lovelace }),
    ).to_cbor_bytes(),
  ).toString("hex");

const writeSyntheticWallet = async ({
  walletsDir,
  seedPhrase,
  index,
}: {
  readonly walletsDir: string;
  readonly seedPhrase: string;
  readonly index: number;
}): Promise<void> => {
  const wallet = walletFromSeed(seedPhrase, { network: "Preprod" });
  const paymentCredential = getAddressDetails(wallet.address).paymentCredential;
  if (paymentCredential?.type !== "Key") {
    throw new Error("synthetic test wallet must have a payment credential");
  }
  const label = index.toString().padStart(4, "0");
  const lovelace = 5_000_000n;
  const record = {
    schemaVersion: STRESS_WALLET_SCHEMA_VERSION,
    walletId: `stress-wallet-${label}`,
    index,
    envName: `SYNTHETIC_STRESS_WALLET_${label}`,
    network: "Preprod",
    seedPhrase,
    l2Address: wallet.address,
    paymentKeyHash: paymentCredential.hash,
    createdAt: "2026-07-14T00:00:00.000Z",
    latestFunding: {
      preparedAt: "2026-07-14T00:00:00.000Z",
      status: "already_funded",
      lovelacePerWallet: lovelace.toString(10),
      nodeEndpoint: "http://127.0.0.1:3000",
      beforeUtxoCount: 1,
      afterUtxoCount: 1,
      verifiedFundingUtxoCount: 1,
      fundingUtxos: [
        {
          outref: `${index.toString(16).padStart(64, "0")}#0`,
          outputCbor: fundingOutputCbor(wallet.address, lovelace),
          lovelace: lovelace.toString(10),
        },
      ],
    },
  };
  await writeFile(
    join(walletsDir, `wallet-${label}.json`),
    `${formatJson(record)}\n`,
    "utf8",
  );
};

type SyntheticFixture = {
  readonly root: string;
  readonly walletsDir: string;
  readonly baseCorpusPath: string;
  readonly baseIndexPath: string;
  readonly config: HistoricalExtensionConfig;
};

const makeSyntheticFixture = async (): Promise<SyntheticFixture> => {
  const root = await mkdtemp(
    join(tmpdir(), "midgard-historical-extension-test-"),
  );
  const walletsDir = join(root, "wallets");
  const baseOutDir = join(root, "base");
  await mkdir(walletsDir, { recursive: true });
  await Promise.all(
    SYNTHETIC_TEST_SEEDS.map((seedPhrase, index) =>
      writeSyntheticWallet({
        walletsDir,
        seedPhrase,
        index: index + 1,
      }),
    ),
  );
  const base = await generateStressCorpus(
    parseStressCorpusGenerateConfig(
      {
        targetRateTps: "1",
        durationMs: "4000",
        walletCount: "2",
        safetyFactor: "1",
        amountLovelace: "1",
        minFeeA: "10",
        minFeeB: "10",
        maxSubmitTxCborBytes: "32768",
        walletsDir,
        outDir: baseOutDir,
        workers: "1",
        corpusSliceIdPrefix: "phase1-synthetic",
        yes: true,
      },
      {},
    ),
  );
  const walletRecords = await Promise.all(
    [1, 2].map(
      async (index) =>
        JSON.parse(
          await readFile(
            join(
              walletsDir,
              `wallet-${index.toString().padStart(4, "0")}.json`,
            ),
            "utf8",
          ),
        ) as Record<string, unknown>,
    ),
  );
  const summaries = walletRecords.map(
    ({ seedPhrase: _seedPhrase, ...rest }) => rest,
  );
  const fanoutPath = join(root, "fanout-report.json");
  await writeFile(
    fanoutPath,
    `${formatJson({
      schemaVersion: "midgard-stress-wallet-fanout-v1",
      requestedCount: 2,
      verifiedWalletCount: 2,
      wallets: summaries.map((wallet) => ({
        wallet,
        verifiedFundingUtxoCount: 1,
      })),
    })}\n`,
    "utf8",
  );
  const manifestSha256 = await sha256File(base.manifestPath);
  const bindingPath = join(root, "phase1-binding.json");
  await writeFile(
    bindingPath,
    `${formatJson({
      schemaVersion: "midgard-phase1-live-corpus-binding-v2",
      walletSetSha256: base.walletSetIdentity.walletSetSha256,
      fundingSetSha256: base.walletSetIdentity.fundingSetSha256,
      corpus: {
        corpusSha256: base.assembled.corpusSha256,
        indexSha256: base.assembled.indexSha256,
        manifestSha256,
      },
    })}\n`,
    "utf8",
  );
  const config: HistoricalExtensionConfig = {
    baseCorpus: {
      path: base.corpusPath,
      sha256: base.assembled.corpusSha256,
    },
    baseIndex: {
      path: base.indexPath,
      sha256: base.assembled.indexSha256,
    },
    baseManifest: { path: base.manifestPath, sha256: manifestSha256 },
    baseVerification: {
      path: base.verified.verificationArtifact.path,
      sha256: base.verified.verificationArtifact.sha256,
    },
    baseBinding: { path: bindingPath, sha256: await sha256File(bindingPath) },
    fanoutReport: {
      path: fanoutPath,
      sha256: await sha256File(fanoutPath),
    },
    walletsDir,
    outDir: join(root, "extension"),
    baseChainCount: 2,
    baseDepth: 2,
    targetRowCount: 7,
    workers: 1,
    yes: true,
  };
  return {
    root,
    walletsDir,
    baseCorpusPath: base.corpusPath,
    baseIndexPath: base.indexPath,
    config,
  };
};

describe("historical stress-corpus extension", () => {
  it("computes the exact balanced 4,096-chain schedule for five million rows", () => {
    const schedule = createHistoricalExtensionSchedule({
      orderedChainIds: Array.from(
        { length: 4_096 },
        (_entry, index) =>
          `stress-wallet-${(index + 1).toString().padStart(4, "0")}`,
      ),
      baseDepth: 748,
      targetRowCount: 5_000_000,
    });

    expect(schedule).toMatchObject({
      baseChainCount: 4_096,
      baseRowCount: 3_063_808,
      targetRowCount: 5_000_000,
      extensionRowCount: 1_936_192,
      minimumTargetDepth: 1_220,
      maximumTargetDepth: 1_221,
      depthHistogram: [
        { targetDepth: 1_220, chainCount: 1_216 },
        { targetDepth: 1_221, chainCount: 2_880 },
      ],
    });
  });

  it("keeps the complete base byte prefix and appends an exact variable-depth schedule", async () => {
    const fixture = await makeSyntheticFixture();
    const result = await generateHistoricalCorpusExtension(fixture.config);

    expect(result).toMatchObject({
      schemaVersion: HISTORICAL_EXTENSION_GENERATION_SCHEMA_VERSION,
      claimScope: "historical-offline-corpus-extension",
      freshLiveClaim: false,
      verification: {
        rowCount: 7,
        uniqueChainCount: 2,
        indexEntryCount: 4,
        checkedPrefixRows: 4,
        checkedExtensionRows: 3,
        checkedContinuationCount: 2,
      },
      fundingModel: {
        source:
          "cryptographically-verified-retained-terminal-output-1-per-wallet",
        retainedTerminalSetHashAlgorithm:
          "sha256-chain-id-outref-output-cbor-sha256-lovelace-lines-v1",
        freshFundingLovelace: "0",
        continuationFundingValueSource:
          "decoded-canonical-retained-terminal-output-1-cross-checked-against-wallet-and-chain",
        proof:
          "every continuation started from canonical retained terminal output 1 and built only its scheduled extension rows",
      },
    });
    expect(result.schedule.depthHistogram).toEqual([
      { targetDepth: 3, chainCount: 1 },
      { targetDepth: 4, chainCount: 1 },
    ]);
    const baseBytes = await readFile(fixture.baseCorpusPath);
    const extendedBytes = await readFile(result.files.corpus.path);
    expect(extendedBytes.subarray(0, baseBytes.length)).toEqual(baseBytes);
    const binding = JSON.parse(
      await readFile(result.files.historicalBinding.path, "utf8"),
    ) as Record<string, unknown>;
    expect(binding.schemaVersion).toBe(
      HISTORICAL_EXTENSION_BINDING_SCHEMA_VERSION,
    );
    expect(binding.schemaVersion).not.toBe(
      "midgard-phase1-live-corpus-binding-v2",
    );
    for (const artifactPath of [
      result.files.manifest.path,
      result.files.verification.path,
      result.files.historicalBinding.path,
      result.generationResultPath,
    ]) {
      const bytes = await readFile(artifactPath, "utf8");
      expect(bytes).not.toContain(SYNTHETIC_TEST_SEEDS[0]);
      expect(bytes).not.toContain(SYNTHETIC_TEST_SEEDS[1]);
      expect(bytes).toContain('"freshLiveClaim": false');
    }
  });

  it("continues a legacy terminal without rebuilding it through the current required-signer builder", async () => {
    const fixture = await makeSyntheticFixture();
    const wallet = JSON.parse(
      await readFile(join(fixture.walletsDir, "wallet-0001.json"), "utf8"),
    ) as {
      readonly walletId: string;
      readonly seedPhrase: string;
      readonly l2Address: string;
      readonly latestFunding: {
        readonly fundingUtxos: readonly {
          readonly outref: string;
          readonly outputCbor: string;
        }[];
      };
    };
    const secondWallet = JSON.parse(
      await readFile(join(fixture.walletsDir, "wallet-0002.json"), "utf8"),
    ) as typeof wallet;
    const initialFunding = wallet.latestFunding.fundingUtxos[0]!;
    const [initialTxHash, initialOutputIndex] =
      initialFunding.outref.split("#");
    const corpusFundingUtxo = {
      txHash: initialTxHash!,
      outputIndex: Number(initialOutputIndex),
      outputCborHex: initialFunding.outputCbor,
    };
    const currentStrict = await buildCorpusChain({
      seedPhrase: wallet.seedPhrase,
      walletId: wallet.walletId,
      fundingUtxo: corpusFundingUtxo,
      depth: 1,
      amountLovelace: 1n,
      feeParams: { minFeeA: 10n, minFeeB: 10n },
      network: "Preprod",
      networkId: 0n,
      maxSubmitTxCborBytes: 32_768,
      corpusSliceId: "legacy-required-signer-regression",
      planShape: "chain",
      terminalChangeFloorLovelace: 1n,
    });
    const strictRow = currentStrict.rows[0]!;
    const strictNative = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(strictRow.canonicalCborHex, "hex"),
    );
    expect(
      strictNative.body.requiredSignersPreimageCbor.toString("hex"),
    ).not.toBe("80");

    const derivedWallet = walletFromSeed(wallet.seedPhrase, {
      network: "Preprod",
    });
    const legacyMidgard = await makeTransferMidgard({
      senderAddress: wallet.l2Address,
      signer: derivedWallet.paymentKey,
      utxos: [nodeUtxoFromCorpusFunding(corpusFundingUtxo)],
      network: "Preprod",
      networkId: 0n,
      minFeeA: 10n,
      minFeeB: 10n,
      maxSubmitTxCborBytes: 32_768,
    });
    const legacyCompleted = await legacyMidgard
      .newTx()
      .pay.ToAddress(wallet.l2Address, { lovelace: 1n })
      .complete({
        changeAddress: wallet.l2Address,
        feePolicy: "provider",
      });
    const legacySigned = await legacyCompleted.sign();
    const legacyCbor = legacySigned.txCbor;
    const legacyNative = decodeMidgardNativeTxFullFromCanonicalCbor(legacyCbor);
    const legacyTxHash = computeMidgardNativeTxId(legacyNative).toString("hex");
    expect(legacyNative.body.requiredSignersPreimageCbor.toString("hex")).toBe(
      "80",
    );
    expect(legacyTxHash).not.toBe(strictRow.txHash);
    const legacyOutputs = decodeMidgardNativeByteListPreimage(
      legacyNative.body.outputsPreimageCbor,
      "native.outputs",
    );
    const legacyRow = {
      txHash: legacyTxHash,
      canonicalCborHex: legacyCbor.toString("hex"),
      canonicalCborSha256: createHash("sha256")
        .update(legacyCbor)
        .digest("hex"),
      canonicalCborByteLength: legacyCbor.length,
      senderWalletId: wallet.walletId,
      selectedInputOutref: initialFunding.outref,
      outputOutrefs: legacyOutputs.map(
        (_outref, outputIndex) => `${legacyTxHash}#${outputIndex.toString()}`,
      ),
      planShape: "chain" as const,
      parentTxHash: null,
      corpusSliceId: "legacy-required-signer-regression",
    };
    const secondFunding = secondWallet.latestFunding.fundingUtxos[0]!;
    const [secondTxHash, secondOutputIndex] = secondFunding.outref.split("#");
    const secondStrict = await buildCorpusChain({
      seedPhrase: secondWallet.seedPhrase,
      walletId: secondWallet.walletId,
      fundingUtxo: {
        txHash: secondTxHash!,
        outputIndex: Number(secondOutputIndex),
        outputCborHex: secondFunding.outputCbor,
      },
      depth: 1,
      amountLovelace: 1n,
      feeParams: { minFeeA: 10n, minFeeB: 10n },
      network: "Preprod",
      networkId: 0n,
      maxSubmitTxCborBytes: 32_768,
      corpusSliceId: "legacy-required-signer-regression",
      planShape: "chain",
      terminalChangeFloorLovelace: 1n,
    });
    const baseShardPath = join(fixture.root, "legacy-base.raw.ndjson");
    await writeFile(
      baseShardPath,
      `${JSON.stringify(legacyRow)}\n${JSON.stringify(secondStrict.rows[0]!)}\n`,
      "utf8",
    );
    const baseCorpusPath = join(fixture.root, "legacy-base.ndjson");
    const baseIndexPath = `${baseCorpusPath}.index.ndjson`;
    const base = await assembleCorpusShards({
      shardPaths: [baseShardPath],
      corpusPath: baseCorpusPath,
      indexPath: baseIndexPath,
    });
    const schedule = createHistoricalExtensionSchedule({
      orderedChainIds: [wallet.walletId, secondWallet.walletId],
      baseDepth: 1,
      targetRowCount: 4,
    });
    const [terminal, secondTerminal] =
      await readVerifiedHistoricalRetainedTerminals({
        corpusPath: baseCorpusPath,
        index: base.indexEntries,
        expectations: [
          {
            chainId: wallet.walletId,
            address: wallet.l2Address,
            corpusSliceId: "legacy-required-signer-regression",
            baseDepth: 1,
          },
          {
            chainId: secondWallet.walletId,
            address: secondWallet.l2Address,
            corpusSliceId: "legacy-required-signer-regression",
            baseDepth: 1,
          },
        ],
      });
    expect(terminal?.parentTxHash).toBe(legacyTxHash);

    const extensionShardPath = join(fixture.root, "legacy-extension.ndjson");
    await runCorpusChainWorker({
      shardPath: extensionShardPath,
      walletBatch: [
        {
          seedPhrase: wallet.seedPhrase,
          walletId: wallet.walletId,
          fundingUtxo: terminal!.fundingUtxo,
          corpusSliceId: "legacy-required-signer-regression",
          depth: 1,
          retainedParentTxHash: terminal!.parentTxHash,
        },
        {
          seedPhrase: secondWallet.seedPhrase,
          walletId: secondWallet.walletId,
          fundingUtxo: secondTerminal!.fundingUtxo,
          corpusSliceId: "legacy-required-signer-regression",
          depth: 1,
          retainedParentTxHash: secondTerminal!.parentTxHash,
        },
      ],
      depth: 1,
      amountLovelace: "1",
      feeParams: { minFeeA: "10", minFeeB: "10" },
      network: "Preprod",
      networkId: "0",
      maxSubmitTxCborBytes: 32_768,
      planShape: "chain",
      terminalChangeFloorLovelace: "1",
    });
    const extendedCorpusPath = join(fixture.root, "legacy-extended.ndjson");
    const extendedIndexPath = `${extendedCorpusPath}.index.ndjson`;
    await assembleCorpusShards({
      shardPaths: [baseCorpusPath, extensionShardPath],
      corpusPath: extendedCorpusPath,
      indexPath: extendedIndexPath,
    });
    const verification = await verifyHistoricalExtensionCorpus({
      baseCorpusPath,
      baseIndexPath,
      extendedCorpusPath,
      extendedIndexPath,
      schedule,
    });
    expect(verification).toMatchObject({
      rowCount: 4,
      checkedContinuationCount: 2,
      checkedCanonicalBaseTerminalRows: 2,
      checkedCanonicalContinuationRows: 2,
    });
    const continuation = JSON.parse(
      (await readFile(extensionShardPath, "utf8")).trim().split("\n")[0]!,
    ) as {
      readonly parentTxHash: string;
      readonly selectedInputOutref: string;
    };
    expect(continuation).toMatchObject({
      parentTxHash: legacyTxHash,
      selectedInputOutref: `${legacyTxHash}#1`,
    });
  });

  it("fails closed on retained terminal address, outref, and canonical identity mismatch", async () => {
    const fixture = await makeSyntheticFixture();
    const index = await parseCorpusIndexEntries(fixture.baseIndexPath);
    const records = await Promise.all(
      [1, 2].map(
        async (walletIndex) =>
          JSON.parse(
            await readFile(
              join(
                fixture.walletsDir,
                `wallet-${walletIndex.toString().padStart(4, "0")}.json`,
              ),
              "utf8",
            ),
          ) as { readonly walletId: string; readonly l2Address: string },
      ),
    );
    const recordsById = new Map(
      records.map((record) => [record.walletId, record]),
    );
    const expectations = index.map((entry) => ({
      chainId: entry.chainId,
      address: recordsById.get(entry.chainId)!.l2Address,
      corpusSliceId: entry.corpusSliceId,
      baseDepth: entry.rowCount,
    }));
    await expect(
      readVerifiedHistoricalRetainedTerminals({
        corpusPath: fixture.baseCorpusPath,
        index,
        expectations: expectations.map((expectation, position) =>
          position === 0
            ? { ...expectation, address: records[1]!.l2Address }
            : expectation,
        ),
      }),
    ).rejects.toThrow("belongs to");

    const baseRows = (await readFile(fixture.baseCorpusPath, "utf8"))
      .trimEnd()
      .split("\n")
      .map((line) => JSON.parse(line) as Record<string, unknown>);
    const verifyMutatedTerminal = async ({
      label,
      mutate,
      expectedError,
    }: {
      readonly label: string;
      readonly mutate: (
        terminal: Record<string, unknown>,
      ) => Record<string, unknown>;
      readonly expectedError: string;
    }): Promise<void> => {
      const rows = baseRows.map((row) => ({ ...row }));
      rows[1] = mutate(rows[1]!);
      const rawPath = join(fixture.root, `${label}.raw.ndjson`);
      const corpusPath = join(fixture.root, `${label}.ndjson`);
      const indexPath = `${corpusPath}.index.ndjson`;
      await writeFile(
        rawPath,
        `${rows.map((row) => JSON.stringify(row)).join("\n")}\n`,
        "utf8",
      );
      const assembled = await assembleCorpusShards({
        shardPaths: [rawPath],
        corpusPath,
        indexPath,
      });
      await expect(
        readVerifiedHistoricalRetainedTerminals({
          corpusPath,
          index: assembled.indexEntries,
          expectations,
        }),
      ).rejects.toThrow(expectedError);
    };
    await verifyMutatedTerminal({
      label: "terminal-outref-mismatch",
      mutate: (terminal) => ({
        ...terminal,
        outputOutrefs: [
          ...(terminal.outputOutrefs as string[]).slice(0, 1),
          `${"f".repeat(64)}#1`,
        ],
      }),
      expectedError: "declared output outrefs",
    });
    await verifyMutatedTerminal({
      label: "terminal-canonical-identity-mismatch",
      mutate: (terminal) => ({
        ...terminal,
        canonicalCborHex: baseRows[0]!.canonicalCborHex,
        canonicalCborSha256: baseRows[0]!.canonicalCborSha256,
        canonicalCborByteLength: baseRows[0]!.canonicalCborByteLength,
      }),
      expectedError: "transaction ID does not match",
    });
  });

  it("rejects a tampered retained prefix and a tampered depth schedule", async () => {
    const fixture = await makeSyntheticFixture();
    const result = await generateHistoricalCorpusExtension(fixture.config);
    const rows = (await readFile(result.files.corpus.path, "utf8"))
      .trimEnd()
      .split("\n");
    const first = JSON.parse(rows[0]!) as Record<string, unknown>;
    rows[0] = JSON.stringify({ ...first, corpusSliceId: "tampered" });
    const tamperedCorpusPath = join(fixture.root, "tampered-corpus.ndjson");
    await writeFile(tamperedCorpusPath, `${rows.join("\n")}\n`, "utf8");
    await expect(
      verifyHistoricalExtensionCorpus({
        baseCorpusPath: fixture.baseCorpusPath,
        baseIndexPath: fixture.baseIndexPath,
        extendedCorpusPath: tamperedCorpusPath,
        extendedIndexPath: result.files.index.path,
        schedule: result.schedule,
      }),
    ).rejects.toThrow("prefix differs");

    const tamperedSchedule = {
      ...result.schedule,
      entries: result.schedule.entries.map((entry, index) =>
        index === 0 ? { ...entry, targetDepth: entry.targetDepth + 1 } : entry,
      ),
    };
    await expect(
      verifyHistoricalExtensionCorpus({
        baseCorpusPath: fixture.baseCorpusPath,
        baseIndexPath: fixture.baseIndexPath,
        extendedCorpusPath: result.files.corpus.path,
        extendedIndexPath: result.files.index.path,
        schedule: tamperedSchedule,
      }),
    ).rejects.toThrow("deterministic balanced schedule");
  });

  it("decodes every continuation and rejects canonical transaction identity tampering", async () => {
    const fixture = await makeSyntheticFixture();
    const result = await generateHistoricalCorpusExtension(fixture.config);
    const rows = (await readFile(result.files.corpus.path, "utf8"))
      .trimEnd()
      .split("\n");
    const target = JSON.parse(rows[4]!) as Record<string, unknown>;
    const donor = JSON.parse(rows[5]!) as Record<string, unknown>;
    rows[4] = JSON.stringify({
      ...target,
      canonicalCborHex: donor.canonicalCborHex,
      canonicalCborSha256: donor.canonicalCborSha256,
      canonicalCborByteLength: donor.canonicalCborByteLength,
    });
    const rawPath = join(fixture.root, "native-identity-tampered.raw.ndjson");
    const corpusPath = join(fixture.root, "native-identity-tampered.ndjson");
    const indexPath = `${corpusPath}.index.ndjson`;
    await writeFile(rawPath, `${rows.join("\n")}\n`, "utf8");
    await assembleCorpusShards({
      shardPaths: [rawPath],
      corpusPath,
      indexPath,
    });

    await expect(
      verifyHistoricalExtensionCorpus({
        baseCorpusPath: fixture.baseCorpusPath,
        baseIndexPath: fixture.baseIndexPath,
        extendedCorpusPath: corpusPath,
        extendedIndexPath: indexPath,
        schedule: result.schedule,
      }),
    ).rejects.toThrow("transaction ID does not match");
  });

  it("fails before publication when original evidence mutates after its immutable snapshot", async () => {
    const fixture = await makeSyntheticFixture();
    const originalBaseBytes = await readFile(fixture.baseCorpusPath);
    await expect(
      generateHistoricalCorpusExtension(fixture.config, {
        afterImmutableSnapshot: async () => {
          await writeFile(
            fixture.baseCorpusPath,
            Buffer.concat([originalBaseBytes, Buffer.from("{}\n", "utf8")]),
          );
        },
      }),
    ).rejects.toThrow("base corpus SHA-256 mismatch");
    await expect(stat(fixture.config.outDir)).rejects.toMatchObject({
      code: "ENOENT",
    });
    expect(await sha256File(fixture.baseCorpusPath)).not.toBe(
      fixture.config.baseCorpus.sha256,
    );
  });

  it("publishes no canonical partial directory and retries after staged residue", async () => {
    const fixture = await makeSyntheticFixture();
    const stagePrefix = `.historical-extension-${basename(fixture.config.outDir)}-`;
    const staleStage = join(
      dirname(fixture.config.outDir),
      `${stagePrefix}stale-residue`,
    );
    await mkdir(staleStage, { recursive: true });
    await writeFile(join(staleStage, "partial"), "incomplete\n", "utf8");

    await expect(
      generateHistoricalCorpusExtension(fixture.config, {
        beforeAtomicPromotion: async () => {
          const activeStages = (
            await readdir(dirname(fixture.config.outDir), {
              withFileTypes: true,
            })
          ).filter(
            (entry) =>
              entry.isDirectory() &&
              entry.name.startsWith(stagePrefix) &&
              entry.name !== basename(staleStage),
          );
          expect(activeStages).toHaveLength(1);
          const stagedFiles = await listRegularFiles(
            join(dirname(fixture.config.outDir), activeStages[0]!.name),
          );
          expect(stagedFiles.length).toBeGreaterThan(0);
          for (const stagedFile of stagedFiles) {
            const bytes = await readFile(stagedFile);
            for (const seedPhrase of SYNTHETIC_TEST_SEEDS) {
              expect(bytes.includes(Buffer.from(seedPhrase, "utf8"))).toBe(
                false,
              );
            }
          }
          throw new Error("injected pre-promotion crash");
        },
      }),
    ).rejects.toThrow("injected pre-promotion crash");
    await expect(stat(fixture.config.outDir)).rejects.toMatchObject({
      code: "ENOENT",
    });
    const stagedAfterFailure = (await readdir(dirname(fixture.config.outDir)))
      .filter((entry) => entry.startsWith(stagePrefix))
      .sort();
    expect(stagedAfterFailure).toEqual([basename(staleStage)]);

    const result = await generateHistoricalCorpusExtension(fixture.config);
    await expect(stat(result.generationResultPath)).resolves.toMatchObject({
      isFile: expect.any(Function),
    });
    expect(await readFile(join(staleStage, "partial"), "utf8")).toBe(
      "incomplete\n",
    );
  });

  it("fails closed on wallet/funding drift and missing fanout evidence", async () => {
    expect(() =>
      parseHistoricalExtensionConfig({
        baseCorpusPath: "/tmp/base.ndjson",
        baseCorpusSha256: "0".repeat(64),
        baseIndexPath: "/tmp/base.index.ndjson",
        baseIndexSha256: "1".repeat(64),
        baseManifestPath: "/tmp/base.manifest.json",
        baseManifestSha256: "2".repeat(64),
        baseVerificationPath: "/tmp/base.verify.json",
        baseVerificationSha256: "3".repeat(64),
        baseBindingPath: "/tmp/base.binding.json",
        baseBindingSha256: "4".repeat(64),
        walletsDir: "/tmp/wallets",
        outDir: "/tmp/out",
      }),
    ).toThrow("--fanoutReportPath");

    const fixture = await makeSyntheticFixture();
    const walletPath = join(fixture.walletsDir, "wallet-0001.json");
    const wallet = JSON.parse(await readFile(walletPath, "utf8")) as Record<
      string,
      unknown
    >;
    const latestFunding = wallet.latestFunding as Record<string, unknown>;
    const fundingUtxos = latestFunding.fundingUtxos as Array<
      Record<string, unknown>
    >;
    fundingUtxos[0] = {
      ...fundingUtxos[0],
      outref: `${"f".repeat(64)}#0`,
    };
    await writeFile(walletPath, `${formatJson(wallet)}\n`, "utf8");
    await expect(
      verifyHistoricalExtensionBaseEvidence(fixture.config),
    ).rejects.toThrow("wallet/funding identity");
  });

  it("derives funding lovelace from bound output CBOR and rejects metadata drift", async () => {
    const fixture = await makeSyntheticFixture();
    const walletPath = join(fixture.walletsDir, "wallet-0001.json");
    const wallet = JSON.parse(await readFile(walletPath, "utf8")) as Record<
      string,
      unknown
    >;
    const latestFunding = wallet.latestFunding as Record<string, unknown>;
    const fundingUtxos = latestFunding.fundingUtxos as Array<
      Record<string, unknown>
    >;
    fundingUtxos[0] = { ...fundingUtxos[0], lovelace: "4999999" };
    await writeFile(walletPath, `${formatJson(wallet)}\n`, "utf8");

    const fanout = JSON.parse(
      await readFile(fixture.config.fanoutReport.path, "utf8"),
    ) as {
      readonly wallets: Array<{
        readonly wallet: {
          readonly latestFunding: {
            fundingUtxos: Array<Record<string, unknown>>;
          };
        };
      }>;
    };
    fanout.wallets[0]!.wallet.latestFunding.fundingUtxos[0] = {
      ...fanout.wallets[0]!.wallet.latestFunding.fundingUtxos[0],
      lovelace: "4999999",
    };
    await writeFile(
      fixture.config.fanoutReport.path,
      `${formatJson(fanout)}\n`,
      "utf8",
    );
    const driftedConfig: HistoricalExtensionConfig = {
      ...fixture.config,
      fanoutReport: {
        ...fixture.config.fanoutReport,
        sha256: await sha256File(fixture.config.fanoutReport.path),
      },
    };
    await expect(
      verifyHistoricalExtensionBaseEvidence(driftedConfig),
    ).rejects.toThrow("does not match bound output CBOR");
  });

  it("keeps the uniform worker path byte-identical when no per-wallet override is supplied", async () => {
    const fixture = await makeSyntheticFixture();
    const wallet = JSON.parse(
      await readFile(join(fixture.walletsDir, "wallet-0001.json"), "utf8"),
    ) as {
      readonly walletId: string;
      readonly seedPhrase: string;
      readonly latestFunding: {
        readonly fundingUtxos: readonly {
          readonly outref: string;
          readonly outputCbor: string;
        }[];
      };
    };
    const funding = wallet.latestFunding.fundingUtxos[0]!;
    const [txHash, outputIndex] = funding.outref.split("#");
    const direct = await buildCorpusChain({
      seedPhrase: wallet.seedPhrase,
      walletId: wallet.walletId,
      fundingUtxo: {
        txHash: txHash!,
        outputIndex: Number(outputIndex),
        outputCborHex: funding.outputCbor,
      },
      depth: 2,
      amountLovelace: 1n,
      feeParams: { minFeeA: 10n, minFeeB: 10n },
      network: "Preprod",
      networkId: 0n,
      maxSubmitTxCborBytes: 32_768,
      corpusSliceId: "uniform-regression",
      planShape: "chain",
      terminalChangeFloorLovelace: 1n,
    });
    const shardPath = join(fixture.root, "uniform-worker.ndjson");
    await runCorpusChainWorker({
      shardPath,
      walletBatch: [
        {
          seedPhrase: wallet.seedPhrase,
          walletId: wallet.walletId,
          fundingUtxo: {
            txHash: txHash!,
            outputIndex: Number(outputIndex),
            outputCborHex: funding.outputCbor,
          },
          corpusSliceId: "uniform-regression",
        },
      ],
      depth: 2,
      amountLovelace: "1",
      feeParams: { minFeeA: "10", minFeeB: "10" },
      network: "Preprod",
      networkId: "0",
      maxSubmitTxCborBytes: 32_768,
      planShape: "chain",
      terminalChangeFloorLovelace: "1",
    });
    expect(await readFile(shardPath, "utf8")).toBe(
      `${direct.rows.map((row) => JSON.stringify(row)).join("\n")}\n`,
    );
  });

  it("terminates and awaits every worker after one fleet worker fails", async () => {
    const workers = [new FakeCorpusWorker(), new FakeCorpusWorker()];
    const fleet = runCorpusWorkerFleet(
      [emptyFleetInput(0), emptyFleetInput(1)],
      (_input, index) => workers[index]! as unknown as CorpusWorkerFleetHandle,
    );
    queueMicrotask(() => {
      workers[0]!.emit("message", {
        type: "failure",
        error: "injected worker failure",
      });
    });

    await expect(fleet).rejects.toThrow("injected worker failure");
    expect(workers.every((worker) => worker.terminated)).toBe(true);
    expect(workers.every((worker) => worker.joined)).toBe(true);
  });

  it("preserves worker input ordinal when terminal messages arrive out of order", async () => {
    const workers = [new FakeCorpusWorker(), new FakeCorpusWorker()];
    const inputs = [emptyFleetInput(999), emptyFleetInput(1000)];
    const fleet = runCorpusWorkerFleet(
      inputs,
      (_input, index) => workers[index]! as unknown as CorpusWorkerFleetHandle,
    );
    queueMicrotask(() => {
      workers[1]!.emit("message", {
        type: "done",
        shardPath: inputs[1]!.shardPath,
        rowCount: 1,
        sha256: "1".repeat(64),
        walletIds: ["second"],
      });
      workers[0]!.emit("message", {
        type: "done",
        shardPath: inputs[0]!.shardPath,
        rowCount: 1,
        sha256: "0".repeat(64),
        walletIds: ["first"],
      });
    });

    await expect(fleet).resolves.toMatchObject([
      { shardPath: inputs[0]!.shardPath, walletIds: ["first"] },
      { shardPath: inputs[1]!.shardPath, walletIds: ["second"] },
    ]);
  });

  it("rejects a zero-code worker exit before a terminal message", async () => {
    const worker = new FakeCorpusWorker();
    const fleet = runCorpusWorkerFleet(
      [emptyFleetInput(0)],
      () => worker as unknown as CorpusWorkerFleetHandle,
    );
    queueMicrotask(() => worker.emit("exit", 0));

    await expect(fleet).rejects.toThrow(
      "exited with 0 before a terminal message",
    );
    expect(worker.terminated).toBe(true);
    expect(worker.joined).toBe(true);
  });
});
