import { mkdtemp, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { formatJson } from "midgard-node/commands/command-utils";
import { describe, expect, it } from "vitest";

import {
  parseStressCorpusIndexLine,
  parseStressCorpusManifest,
  parseStressCorpusVerificationArtifact,
  verifyStressCorpus,
} from "../src/commands/stress-corpus/verify.js";
import { computeStressCorpusWalletSetIdentity } from "../src/commands/stress-corpus/wallet-set-identity.js";
import {
  generateStressCorpus,
  parseStressCorpusGenerateConfig,
  parseStressCorpusGenerationArtifact,
  parseStressCorpusVerifyConfig,
} from "../src/commands/stress-corpus-generate.js";
import { parseOpenLoopCorpusLine } from "../src/commands/stress-open-loop.js";
import {
  makeWalletDir,
  MAX_SUBMIT_TX_CBOR_BYTES,
  MAX_SUBMIT_TX_CBOR_BYTES_TEXT,
  TEST_SEEDS,
  walletLabel,
} from "./stress-corpus.stress-corpus-planner.js";

describe("stress corpus generation", () => {
  it("generates, assembles, manifests, and verifies a grouped chain corpus", async () => {
    const walletsDir = await makeWalletDir();
    const outDir = await mkdtemp(join(tmpdir(), "midgard-stress-corpus-out-"));
    const config = parseStressCorpusGenerateConfig(
      {
        targetRateTps: "1",
        durationMs: "4000",
        walletCount: "2",
        safetyFactor: "1",
        amountLovelace: "1000000",
        minFeeA: "0",
        minFeeB: "0",
        maxSubmitTxCborBytes: MAX_SUBMIT_TX_CBOR_BYTES_TEXT,
        walletsDir,
        outDir,
        workers: "1",
        sliceWalletCounts: "1,1",
        corpusSliceIdPrefix: "phase1",
        yes: true,
      },
      {},
    );

    const result = await generateStressCorpus(config);
    const generationArtifact = JSON.parse(formatJson(result)) as Record<
      string,
      unknown
    >;
    const parsedGeneration =
      parseStressCorpusGenerationArtifact(generationArtifact);

    expect(result.plan.chainDepth).toBe(2);
    expect(parsedGeneration.plan.amountLovelace).toBe("1000000");
    expect(() =>
      parseStressCorpusGenerationArtifact({
        ...generationArtifact,
        manifestPath: join(outDir, "other-manifest.json"),
      }),
    ).toThrow("result binding is inconsistent");
    expect(() =>
      parseStressCorpusGenerationArtifact({
        ...generationArtifact,
        verified: {
          ...(generationArtifact.verified as Record<string, unknown>),
          verificationArtifact: {
            ...((generationArtifact.verified as Record<string, unknown>)
              .verificationArtifact as Record<string, unknown>),
            path: join(outDir, "other-verification.json"),
          },
        },
      }),
    ).toThrow("result binding is inconsistent");
    expect(result.assembled.rowCount).toBe(4);
    expect(result.assembled.chainCount).toBe(2);
    expect(result.verified.rowCount).toBe(4);
    const manifest = JSON.parse(
      await readFile(result.manifestPath, "utf8"),
    ) as Record<string, unknown> & {
      readonly corpusSliceIds: readonly string[];
      readonly walletSetIdentity: typeof result.walletSetIdentity;
      readonly sliceSummary: readonly {
        readonly corpusSliceId: string;
        readonly walletCount: number;
        readonly rowCount: number;
      }[];
    };
    expect(manifest.walletSetIdentity).toEqual(result.walletSetIdentity);
    expect(manifest.corpusSliceIds).toEqual(["phase1-1", "phase1-2"]);
    expect(manifest.sliceSummary).toEqual([
      { corpusSliceId: "phase1-1", walletCount: 1, rowCount: 2 },
      { corpusSliceId: "phase1-2", walletCount: 1, rowCount: 2 },
    ]);
    expect(parseStressCorpusManifest(manifest).schemaVersion).toBe(
      "midgard-stress-corpus-manifest-v1",
    );
    expect(() =>
      parseStressCorpusManifest({ ...manifest, schemaVersion: "legacy-v2" }),
    ).toThrow("Unsupported stress corpus manifest schemaVersion");
    const { files: _files, ...manifestWithoutFiles } = manifest;
    expect(() => parseStressCorpusManifest(manifestWithoutFiles)).toThrow(
      "missing=[files]",
    );
    expect(() =>
      parseStressCorpusManifest({ ...manifest, extension: "historical" }),
    ).toThrow("extra=[extension]");
    expect(() =>
      parseStressCorpusManifest({
        ...manifest,
        generatedAtIso: "2026-07-27",
      }),
    ).toThrow("canonical ISO-8601");
    expect(() =>
      parseStressCorpusManifest({ ...manifest, networkId: "1" }),
    ).toThrow("cardinality binding");
    expect(() =>
      parseStressCorpusManifest({
        ...manifest,
        sliceSummary: [...manifest.sliceSummary].reverse(),
      }),
    ).toThrow("cardinality binding");
    expect(() =>
      parseStressCorpusManifest({
        ...manifest,
        fundingSummary: {
          ...(manifest.fundingSummary as Record<string, unknown>),
          totalFundingLovelace: "1",
        },
      }),
    ).toThrow("cardinality binding");
    expect(() =>
      parseStressCorpusManifest({
        ...manifest,
        files: {
          ...(manifest.files as Record<string, unknown>),
          shards: [
            ...(manifest.files as { readonly shards: readonly string[] })
              .shards,
            (manifest.files as { readonly shards: readonly string[] })
              .shards[0],
          ],
        },
      }),
    ).toThrow("cardinality binding");

    const corpusLine = (await readFile(result.corpusPath, "utf8"))
      .trim()
      .split("\n")[0]!;
    const corpusRow = JSON.parse(corpusLine) as Record<string, unknown>;
    const { parentTxHash: _parentTxHash, ...rowWithoutParent } = corpusRow;
    expect(() =>
      parseOpenLoopCorpusLine(JSON.stringify(rowWithoutParent), 1),
    ).toThrow("missing=[parentTxHash]");
    expect(() =>
      parseOpenLoopCorpusLine(
        JSON.stringify({ ...corpusRow, extension: true }),
        1,
      ),
    ).toThrow("extra=[extension]");
    expect(() =>
      parseOpenLoopCorpusLine(
        JSON.stringify({
          ...corpusRow,
          senderWalletId: ` ${String(corpusRow.senderWalletId)}`,
        }),
        1,
      ),
    ).toThrow("exact non-empty string");
    expect(() =>
      parseOpenLoopCorpusLine(
        JSON.stringify({ ...corpusRow, txHash: "00".repeat(32) }),
        1,
      ),
    ).toThrow("does not bind canonicalCborHex");
    expect(() =>
      parseOpenLoopCorpusLine(
        JSON.stringify({ ...corpusRow, selectedInputOutref: "not-an-outref" }),
        1,
      ),
    ).toThrow("must be canonical");
    expect(() =>
      parseOpenLoopCorpusLine(
        JSON.stringify({
          ...corpusRow,
          outputOutrefs: [`${String(corpusRow.txHash)}#9`],
        }),
        1,
      ),
    ).toThrow("must exactly enumerate");

    const indexLine = (await readFile(result.indexPath, "utf8"))
      .trim()
      .split("\n")[0]!;
    const indexRow = JSON.parse(indexLine) as Record<string, unknown>;
    expect(() =>
      parseStressCorpusIndexLine(
        JSON.stringify({ ...indexRow, extension: true }),
        1,
      ),
    ).toThrow("extra=[extension]");
    await expect(
      verifyStressCorpus({
        corpusPath: result.corpusPath,
        indexPath: result.indexPath,
        manifestPath: join(outDir, "absent-manifest.json"),
      }),
    ).rejects.toThrow();
    expect(result.verified.rebuildSample).toMatchObject({
      sampleRate: 0.001,
      checkedChainCount: 1,
      checkedRowCount: 2,
    });
    expect(result.walletSetIdentity).toEqual(result.verified.walletSetIdentity);
    expect(result.verified.verificationArtifact.sha256).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    const verificationArtifactText = await readFile(
      result.verified.verificationArtifact.path,
      "utf8",
    );
    const verificationArtifact = JSON.parse(verificationArtifactText) as Record<
      string,
      unknown
    >;
    expect(
      parseStressCorpusVerificationArtifact(verificationArtifact),
    ).toMatchObject({
      schemaVersion: "midgard-stress-corpus-verification-v1",
      rowCount: 4,
      chainCount: 2,
    });
    expect(verificationArtifactText).not.toContain(TEST_SEEDS[0]);
    expect(verificationArtifactText).not.toContain(TEST_SEEDS[1]);
    expect(result.verified.rebuildSample.livePreflightEntries).toHaveLength(1);
    expect(result.walletSetIdentity).toMatchObject({
      walletCount: 2,
      fundingRowCount: 2,
      uniqueFirstFundingOutrefCount: 2,
      walletSetSha256: expect.stringMatching(/^[0-9a-f]{64}$/u),
      fundingSetSha256: expect.stringMatching(/^[0-9a-f]{64}$/u),
    });
    expect(JSON.stringify(result.walletSetIdentity)).not.toContain(
      TEST_SEEDS[0],
    );

    expect(() =>
      parseStressCorpusGenerationArtifact({
        ...generationArtifact,
        schemaVersion: "midgard-stress-corpus-generation-v0",
      }),
    ).toThrow("Unsupported stress corpus generation artifact schemaVersion");
    const { verified: _generationVerified, ...generationWithoutVerified } =
      generationArtifact;
    expect(() =>
      parseStressCorpusGenerationArtifact(generationWithoutVerified),
    ).toThrow("missing=[verified]");
    const generationPlan = generationArtifact.plan as Record<string, unknown>;
    const {
      amountLovelace: _generationAmountLovelace,
      ...generationPlanWithoutAmount
    } = generationPlan;
    expect(() =>
      parseStressCorpusGenerationArtifact({
        ...generationArtifact,
        plan: generationPlanWithoutAmount,
      }),
    ).toThrow("missing=[amountLovelace]");
    expect(() =>
      parseStressCorpusGenerationArtifact({
        ...generationArtifact,
        plan: { ...generationPlan, extension: "historical" },
      }),
    ).toThrow("extra=[extension]");

    expect(() =>
      parseStressCorpusVerificationArtifact({
        ...verificationArtifact,
        schemaVersion: "midgard-stress-corpus-verification-v0",
      }),
    ).toThrow("Unsupported stress corpus verification artifact schemaVersion");
    const verificationCorpus = verificationArtifact.corpus as Record<
      string,
      unknown
    >;
    const {
      manifestSha256: _verificationManifestSha256,
      ...verificationCorpusWithoutManifestSha256
    } = verificationCorpus;
    expect(() =>
      parseStressCorpusVerificationArtifact({
        ...verificationArtifact,
        corpus: verificationCorpusWithoutManifestSha256,
      }),
    ).toThrow("missing=[manifestSha256]");
    expect(() =>
      parseStressCorpusVerificationArtifact({
        ...verificationArtifact,
        rebuildSample: {
          ...(verificationArtifact.rebuildSample as Record<string, unknown>),
          extension: "historical",
        },
      }),
    ).toThrow("extra=[extension]");

    const standaloneVerifyConfig = parseStressCorpusVerifyConfig(
      {
        corpusPath: result.corpusPath,
        rebuildWalletsDir: walletsDir,
        amountLovelace: "1000000",
        minFeeA: "0",
        minFeeB: "0",
        maxSubmitTxCborBytes: MAX_SUBMIT_TX_CBOR_BYTES_TEXT,
      },
      {},
    );
    expect(standaloneVerifyConfig.manifestPath).toBe(result.manifestPath);
    await expect(
      verifyStressCorpus({
        corpusPath: result.corpusPath,
        indexPath: result.indexPath,
        manifestPath: result.manifestPath,
        rebuildSample: standaloneVerifyConfig.rebuildSample!,
      }),
    ).resolves.toMatchObject({
      rowCount: 4,
      chainCount: 2,
      rebuildSample: {
        checkedChainCount: 1,
        checkedRowCount: 2,
      },
      walletSetIdentity: result.walletSetIdentity,
    });
  });

  it("rejects a wallet directory with records outside the exact current-run set", async () => {
    const walletsDir = await makeWalletDir();
    await writeFile(join(walletsDir, "wallet-0003.json"), "{}\n", "utf8");
    const outDir = await mkdtemp(
      join(tmpdir(), "midgard-corpus-extra-wallet-"),
    );
    const config = parseStressCorpusGenerateConfig(
      {
        targetRateTps: "1",
        durationMs: "4000",
        walletCount: "2",
        safetyFactor: "1",
        amountLovelace: "1000000",
        minFeeA: "0",
        minFeeB: "0",
        maxSubmitTxCborBytes: MAX_SUBMIT_TX_CBOR_BYTES_TEXT,
        walletsDir,
        outDir,
        workers: "1",
        yes: true,
      },
      {},
    );

    await expect(generateStressCorpus(config)).rejects.toThrow(
      "expected exactly 2 for the current run",
    );
  });

  it("standalone verification requires complete funding snapshots for every chain, not only the rebuild sample", async () => {
    const walletsDir = await makeWalletDir();
    const outDir = await mkdtemp(
      join(tmpdir(), "midgard-corpus-full-funding-"),
    );
    const config = parseStressCorpusGenerateConfig(
      {
        targetRateTps: "1",
        durationMs: "4000",
        walletCount: "2",
        safetyFactor: "1",
        amountLovelace: "1000000",
        minFeeA: "0",
        minFeeB: "0",
        maxSubmitTxCborBytes: MAX_SUBMIT_TX_CBOR_BYTES_TEXT,
        walletsDir,
        outDir,
        workers: "1",
        rebuildSampleRate: "0.5",
        yes: true,
      },
      {},
    );
    const result = await generateStressCorpus(config);
    const sampled = new Set(result.verified.rebuildSample.sampledChainIds);
    const unsampledIndex = sampled.has("stress-wallet-0001") ? 2 : 1;
    const unsampledPath = join(
      walletsDir,
      `wallet-${walletLabel(unsampledIndex)}.json`,
    );
    const unsampledRecord = JSON.parse(
      await readFile(unsampledPath, "utf8"),
    ) as Record<string, unknown>;
    delete unsampledRecord.latestFunding;
    await writeFile(unsampledPath, `${formatJson(unsampledRecord)}\n`, "utf8");

    await expect(
      verifyStressCorpus({
        corpusPath: result.corpusPath,
        indexPath: result.indexPath,
        manifestPath: result.manifestPath,
        rebuildSample: {
          walletsDir,
          amountLovelace: 1_000_000n,
          feeParams: { minFeeA: 0n, minFeeB: 0n },
          network: "Preprod",
          networkId: 0n,
          maxSubmitTxCborBytes: MAX_SUBMIT_TX_CBOR_BYTES,
          sampleRate: 0.5,
        },
      }),
    ).rejects.toThrow("must contain at least one latestFunding");
  });

  it("standalone verification rejects wallet records outside the exact indexed chain set", async () => {
    const walletsDir = await makeWalletDir();
    const outDir = await mkdtemp(join(tmpdir(), "midgard-corpus-exact-set-"));
    const config = parseStressCorpusGenerateConfig(
      {
        targetRateTps: "1",
        durationMs: "4000",
        walletCount: "2",
        safetyFactor: "1",
        amountLovelace: "1000000",
        minFeeA: "0",
        minFeeB: "0",
        maxSubmitTxCborBytes: MAX_SUBMIT_TX_CBOR_BYTES_TEXT,
        walletsDir,
        outDir,
        workers: "1",
        yes: true,
      },
      {},
    );
    const result = await generateStressCorpus(config);
    await writeFile(
      join(walletsDir, "wallet-0003.json"),
      await readFile(join(walletsDir, "wallet-0001.json"), "utf8"),
      "utf8",
    );

    await expect(
      verifyStressCorpus({
        corpusPath: result.corpusPath,
        indexPath: result.indexPath,
        manifestPath: result.manifestPath,
        rebuildSample: {
          walletsDir,
          amountLovelace: 1_000_000n,
          feeParams: { minFeeA: 0n, minFeeB: 0n },
          network: "Preprod",
          networkId: 0n,
          maxSubmitTxCborBytes: MAX_SUBMIT_TX_CBOR_BYTES,
        },
      }),
    ).rejects.toThrow(
      "wallet record count 3 must equal expected current-run count 2",
    );
  });

  it("computes stable full-set hashes without seed phrase material", async () => {
    const walletsDir = await makeWalletDir();
    const records = await Promise.all(
      [1, 2].map(async (index) =>
        JSON.parse(
          await readFile(
            join(walletsDir, `wallet-${walletLabel(index)}.json`),
            "utf8",
          ),
        ),
      ),
    );
    const forward = computeStressCorpusWalletSetIdentity({
      records,
      expectedWalletCount: 2,
    });
    const reversed = computeStressCorpusWalletSetIdentity({
      records: [...records].reverse(),
      expectedWalletCount: 2,
    });

    expect(reversed).toEqual(forward);
    expect(forward.walletSetSha256).toMatch(/^[0-9a-f]{64}$/u);
    expect(forward.fundingSetSha256).toMatch(/^[0-9a-f]{64}$/u);
    expect(JSON.stringify(forward)).not.toContain(TEST_SEEDS[0]);
    expect(JSON.stringify(forward)).not.toContain(TEST_SEEDS[1]);
  });

  it("rejects duplicate first funding outrefs across the full wallet set", async () => {
    const walletsDir = await makeWalletDir();
    const records = await Promise.all(
      [1, 2].map(async (index) =>
        JSON.parse(
          await readFile(
            join(walletsDir, `wallet-${walletLabel(index)}.json`),
            "utf8",
          ),
        ),
      ),
    );
    records[1].latestFunding.fundingUtxos[0] =
      records[0].latestFunding.fundingUtxos[0];

    expect(() =>
      computeStressCorpusWalletSetIdentity({
        records,
        expectedWalletCount: 2,
      }),
    ).toThrow("duplicate first funding outref");
  });

  it("rejects duplicate selected inputs during verification", async () => {
    const walletsDir = await makeWalletDir();
    const outDir = await mkdtemp(join(tmpdir(), "midgard-stress-corpus-bad-"));
    const config = parseStressCorpusGenerateConfig(
      {
        targetRateTps: "1",
        durationMs: "4000",
        walletCount: "2",
        safetyFactor: "1",
        amountLovelace: "1000000",
        minFeeA: "0",
        minFeeB: "0",
        maxSubmitTxCborBytes: MAX_SUBMIT_TX_CBOR_BYTES_TEXT,
        walletsDir,
        outDir,
        workers: "1",
        yes: true,
      },
      {},
    );
    const result = await generateStressCorpus(config);
    const lines = (await readFile(result.corpusPath, "utf8"))
      .trim()
      .split("\n");
    const first = JSON.parse(lines[0]!) as {
      readonly selectedInputOutref: string;
    };
    const second = JSON.parse(lines[1]!) as Record<string, unknown>;
    second.selectedInputOutref = first.selectedInputOutref;
    lines[1] = JSON.stringify(second);
    await writeFile(result.corpusPath, `${lines.join("\n")}\n`, "utf8");

    await expect(
      verifyStressCorpus({
        corpusPath: result.corpusPath,
        indexPath: result.indexPath,
        manifestPath: result.manifestPath,
      }),
    ).rejects.toThrow("duplicate selected input");
  });
});
