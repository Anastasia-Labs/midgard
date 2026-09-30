import { createHash } from "node:crypto";

import { outRefToCbor } from "@al-ft/lucid-midgard";

import {
  loadAndValidateGenerationResult,
  requireExact,
} from "./phase1-formal-identity.load-and-validate-generation-result.mjs";
import {
  PHASE1_FORMAL_BINDING_SCHEMA,
  PHASE1_FORMAL_CHAIN_COUNT,
  PHASE1_FORMAL_CHAIN_DEPTH,
  PHASE1_FORMAL_LIVE_SAMPLE_SIZE,
  PHASE1_FORMAL_ROW_COUNT,
  PHASE1_FORMAL_SAMPLE_ALGORITHM,
} from "./phase1-formal-identity.parse-phase1-formal-binding-document.mjs";

export const validatePhase1FormalCorpus = ({
  binding,
  corpusManifest,
  corpusArtifactIdentity,
  selectedIndexEntries,
}) => {
  const document = binding.document;
  requireExact(
    corpusArtifactIdentity.corpusSha256,
    document.corpus.corpusSha256,
    "corpus SHA-256",
  );
  requireExact(
    corpusArtifactIdentity.indexSha256,
    document.corpus.indexSha256,
    "corpus index SHA-256",
  );
  requireExact(
    corpusArtifactIdentity.manifestSha256,
    document.corpus.manifestSha256,
    "corpus manifest SHA-256",
  );
  requireExact(
    corpusManifest.chainCount,
    PHASE1_FORMAL_CHAIN_COUNT,
    "manifest chain count",
  );
  requireExact(
    corpusManifest.chainDepth,
    PHASE1_FORMAL_CHAIN_DEPTH,
    "manifest chain depth",
  );
  requireExact(
    corpusManifest.files?.corpus?.rowCount,
    PHASE1_FORMAL_ROW_COUNT,
    "manifest corpus row count",
  );
  for (const [actual, expected, label] of [
    [corpusManifest.targetRateTps, 5_000, "manifest target rate"],
    [corpusManifest.durationMs, 600_000, "manifest duration"],
    [corpusManifest.warmupCount, 0, "manifest warmup count"],
    [corpusManifest.cooldownCount, 0, "manifest cooldown count"],
    [corpusManifest.safetyFactor, 1.02, "manifest safety factor"],
    [
      corpusManifest.assumedAcceptanceLatencyMs,
      819,
      "manifest assumed acceptance latency",
    ],
    [corpusManifest.corpusShape, "chain", "manifest corpus shape"],
    [corpusManifest.network, "Preprod", "manifest network"],
    [corpusManifest.networkId, "0", "manifest network ID"],
    [
      corpusManifest.maxSubmitTxCborBytes,
      32_768,
      "manifest max submit CBOR bytes",
    ],
    [corpusManifest.feeParams?.minFeeA, "10", "manifest MIN_FEE_A"],
    [corpusManifest.feeParams?.minFeeB, "10", "manifest MIN_FEE_B"],
    [corpusManifest.amountTemplate?.lovelace, "1", "manifest transfer amount"],
    [
      corpusManifest.amountTemplate?.shape,
      "self-transfer-change-chain",
      "manifest amount shape",
    ],
    [
      corpusManifest.fundingSummary?.walletCount,
      PHASE1_FORMAL_CHAIN_COUNT,
      "manifest funding wallet count",
    ],
    [
      corpusManifest.fundingSummary?.perWalletFundingLovelace,
      "11228229",
      "manifest per-wallet funding",
    ],
    [
      corpusManifest.fundingSummary?.totalFundingLovelace,
      "45990825984",
      "manifest total funding",
    ],
    [
      corpusManifest.verification?.rebuildSampleRate,
      0.001,
      "manifest rebuild sample rate",
    ],
    [
      corpusManifest.verification?.rebuildSampleAlgorithm,
      PHASE1_FORMAL_SAMPLE_ALGORITHM,
      "manifest rebuild sample algorithm",
    ],
  ]) {
    requireExact(actual, expected, label);
  }
  requireExact(
    JSON.stringify(corpusManifest.corpusSliceIds),
    JSON.stringify([document.corpus.sliceId]),
    "manifest corpus slice IDs",
  );
  requireExact(
    JSON.stringify(corpusManifest.sliceSummary),
    JSON.stringify([
      {
        corpusSliceId: document.corpus.sliceId,
        walletCount: PHASE1_FORMAL_CHAIN_COUNT,
        rowCount: PHASE1_FORMAL_ROW_COUNT,
      },
    ]),
    "manifest slice summary",
  );
  requireExact(
    corpusManifest.walletSetIdentity?.walletCount,
    PHASE1_FORMAL_CHAIN_COUNT,
    "wallet-set wallet count",
  );
  requireExact(
    corpusManifest.walletSetIdentity?.uniqueFirstFundingOutrefCount,
    PHASE1_FORMAL_CHAIN_COUNT,
    "unique first funding outref count",
  );
  requireExact(
    corpusManifest.walletSetIdentity?.walletSetHashAlgorithm,
    "sha256-wallet-id-l2-address-lines-v1",
    "wallet-set hash algorithm",
  );
  requireExact(
    corpusManifest.walletSetIdentity?.fundingSetHashAlgorithm,
    "sha256-wallet-id-outref-output-cbor-sha256-lines-v1",
    "funding-set hash algorithm",
  );
  requireExact(
    corpusManifest.walletSetIdentity?.walletSetSha256,
    document.walletSetSha256,
    "wallet-set SHA-256",
  );
  requireExact(
    corpusManifest.walletSetIdentity?.fundingSetSha256,
    document.fundingSetSha256,
    "funding-set SHA-256",
  );
  requireExact(
    selectedIndexEntries.length,
    PHASE1_FORMAL_CHAIN_COUNT,
    "selected chain count",
  );
  const uniqueChainIds = new Set(
    selectedIndexEntries.map((entry) => entry.chainId),
  );
  requireExact(
    uniqueChainIds.size,
    PHASE1_FORMAL_CHAIN_COUNT,
    "unique selected chain count",
  );
  const selectedRows = selectedIndexEntries.reduce(
    (total, entry) => total + entry.rowCount,
    0,
  );
  requireExact(selectedRows, PHASE1_FORMAL_ROW_COUNT, "selected corpus rows");
  const invalidDepth = selectedIndexEntries.find(
    (entry) => entry.rowCount !== PHASE1_FORMAL_CHAIN_DEPTH,
  );
  if (invalidDepth !== undefined) {
    throw new Error(
      `Phase 1 formal identity mismatch for chain ${invalidDepth.chainId} depth: expected ${PHASE1_FORMAL_CHAIN_DEPTH.toString()}, received ${invalidDepth.rowCount.toString()}`,
    );
  }
  const deterministicSampleIds = [...selectedIndexEntries]
    .sort((left, right) => {
      const key = (entry) =>
        createHash("sha256")
          .update(document.corpus.corpusSha256)
          .update("\0")
          .update(entry.chainId)
          .update("\0")
          .update(String(entry.startByteOffset))
          .digest("hex");
      return key(left).localeCompare(key(right));
    })
    .slice(0, PHASE1_FORMAL_LIVE_SAMPLE_SIZE)
    .map((entry) => entry.chainId);
  requireExact(
    JSON.stringify(
      document.livePreflight.entries.map((entry) => entry.walletId),
    ),
    JSON.stringify(deterministicSampleIds),
    "deterministic live preflight chain selection",
  );
  const generationResult = loadAndValidateGenerationResult(
    binding,
    corpusManifest,
  );

  return {
    schemaVersion: PHASE1_FORMAL_BINDING_SCHEMA,
    bindingArtifact: {
      path: binding.path,
      sha256: binding.sha256,
    },
    deploymentManifestId: document.deploymentManifestId,
    nodeImageId: document.nodeImageId,
    nodeContainerId: document.nodeContainerId,
    walletSetSha256: document.walletSetSha256,
    fundingSetSha256: document.fundingSetSha256,
    corpus: document.corpus,
    generationResult,
    livePreflight: document.livePreflight,
    harness: document.harness,
    stressCorpusEnv: document.stressCorpusEnv,
    selectedChainCount: selectedIndexEntries.length,
    selectedRowCount: selectedRows,
  };
};

export const verifyPhase1LivePreflight = async ({ expected, fetchUtxos }) => {
  const entries = [];
  for (const entry of expected.entries) {
    const utxos = await fetchUtxos(entry.l2Address);
    const [transactionId, outputIndex] = entry.firstInputOutref.split("#");
    // The ledger `outref` column carries the §5.3 field-0/1 item bytes
    // (`82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes), not CML's
    // minimal-index `TransactionInput` CBOR — so the expected bytes must come
    // from the shared encoder.
    const expectedOutrefCbor = outRefToCbor({
      txHash: transactionId,
      outputIndex: Number(outputIndex),
    }).toString("hex");
    const live = utxos.find(
      (utxo) => String(utxo.outref).toLowerCase() === expectedOutrefCbor,
    );
    if (live === undefined) {
      throw new Error(
        `phase1_live_preflight_missing_first_input: wallet=${entry.walletId},outref=${entry.firstInputOutref}`,
      );
    }
    const outputCbor = String(live.outputCbor).trim().toLowerCase();
    const outputBytes = Buffer.from(outputCbor, "hex");
    if (
      outputCbor.length === 0 ||
      outputCbor.length % 2 !== 0 ||
      outputBytes.toString("hex") !== outputCbor
    ) {
      throw new Error(
        `phase1_live_preflight_invalid_output_cbor: wallet=${entry.walletId},outref=${entry.firstInputOutref}`,
      );
    }
    const outputCborSha256 = createHash("sha256")
      .update(outputBytes)
      .digest("hex");
    if (outputCborSha256 !== entry.outputCborSha256) {
      throw new Error(
        `phase1_live_preflight_output_mismatch: wallet=${entry.walletId},outref=${entry.firstInputOutref},expected=${entry.outputCborSha256},actual=${outputCborSha256}`,
      );
    }
    entries.push({ ...entry, observedOutputCborSha256: outputCborSha256 });
  }
  return {
    algorithm: expected.algorithm,
    sampleSize: expected.sampleSize,
    checkedAtIso: new Date().toISOString(),
    passed: true,
    entries,
  };
};
