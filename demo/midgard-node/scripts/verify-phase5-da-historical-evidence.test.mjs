import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardNativeTxCanonical,
  toMidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec/native";
import { decodeMidgardAddressText } from "@al-ft/midgard-core/codec/address";
import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import {
  PHASE5_HISTORICAL_BINDING_SCHEMA,
  PHASE5_HISTORICAL_CLAIM_SCOPE,
  PHASE5_HISTORICAL_COMPATIBILITY,
  PHASE5_HISTORICAL_GENERATION_SCHEMA,
  PHASE5_HISTORICAL_MANIFEST_SCHEMA,
  PHASE5_HISTORICAL_RETAINED_TERMINAL_SET_HASH_ALGORITHM,
  PHASE5_HISTORICAL_VERIFICATION_SCHEMA,
  verifyPhase5DaCorpusEvidence,
  verifyPhase5DaSourceCorpusEvidence,
} from "./verify-phase5-da-50k-distribution-report.mjs";

const checkedEnvelope = await readFile(
  new URL(
    "../tests/fixtures/da-operational-50k/envelope-50000.cbor",
    import.meta.url,
  ),
);
const checkedInner = await unwrapDaPayload(checkedEnvelope, {
  maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
  schemaVersion: 3,
});
const checkedTransactionEntries = SDK.decodeDaPayloadV2Canonical(
  checkedInner.innerBytes,
).block_body.transactions;
const checkedTransactions = checkedTransactionEntries.slice(0, 3);

const strictChainFixture = JSON.parse(
  await readFile(
    new URL(
      "../tests/fixtures/phase5-historical-strict-chains.json",
      import.meta.url,
    ),
    "utf8",
  ),
);
assert.equal(
  strictChainFixture.schemaVersion,
  "midgard-phase5-strict-chain-fixture-v1",
);
const historicalChains = strictChainFixture.chains.map((chain) =>
  chain.rows.map((row) => [row.txHash, row.canonicalCborHex]),
);
const historicalWallets = strictChainFixture.chains.map((chain) =>
  structuredClone(chain.wallet),
);
const historicalChainTransactions = historicalChains[0];

const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");

const jsonBytes = (value) => Buffer.from(`${JSON.stringify(value)}\n`);

const mutateHistoricalTransaction = ([, canonicalCborHex], mutateBody) => {
  const transaction = decodeMidgardNativeTxFullFromCanonicalCbor(
    Buffer.from(canonicalCborHex, "hex"),
  );
  const canonicalCbor = encodeMidgardNativeTxCanonical({
    ...toMidgardNativeTxCanonical(transaction),
    body: mutateBody(transaction.body),
  });
  const updated = decodeMidgardNativeTxFullFromCanonicalCbor(canonicalCbor);
  const txHash = computeMidgardNativeTxId(updated).toString("hex");
  const outputs = decodeMidgardNativeByteListPreimage(
    updated.body.outputsPreimageCbor,
    "native.outputs",
  );
  return {
    txHash,
    canonicalCborHex: canonicalCbor.toString("hex"),
    canonicalCborSha256: sha256(canonicalCbor),
    canonicalCborByteLength: canonicalCbor.length,
    outputOutrefs: outputs.map(
      (_output, outputIndex) => `${txHash}#${outputIndex.toString()}`,
    ),
  };
};

const marker = (schemaVersion) => ({
  schemaVersion,
  claimScope: PHASE5_HISTORICAL_CLAIM_SCOPE,
  freshLiveClaim: false,
  compatibility: PHASE5_HISTORICAL_COMPATIBILITY,
});

const transactionSetSha256 = (entries) => {
  const digest = createHash("sha256");
  for (const [key, value] of [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  )) {
    const keyBytes = Buffer.from(key, "hex");
    const valueBytes = Buffer.from(value, "hex");
    const lengths = Buffer.allocUnsafe(8);
    lengths.writeUInt32BE(keyBytes.length, 0);
    lengths.writeUInt32BE(valueBytes.length, 4);
    digest.update(lengths).update(keyBytes).update(valueBytes);
  }
  return digest.digest("hex");
};

const transactionContentSha256 = (entries) => {
  const digest = createHash("sha256");
  for (const value of entries.map(([, cbor]) => cbor).sort()) {
    const bytes = Buffer.from(value, "hex");
    const length = Buffer.allocUnsafe(4);
    length.writeUInt32BE(bytes.length);
    digest.update(length).update(bytes);
  }
  return digest.digest("hex");
};

const makeFixture = async ({
  retainedTerminalOverride,
  continuationOverride,
  fanoutWalletOverride,
  fundingModelOverride,
  baseBindingOverride,
  maxSubmitTxCborBytesOverride,
  chainCount = 1,
  duplicateFirstFundingOutref = false,
  continuationBoundaryShift = 0,
  reverseLivePreflightEntries = false,
} = {}) => {
  const root = await mkdtemp(join(tmpdir(), "phase5-historical-evidence-"));
  assert.ok(chainCount === 1 || chainCount === 2);
  assert.ok(Number.isSafeInteger(continuationBoundaryShift));
  assert.ok(
    continuationBoundaryShift === 0 ||
      (chainCount === 2 && continuationBoundaryShift > 0),
  );
  const chains = historicalChains.slice(0, chainCount);
  const chainIds = chains.map(
    (_chain, chainIndex) =>
      `stress-wallet-${(chainIndex + 1).toString().padStart(4, "0")}`,
  );
  const makeRow = (
    [txHash, canonicalCborHex],
    chainIndex,
    transactionIndex,
  ) => {
    const canonicalCbor = Buffer.from(canonicalCborHex, "hex");
    const native = decodeMidgardNativeTxFullFromCanonicalCbor(canonicalCbor);
    const spendInputs = decodeMidgardNativeByteListPreimage(
      native.body.spendInputsPreimageCbor,
      "native.spend_inputs",
    );
    const outputs = decodeMidgardNativeByteListPreimage(
      native.body.outputsPreimageCbor,
      "native.outputs",
    );
    assert.equal(spendInputs.length, 1);
    assert.ok(outputs[1] !== undefined);
    const selectedInput = CML.TransactionInput.from_cbor_bytes(spendInputs[0]);
    return {
      txHash,
      canonicalCborHex,
      canonicalCborSha256: sha256(canonicalCbor),
      canonicalCborByteLength: canonicalCbor.length,
      senderWalletId: chainIds[chainIndex],
      selectedInputOutref: `${selectedInput.transaction_id().to_hex()}#${selectedInput.index().toString()}`,
      outputOutrefs: outputs.map(
        (_output, outputIndex) => `${txHash}#${outputIndex}`,
      ),
      planShape: "chain",
      parentTxHash: transactionIndex === 0 ? null : chains[chainIndex][0][0],
      corpusSliceId: "retained",
    };
  };
  const rows = [
    ...chains.map((chain, chainIndex) => makeRow(chain[0], chainIndex, 0)),
    ...chains.map((chain, chainIndex) => makeRow(chain[1], chainIndex, 1)),
  ];
  for (let chainIndex = 0; chainIndex < chainCount; chainIndex += 1) {
    assert.equal(
      rows[chainCount + chainIndex].selectedInputOutref,
      `${rows[chainIndex].txHash}#1`,
    );
  }
  if (retainedTerminalOverride !== undefined) {
    rows[0] = { ...rows[0], ...retainedTerminalOverride };
  }
  if (continuationOverride !== undefined) {
    rows[chainCount] = { ...rows[chainCount], ...continuationOverride };
  }
  const lines = rows.map((row) => JSON.stringify(row));
  const baseCorpusBytes = Buffer.from(
    `${lines.slice(0, chainCount).join("\n")}\n`,
  );
  const corpusBytes = Buffer.from(`${lines.join("\n")}\n`);
  const baseCorpusPath = join(root, "base.ndjson");
  const corpusPath = join(root, "historical.ndjson");
  const baseIndexPath = join(root, "base.index.ndjson");
  const indexPath = join(root, "historical.index.ndjson");
  const lineEndByteOffsets = [];
  let byteOffset = 0;
  for (const line of lines) {
    byteOffset += Buffer.byteLength(line, "utf8") + 1;
    lineEndByteOffsets.push(byteOffset);
  }
  const allEntries = rows.map((row, rowIndex) => ({
    corpusSliceId: "retained",
    planShape: "chain",
    chainId: chainIds[rowIndex % chainCount],
    startByteOffset: rowIndex === 0 ? 0 : lineEndByteOffsets[rowIndex - 1],
    endByteOffset: lineEndByteOffsets[rowIndex],
    rowCount: 1,
  }));
  const baseEntries = allEntries.slice(0, chainCount);
  const continuationEntries = allEntries.slice(chainCount);
  if (continuationBoundaryShift > 0) {
    continuationEntries[0].endByteOffset += continuationBoundaryShift;
    continuationEntries[1].startByteOffset += continuationBoundaryShift;
  }
  const extendedEntries = [...baseEntries, ...continuationEntries];
  const baseIndexBytes = Buffer.from(
    `${baseEntries.map((entry) => JSON.stringify(entry)).join("\n")}\n`,
  );
  const indexBytes = Buffer.from(
    `${extendedEntries.map((entry) => JSON.stringify(entry)).join("\n")}\n`,
  );
  await Promise.all([
    writeFile(baseCorpusPath, baseCorpusBytes),
    writeFile(corpusPath, corpusBytes),
    writeFile(baseIndexPath, baseIndexBytes),
    writeFile(indexPath, indexBytes),
  ]);

  const defaultWallets = historicalWallets
    .slice(0, chainCount)
    .map((wallet, chainIndex) => {
      assert.equal(wallet.walletId, chainIds[chainIndex]);
      assert.equal(
        wallet.latestFunding.fundingUtxos[0].outref,
        rows[chainIndex].selectedInputOutref,
      );
      return structuredClone(wallet);
    });
  if (fanoutWalletOverride !== undefined) {
    defaultWallets[0] = fanoutWalletOverride(defaultWallets[0]);
  }
  if (duplicateFirstFundingOutref) {
    defaultWallets[1] = {
      ...defaultWallets[1],
      latestFunding: {
        ...defaultWallets[1].latestFunding,
        fundingUtxos: [
          {
            ...defaultWallets[1].latestFunding.fundingUtxos[0],
            outref: defaultWallets[0].latestFunding.fundingUtxos[0].outref,
          },
        ],
      },
    };
  }
  const fanoutWallets = [...defaultWallets].sort((left, right) =>
    left.walletId.localeCompare(right.walletId),
  );
  const fundingRows = fanoutWallets.flatMap((wallet) =>
    wallet.latestFunding.fundingUtxos.map(
      (funding) =>
        `${wallet.walletId}|${funding.outref.toLowerCase()}|${sha256(Buffer.from(funding.outputCbor, "hex"))}`,
    ),
  );
  const firstFundingOutrefs = new Set(
    fanoutWallets.map((wallet) => wallet.latestFunding.fundingUtxos[0].outref),
  );
  const walletSetIdentity = {
    walletCount: chainCount,
    fundingRowCount: fundingRows.length,
    uniqueFirstFundingOutrefCount: firstFundingOutrefs.size,
    walletSetHashAlgorithm: "sha256-wallet-id-l2-address-lines-v1",
    walletSetSha256: sha256(
      Buffer.from(
        fanoutWallets
          .map((wallet) => `${wallet.walletId}|${wallet.l2Address}`)
          .join("\n"),
      ),
    ),
    fundingSetHashAlgorithm:
      "sha256-wallet-id-outref-output-cbor-sha256-lines-v1",
    fundingSetSha256: sha256(Buffer.from(fundingRows.join("\n"))),
  };
  const fanout = {
    schemaVersion: "midgard-stress-wallet-fanout-v1",
    requestedCount: chainCount,
    verifiedWalletCount: chainCount,
    wallets: fanoutWallets.map((wallet) => ({
      wallet,
      verifiedFundingUtxoCount: wallet.latestFunding.fundingUtxos.length,
    })),
  };
  const fanoutPath = join(root, "fanout.json");
  const fanoutBytes = jsonBytes(fanout);
  await writeFile(fanoutPath, fanoutBytes);
  const transferAmounts = rows.map((row) => {
    const native = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(row.canonicalCborHex, "hex"),
    );
    const outputs = decodeMidgardNativeByteListPreimage(
      native.body.outputsPreimageCbor,
      "native.outputs",
    );
    return CML.TransactionOutput.from_cbor_bytes(outputs[0])
      .amount()
      .coin()
      .toString(10);
  });
  assert.equal(new Set(transferAmounts).size, 1);
  for (const row of rows) {
    const native = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(row.canonicalCborHex, "hex"),
    );
    assert.equal(
      native.body.fee,
      BigInt(10 * row.canonicalCborByteLength + 10),
    );
  }
  const amountLovelacePerRow = transferAmounts[0];
  const fanoutByWalletId = new Map(
    fanoutWallets.map((wallet) => [wallet.walletId, wallet]),
  );
  const livePreflightEntries = [...baseEntries]
    .sort((left, right) => {
      const key = (entry) =>
        createHash("sha256")
          .update(sha256(baseCorpusBytes))
          .update("\0")
          .update(entry.chainId)
          .update("\0")
          .update(String(entry.startByteOffset))
          .digest("hex");
      return key(left).localeCompare(key(right));
    })
    .map((entry) => {
      const wallet = fanoutByWalletId.get(entry.chainId);
      assert.ok(wallet !== undefined);
      const funding = wallet.latestFunding.fundingUtxos[0];
      return {
        walletId: wallet.walletId,
        l2Address: wallet.l2Address,
        firstInputOutref: funding.outref.toLowerCase(),
        outputCborSha256: sha256(Buffer.from(funding.outputCbor, "hex")),
      };
    });
  if (reverseLivePreflightEntries) livePreflightEntries.reverse();
  const baseManifest = {
    schemaVersion: "midgard-stress-corpus-manifest-v1",
    chainCount,
    chainDepth: 1,
    corpusShape: "chain",
    network: "Preprod",
    networkId: "0",
    maxSubmitTxCborBytes:
      maxSubmitTxCborBytesOverride ??
      Math.max(...rows.map((row) => row.canonicalCborByteLength)),
    amountTemplate: {
      lovelace: amountLovelacePerRow,
      shape: "self-transfer-change-chain",
    },
    feeParams: { minFeeA: "10", minFeeB: "10" },
    verification: {
      rebuildSampleRate: 1,
      rebuildSampleAlgorithm: "sha256-corpus-chain-id-order-v1",
    },
    walletSetIdentity,
    files: {
      corpus: { sha256: sha256(baseCorpusBytes), rowCount: chainCount },
      index: { sha256: sha256(baseIndexBytes), rowCount: chainCount },
    },
  };
  const baseManifestPath = join(root, "base.manifest.json");
  const baseManifestBytes = jsonBytes(baseManifest);
  await writeFile(baseManifestPath, baseManifestBytes);
  const baseVerification = {
    schemaVersion: "midgard-stress-corpus-verification-v1",
    rowCount: chainCount,
    chainCount,
    walletSetIdentity,
    corpus: {
      corpusSha256: sha256(baseCorpusBytes),
      indexSha256: sha256(baseIndexBytes),
      manifestSha256: sha256(baseManifestBytes),
    },
    rebuildSample: {
      algorithm: "sha256-corpus-chain-id-order-v1",
      sampleRate: 1,
      checkedChainCount: chainCount,
      checkedRowCount: chainCount,
      sampledChainIds: livePreflightEntries.map((entry) => entry.walletId),
      livePreflightEntries,
    },
  };
  const baseVerificationPath = join(root, "base.verify.json");
  const baseVerificationBytes = jsonBytes(baseVerification);
  await writeFile(baseVerificationPath, baseVerificationBytes);
  const standaloneVerificationPath = join(
    root,
    "standalone-verify-result.json",
  );
  await writeFile(standaloneVerificationPath, baseVerificationBytes);
  const baseBindingBase = {
    schemaVersion: "midgard-phase1-live-corpus-binding-v2",
    deploymentManifestId: "1".repeat(64),
    nodeImageId: `sha256:${"2".repeat(64)}`,
    nodeContainerId: "3".repeat(64),
    walletSetSha256: walletSetIdentity.walletSetSha256,
    fundingSetSha256: walletSetIdentity.fundingSetSha256,
    corpus: {
      path: baseCorpusPath,
      indexPath: baseIndexPath,
      manifestPath: baseManifestPath,
      sliceId: "strict",
      corpusSha256: sha256(baseCorpusBytes),
      indexSha256: sha256(baseIndexBytes),
      manifestSha256: sha256(baseManifestBytes),
    },
    verifier: {
      path: standaloneVerificationPath,
      sha256: sha256(baseVerificationBytes),
    },
    livePreflight: {
      algorithm: "sha256-corpus-chain-id-order-v1",
      sampleSize: chainCount,
      entries: livePreflightEntries,
    },
    harness: {
      scenarioId: "4".repeat(64),
      engineId: "5".repeat(64),
    },
    stressCorpusEnv: {
      STRESS_CORPUS_INDEX_PATH: baseIndexPath,
      STRESS_CORPUS_MANIFEST_PATH: baseManifestPath,
      STRESS_CORPUS_PATH: baseCorpusPath,
      STRESS_CORPUS_READAHEAD_ROWS: "50",
      STRESS_CORPUS_SHAPE: "chain",
      STRESS_CORPUS_SLICE_ID: "strict",
    },
  };
  const baseBinding =
    baseBindingOverride === undefined
      ? baseBindingBase
      : baseBindingOverride(baseBindingBase);
  const baseBindingPath = join(root, "base.binding.json");
  const baseBindingBytes = jsonBytes(baseBinding);
  await writeFile(baseBindingPath, baseBindingBytes);
  const baseEvidence = {
    corpus: { path: "base.ndjson", sha256: sha256(baseCorpusBytes) },
    index: { path: "base.index.ndjson", sha256: sha256(baseIndexBytes) },
    manifest: {
      path: "base.manifest.json",
      sha256: sha256(baseManifestBytes),
    },
    verification: {
      path: "base.verify.json",
      sha256: sha256(baseVerificationBytes),
    },
    phase1Binding: {
      path: "base.binding.json",
      sha256: sha256(baseBindingBytes),
      schemaVersion: "midgard-phase1-live-corpus-binding-v2",
    },
    fanoutReport: {
      path: "fanout.json",
      sha256: sha256(fanoutBytes),
      schemaVersion: "midgard-stress-wallet-fanout-v1",
    },
  };
  const scheduleEntries = chainIds.map((chainId) => ({
    chainId,
    baseDepth: 1,
    targetDepth: 2,
    extensionRows: 1,
  }));
  const schedule = {
    algorithm: "balanced-prefix-preserving-chain-depth-v1",
    baseChainCount: chainCount,
    baseDepth: 1,
    baseRowCount: chainCount,
    targetRowCount: chainCount * 2,
    extensionRowCount: chainCount,
    minimumTargetDepth: 2,
    maximumTargetDepth: 2,
    depthHistogram: [{ targetDepth: 2, chainCount }],
    entriesSha256: sha256(
      Buffer.from(
        scheduleEntries
          .map(
            (entry) =>
              `${entry.chainId}|${entry.baseDepth}|${entry.targetDepth}|${entry.extensionRows}`,
          )
          .join("\n"),
      ),
    ),
    entries: scheduleEntries,
  };
  const fundingModelBase = {
    source: "cryptographically-verified-retained-terminal-output-1-per-wallet",
    retainedBaseOriginalFundingSetSha256: walletSetIdentity.fundingSetSha256,
    retainedTerminalSetHashAlgorithm:
      PHASE5_HISTORICAL_RETAINED_TERMINAL_SET_HASH_ALGORITHM,
    retainedTerminalSetSha256: sha256(
      Buffer.from(
        rows
          .slice(0, chainCount)
          .map((row, chainIndex) => {
            const native = decodeMidgardNativeTxFullFromCanonicalCbor(
              Buffer.from(row.canonicalCborHex, "hex"),
            );
            const outputs = decodeMidgardNativeByteListPreimage(
              native.body.outputsPreimageCbor,
              "native.outputs",
            );
            const lovelace = CML.TransactionOutput.from_cbor_bytes(outputs[1])
              .amount()
              .coin();
            return `${chainIds[chainIndex]}|${row.txHash}#1|${sha256(outputs[1])}|${lovelace.toString(10)}`;
          })
          .join("\n"),
      ),
    ),
    freshFundingLovelace: "0",
    retainedTerminalLovelaceTotal: rows
      .slice(0, chainCount)
      .reduce((total, row) => {
        const native = decodeMidgardNativeTxFullFromCanonicalCbor(
          Buffer.from(row.canonicalCborHex, "hex"),
        );
        const outputs = decodeMidgardNativeByteListPreimage(
          native.body.outputsPreimageCbor,
          "native.outputs",
        );
        return (
          total +
          CML.TransactionOutput.from_cbor_bytes(outputs[1]).amount().coin()
        );
      }, 0n)
      .toString(10),
    continuationFundingValueSource:
      "decoded-canonical-retained-terminal-output-1-cross-checked-against-wallet-and-chain",
    amountLovelacePerRow,
    retainedBaseRequestedTransferLovelace: (
      BigInt(amountLovelacePerRow) * BigInt(chainCount)
    ).toString(10),
    extensionRequestedTransferLovelace: (
      BigInt(amountLovelacePerRow) * BigInt(chainCount)
    ).toString(10),
    feeFormula: {
      minFeeA: "10",
      minFeeB: "10",
      formula: "minFeeA * canonicalCborByteLength + minFeeB",
    },
    minimumTerminalChangeLovelacePerChain: "1",
    proof:
      "every continuation started from canonical retained terminal output 1 and built only its scheduled extension rows",
  };
  const fundingModel =
    fundingModelOverride === undefined
      ? fundingModelBase
      : fundingModelOverride(fundingModelBase);
  const corpusSha256 = sha256(corpusBytes);
  const indexSha256 = sha256(indexBytes);
  const historicalManifest = {
    ...marker(PHASE5_HISTORICAL_MANIFEST_SCHEMA),
    baseEvidence,
    walletSetIdentity,
    schedule,
    fundingModel,
    files: {
      corpus: {
        path: "historical.ndjson",
        sha256: corpusSha256,
        rowCount: schedule.targetRowCount,
      },
      index: {
        path: "historical.index.ndjson",
        sha256: indexSha256,
        rowCount: extendedEntries.length,
      },
    },
  };
  const manifestPath = join(root, "historical.manifest.json");
  const manifestBytes = jsonBytes(historicalManifest);
  await writeFile(manifestPath, manifestBytes);
  const verification = {
    ...marker(PHASE5_HISTORICAL_VERIFICATION_SCHEMA),
    baseEvidence,
    walletSetIdentity,
    schedule,
    fundingModel,
    corpus: {
      path: "historical.ndjson",
      indexPath: "historical.index.ndjson",
      manifestPath: "historical.manifest.json",
      corpusSha256,
      indexSha256,
      manifestSha256: sha256(manifestBytes),
    },
    checks: {
      baseGlobalPrefixByteIdentical: true,
      everyBaseChainPrefixByteIdentical: true,
      everyContinuationMetadataLinkValidByStressCorpusVerifier: true,
      everyRetainedTerminalCanonicalNativeIdentityAndDeclaredIoValid: true,
      everyContinuationCanonicalNativeIdentityAndDeclaredIoValid: true,
      exactTargetRowCount: true,
      rowCount: schedule.targetRowCount,
      checkedPrefixRows: schedule.baseRowCount,
      checkedExtensionRows: schedule.extensionRowCount,
      checkedContinuationCount: chainCount,
      checkedCanonicalBaseTerminalRows: chainCount,
      checkedCanonicalContinuationRows: schedule.extensionRowCount,
    },
  };
  const verificationPath = join(root, "historical.verify.json");
  const verificationBytes = jsonBytes(verification);
  await writeFile(verificationPath, verificationBytes);
  const binding = {
    ...marker(PHASE5_HISTORICAL_BINDING_SCHEMA),
    baseEvidence,
    walletSetIdentity,
    schedule,
    fundingModel,
    corpus: {
      path: "historical.ndjson",
      indexPath: "historical.index.ndjson",
      manifestPath: "historical.manifest.json",
      verificationPath: "historical.verify.json",
      corpusSha256,
      indexSha256,
      manifestSha256: sha256(manifestBytes),
      verificationSha256: sha256(verificationBytes),
      rowCount: schedule.targetRowCount,
      uniqueChainCount: chainCount,
      indexEntryCount: extendedEntries.length,
    },
  };
  const bindingPath = join(root, "historical.binding.json");
  const bindingBytes = jsonBytes(binding);
  await writeFile(bindingPath, bindingBytes);
  const generation = {
    ...marker(PHASE5_HISTORICAL_GENERATION_SCHEMA),
    baseEvidence,
    walletSetIdentity,
    schedule,
    fundingModel,
    files: {
      corpus: {
        path: "historical.ndjson",
        sha256: corpusSha256,
        rowCount: schedule.targetRowCount,
      },
      index: {
        path: "historical.index.ndjson",
        sha256: indexSha256,
        rowCount: extendedEntries.length,
      },
      manifest: {
        path: "historical.manifest.json",
        sha256: sha256(manifestBytes),
      },
      verification: {
        path: "historical.verify.json",
        sha256: sha256(verificationBytes),
      },
      historicalBinding: {
        path: "historical.binding.json",
        sha256: sha256(bindingBytes),
      },
    },
    assembled: {
      rowCount: schedule.targetRowCount,
      indexEntryCount: extendedEntries.length,
      corpusSha256,
      indexSha256,
    },
    verification: {
      rowCount: schedule.targetRowCount,
      corpusSha256,
      indexSha256,
    },
  };
  const generationPath = join(root, "historical.generation.json");
  const generationBytes = jsonBytes(generation);
  await writeFile(generationPath, generationBytes);
  const suite = {
    sourceCorpusBindingPath: "historical.binding.json",
    sourceCorpusBindingSha256: sha256(bindingBytes),
    sourceCorpusManifestPath: "historical.manifest.json",
    sourceCorpusManifestSha256: sha256(manifestBytes),
    sourceCorpusGenerationResultPath: "historical.generation.json",
    sourceCorpusGenerationResultSha256: sha256(generationBytes),
  };
  return {
    root,
    rows,
    lines,
    corpusPath,
    corpusBytes,
    indexPath,
    bindingPath,
    binding,
    suite,
    baseCorpusBytes,
    fanoutPath,
    fanout,
    walletSetIdentity,
    extendedEntries,
  };
};

test("accepts an explicitly offline historical extension and proves its raw prefix", async () => {
  const fixture = await makeFixture();
  try {
    const provenance = await verifyPhase5DaSourceCorpusEvidence(
      fixture.root,
      fixture.suite,
      { sampleCount: 1, transactionCount: 2 },
    );
    assert.equal(provenance.evidenceMode, "historical-offline-extension");
    assert.equal(provenance.corpusRows, 2);
    assert.equal(provenance.prefixBytes, fixture.baseCorpusBytes.length);
    assert.equal(provenance.prefixSha256, sha256(fixture.baseCorpusBytes));

    const first = historicalChainTransactions[0];
    const entries = [
      {
        sampleIndex: 0,
        transactionSetSha256: transactionSetSha256([first]),
        transactionContentSha256: transactionContentSha256([first]),
        corpusWindow: {
          sha256: sha256(Buffer.from(`${fixture.lines[0]}\n`)),
        },
      },
    ];
    await verifyPhase5DaCorpusEvidence(fixture.corpusPath, entries, {
      sampleCount: 1,
      transactionCount: 1,
      expectedRows: 2,
      expectedNormalizedSha256: sha256(fixture.corpusBytes),
      expectedFileSha256: sha256(fixture.corpusBytes),
      expectedPrefixBytes: provenance.prefixBytes,
      expectedPrefixSha256: provenance.prefixSha256,
    });

    const swappedBytes = Buffer.from(
      `${fixture.lines[1]}\n${fixture.lines[0]}\n`,
    );
    await writeFile(fixture.corpusPath, swappedBytes);
    const second = historicalChainTransactions[1];
    await assert.rejects(
      verifyPhase5DaCorpusEvidence(
        fixture.corpusPath,
        [
          {
            sampleIndex: 0,
            transactionSetSha256: transactionSetSha256([second]),
            transactionContentSha256: transactionContentSha256([second]),
            corpusWindow: {
              sha256: sha256(Buffer.from(`${fixture.lines[1]}\n`)),
            },
          },
        ],
        {
          sampleCount: 1,
          transactionCount: 1,
          expectedRows: 2,
          expectedNormalizedSha256: sha256(swappedBytes),
          expectedFileSha256: sha256(swappedBytes),
          expectedPrefixBytes: provenance.prefixBytes,
          expectedPrefixSha256: provenance.prefixSha256,
        },
      ),
      /source corpus bytes disagree with the declared identity/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects retained-terminal metadata omitted behind internally rebound evidence", async () => {
  const fixture = await makeFixture({
    retainedTerminalOverride: {
      senderWalletId: undefined,
      outputOutrefs: ["0".repeat(64) + "#0", "0".repeat(64) + "#1"],
    },
  });
  try {
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 2,
      }),
      /retained terminal 0 disagrees with its chain/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects a self-attested retained-terminal digest and lovelace total", async () => {
  const fixture = await makeFixture({
    fundingModelOverride: (fundingModel) => ({
      ...fundingModel,
      retainedTerminalSetSha256: "f".repeat(64),
      retainedTerminalLovelaceTotal: (
        BigInt(fundingModel.retainedTerminalLovelaceTotal) + 1n
      ).toString(10),
    }),
  });
  try {
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 2,
      }),
      /(?:funding model does not match|retained-terminal set digest disagrees)/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects a fanout wallet that does not own the retained terminal output", async () => {
  const retained = decodeMidgardNativeTxFullFromCanonicalCbor(
    Buffer.from(historicalChainTransactions[0][1], "hex"),
  );
  const outputs = decodeMidgardNativeByteListPreimage(
    retained.body.outputsPreimageCbor,
    "native.outputs",
  );
  const retainedTerminalOutput = CML.TransactionOutput.from_cbor_bytes(
    outputs[1],
  );
  const retainedTerminalAddress = retainedTerminalOutput.address().to_bech32();
  let alternateOutput;
  for (const [, canonicalCborHex] of checkedTransactionEntries) {
    const transaction = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(canonicalCborHex, "hex"),
    );
    const candidateOutputs = decodeMidgardNativeByteListPreimage(
      transaction.body.outputsPreimageCbor,
      "native.outputs",
    );
    alternateOutput = candidateOutputs
      .map((output) => CML.TransactionOutput.from_cbor_bytes(output))
      .find(
        (output) => output.address().to_bech32() !== retainedTerminalAddress,
      );
    if (alternateOutput !== undefined) break;
  }
  assert.ok(alternateOutput !== undefined);
  const fixture = await makeFixture({
    fanoutWalletOverride: (wallet) => {
      const l2Address = alternateOutput.address().to_bech32();
      const { paymentCredential } = decodeMidgardAddressText(l2Address);
      assert.equal(paymentCredential.kind, "PubKey");
      return {
        ...wallet,
        l2Address,
        paymentKeyHash: paymentCredential.hash.toString("hex"),
        latestFunding: {
          ...wallet.latestFunding,
          fundingUtxos: [
            {
              ...wallet.latestFunding.fundingUtxos[0],
              outputCbor: Buffer.from(alternateOutput.to_cbor_bytes()).toString(
                "hex",
              ),
              lovelace: alternateOutput.amount().coin().toString(10),
            },
          ],
        },
      };
    },
  });
  try {
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 2,
      }),
      /(?:required signer does not match its bound wallet|is not witnessed solely by its bound wallet|transfer amount, address, or value conservation changed|terminal output does not belong to its bound fanout wallet)/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects a fanout wallet whose payment key is not bound to its address", async () => {
  const fixture = await makeFixture({
    fanoutWalletOverride: (wallet) => ({
      ...wallet,
      paymentKeyHash: "f".repeat(56),
    }),
  });
  try {
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 2,
      }),
      /payment key does not own its address/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects a rebound funding model whose fee parameters differ from the base manifest", async () => {
  const fixture = await makeFixture({
    fundingModelOverride: (fundingModel) => ({
      ...fundingModel,
      feeFormula: {
        ...fundingModel.feeFormula,
        minFeeA: (BigInt(fundingModel.feeFormula.minFeeA) + 1n).toString(10),
      },
    }),
  });
  try {
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 2,
      }),
      /funding model does not match the immutable base manifest/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects canonical transactions above the rebound base manifest limit", async () => {
  const fixture = await makeFixture({ maxSubmitTxCborBytesOverride: 1 });
  try {
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 2,
      }),
      /not an exact fee-bound plain Preprod transfer/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects a rebound transaction with non-empty auxiliary data commitment", async () => {
  const fixture = await makeFixture({
    continuationOverride: mutateHistoricalTransaction(
      historicalChainTransactions[1],
      (body) => ({ ...body, auxiliaryDataHash: Buffer.alloc(32, 0x42) }),
    ),
  });
  try {
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 2,
      }),
      /not an exact fee-bound plain Preprod transfer/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects a rebound transaction with bounded validity", async () => {
  const fixture = await makeFixture({
    continuationOverride: mutateHistoricalTransaction(
      historicalChainTransactions[1],
      (body) => ({
        ...body,
        validityIntervalStart: 1n,
        validityIntervalEnd: 2n,
      }),
    ),
  });
  try {
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 2,
      }),
      /not an exact fee-bound plain Preprod transfer/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects a rebound Phase 1 binding without live preflight provenance", async () => {
  const fixture = await makeFixture({
    baseBindingOverride: ({ livePreflight: _livePreflight, ...binding }) =>
      binding,
  });
  try {
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 2,
      }),
      /live preflight contract is incomplete/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects a rebound Phase 1 harness path that differs from its bound corpus", async () => {
  const fixture = await makeFixture({
    baseBindingOverride: (binding) => ({
      ...binding,
      stressCorpusEnv: {
        ...binding.stressCorpusEnv,
        STRESS_CORPUS_PATH: "/tmp/unrelated-corpus.ndjson",
      },
    }),
  });
  try {
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 2,
      }),
      /stress-corpus environment contract is incomplete/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects a continuation whose declared parent was internally rebound", async () => {
  const fixture = await makeFixture({
    continuationOverride: { parentTxHash: "f".repeat(64) },
  });
  try {
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 2,
      }),
      /does not continue its exact previous chain state/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects a fully rebound non-deterministic live preflight sample", async () => {
  const fixture = await makeFixture({
    chainCount: 2,
    reverseLivePreflightEntries: true,
  });
  try {
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 4,
      }),
      /live preflight contract is incomplete/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects a continuation whose selected input metadata was internally rebound", async () => {
  const fixture = await makeFixture({
    continuationOverride: {
      selectedInputOutref: `${"f".repeat(64)}#1`,
    },
  });
  try {
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 2,
      }),
      /native input does not match selectedInputOutref/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects fresh-live relabeling and retained-evidence mutation", async () => {
  const fixture = await makeFixture();
  try {
    const relabeledBindingBytes = jsonBytes({
      ...fixture.binding,
      freshLiveClaim: true,
    });
    await writeFile(fixture.bindingPath, relabeledBindingBytes);
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(
        fixture.root,
        {
          ...fixture.suite,
          sourceCorpusBindingSha256: sha256(relabeledBindingBytes),
        },
        { sampleCount: 1, transactionCount: 2 },
      ),
      /not explicitly Phase-5-only historical evidence/u,
    );

    const bindingBytes = jsonBytes(fixture.binding);
    await writeFile(fixture.bindingPath, bindingBytes);
    await writeFile(fixture.fanoutPath, jsonBytes({ tampered: true }));
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 2,
      }),
      /historical base fanout report bytes changed/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects an extended index whose bytes no longer match its bound hash", async () => {
  const fixture = await makeFixture();
  try {
    const entries = (await readFile(fixture.indexPath, "utf8"))
      .trim()
      .split("\n")
      .map((line) => ({ ...JSON.parse(line), corpusSliceId: "relabeled" }));
    await writeFile(
      fixture.indexPath,
      Buffer.from(
        `${entries.map((entry) => JSON.stringify(entry)).join("\n")}\n`,
      ),
    );
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 2,
      }),
      /historical extended index bytes changed/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects distinct chains whose rebound fanout identity shares a first funding outref", async () => {
  const fixture = await makeFixture({
    chainCount: 2,
    duplicateFirstFundingOutref: true,
  });
  try {
    const wallets = fixture.fanout.wallets.map((entry) => entry.wallet);
    assert.equal(new Set(wallets.map((wallet) => wallet.l2Address)).size, 2);
    assert.equal(
      new Set(
        wallets.map((wallet) => wallet.latestFunding.fundingUtxos[0].outref),
      ).size,
      1,
    );
    assert.equal(fixture.walletSetIdentity.walletCount, 2);
    assert.equal(fixture.walletSetIdentity.uniqueFirstFundingOutrefCount, 1);
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 4,
      }),
      /first funding outrefs must be unique per chain/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects a rebound continuation-run boundary shifted into an NDJSON row", async () => {
  const fixture = await makeFixture({
    chainCount: 2,
    continuationBoundaryShift: 1,
  });
  try {
    assert.equal(fixture.extendedEntries.length, 4);
    assert.equal(
      fixture.extendedEntries[2].endByteOffset,
      fixture.extendedEntries[3].startByteOffset,
    );
    assert.notEqual(
      fixture.corpusBytes[fixture.extendedEntries[2].endByteOffset - 1],
      0x0a,
    );
    await assert.rejects(
      verifyPhase5DaSourceCorpusEvidence(fixture.root, fixture.suite, {
        sampleCount: 1,
        transactionCount: 4,
      }),
      /historical corpus run 2 does not end at its indexed byte offset/u,
    );
  } finally {
    await rm(fixture.root, { recursive: true, force: true });
  }
});

test("rejects a transaction repeated across otherwise distinct formal windows", async () => {
  const root = await mkdtemp(join(tmpdir(), "phase5-overlap-evidence-"));
  try {
    const transactions = [
      checkedTransactions[0],
      checkedTransactions[1],
      checkedTransactions[0],
      checkedTransactions[2],
    ];
    const lines = transactions.map(([txHash, canonicalCborHex]) =>
      JSON.stringify({ txHash, canonicalCborHex }),
    );
    const corpusBytes = Buffer.from(`${lines.join("\n")}\n`);
    const corpusPath = join(root, "overlap.ndjson");
    await writeFile(corpusPath, corpusBytes);
    const entries = [0, 1].map((sampleIndex) => {
      const window = transactions.slice(sampleIndex * 2, sampleIndex * 2 + 2);
      return {
        sampleIndex,
        transactionSetSha256: transactionSetSha256(window),
        transactionContentSha256: transactionContentSha256(window),
        corpusWindow: {
          sha256: sha256(
            Buffer.from(
              `${lines.slice(sampleIndex * 2, sampleIndex * 2 + 2).join("\n")}\n`,
            ),
          ),
        },
      };
    });
    await assert.rejects(
      verifyPhase5DaCorpusEvidence(corpusPath, entries, {
        sampleCount: 2,
        transactionCount: 2,
        expectedRows: 4,
        expectedNormalizedSha256: sha256(corpusBytes),
        expectedFileSha256: sha256(corpusBytes),
      }),
      /duplicated globally at rows 0 and 2/u,
    );
  } finally {
    await rm(root, { recursive: true, force: true });
  }
});
