import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { readFileSync, utimesSync, writeFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

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

import { sha256File } from "./phase3-architecture-g-closure-lib.mjs";
import { createPhase3SoakCorpusPreflight } from "./phase3-architecture-g-soak-preflight.mjs";
import {
  loadCorpusIndex,
  loadCorpusManifest,
  selectCorpusIndexEntries,
} from "./throughput-valid-stress-corpus.mjs";

export const hash = (character) => character.repeat(64);

export const dockerRuntime = () => ({
  schemaVersion: "midgard-phase3-trusted-docker-runtime-v1",
  client: {
    path: "/usr/bin/docker",
    realPath: "/trusted/docker",
    sha256: hash("d"),
    bytes: 1_024,
    mode: 0o755,
    uid: 0,
    gid: 0,
    dev: "1",
    ino: "2",
  },
  socket: {
    path: "/var/run/docker.sock",
    realPath: "/run/docker.sock",
    endpoint: "unix:///var/run/docker.sock",
    mode: 0o660,
    uid: 0,
    gid: 999,
    dev: "3",
    ino: "4",
  },
  daemon: {
    id: "daemon-id",
    name: "docker-desktop",
    serverVersion: "29.2.0",
    operatingSystem: "Docker Desktop",
    osType: "linux",
    architecture: "x86_64",
  },
  environment: {
    inheritedDockerVariables: [],
    pathResolutionRealPath: "/trusted/docker",
    daemonEndpoint: "unix:///var/run/docker.sock",
    home: "/nonexistent",
  },
});

export const soakCliPath = fileURLToPath(
  new URL("./phase3-architecture-g-soak.mjs", import.meta.url),
);

export const runCliSetupFailure = ({ args, env, evidenceDirectory, phase }) => {
  const result = spawnSync(process.execPath, [soakCliPath, ...args], {
    cwd: path.dirname(path.dirname(soakCliPath)),
    env: { ...process.env, ...env },
    encoding: "utf8",
    maxBuffer: 4 * 1024 * 1024,
  });
  assert.equal(
    result.status,
    1,
    JSON.stringify({ error: result.error?.message, signal: result.signal }),
  );
  const retainedReport = JSON.parse(
    readFileSync(path.join(evidenceDirectory, "report.json"), "utf8"),
  );
  const retainedVerification = JSON.parse(
    readFileSync(path.join(evidenceDirectory, "verification.json"), "utf8"),
  );
  assert.equal(retainedReport.termination.phase, phase);
  assert.equal(retainedReport.startedAtMs, null);
  assert.equal(retainedVerification.phase, phase);
  assert.equal(retainedVerification.passed, false);
};

export const corpusRow = ({ chain, index, input }) => {
  const transaction = materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: encodeCbor(
        [0x11, 0x22].map((fill) =>
          encodeMidgardTxOutput({
            address: Buffer.concat([
              Buffer.from([0x60]),
              Buffer.alloc(28, fill),
            ]),
            value: {
              lovelace: BigInt(chain * 1_000 + index),
              assets: new Map(),
            },
          }),
        ),
      ),
      fee: BigInt(chain * 1_000 + index),
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
  const bytes = encodeMidgardNativeTxCanonical(transaction);
  const txHash = computeMidgardNativeTxId(transaction).toString("hex");
  return {
    txHash,
    canonicalCborHex: bytes.toString("hex"),
    canonicalCborSha256: createHash("sha256").update(bytes).digest("hex"),
    canonicalCborByteLength: bytes.length,
    senderWalletId: `wallet-${chain.toString()}`,
    selectedInputOutref: input,
    outputOutrefs: [`${txHash}#0`, `${txHash}#1`],
    planShape: "chain",
    parentTxHash: null,
    corpusSliceId: "default",
  };
};

export const makeCorpusPreflightFixture = async (directory) => {
  const corpusPath = path.join(directory, "corpus.ndjson");
  const indexPath = path.join(directory, "corpus.index.ndjson");
  const manifestPath = path.join(directory, "corpus.manifest.json");
  const phase1Path = path.join(directory, "phase1.json");
  const outPath = path.join(directory, "preflight.json");
  const rows = [
    corpusRow({ chain: 1, index: 1, input: `${"a".repeat(64)}#0` }),
    corpusRow({ chain: 2, index: 1, input: `${"b".repeat(64)}#0` }),
  ];
  const lines = rows.map((row) => `${JSON.stringify(row)}\n`);
  writeFileSync(corpusPath, lines.join(""));
  const stableCorpusTime = new Date(1_700_000_000_000);
  utimesSync(corpusPath, stableCorpusTime, stableCorpusTime);
  const firstBytes = Buffer.byteLength(lines[0]);
  const index = [
    {
      corpusSliceId: "default",
      planShape: "chain",
      chainId: "wallet-1",
      startByteOffset: 0,
      endByteOffset: firstBytes,
      rowCount: 1,
    },
    {
      corpusSliceId: "default",
      planShape: "chain",
      chainId: "wallet-2",
      startByteOffset: firstBytes,
      endByteOffset: Buffer.byteLength(lines.join("")),
      rowCount: 1,
    },
  ];
  writeFileSync(indexPath, `${index.map(JSON.stringify).join("\n")}\n`);
  const manifest = {
    schemaVersion: "midgard-stress-corpus-manifest-v1",
    targetRateTps: 1,
    durationMs: 1_000,
    warmupCount: 0,
    cooldownCount: 0,
    safetyFactor: 1,
    assumedAcceptanceLatencyMs: 1,
    chainCount: 2,
    chainDepth: 1,
    corpusShape: "chain",
    corpusSliceIds: ["default"],
    generatedAtIso: "2026-07-01T00:00:00.000Z",
    generatorGitSha: "1".repeat(40),
    lucidMidgardVersion: "fixture-v1",
    feeParams: { minFeeA: "1", minFeeB: "1" },
    network: "Preprod",
    networkId: "0",
    maxSubmitTxCborBytes: 16_384,
    amountTemplate: {
      lovelace: "2000000",
      shape: "self-transfer-change-chain",
    },
    verification: {
      rebuildSampleRate: 1,
      rebuildSampleAlgorithm: "sha256-corpus-chain-id-order-v1",
    },
    fundingSummary: {
      walletCount: 2,
      perWalletFundingLovelace: "1",
      totalFundingLovelace: "2",
    },
    walletSetIdentity: {
      walletCount: 2,
      fundingRowCount: 2,
      uniqueFirstFundingOutrefCount: 2,
      walletSetHashAlgorithm: "sha256-wallet-id-l2-address-lines-v1",
      walletSetSha256: hash("d"),
      fundingSetHashAlgorithm:
        "sha256-wallet-id-outref-output-cbor-sha256-lines-v1",
      fundingSetSha256: hash("e"),
    },
    sliceSummary: [{ corpusSliceId: "default", walletCount: 2, rowCount: 2 }],
    files: {
      corpus: {
        path: corpusPath,
        sha256: sha256File(corpusPath),
        rowCount: 2,
      },
      index: {
        path: indexPath,
        sha256: sha256File(indexPath),
        rowCount: 2,
      },
      shards: ["fixture-shard-0"],
    },
  };
  writeFileSync(manifestPath, `${JSON.stringify(manifest)}\n`);
  const corpusIdentity = {
    path: corpusPath,
    indexPath,
    manifestPath,
    sliceId: "default",
    corpusSha256: sha256File(corpusPath),
    indexSha256: sha256File(indexPath),
    manifestSha256: sha256File(manifestPath),
  };
  const phase1Binding = {
    corpus: corpusIdentity,
    stressCorpusEnv: {
      STRESS_CORPUS_PATH: corpusPath,
      STRESS_CORPUS_INDEX_PATH: indexPath,
      STRESS_CORPUS_MANIFEST_PATH: manifestPath,
      STRESS_CORPUS_SLICE_ID: "default",
      STRESS_CORPUS_SHAPE: "chain",
    },
  };
  writeFileSync(phase1Path, `${JSON.stringify(phase1Binding)}\n`);
  const phase1BindingSha256 = sha256File(phase1Path);
  const sourceIdentity = { sourceTreeSha256: hash("c") };
  const artifact = await createPhase3SoakCorpusPreflight({
    outPath,
    phase1Binding,
    phase1BindingPath: phase1Path,
    phase1BindingSha256,
    sourceIdentity,
    corpusIdentity,
    corpusSliceId: "default",
    corpusShape: "chain",
  });
  const loadedManifest = await loadCorpusManifest(manifestPath);
  const fullIndex = await loadCorpusIndex(indexPath);
  const selectedEntries = selectCorpusIndexEntries({
    index: fullIndex,
    corpusSliceId: "default",
    corpusShape: "chain",
    maxChains: null,
  });
  return {
    artifact,
    corpusIdentity,
    phase1BindingSha256,
    sourceIdentity,
    loadedManifest,
    fullIndex,
    selectedEntries,
  };
};

export const sample = (elapsedMs, overrides = {}) => ({
  observedAtMs: 1_750_000_000_000 + elapsedMs,
  elapsedMs,
  readiness: { httpStatus: 200, ready: true, reasons: [] },
  metrics: {
    auditDivergence: 0,
    auditAgeMs: 1_000,
    auditCompletedAtMs: 1_749_999_999_000,
    confirmedLedgerFullScanTotal: 9,
    validationWorkerTimeoutTotal: 3,
    l1ControlPlaneTimeoutTotal: 1,
    timeoutInsteadOfBackpressureTotal: 4,
    daPublicationBacklog: 0,
    mergeQueueDepth: 0,
  },
  owner: {
    durableRoot: hash("a"),
    residentNodes: 1_000_000,
    residentBytes: 700 * 1024 ** 2,
    activeGenerations: 0,
    generatedNodes: 500_000,
    generatedBytes: 500 * 1024 ** 2,
    rssBytes: 800 * 1024 ** 2,
    peakRssBytes: 900 * 1024 ** 2,
    childRestarts: 0,
  },
  process: {
    pid: 42,
    startTicks: "123456",
    rssBytes: Math.round(1024 ** 3 * (1 + 0.05 * (elapsedMs / 86_400_000))),
  },
  ...overrides,
});
