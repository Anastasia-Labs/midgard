import { validateArchitectureGRootGateSummary } from "./mpf-architecture-g-gate-config.mjs";

export const hash = (byte) => byte.toString(16).padStart(2, "0").repeat(32);

export const corpusFundingDocument = () => {
  const roots = [
    { walletId: "wallet-0", outref: `${hash(92)}#0` },
    { walletId: "wallet-1", outref: `${hash(93)}#1` },
  ];
  return {
    roots,
    artifact: {
      schemaVersion: "midgard-architecture-g-corpus-funding-v1",
      corpusSha256: hash(94),
      sliceSha256: hash(95),
      entries: roots.map((root, index) => ({
        ...root,
        outputCbor: index.toString(16).padStart(2, "0"),
      })),
    },
  };
};

export const nearestRank = (values, quantile) => {
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.max(0, Math.ceil(sorted.length * quantile) - 1)];
};

export const rootGateOwnerDiagnostics = (durableRoot) => ({
  ownerEpoch: { type: "Buffer", data: Array(16).fill(7) },
  durableRoot,
  residentNodes: 10,
  residentEdges: 9,
  residentBytes: 1024,
  activeGenerations: 0,
  generatedNodes: 20,
  generatedBytes: 2048,
  rssBytes: 4096,
  peakRssBytes: 8192,
  childRestarts: 0,
});

export const validateRootGateSummary = (summary) =>
  validateArchitectureGRootGateSummary({
    summary,
    mode: summary.mode,
    runs: 2,
    transactions: 2,
    cpuSet: "28-31",
  });

export const candidateProbeResult = () => ({
  schemaVersion: "midgard-architecture-g-commit-candidate-probe-v1",
  probePath: "/probes/mpf-commit-candidate-probe.js",
  probeSha256: "77".repeat(32),
  inputPath: "/inputs/candidate-input.json",
  inputSha256: "66".repeat(32),
  expectedTransactionCount: 50_000,
  cpuAffinity: "2-9",
  corpusSha256: "11".repeat(32),
  corpusSliceSha256: "22".repeat(32),
  fundingMapSha256: "33".repeat(32),
  fixtureCreationSha256: "55".repeat(32),
  fixtureInitialUtxoCount: 1_000_000,
  baseUtxoPayloadAggregate: {
    entryCount: 1_000_000,
    encodedTupleBytes: 80_000_000,
  },
  binarySha256: "44".repeat(32),
  durationMs: 9_000,
  confirmedLedgerFullScans: 1,
  userEventRows: { deposits: 0, forcedTransactions: 0, withdrawals: 0 },
  providerReads: 4,
  providerBoundaryAttempts: 0,
  submissionAttempts: 0,
  journalRowsBefore: 0,
  journalRowsAfter: 0,
  candidateConfig: {
    mpfEngine: "architecture_g",
    scratchBuild: "fromlist",
    payloadRootCheck: "off",
    parallelRoots: true,
    costModel: "ewma",
    mempoolRetrievePageSize: 50_000,
    maxL2TxCount: 50_000,
    maxLedgerOpCount: 150_000,
    maxTransitionStepCount: 50_000,
  },
  candidate: {
    endTimeMs: 1_700_000_000_000,
    l2TransactionCount: 50_000,
    roots: Object.fromEntries(
      [
        "utxos",
        "rawTransactions",
        "transactions",
        "transitionTrace",
        "eventToStep",
      ].map((name, index) => [
        name,
        (index + 1).toString(16).padStart(2, "0").repeat(32),
      ]),
    ),
  },
  ownerBefore: rootGateOwnerDiagnostics(hash(120)),
  ownerAfter: rootGateOwnerDiagnostics(hash(120)),
});
