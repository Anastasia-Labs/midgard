import assert from "node:assert/strict";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { after, test } from "node:test";

import {
  evaluatePhase4PipelinedProcessSummary,
  PHASE4_PROCESS_CHECKPOINTS,
  PHASE4_PROCESS_SUMMARY_MODE,
  PHASE4_PROCESS_SUMMARY_SCHEMA,
  runPhase4PipelinedProcessSummaryVerifierCli,
  verifyPhase4PipelinedProcessSummaryFile,
} from "../scripts/verify-phase4-pipelined-process-summary.mjs";

const fixtureDirectory = mkdtempSync(
  join(
    process.platform === "win32" ? tmpdir() : "/tmp",
    "midgard-phase4-process-verifier-",
  ),
);
after(() => rmSync(fixtureDirectory, { recursive: true, force: true }));

const runDir = "/evidence/phase4/acceptance";
const h56 = (digit) => digit.repeat(56);
const h64 = (digit) => digit.repeat(64);
const txOne = h64("1");
const txTwo = h64("2");
const phasRegistrationTxHash =
  "f7f901aee5bef259fbc62f97cf5b89aae7a11515b490882e03009a5ea952e0ce";
const phasRegistrationCborSha256 =
  "6151d248776808489a06558ae4ccebab1c648f2ab41606f2a3e05e279ee49234";
const phasRegistrationTransactionBody = {
  type: "Unwitnessed Tx ConwayEra",
  description: "PHAS registration transaction body",
  cborHex:
    "84a400d901028182582000000000000000000000000000000000000000000000000000000000000000000001818258390056256482f4e32203bbf0e61f5c0208f776216707b8c1a198e945149ee41bf07d00d3b340e0ba35ee9c82110e6190de18b6d730577223e6c51b00000006fc0299cb021a00028db504d901028182008201581c46df0027fc0af07197924dc07f1c27ac6b15eb2bd6efc7a73b0dbb4da0f5f6",
};

const requiredNodeEnvKeys = [
  "NETWORK",
  "L1_PROVIDER",
  "L1_OGMIOS_KEY",
  "L1_KUPO_KEY",
  "POSTGRES_HOST",
  "POSTGRES_PORT",
  "POSTGRES_DB",
  "PORT",
  "PROM_METRICS_PORT",
  "RUN_GENESIS_ON_STARTUP",
  "MIDGARD_DEPLOYMENT_MANIFEST_PATH",
  "SPECULATIVE_COMMIT_BUILD",
  "LEDGER_MPF_DB_PATH",
  "TRANSACTIONS_MPF_DB_PATH",
  "STATE_QUEUE_MUTATION_LEASE_TTL_MS",
];

const classification = () => ({
  class: "restartable_runtime",
  reason: "service was externally terminated for bounded acceptance evidence",
  restartable: true,
});

const cleanup = (signal) => ({
  attempted: true,
  pid: 4242,
  target: "process_group",
  signal,
  success: true,
  error: null,
  ownershipValidation: { valid: true, reason: "owned process group matched" },
});

const supervisor = ({
  nodeId = "node-a",
  label,
  marker,
  signal,
  stopFile = false,
}) => {
  const observedAt = "2026-07-14T05:00:01.000Z";
  const attemptClassification = classification();
  return {
    schemaVersion: "midgard-e2e-service-supervisor-v1",
    service: `midgard-node-listen:${nodeId}:${label}`,
    command: {
      command: "/usr/bin/node",
      args: ["/repo/demo/midgard-node/dist/index.js", "listen"],
      cwd: "/repo/demo/midgard-node",
      envKeys: [...requiredNodeEnvKeys],
      envFiles: [],
      envInheritance: "none",
    },
    status: "restart_budget_exhausted",
    rawLogPath: `${runDir}/${label}/${nodeId}.log`,
    attempts: [
      {
        attempt: 1,
        pid: 4242,
        startedAt: "2026-07-14T05:00:00.000Z",
        finishedAt: observedAt,
        durationMs: 1_000,
        exitCode: null,
        signal,
        timedOut: false,
        classification: attemptClassification,
        cleanup: cleanup(signal),
        outputTermination: stopFile
          ? null
          : { marker, occurrence: 1, signal, at: observedAt },
        fileTermination: stopFile
          ? {
              path: `${runDir}/${label}/${nodeId}.submitted.stop`,
              signal,
              at: observedAt,
            }
          : null,
      },
    ],
    restartCount: 0,
    terminalClassification: { ...attemptClassification },
  };
};

const journalMember = (sourceId, ordinal = 0) => ({
  memberId: h64("a"),
  ordinal,
  payloadSha256: h64("b"),
  sourceTable: "mempool",
  sourceId,
});

const databaseState = ({
  headerHash = h56("3"),
  baseHeaderHash = h56("4"),
  submittedTxHash = h64("5"),
  transactionIds = [txOne],
  retainedIds = transactionIds,
  leaseToken = "lease-token",
  activeLeaseToken = "active-lease-token",
  recentLeases = [],
} = {}) => ({
  activeJournalCount: 1,
  activeJournal: {
    headerHash,
    headerCbor: "00",
    journalPayloadIdentity: {
      deposits: [],
      forcedTransactions: [],
      withdrawals: [],
      transactions: transactionIds.map((txId, ordinal) =>
        journalMember(txId, ordinal),
      ),
      transitionTrace: [],
      eventToStep: [],
      utxos: [],
    },
    submittedTxHash,
    status: "submitted_unconfirmed",
    baseTailHeaderHash: baseHeaderHash,
    baseTailOutRef: `${h64("6")}#0`,
    baseTailDatumCbor: "00",
    baseRoots: {
      utxos: h64("1"),
      forcedTransactions: h64("2"),
      transactions: h64("3"),
      deposits: h64("4"),
      withdrawals: h64("5"),
    },
    expectedRoots: {
      utxos: h64("1"),
      forcedTransactions: h64("2"),
      transactions: h64("3"),
      deposits: h64("4"),
      withdrawals: h64("5"),
      transitionTrace: h64("6"),
      eventToStep: h64("7"),
    },
    mpfReplay: {
      baseRoot: null,
      candidateRoot: null,
      eventLogDigest: null,
      eventRoots: null,
      eventCount: 0,
    },
    leaseToken,
    depositCount: 0,
    mempoolTxCount: transactionIds.length,
  },
  activeLease: {
    holder: "node-a",
    token: activeLeaseToken,
    status: "active",
  },
  recentLeases,
  deposits: [],
  mempool: retainedIds.map((txId) => ({ txId, tx: "00" })),
  processed: [],
});

const crashEvidence = (checkpoint, index) => {
  const baseHeaderHash = h56((index + 4).toString());
  const afterCrash = databaseState({
    headerHash: baseHeaderHash,
    baseHeaderHash: h56("1"),
  });
  const flagOnSubmitted = databaseState({
    headerHash: h56((index + 7).toString()),
    baseHeaderHash,
    leaseToken: `flag-on-${checkpoint}`,
    activeLeaseToken: `active-on-${checkpoint}`,
  });
  const flagOffControl = structuredClone(flagOnSubmitted);
  flagOffControl.activeJournal.leaseToken = `flag-off-${checkpoint}`;
  flagOffControl.activeLease.token = `active-off-${checkpoint}`;
  flagOffControl.recentLeases = [
    { holder: "control-node", status: "released", lastError: null },
  ];
  return {
    checkpoint,
    baseHeaderHash,
    crash: supervisor({
      label: `${checkpoint}-crash`,
      marker: `pipeline_trace phase=e2e_crash_checkpoint checkpoint=${checkpoint}`,
      signal: "SIGKILL",
    }),
    restartReady: supervisor({
      label: `${checkpoint}-restart-ready`,
      marker: "pipeline_trace phase=candidate_ready",
      signal: "SIGTERM",
    }),
    restartSubmitted: supervisor({
      label: `${checkpoint}-restart-submitted`,
      marker: "pipeline_trace phase=candidate_submitted",
      signal: "SIGTERM",
    }),
    afterCrash,
    afterRestartReady: structuredClone(afterCrash),
    flagOnSubmitted,
    flagOffControl,
  };
};

const contention = (kind) => {
  const normal = kind === "normal";
  const winnerMarker = normal
    ? "pipeline_trace phase=candidate_submitted"
    : "pipeline_trace phase=e2e_crash_checkpoint checkpoint=journal_prepared_before_submit";
  return {
    winnerNodeId: "node-a",
    loserNodeId: "node-b",
    winner: supervisor({
      nodeId: "node-a",
      label: `${kind}-contention`,
      marker: winnerMarker,
      signal: normal ? "SIGTERM" : "SIGKILL",
    }),
    loser: supervisor({
      nodeId: "node-b",
      label: `${kind}-contention`,
      marker: normal
        ? "pipeline_trace phase=candidate_invalidated reason=T2"
        : undefined,
      signal: "SIGTERM",
      stopFile: !normal,
    }),
    winnerLog: winnerMarker,
    loserLog: normal
      ? "pipeline_trace phase=candidate_invalidated reason=T2 reason=state_queue_lease_busy"
      : "reason=state_queue_lease_busy abandoning unsubmitted journal pipeline_trace phase=candidate_submitted",
  };
};

const validSummary = () => {
  const abandonedHeaderHash = h56("a");
  const originalBaseHeaderHash = h56("b");
  const recoveredTipHeaderHash = h56("c");
  const replacementHeaderHash = h56("d");
  const replacementSubmittedTxHash = h64("d");
  const composeProject = "midgard_phase4_process_live_20260714t050000z_v19";
  return {
    schemaVersion: PHASE4_PROCESS_SUMMARY_SCHEMA,
    mode: PHASE4_PROCESS_SUMMARY_MODE,
    runDir,
    checkpoints: [...PHASE4_PROCESS_CHECKPOINTS],
    isolation: {
      envFile: "/evidence/phase4/run.env",
      deploymentManifestPath: "/evidence/phase4/deployment-manifest.json",
      deploymentManifestSha256: h64("1"),
      snapshotIdentityPath: "/evidence/phase4/snapshot-identity.json",
      snapshotIdentitySha256: h64("2"),
      snapshotCardanoTip: { slot: 6493, hash: h64("3") },
      snapshotKupoCheckpoint: 6493,
      snapshotBlueprintSha256: h64("4"),
      snapshotPhasRegistrationProofSha256: h64("6"),
      snapshotPhasRegistration: {
        schemaVersion: "midgard-phase4-phas-registration-proof-v1",
        source: "cardano-cli-local-state-query",
        readOnly: true,
        registered: true,
        cardanoImage: {
          ref: `cardano-node@sha256:${h64("7")}`,
          id: `sha256:${h64("8")}`,
        },
        networkMagic: 424242,
        manifestId: h64("9"),
        registrationTxHash: phasRegistrationTxHash,
        rewardAddress:
          "stake_test17prd7qp8ls90quvhjfxuqlcuy7kxk90t90twl3a88vxmknguu7vsa",
        rewardAddressBase16:
          "f046df0027fc0af07197924dc07f1c27ac6b15eb2bd6efc7a73b0dbb4d",
        scriptHash: "46df0027fc0af07197924dc07f1c27ac6b15eb2bd6efc7a73b0dbb4d",
        transactionBody: {
          schemaVersion: "midgard-phas-registration-transaction-body-v1",
          artifactSha256:
            "5d19fdf1cebce4c95165dbd317ff582e8c01be67a14e4eed2f13ceb1c9ee9610",
          cborSha256: phasRegistrationCborSha256,
          cborSizeBytes: 162,
          cardanoCliTxHash: phasRegistrationTxHash,
          certificate: {
            kind: "stake_registration",
            index: 0,
            count: 1,
            credentialType: "script",
            scriptHash:
              "46df0027fc0af07197924dc07f1c27ac6b15eb2bd6efc7a73b0dbb4d",
          },
        },
        registrationDepositLovelace: 400_000,
        confirmation: { slot: 6400, blockHeaderHash: h64("b") },
        observedAtTip: { slot: 6493, hash: h64("3") },
      },
      snapshotPhasRegistrationTransactionBody: structuredClone(
        phasRegistrationTransactionBody,
      ),
      composeProject,
      networkMagic: 424242,
      postgresDatabase: composeProject,
      postgresPort: 5547,
      ogmiosPort: 2340,
      kupoPort: 2445,
    },
    crashes: PHASE4_PROCESS_CHECKPOINTS.map(crashEvidence),
    t1Recovery: {
      abandonedHeaderHash,
      abandonedSubmittedTxHash: h64("a"),
      abandonedHeaderEndTimeMs: 1_720_000_000_000,
      originalBaseHeaderHash,
      recoveredTipHeaderHash,
      candidateBaseHeaderHash: recoveredTipHeaderHash,
      recovery: {
        schemaVersion: "midgard-phase4-t1-recovery-attestation-v1",
        scenarioLabel: "t1-recovered-tip",
        attemptId: "t1-1720000000000-4242",
        composeProject,
        networkMagic: 424242,
        snapshotSetSha256: h64("5"),
        snapshotIdentitySha256: h64("2"),
        abandonedHeaderHash,
        abandonedSubmittedTxHash: h64("a"),
        baseHeaderHash: originalBaseHeaderHash,
        recoveredTipHeaderHash,
        canonicalAdvanceTxHash: h64("c"),
        journalSha256Before: h64("e"),
        journalSha256After: h64("e"),
        cardanoTip: { slot: 6501, hash: h64("f") },
        kupoCheckpoint: 6501,
      },
      preRecoveryCandidateLine: `pipeline_trace phase=candidate_ready base_header_hash=${abandonedHeaderHash}`,
      replacementCandidateLine: `pipeline_trace phase=candidate_ready base_header_hash=${recoveredTipHeaderHash}`,
      journalByteIdenticalAcrossChainRestore: true,
      abandonedPayloadTxIds: [txOne],
      retainedPayloadTxIds: [txOne, txTwo],
      replacementHeaderHash,
      replacementSubmittedTxHash,
      replacementPayloadTxIds: [txOne, txTwo],
      continuedSpeculation: true,
      restart: supervisor({
        label: "t1-recovered-tip",
        marker: "pipeline_trace phase=candidate_submitted",
        signal: "SIGTERM",
      }),
      state: databaseState({
        headerHash: replacementHeaderHash,
        baseHeaderHash: recoveredTipHeaderHash,
        submittedTxHash: replacementSubmittedTxHash,
        transactionIds: [txOne, txTwo],
        retainedIds: [txOne, txTwo],
      }),
    },
    normalContention: contention("normal"),
    normalContentionState: databaseState(),
    journalKillContention: contention("journal-kill"),
    journalKillContentionState: databaseState({
      recentLeases: [
        {
          holder: "node-a",
          status: "failed",
          lastError: "lease expired before release",
        },
      ],
    }),
  };
};

test("accepts a complete internally consistent process-acceptance fixture", () => {
  const result = evaluatePhase4PipelinedProcessSummary(validSummary());
  assert.deepEqual(result.reasons, []);
  assert.equal(result.passed, true);
  assert.equal(
    result.artifactIdentity.composeProject,
    "midgard_phase4_process_live_20260714t050000z_v19",
  );
});

test("matches the live flag comparison while ignoring generated lease values and history", () => {
  const summary = validSummary();
  assert.notDeepEqual(
    summary.crashes[0].flagOnSubmitted,
    summary.crashes[0].flagOffControl,
  );
  assert.equal(evaluatePhase4PipelinedProcessSummary(summary).passed, true);

  summary.crashes[0].flagOffControl.mempool[0].tx = "01";
  const result = evaluatePhase4PipelinedProcessSummary(summary);
  assert.equal(result.passed, false);
  assert(
    result.reasons.some((reason) => reason.includes("logical database states")),
  );
});

test("fails closed on schema, isolation, crash, T1, and contention mutations", () => {
  const cases = [
    ["extra summary field", (value) => (value.unreviewed = true)],
    ["protected service port", (value) => (value.isolation.ogmiosPort = 1337)],
    [
      "unrelated PHAS registration transaction body",
      (value) => {
        value.isolation.snapshotPhasRegistrationTransactionBody.cborHex =
          value.isolation.snapshotPhasRegistrationTransactionBody.cborHex.replace(
            value.isolation.snapshotPhasRegistration.scriptHash,
            "0".repeat(56),
          );
      },
    ],
    [
      "64-character L2 header",
      (value) => (value.t1Recovery.recoveredTipHeaderHash = h64("c")),
    ],
    [
      "wrong crash signal",
      (value) => {
        value.crashes[0].crash.attempts[0].signal = "SIGTERM";
      },
    ],
    [
      "journal beyond submitted base",
      (value) =>
        (value.crashes[0].afterCrash.activeJournal.headerHash = h56("f")),
    ],
    [
      "T1 journal digest mismatch",
      (value) => (value.t1Recovery.recovery.journalSha256After = h64("f")),
    ],
    [
      "T1 payload loss",
      (value) => (value.t1Recovery.replacementPayloadTxIds = [txTwo]),
    ],
    [
      "T1 replacement based on N instead of F",
      (value) =>
        (value.t1Recovery.candidateBaseHeaderHash =
          value.t1Recovery.abandonedHeaderHash),
    ],
    [
      "normal loser lacks Busy or journal refusal",
      (value) => (value.normalContention.loserLog = "candidate invalidated"),
    ],
    [
      "journal-kill lease-expiry record missing",
      (value) => (value.journalKillContentionState.recentLeases = []),
    ],
    [
      "extra recovery field",
      (value) => (value.t1Recovery.recovery.unreviewed = "field"),
    ],
  ];

  for (const [label, mutate] of cases) {
    const summary = validSummary();
    mutate(summary);
    const result = evaluatePhase4PipelinedProcessSummary(summary);
    assert.equal(result.passed, false, label);
    assert(result.reasons.length > 0, label);
  }
});

test("returns reasons instead of throwing for malformed nested evidence", () => {
  const summary = validSummary();
  summary.runDir = null;
  summary.crashes[0].flagOnSubmitted = null;
  summary.crashes[1].flagOnSubmitted.deposits = [null];
  summary.crashes[1].flagOffControl.deposits = [null];
  summary.t1Recovery.abandonedPayloadTxIds = null;
  summary.t1Recovery.state.mempool = null;
  summary.normalContention.loserLog = null;
  summary.journalKillContentionState.recentLeases = null;
  const result = evaluatePhase4PipelinedProcessSummary(summary);
  assert.equal(result.passed, false);
  assert(result.reasons.length > 0);
});

test("file and package-facing CLI verification report a frozen artifact hash", () => {
  const validPath = join(fixtureDirectory, "valid-summary.json");
  writeFileSync(validPath, `${JSON.stringify(validSummary(), null, 2)}\n`);

  const fileResult = verifyPhase4PipelinedProcessSummaryFile(validPath);
  assert.equal(fileResult.passed, true);
  assert.match(fileResult.summarySha256, /^[a-f0-9]{64}$/u);
  assert.equal(fileResult.summaryPath, validPath);

  const stdout = [];
  const stderr = [];
  const io = {
    log: (value) => stdout.push(value),
    error: (value) => stderr.push(value),
  };
  assert.equal(runPhase4PipelinedProcessSummaryVerifierCli([validPath], io), 0);
  const output = JSON.parse(stdout[0]);
  assert.equal(output.passed, true);
  assert.equal(output.summarySha256, fileResult.summarySha256);
  assert.deepEqual(stderr, []);

  const invalidPath = join(fixtureDirectory, "invalid-summary.json");
  writeFileSync(invalidPath, "{not-json\n");
  stdout.length = 0;
  assert.equal(
    runPhase4PipelinedProcessSummaryVerifierCli([invalidPath], io),
    1,
  );
  assert.equal(JSON.parse(stdout[0]).passed, false);

  assert.equal(runPhase4PipelinedProcessSummaryVerifierCli([], io), 2);
  assert.match(stderr.at(-1), /usage:/u);
  assert.equal(
    runPhase4PipelinedProcessSummaryVerifierCli(
      [join(fixtureDirectory, "missing.json")],
      io,
    ),
    2,
  );
});
