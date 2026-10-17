import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { after, test } from "node:test";

import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import {
  decodePhase4JournalKillRecoverySummaryV1,
  evaluatePhase4JournalKillRecoverySummary,
  JOURNAL_KILL_CHECKPOINT_MARKER,
  JOURNAL_KILL_SURVIVOR_MARKERS,
  markersAppearInOrder,
  PHASE4_PROCESS_SUMMARY_MODE,
  PHASE4_PROCESS_SUMMARY_SCHEMA,
  runPhase4JournalKillRecoverySummaryVerifierCli,
  verifyPhase4JournalKillRecoverySummaryFile,
} from "../scripts/verify-phase4-journal-kill-recovery-summary.mjs";

const fixtureDirectory = mkdtempSync(
  join(
    process.platform === "win32" ? tmpdir() : "/tmp",
    "midgard-phase4-journal-kill-verifier-",
  ),
);
after(() => rmSync(fixtureDirectory, { recursive: true, force: true }));

const runDir = "/evidence/phase4/acceptance";
const h56 = (digit) => digit.repeat(56);
const h64 = (digit) => digit.repeat(64);
const txOne = h64("1");
const canonicalPhasIdentity = (() => {
  const blueprint = SDK.parsePhasMembershipBlueprint(
    JSON.parse(
      readFileSync(
        new URL("../../../onchain/aiken/plutus.json", import.meta.url),
        "utf8",
      ),
    ),
  );
  return SDK.phasMembershipIdentity(
    "Custom",
    SDK.phasMembershipWithdrawalScriptFromBlueprint(blueprint),
  );
})();
const cborTemplateScriptHash =
  "46df0027fc0af07197924dc07f1c27ac6b15eb2bd6efc7a73b0dbb4d";
const phasRegistrationTransactionBody = {
  type: "Unwitnessed Tx ConwayEra",
  description: "PHAS registration transaction body",
  cborHex:
    "84a400d901028182582000000000000000000000000000000000000000000000000000000000000000000001818258390056256482f4e32203bbf0e61f5c0208f776216707b8c1a198e945149ee41bf07d00d3b340e0ba35ee9c82110e6190de18b6d730577223e6c51b00000006fc0299cb021a00028db504d901028182008201581c46df0027fc0af07197924dc07f1c27ac6b15eb2bd6efc7a73b0dbb4da0f5f6".replace(
      cborTemplateScriptHash,
      canonicalPhasIdentity.scriptHash,
    ),
};
const phasRegistrationTransaction = CML.Transaction.from_cbor_hex(
  phasRegistrationTransactionBody.cborHex,
);
const phasRegistrationTxHash = CML.hash_transaction(
  phasRegistrationTransaction.body(),
).to_hex();
const phasRegistrationCborSha256 = createHash("sha256")
  .update(Buffer.from(phasRegistrationTransactionBody.cborHex, "hex"))
  .digest("hex");

const requiredNodeEnvKeys = [
  "NETWORK",
  "POSTGRES_HOST",
  "POSTGRES_PORT",
  "POSTGRES_DB",
  "PORT",
  "PROM_METRICS_PORT",
  "RUN_GENESIS_ON_STARTUP",
  "MIDGARD_DEPLOYMENT_MANIFEST_PATH",
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
  memberId: sourceId,
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
      ledgerDelta: { spent: [], produced: [] },
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

const journalKillContention = () => ({
  winnerNodeId: "node-a",
  loserNodeId: "node-b",
  winner: supervisor({
    nodeId: "node-a",
    label: "journal-kill-contention",
    marker: JOURNAL_KILL_CHECKPOINT_MARKER,
    signal: "SIGKILL",
  }),
  loser: supervisor({
    nodeId: "node-b",
    label: "journal-kill-contention",
    signal: "SIGTERM",
    stopFile: true,
  }),
  winnerLog: JOURNAL_KILL_CHECKPOINT_MARKER,
  loserLog: JOURNAL_KILL_SURVIVOR_MARKERS.join("\n"),
});

const validSummary = () => {
  const composeProject = "midgard_phase4_process_live_20260714t050000z_v19";
  return {
    schemaVersion: PHASE4_PROCESS_SUMMARY_SCHEMA,
    mode: PHASE4_PROCESS_SUMMARY_MODE,
    runDir,
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
        rewardAddress: canonicalPhasIdentity.rewardAddress,
        rewardAddressBase16: `f0${canonicalPhasIdentity.scriptHash}`,
        scriptHash: canonicalPhasIdentity.scriptHash,
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
            scriptHash: canonicalPhasIdentity.scriptHash,
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
    journalKillContention: journalKillContention(),
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

test("accepts a complete internally consistent journal-kill recovery fixture", () => {
  assert.deepEqual(
    decodePhase4JournalKillRecoverySummaryV1(validSummary()),
    validSummary(),
  );
  const result = evaluatePhase4JournalKillRecoverySummary(validSummary());
  assert.deepEqual(result.reasons, []);
  assert.equal(result.passed, true);
  assert.equal(
    result.artifactIdentity.composeProject,
    "midgard_phase4_process_live_20260714t050000z_v19",
  );
});

test("requires the survivor markers in lease-busy, recovery, submission order", () => {
  const ordered = JOURNAL_KILL_SURVIVOR_MARKERS.join("\n");
  assert.equal(
    markersAppearInOrder(ordered, JOURNAL_KILL_SURVIVOR_MARKERS),
    true,
  );
  const reversed = [...JOURNAL_KILL_SURVIVOR_MARKERS].reverse().join("\n");
  assert.equal(
    markersAppearInOrder(reversed, JOURNAL_KILL_SURVIVOR_MARKERS),
    false,
  );

  const summary = validSummary();
  summary.journalKillContention.loserLog = reversed;
  const result = evaluatePhase4JournalKillRecoverySummary(summary);
  assert.equal(result.passed, false);
  assert(
    result.reasons.some((reason) =>
      reason.includes("journalKillContention.loser lacks ordered"),
    ),
  );
});

test("fails closed on schema, isolation, and journal-kill mutations", () => {
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
      (value) =>
        (value.journalKillContentionState.activeJournal.headerHash = h64("c")),
    ],
    [
      "winner not SIGKILLed at the journal checkpoint",
      (value) => {
        value.journalKillContention.winner.attempts[0].signal = "SIGTERM";
      },
    ],
    [
      "winner killed at a different marker",
      (value) => {
        value.journalKillContention.winner.attempts[0].outputTermination.marker =
          "pipeline_trace phase=e2e_crash_checkpoint checkpoint=some_other_checkpoint";
      },
    ],
    [
      "survivor never recovered the unsubmitted journal",
      (value) =>
        (value.journalKillContention.loserLog = [
          JOURNAL_KILL_SURVIVOR_MARKERS[0],
          JOURNAL_KILL_SURVIVOR_MARKERS[2],
        ].join("\n")),
    ],
    [
      "survivor never submitted",
      (value) =>
        (value.journalKillContention.loserLog =
          JOURNAL_KILL_SURVIVOR_MARKERS.slice(0, 2).join("\n")),
    ],
    [
      "survivor stopped by output instead of the stop file",
      (value) => {
        value.journalKillContention.loser.attempts[0].fileTermination = null;
      },
    ],
    [
      "winner and loser are the same node",
      (value) => (value.journalKillContention.loserNodeId = "node-a"),
    ],
    [
      "more than one survivor journal",
      (value) => (value.journalKillContentionState.activeJournalCount = 2),
    ],
    [
      "journal-kill lease-expiry record missing",
      (value) => (value.journalKillContentionState.recentLeases = []),
    ],
    [
      "extra journal-member field",
      (value) =>
        (value.journalKillContentionState.activeJournal.journalPayloadIdentity.transactions[0].unreviewed = true),
    ],
    [
      "obsolete cross-family journal payload",
      (value) => {
        const payload =
          value.journalKillContentionState.activeJournal.journalPayloadIdentity;
        payload.utxos = [];
        delete payload.ledgerDelta;
      },
    ],
    [
      "extra ledger-delta output field",
      (value) => {
        value.journalKillContentionState.activeJournal.journalPayloadIdentity.ledgerDelta.produced =
          [{ outref: "00", output: "00", unreviewed: true }];
      },
    ],
    [
      "noncanonical supervisor timestamp",
      (value) =>
        (value.journalKillContention.winner.attempts[0].startedAt =
          "2026-07-14T05:00:00Z"),
    ],
    [
      "wrong PHAS family schema",
      (value) =>
        (value.isolation.snapshotPhasRegistration.schemaVersion =
          "midgard-phase4-unrelated-proof-v1"),
    ],
  ];

  for (const [label, mutate] of cases) {
    const summary = validSummary();
    mutate(summary);
    const result = evaluatePhase4JournalKillRecoverySummary(summary);
    assert.equal(result.passed, false, label);
    assert(result.reasons.length > 0, label);
    assert.throws(
      () => decodePhase4JournalKillRecoverySummaryV1(summary),
      /not exact canonical V1/u,
      label,
    );
  }
});

test("returns reasons instead of throwing for malformed nested evidence", () => {
  const summary = validSummary();
  summary.runDir = null;
  summary.journalKillContention.winner = null;
  summary.journalKillContention.loserLog = null;
  summary.journalKillContentionState.deposits = [null];
  summary.journalKillContentionState.mempool = null;
  summary.journalKillContentionState.recentLeases = null;
  const result = evaluatePhase4JournalKillRecoverySummary(summary);
  assert.equal(result.passed, false);
  assert(result.reasons.length > 0);
});

test("file and package-facing CLI verification report a frozen artifact hash", () => {
  const validPath = join(fixtureDirectory, "valid-summary.json");
  writeFileSync(validPath, `${JSON.stringify(validSummary(), null, 2)}\n`);

  const fileResult = verifyPhase4JournalKillRecoverySummaryFile(validPath);
  assert.equal(fileResult.passed, true);
  assert.match(fileResult.summarySha256, /^[a-f0-9]{64}$/u);
  assert.equal(fileResult.summaryPath, validPath);

  const stdout = [];
  const stderr = [];
  const io = {
    log: (value) => stdout.push(value),
    error: (value) => stderr.push(value),
  };
  assert.equal(
    runPhase4JournalKillRecoverySummaryVerifierCli([validPath], io),
    0,
  );
  const output = JSON.parse(stdout[0]);
  assert.equal(output.passed, true);
  assert.equal(output.summarySha256, fileResult.summarySha256);
  assert.deepEqual(stderr, []);

  const invalidPath = join(fixtureDirectory, "invalid-summary.json");
  writeFileSync(invalidPath, "{not-json\n");
  stdout.length = 0;
  assert.equal(
    runPhase4JournalKillRecoverySummaryVerifierCli([invalidPath], io),
    1,
  );
  assert.equal(JSON.parse(stdout[0]).passed, false);

  assert.equal(runPhase4JournalKillRecoverySummaryVerifierCli([], io), 2);
  assert.match(stderr.at(-1), /usage:/u);
  assert.equal(
    runPhase4JournalKillRecoverySummaryVerifierCli(
      [join(fixtureDirectory, "missing.json")],
      io,
    ),
    2,
  );
});
