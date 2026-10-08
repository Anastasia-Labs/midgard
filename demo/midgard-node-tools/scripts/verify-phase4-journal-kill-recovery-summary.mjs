#!/usr/bin/env node

import "node:fs";
import "node:path";
import "node:crypto";
import "node:url";
import "node:util";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "./verify-phase4-journal-kill-recovery-summary.validate-cleanup.mjs";
import "./verify-phase4-journal-kill-recovery-summary.validate-phas-registration-transaction-body.mjs";

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { isDeepStrictEqual } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";

import {
  ACTIVE_JOURNAL_STATUSES,
  CARDANO_OUT_REF,
  check,
  exactKeys,
  HASH_32,
  hexBytes,
  ISOLATED_PREFIX,
  JOURNAL_KILL_CHECKPOINT_MARKER,
  JOURNAL_KILL_SURVIVOR_MARKERS,
  isoTimestamp,
  L2_HEADER_HASH,
  LEASE_STATUSES,
  markersAppearInOrder,
  object,
  pathWithin,
  PHASE4_PROCESS_SUMMARY_MODE,
  PHASE4_PROCESS_SUMMARY_SCHEMA,
  requireFileTermination,
  requireOutputTermination,
  safeNonnegative,
  safePositive,
  sha256,
  SUPERVISOR_SCHEMA,
  validateClassification,
  validateCleanup,
  validateJournalMembers,
  validateRoots,
  validateTermination,
} from "./verify-phase4-journal-kill-recovery-summary.validate-cleanup.mjs";
import {
  validateLedgerDelta,
  validatePhasRegistrationTransactionBody,
} from "./verify-phase4-journal-kill-recovery-summary.validate-phas-registration-transaction-body.mjs";

const canonicalPhasIdentity = (() => {
  const blueprint = SDK.parsePhasMembershipBlueprint(
    JSON.parse(
      fs.readFileSync(
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

const TOP_LEVEL_KEYS = [
  "schemaVersion",
  "mode",
  "runDir",
  "isolation",
  "journalKillContention",
  "journalKillContentionState",
];

const REQUIRED_NODE_ENV_KEYS = [
  "NETWORK",
  "L1_OGMIOS_KEY",
  "L1_KUPO_KEY",
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

const validateSupervisor = (reasons, value, runDir, label) => {
  if (
    !exactKeys(
      reasons,
      value,
      [
        "schemaVersion",
        "service",
        "command",
        "status",
        "rawLogPath",
        "attempts",
        "restartCount",
        "terminalClassification",
      ],
      label,
    )
  ) {
    return;
  }
  check(
    reasons,
    value.schemaVersion === SUPERVISOR_SCHEMA,
    `${label} has an unexpected supervisor schema`,
  );
  check(
    reasons,
    typeof value.service === "string" && value.service.length > 0,
    `${label}.service is invalid`,
  );
  check(
    reasons,
    value.status === "restart_budget_exhausted",
    `${label} must end through the expected supervised termination`,
  );
  check(
    reasons,
    typeof value.rawLogPath === "string" &&
      pathWithin(runDir, value.rawLogPath),
    `${label}.rawLogPath must be inside runDir`,
  );
  check(
    reasons,
    value.restartCount === 0,
    `${label}.restartCount must be zero`,
  );

  if (
    exactKeys(
      reasons,
      value.command,
      ["command", "args", "cwd", "envKeys", "envFiles", "envInheritance"],
      `${label}.command`,
    )
  ) {
    check(
      reasons,
      typeof value.command.command === "string" &&
        path.isAbsolute(value.command.command),
      `${label}.command.command must be absolute`,
    );
    check(
      reasons,
      Array.isArray(value.command.args) &&
        value.command.args.length === 2 &&
        path.isAbsolute(value.command.args[0]) &&
        value.command.args[0].endsWith(`${path.sep}dist${path.sep}index.js`) &&
        value.command.args[1] === "listen",
      `${label} must supervise the built Midgard listen command`,
    );
    check(
      reasons,
      typeof value.command.cwd === "string" &&
        path.isAbsolute(value.command.cwd),
      `${label}.command.cwd must be absolute`,
    );
    check(
      reasons,
      value.command.envInheritance === "none",
      `${label} must disable ambient env inheritance`,
    );
    check(
      reasons,
      Array.isArray(value.command.envFiles) &&
        value.command.envFiles.length === 0,
      `${label} may not load mutable child env files`,
    );
    const envKeys = Array.isArray(value.command.envKeys)
      ? value.command.envKeys
      : [];
    check(
      reasons,
      envKeys.every((entry) => typeof entry === "string") &&
        new Set(envKeys).size === envKeys.length,
      `${label}.command.envKeys is invalid`,
    );
    for (const key of REQUIRED_NODE_ENV_KEYS) {
      check(
        reasons,
        envKeys.includes(key),
        `${label}.command.envKeys is missing ${key}`,
      );
    }
  }

  if (!Array.isArray(value.attempts) || value.attempts.length !== 1) {
    reasons.push(`${label} must contain exactly one bounded process attempt`);
    return;
  }
  const attempt = value.attempts[0];
  if (
    !exactKeys(
      reasons,
      attempt,
      [
        "attempt",
        "pid",
        "startedAt",
        "finishedAt",
        "durationMs",
        "exitCode",
        "signal",
        "timedOut",
        "classification",
        "cleanup",
        "outputTermination",
        "fileTermination",
      ],
      `${label}.attempts[0]`,
    )
  ) {
    return;
  }
  check(reasons, attempt.attempt === 1, `${label} attempt number must be one`);
  check(
    reasons,
    safePositive(attempt.pid),
    `${label} attempt must record a positive pid`,
  );
  check(
    reasons,
    isoTimestamp(attempt.startedAt),
    `${label}.startedAt is invalid`,
  );
  check(
    reasons,
    isoTimestamp(attempt.finishedAt),
    `${label}.finishedAt is invalid`,
  );
  check(
    reasons,
    safeNonnegative(attempt.durationMs),
    `${label}.durationMs is invalid`,
  );
  check(reasons, attempt.exitCode === null, `${label}.exitCode must be null`);
  check(reasons, attempt.timedOut === false, `${label} may not time out`);
  validateClassification(
    reasons,
    attempt.classification,
    `${label}.classification`,
  );
  validateClassification(
    reasons,
    value.terminalClassification,
    `${label}.terminalClassification`,
  );
  check(
    reasons,
    isDeepStrictEqual(attempt.classification, value.terminalClassification),
    `${label} terminal classification does not match its attempt`,
  );
  check(
    reasons,
    attempt.classification?.class === "restartable_runtime" &&
      attempt.classification?.restartable === true,
    `${label} expected termination classification is missing`,
  );
  validateCleanup(reasons, attempt.cleanup, `${label}.cleanup`);
  validateTermination(
    reasons,
    attempt.outputTermination,
    `${label}.outputTermination`,
    "output",
  );
  validateTermination(
    reasons,
    attempt.fileTermination,
    `${label}.fileTermination`,
    "file",
  );
};

const ROOT_KEYS = [
  "utxos",
  "forcedTransactions",
  "transactions",
  "deposits",
  "withdrawals",
];

const EXPECTED_ROOT_KEYS = [...ROOT_KEYS, "transitionTrace", "eventToStep"];

const JOURNAL_PAYLOAD_KEYS = [
  "deposits",
  "forcedTransactions",
  "withdrawals",
  "transactions",
  "transitionTrace",
  "eventToStep",
  "ledgerDelta",
];

const validateDatabaseState = (reasons, value, label) => {
  if (
    !exactKeys(
      reasons,
      value,
      [
        "activeJournalCount",
        "activeJournal",
        "activeLease",
        "recentLeases",
        "deposits",
        "mempool",
        "processed",
      ],
      label,
    )
  ) {
    return;
  }
  check(
    reasons,
    safeNonnegative(value.activeJournalCount),
    `${label}.activeJournalCount is invalid`,
  );
  if (value.activeJournalCount === 0) {
    check(
      reasons,
      value.activeJournal === null,
      `${label} has an uncounted journal`,
    );
  } else if (value.activeJournalCount === 1) {
    check(
      reasons,
      object(value.activeJournal),
      `${label} is missing its journal`,
    );
  }

  if (value.activeJournal !== null) {
    const journal = value.activeJournal;
    if (
      exactKeys(
        reasons,
        journal,
        [
          "headerHash",
          "headerCbor",
          "journalPayloadIdentity",
          "submittedTxHash",
          "status",
          "baseTailHeaderHash",
          "baseTailOutRef",
          "baseTailDatumCbor",
          "baseRoots",
          "expectedRoots",
          "mpfReplay",
          "leaseToken",
          "depositCount",
          "mempoolTxCount",
        ],
        `${label}.activeJournal`,
      )
    ) {
      check(
        reasons,
        L2_HEADER_HASH.test(journal.headerHash),
        `${label}.activeJournal.headerHash must be 56 lowercase hex`,
      );
      check(
        reasons,
        hexBytes(journal.headerCbor),
        `${label}.activeJournal.headerCbor is invalid`,
      );
      check(
        reasons,
        journal.submittedTxHash === null ||
          HASH_32.test(journal.submittedTxHash),
        `${label}.activeJournal.submittedTxHash must be null or 64 lowercase hex`,
      );
      check(
        reasons,
        ACTIVE_JOURNAL_STATUSES.has(journal.status),
        `${label}.activeJournal.status is not active`,
      );
      check(
        reasons,
        journal.baseTailHeaderHash === null ||
          L2_HEADER_HASH.test(journal.baseTailHeaderHash),
        `${label}.activeJournal.baseTailHeaderHash is invalid`,
      );
      check(
        reasons,
        journal.baseTailOutRef === null ||
          CARDANO_OUT_REF.test(journal.baseTailOutRef),
        `${label}.activeJournal.baseTailOutRef is invalid`,
      );
      check(
        reasons,
        journal.baseTailDatumCbor === null ||
          hexBytes(journal.baseTailDatumCbor),
        `${label}.activeJournal.baseTailDatumCbor is invalid`,
      );
      check(
        reasons,
        typeof journal.leaseToken === "string" && journal.leaseToken.length > 0,
        `${label}.activeJournal.leaseToken is missing`,
      );
      check(
        reasons,
        safeNonnegative(journal.depositCount) &&
          safeNonnegative(journal.mempoolTxCount),
        `${label}.activeJournal counts are invalid`,
      );
      validateRoots(
        reasons,
        journal.baseRoots,
        ROOT_KEYS,
        `${label}.baseRoots`,
      );
      validateRoots(
        reasons,
        journal.expectedRoots,
        EXPECTED_ROOT_KEYS,
        `${label}.expectedRoots`,
      );
      if (
        exactKeys(
          reasons,
          journal.journalPayloadIdentity,
          JOURNAL_PAYLOAD_KEYS,
          `${label}.journalPayloadIdentity`,
        )
      ) {
        for (const key of JOURNAL_PAYLOAD_KEYS.slice(0, -1)) {
          validateJournalMembers(
            reasons,
            journal.journalPayloadIdentity[key],
            `${label}.journalPayloadIdentity.${key}`,
          );
        }
        validateLedgerDelta(
          reasons,
          journal.journalPayloadIdentity.ledgerDelta,
          `${label}.journalPayloadIdentity.ledgerDelta`,
        );
      }
      if (
        exactKeys(
          reasons,
          journal.mpfReplay,
          [
            "baseRoot",
            "candidateRoot",
            "eventLogDigest",
            "eventRoots",
            "eventCount",
          ],
          `${label}.mpfReplay`,
        )
      ) {
        for (const key of [
          "baseRoot",
          "candidateRoot",
          "eventLogDigest",
          "eventRoots",
        ]) {
          check(
            reasons,
            journal.mpfReplay[key] === null ||
              (typeof journal.mpfReplay[key] === "string" &&
                journal.mpfReplay[key].length > 0),
            `${label}.mpfReplay.${key} is invalid`,
          );
        }
        check(
          reasons,
          journal.mpfReplay.eventCount === null ||
            safeNonnegative(journal.mpfReplay.eventCount),
          `${label}.mpfReplay.eventCount is invalid`,
        );
      }
    }
  }

  if (value.activeLease !== null) {
    if (
      exactKeys(
        reasons,
        value.activeLease,
        ["holder", "token", "status"],
        `${label}.activeLease`,
      )
    ) {
      check(
        reasons,
        typeof value.activeLease.holder === "string" &&
          value.activeLease.holder.length > 0 &&
          typeof value.activeLease.token === "string" &&
          value.activeLease.token.length > 0 &&
          LEASE_STATUSES.has(value.activeLease.status),
        `${label}.activeLease is invalid`,
      );
    }
  }

  const arrays = ["recentLeases", "deposits", "mempool", "processed"];
  for (const key of arrays) {
    check(
      reasons,
      Array.isArray(value[key]),
      `${label}.${key} must be an array`,
    );
  }
  if (Array.isArray(value.recentLeases)) {
    value.recentLeases.forEach((lease, index) => {
      const itemLabel = `${label}.recentLeases[${index.toString()}]`;
      if (
        !exactKeys(reasons, lease, ["holder", "status", "lastError"], itemLabel)
      ) {
        return;
      }
      check(
        reasons,
        typeof lease.holder === "string" &&
          lease.holder.length > 0 &&
          LEASE_STATUSES.has(lease.status) &&
          (lease.lastError === null || typeof lease.lastError === "string"),
        `${itemLabel} is invalid`,
      );
    });
  }
  if (Array.isArray(value.deposits)) {
    value.deposits.forEach((deposit, index) => {
      const itemLabel = `${label}.deposits[${index.toString()}]`;
      if (
        !exactKeys(
          reasons,
          deposit,
          ["id", "status", "projectedHeaderHash"],
          itemLabel,
        )
      ) {
        return;
      }
      check(
        reasons,
        typeof deposit.id === "string" &&
          deposit.id.length > 0 &&
          typeof deposit.status === "string" &&
          deposit.status.length > 0 &&
          (deposit.projectedHeaderHash === null ||
            L2_HEADER_HASH.test(deposit.projectedHeaderHash)),
        `${itemLabel} is invalid`,
      );
    });
  }
  for (const key of ["mempool", "processed"]) {
    if (!Array.isArray(value[key])) continue;
    value[key].forEach((transaction, index) => {
      const itemLabel = `${label}.${key}[${index.toString()}]`;
      if (!exactKeys(reasons, transaction, ["txId", "tx"], itemLabel)) return;
      check(
        reasons,
        HASH_32.test(transaction.txId) && hexBytes(transaction.tx),
        `${itemLabel} has an invalid transaction identity or CBOR`,
      );
    });
  }
};

const validatePhasRegistrationProof = (reasons, proof, isolation) => {
  const keys = [
    "schemaVersion",
    "source",
    "readOnly",
    "registered",
    "cardanoImage",
    "networkMagic",
    "manifestId",
    "registrationTxHash",
    "rewardAddress",
    "rewardAddressBase16",
    "scriptHash",
    "transactionBody",
    "registrationDepositLovelace",
    "confirmation",
    "observedAtTip",
  ];
  if (!exactKeys(reasons, proof, keys, "isolation.snapshotPhasRegistration"))
    return;
  check(
    reasons,
    proof.schemaVersion === "midgard-phase4-phas-registration-proof-v1" &&
      proof.source === "cardano-cli-local-state-query" &&
      proof.readOnly === true &&
      proof.registered === true,
    "isolation PHAS proof is not an exact read-only registration proof",
  );
  check(
    reasons,
    HASH_32.test(proof.manifestId) &&
      HASH_32.test(proof.registrationTxHash) &&
      /^[a-f0-9]{56}$/u.test(proof.scriptHash) &&
      /^stake_test1[0-9a-z]+$/u.test(proof.rewardAddress) &&
      proof.rewardAddressBase16 === `f0${proof.scriptHash}` &&
      proof.rewardAddress === canonicalPhasIdentity.rewardAddress &&
      proof.scriptHash === canonicalPhasIdentity.scriptHash &&
      safePositive(proof.registrationDepositLovelace),
    "isolation PHAS proof identity is invalid",
  );
  try {
    const details = getAddressDetails(proof.rewardAddress);
    check(
      reasons,
      details.type === "Reward" &&
        details.networkId === 0 &&
        details.address.hex === proof.rewardAddressBase16 &&
        details.stakeCredential?.type === "Script" &&
        details.stakeCredential.hash === proof.scriptHash,
      "isolation PHAS reward account is not the exact testnet script credential",
    );
  } catch {
    reasons.push("isolation PHAS reward account is not valid canonical bech32");
  }
  const transactionBody = proof.transactionBody;
  if (
    exactKeys(
      reasons,
      transactionBody,
      [
        "schemaVersion",
        "artifactSha256",
        "cborSha256",
        "cborSizeBytes",
        "cardanoCliTxHash",
        "certificate",
      ],
      "isolation.snapshotPhasRegistration.transactionBody",
    )
  ) {
    check(
      reasons,
      transactionBody.schemaVersion ===
        "midgard-phas-registration-transaction-body-v1" &&
        HASH_32.test(transactionBody.artifactSha256) &&
        HASH_32.test(transactionBody.cborSha256) &&
        safePositive(transactionBody.cborSizeBytes) &&
        transactionBody.cardanoCliTxHash === proof.registrationTxHash,
      "isolation PHAS transaction body identity is invalid",
    );
    if (
      exactKeys(
        reasons,
        transactionBody.certificate,
        ["kind", "index", "count", "credentialType", "scriptHash"],
        "isolation.snapshotPhasRegistration.transactionBody.certificate",
      )
    ) {
      check(
        reasons,
        transactionBody.certificate.kind === "stake_registration" &&
          transactionBody.certificate.index === 0 &&
          transactionBody.certificate.count === 1 &&
          transactionBody.certificate.credentialType === "script" &&
          transactionBody.certificate.scriptHash === proof.scriptHash,
        "isolation PHAS transaction body certificate is not exact",
      );
    }
  }
  if (
    exactKeys(
      reasons,
      proof.cardanoImage,
      ["ref", "id"],
      "isolation.snapshotPhasRegistration.cardanoImage",
    )
  ) {
    check(
      reasons,
      /@sha256:[a-f0-9]{64}$/u.test(proof.cardanoImage.ref) &&
        typeof proof.cardanoImage.id === "string" &&
        /^sha256:[a-f0-9]{64}$/u.test(proof.cardanoImage.id),
      "isolation PHAS proof Cardano image identity is invalid",
    );
  }
  for (const [name, point, hashKey] of [
    ["confirmation", proof.confirmation, "blockHeaderHash"],
    ["observedAtTip", proof.observedAtTip, "hash"],
  ]) {
    if (
      exactKeys(
        reasons,
        point,
        ["slot", hashKey],
        `isolation.snapshotPhasRegistration.${name}`,
      )
    ) {
      check(
        reasons,
        safeNonnegative(point.slot) && HASH_32.test(point[hashKey]),
        `isolation PHAS proof ${name} is invalid`,
      );
    }
  }
  check(
    reasons,
    proof.networkMagic === isolation.networkMagic &&
      proof.observedAtTip?.slot === isolation.snapshotCardanoTip?.slot &&
      proof.observedAtTip?.hash === isolation.snapshotCardanoTip?.hash &&
      proof.confirmation?.slot <= proof.observedAtTip?.slot,
    "isolation PHAS proof is not bound to the frozen ledger",
  );
};

const validateIsolation = (reasons, isolation) => {
  const keys = [
    "envFile",
    "deploymentManifestPath",
    "deploymentManifestSha256",
    "snapshotIdentityPath",
    "snapshotIdentitySha256",
    "snapshotCardanoTip",
    "snapshotKupoCheckpoint",
    "snapshotBlueprintSha256",
    "snapshotPhasRegistrationProofSha256",
    "snapshotPhasRegistration",
    "snapshotPhasRegistrationTransactionBody",
    "composeProject",
    "networkMagic",
    "postgresDatabase",
    "postgresPort",
    "ogmiosPort",
    "kupoPort",
  ];
  if (!exactKeys(reasons, isolation, keys, "isolation")) return;
  for (const key of [
    "envFile",
    "deploymentManifestPath",
    "snapshotIdentityPath",
  ]) {
    check(
      reasons,
      typeof isolation[key] === "string" && path.isAbsolute(isolation[key]),
      `isolation.${key} must be absolute`,
    );
  }
  for (const key of [
    "deploymentManifestSha256",
    "snapshotIdentitySha256",
    "snapshotBlueprintSha256",
    "snapshotPhasRegistrationProofSha256",
  ]) {
    check(reasons, HASH_32.test(isolation[key]), `isolation.${key} is invalid`);
  }
  check(
    reasons,
    typeof isolation.composeProject === "string" &&
      isolation.composeProject.startsWith(ISOLATED_PREFIX) &&
      /^[a-z0-9_-]+$/u.test(isolation.composeProject),
    "isolation.composeProject is not an isolated safe project",
  );
  check(
    reasons,
    typeof isolation.postgresDatabase === "string" &&
      isolation.postgresDatabase.startsWith(ISOLATED_PREFIX),
    "isolation.postgresDatabase is not isolated",
  );
  check(
    reasons,
    safePositive(isolation.networkMagic),
    "isolation.networkMagic is invalid",
  );
  check(
    reasons,
    safePositive(isolation.postgresPort) &&
      ![5432, 5433].includes(isolation.postgresPort),
    "isolation.postgresPort is invalid or protected",
  );
  check(
    reasons,
    safePositive(isolation.ogmiosPort) && isolation.ogmiosPort !== 1337,
    "isolation.ogmiosPort is invalid or protected",
  );
  check(
    reasons,
    safePositive(isolation.kupoPort) && isolation.kupoPort !== 1442,
    "isolation.kupoPort is invalid or protected",
  );
  check(
    reasons,
    new Set([isolation.postgresPort, isolation.ogmiosPort, isolation.kupoPort])
      .size === 3,
    "isolation service ports must be distinct",
  );
  if (
    exactKeys(
      reasons,
      isolation.snapshotCardanoTip,
      ["slot", "hash"],
      "isolation.snapshotCardanoTip",
    )
  ) {
    check(
      reasons,
      safeNonnegative(isolation.snapshotCardanoTip.slot) &&
        HASH_32.test(isolation.snapshotCardanoTip.hash),
      "isolation.snapshotCardanoTip is invalid",
    );
    check(
      reasons,
      isolation.snapshotKupoCheckpoint === isolation.snapshotCardanoTip.slot,
      "isolation snapshot Cardano and Kupo checkpoints differ",
    );
  }
  validatePhasRegistrationProof(
    reasons,
    isolation.snapshotPhasRegistration,
    isolation,
  );
  validatePhasRegistrationTransactionBody(
    reasons,
    isolation.snapshotPhasRegistrationTransactionBody,
    isolation.snapshotPhasRegistration,
  );
};

const validateJournalKillContention = (reasons, result, runDir, label) => {
  if (
    !exactKeys(
      reasons,
      result,
      [
        "winnerNodeId",
        "loserNodeId",
        "winner",
        "loser",
        "winnerLog",
        "loserLog",
      ],
      label,
    )
  ) {
    return;
  }
  check(
    reasons,
    ["node-a", "node-b"].includes(result.winnerNodeId) &&
      ["node-a", "node-b"].includes(result.loserNodeId) &&
      result.winnerNodeId !== result.loserNodeId,
    `${label} winner/loser identities are invalid`,
  );
  validateSupervisor(reasons, result.winner, runDir, `${label}.winner`);
  validateSupervisor(reasons, result.loser, runDir, `${label}.loser`);
  check(
    reasons,
    result.winner?.service?.includes(`:${result.winnerNodeId}:`) &&
      result.loser?.service?.includes(`:${result.loserNodeId}:`),
    `${label} supervisor identities do not match winner/loser IDs`,
  );
  check(
    reasons,
    typeof result.winnerLog === "string" && typeof result.loserLog === "string",
    `${label} logs are missing`,
  );
  const loserLog = typeof result.loserLog === "string" ? result.loserLog : "";
  requireOutputTermination(
    reasons,
    result.winner,
    JOURNAL_KILL_CHECKPOINT_MARKER,
    "SIGKILL",
    `${label}.winner`,
  );
  requireFileTermination(reasons, result.loser, "SIGTERM", `${label}.loser`);
  check(
    reasons,
    markersAppearInOrder(loserLog, JOURNAL_KILL_SURVIVOR_MARKERS),
    `${label}.loser lacks ordered lease-busy, unsubmitted-journal recovery and survivor-submission evidence`,
  );
};

export const evaluatePhase4JournalKillRecoverySummary = (summary) => {
  const reasons = [];
  if (!exactKeys(reasons, summary, TOP_LEVEL_KEYS, "summary")) {
    return { passed: false, reasons, artifactIdentity: null };
  }
  check(
    reasons,
    summary.schemaVersion === PHASE4_PROCESS_SUMMARY_SCHEMA,
    `summary.schemaVersion must be ${PHASE4_PROCESS_SUMMARY_SCHEMA}`,
  );
  check(
    reasons,
    summary.mode === PHASE4_PROCESS_SUMMARY_MODE,
    `summary.mode must be ${PHASE4_PROCESS_SUMMARY_MODE}`,
  );
  check(
    reasons,
    typeof summary.runDir === "string" && path.isAbsolute(summary.runDir),
    "summary.runDir must be absolute",
  );
  validateIsolation(reasons, summary.isolation);
  validateJournalKillContention(
    reasons,
    summary.journalKillContention,
    summary.runDir,
    "journalKillContention",
  );
  validateDatabaseState(
    reasons,
    summary.journalKillContentionState,
    "journalKillContentionState",
  );
  check(
    reasons,
    summary.journalKillContentionState?.activeJournalCount === 1,
    "journal-kill recovery must leave exactly one survivor journal",
  );
  check(
    reasons,
    Array.isArray(summary.journalKillContentionState?.recentLeases) &&
      summary.journalKillContentionState.recentLeases.some(
        (lease) =>
          lease.status === "failed" &&
          lease.lastError === "lease expired before release",
      ),
    "journal-kill recovery lacks the expired winner-lease record",
  );

  return {
    passed: reasons.length === 0,
    reasons,
    artifactIdentity: {
      schemaVersion: summary.schemaVersion,
      runDir: summary.runDir,
      composeProject: summary.isolation?.composeProject ?? null,
      postgresDatabase: summary.isolation?.postgresDatabase ?? null,
      snapshotIdentitySha256: summary.isolation?.snapshotIdentitySha256 ?? null,
      phasRegistrationProofSha256:
        summary.isolation?.snapshotPhasRegistrationProofSha256 ?? null,
    },
  };
};

export const decodePhase4JournalKillRecoverySummaryV1 = (value) => {
  const evaluation = evaluatePhase4JournalKillRecoverySummary(value);
  if (!evaluation.passed) {
    throw new Error(
      `Phase 4 journal-kill recovery summary is not exact canonical V1: ${evaluation.reasons.join("; ")}`,
    );
  }
  return value;
};

export const verifyPhase4JournalKillRecoverySummaryFile = (summaryPath) => {
  const bytes = fs.readFileSync(summaryPath);
  let summary;
  try {
    summary = JSON.parse(bytes.toString("utf8"));
  } catch (error) {
    return {
      passed: false,
      reasons: [`summary is not valid JSON: ${String(error)}`],
      artifactIdentity: null,
      summaryPath: path.resolve(summaryPath),
      summarySha256: sha256(bytes),
    };
  }
  const evaluation = evaluatePhase4JournalKillRecoverySummary(summary);
  if (evaluation.passed) decodePhase4JournalKillRecoverySummaryV1(summary);
  return {
    ...evaluation,
    summaryPath: path.resolve(summaryPath),
    summarySha256: sha256(bytes),
  };
};

export const runPhase4JournalKillRecoverySummaryVerifierCli = (
  args,
  io = console,
) => {
  const [summaryPath, extra] = args;
  if (summaryPath === undefined || extra !== undefined) {
    io.error(
      "usage: verify-phase4-journal-kill-recovery-summary.mjs <summary.json>",
    );
    return 2;
  }
  try {
    const result = verifyPhase4JournalKillRecoverySummaryFile(summaryPath);
    io.log(JSON.stringify(result, null, 2));
    return result.passed ? 0 : 1;
  } catch (error) {
    io.error(String(error));
    return 2;
  }
};

const isMain = process.argv[1] === fileURLToPath(import.meta.url);

if (isMain) {
  process.exitCode = runPhase4JournalKillRecoverySummaryVerifierCli(
    process.argv.slice(2),
  );
}
export {
  JOURNAL_KILL_CHECKPOINT_MARKER,
  JOURNAL_KILL_SURVIVOR_MARKERS,
  markersAppearInOrder,
  PHASE4_PROCESS_SUMMARY_MODE,
  PHASE4_PROCESS_SUMMARY_SCHEMA,
} from "./verify-phase4-journal-kill-recovery-summary.validate-cleanup.mjs";
