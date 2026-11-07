import "./artifacts.parse-stress-wallet-fanout-artifact.js";

import {
  artifactDecimal,
  artifactExactString,
  artifactInteger,
  artifactIsoTimestamp,
  asObject,
  assertExactKeys,
  parseExactVersionedArtifact,
} from "./artifact-fields.js";
import {
  STRESS_WALLET_CONSOLIDATION_READINESS_SCHEMA_VERSION,
  STRESS_WALLET_TERMINAL_DRAIN_RESULT_SCHEMA_VERSION,
} from "./constants.js";
import {
  type ConsolidationReadinessSnapshot,
  isFullConsolidationReadiness,
} from "./readiness.js";

export const parseStressWalletConsolidationReadinessEvidence = (
  value: unknown,
): Record<string, unknown> => {
  const raw = asObject(value, "stress wallet consolidation readiness evidence");
  if (
    raw.schemaVersion !== STRESS_WALLET_CONSOLIDATION_READINESS_SCHEMA_VERSION
  ) {
    throw new Error(
      `stress wallet consolidation readiness evidence schemaVersion must be exactly ${STRESS_WALLET_CONSOLIDATION_READINESS_SCHEMA_VERSION}.`,
    );
  }
  if (raw.malformed === true) {
    assertExactKeys(raw, "stress wallet consolidation readiness evidence", [
      "schemaVersion",
      "observedAt",
      "batchIndex",
      "firstWalletId",
      "attempt",
      "malformed",
      "error",
      "response",
    ]);
    artifactIsoTimestamp(
      raw.observedAt,
      "stress wallet consolidation readiness evidence observedAt",
    );
    artifactInteger(
      raw.batchIndex,
      "stress wallet consolidation readiness evidence batchIndex",
    );
    artifactExactString(
      raw.firstWalletId,
      "stress wallet consolidation readiness evidence firstWalletId",
    );
    artifactInteger(
      raw.attempt,
      "stress wallet consolidation readiness evidence attempt",
    );
    artifactExactString(
      raw.error,
      "stress wallet consolidation readiness evidence error",
    );
    try {
      JSON.stringify(raw.response);
    } catch {
      throw new Error(
        "stress wallet consolidation readiness evidence response must be JSON-safe.",
      );
    }
  } else {
    assertExactKeys(raw, "stress wallet consolidation readiness evidence", [
      "schemaVersion",
      "observedAt",
      "batchIndex",
      "firstWalletId",
      "attempt",
      "fullReady",
      "snapshot",
    ]);
    const snapshot = asObject(
      raw.snapshot,
      "stress wallet consolidation readiness evidence snapshot",
    );
    assertExactKeys(
      snapshot,
      "stress wallet consolidation readiness evidence snapshot",
      [
        "httpStatus",
        "ready",
        "reasons",
        "durableAdmissionBacklog",
        "mempoolTxCount",
        "unfinishedLocalMutationJobs",
        "unresolvedBlockSubmissionAgeMs",
        "providerQueryHealthy",
        "leaseStatus",
        "pendingFinalizationCount",
        "commitWorkerActive",
        "commitPipelinePhase",
      ],
    );
    artifactIsoTimestamp(
      raw.observedAt,
      "stress wallet consolidation readiness evidence observedAt",
    );
    artifactInteger(
      raw.batchIndex,
      "stress wallet consolidation readiness evidence batchIndex",
    );
    artifactExactString(
      raw.firstWalletId,
      "stress wallet consolidation readiness evidence firstWalletId",
    );
    artifactInteger(
      raw.attempt,
      "stress wallet consolidation readiness evidence attempt",
    );
    if (typeof raw.fullReady !== "boolean") {
      throw new Error(
        "stress wallet consolidation readiness evidence fullReady must be boolean.",
      );
    }
    const httpStatus = artifactInteger(
      snapshot.httpStatus,
      "stress wallet consolidation readiness evidence snapshot.httpStatus",
      100,
    );
    if (httpStatus > 599 || typeof snapshot.ready !== "boolean") {
      throw new Error(
        "stress wallet consolidation readiness evidence snapshot status/readiness is invalid.",
      );
    }
    if (
      !Array.isArray(snapshot.reasons) ||
      snapshot.reasons.some(
        (reason, index) =>
          artifactExactString(
            reason,
            `stress wallet consolidation readiness evidence snapshot.reasons[${index.toString()}]`,
          ) === "",
      )
    ) {
      throw new Error(
        "stress wallet consolidation readiness evidence snapshot.reasons must be an exact string array.",
      );
    }
    [
      "durableAdmissionBacklog",
      "mempoolTxCount",
      "unfinishedLocalMutationJobs",
      "unresolvedBlockSubmissionAgeMs",
      "pendingFinalizationCount",
    ].forEach((name) =>
      artifactInteger(
        snapshot[name],
        `stress wallet consolidation readiness evidence snapshot.${name}`,
      ),
    );
    if (
      typeof snapshot.providerQueryHealthy !== "boolean" ||
      typeof snapshot.commitWorkerActive !== "boolean"
    ) {
      throw new Error(
        "stress wallet consolidation readiness evidence snapshot booleans are invalid.",
      );
    }
    artifactExactString(
      snapshot.leaseStatus,
      "stress wallet consolidation readiness evidence snapshot.leaseStatus",
    );
    artifactExactString(
      snapshot.commitPipelinePhase,
      "stress wallet consolidation readiness evidence snapshot.commitPipelinePhase",
    );
    const typedSnapshot = snapshot as unknown as ConsolidationReadinessSnapshot;
    if (raw.fullReady !== isFullConsolidationReadiness(typedSnapshot)) {
      throw new Error(
        "stress wallet consolidation readiness evidence fullReady does not bind snapshot.",
      );
    }
  }
  return raw;
};

const STRESS_WALLET_TERMINAL_DRAIN_RESULT_KEYS = [
  "phase",
  "walletDirectory",
  "requestedCount",
  "nodeEndpoint",
  "treasuryAddress",
  "treasuryBeforeLovelace",
  "grossSourceLovelace",
  "totalFeesLovelace",
  "preparedTransferCount",
  "alreadyEmptyCount",
  "submittedTransferCount",
  "resumedTransferCount",
  "statePath",
] as const;

export const parseStressWalletTerminalDrainResult = (
  value: unknown,
): Record<string, unknown> => {
  const label = "stress wallet terminal drain result";
  const raw = parseExactVersionedArtifact(
    value,
    label,
    STRESS_WALLET_TERMINAL_DRAIN_RESULT_SCHEMA_VERSION,
    STRESS_WALLET_TERMINAL_DRAIN_RESULT_KEYS,
    [
      "treasuryAfterLovelace",
      "treasuryDeltaLovelace",
      "residualSourceLovelace",
      "reportPath",
    ],
  );
  const phase = artifactExactString(raw.phase, `${label} phase`);
  if (phase !== "prepared" && phase !== "committed") {
    throw new Error(`${label} phase is unsupported.`);
  }
  ["walletDirectory", "nodeEndpoint", "treasuryAddress", "statePath"].forEach(
    (name) => artifactExactString(raw[name], `${label} ${name}`),
  );
  const requestedCount = artifactInteger(
    raw.requestedCount,
    `${label} requestedCount`,
    1,
  );
  const preparedTransferCount = artifactInteger(
    raw.preparedTransferCount,
    `${label} preparedTransferCount`,
  );
  const alreadyEmptyCount = artifactInteger(
    raw.alreadyEmptyCount,
    `${label} alreadyEmptyCount`,
  );
  const submittedTransferCount = artifactInteger(
    raw.submittedTransferCount,
    `${label} submittedTransferCount`,
  );
  const resumedTransferCount = artifactInteger(
    raw.resumedTransferCount,
    `${label} resumedTransferCount`,
  );
  const treasuryBeforeLovelace = artifactDecimal(
    raw.treasuryBeforeLovelace,
    `${label} treasuryBeforeLovelace`,
  );
  const grossSourceLovelace = artifactDecimal(
    raw.grossSourceLovelace,
    `${label} grossSourceLovelace`,
  );
  const totalFeesLovelace = artifactDecimal(
    raw.totalFeesLovelace,
    `${label} totalFeesLovelace`,
  );
  if (
    requestedCount !== preparedTransferCount + alreadyEmptyCount ||
    (phase === "prepared" &&
      (submittedTransferCount !== 0 ||
        resumedTransferCount !== 0 ||
        [
          raw.treasuryAfterLovelace,
          raw.treasuryDeltaLovelace,
          raw.residualSourceLovelace,
          raw.reportPath,
        ].some((field) => field !== undefined))) ||
    (phase === "committed" &&
      (submittedTransferCount + resumedTransferCount !==
        preparedTransferCount ||
        [
          raw.treasuryAfterLovelace,
          raw.treasuryDeltaLovelace,
          raw.residualSourceLovelace,
          raw.reportPath,
        ].some((field) => field === undefined)))
  ) {
    throw new Error(`${label} phase/cardinality binding is inconsistent.`);
  }
  if (phase === "committed") {
    const treasuryAfterLovelace = artifactDecimal(
      raw.treasuryAfterLovelace,
      `${label} treasuryAfterLovelace`,
    );
    const treasuryDeltaLovelace = artifactDecimal(
      raw.treasuryDeltaLovelace,
      `${label} treasuryDeltaLovelace`,
    );
    const residualSourceLovelace = artifactDecimal(
      raw.residualSourceLovelace,
      `${label} residualSourceLovelace`,
    );
    artifactExactString(raw.reportPath, `${label} reportPath`);
    if (
      residualSourceLovelace !== "0" ||
      BigInt(treasuryAfterLovelace) - BigInt(treasuryBeforeLovelace) !==
        BigInt(treasuryDeltaLovelace) ||
      BigInt(treasuryDeltaLovelace) + BigInt(totalFeesLovelace) !==
        BigInt(grossSourceLovelace)
    ) {
      throw new Error(`${label} conservation binding is inconsistent.`);
    }
  }
  return raw;
};
