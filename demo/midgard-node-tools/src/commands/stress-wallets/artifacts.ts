import {
  artifactDecimal,
  artifactExactString,
  artifactHash32,
  artifactInteger,
  artifactIsoTimestamp,
  asObject,
  assertExactKeys,
  parseExactVersionedArtifact,
} from "./artifact-fields.js";
import {
  STRESS_WALLET_CONSOLIDATION_READINESS_SCHEMA_VERSION,
  STRESS_WALLET_CONSOLIDATION_REPORT_SCHEMA_VERSION,
  STRESS_WALLET_CONSOLIDATION_RESULT_SCHEMA_VERSION,
  STRESS_WALLET_CREATE_RESULT_SCHEMA_VERSION,
  STRESS_WALLET_FANOUT_REPORT_SCHEMA_VERSION,
  STRESS_WALLET_FANOUT_RESULT_SCHEMA_VERSION,
  STRESS_WALLET_PREPARE_RESULT_SCHEMA_VERSION,
  STRESS_WALLET_TERMINAL_DRAIN_REPORT_SCHEMA_VERSION,
  STRESS_WALLET_TERMINAL_DRAIN_RESULT_SCHEMA_VERSION,
} from "./constants.js";
import { parseStressWalletNetwork } from "./options.js";
import {
  type ConsolidationReadinessSnapshot,
  isFullConsolidationReadiness,
} from "./readiness.js";
import { acceptedTxStatuses } from "./runtime.js";
import { parseStressWalletOperationScope } from "./scope.js";
import { parseStressWalletSummaryArtifact } from "./wallet-summary.js";

export const parseStressWalletCreateResult = (
  value: unknown,
): Record<string, unknown> => {
  const raw = parseExactVersionedArtifact(
    value,
    "stress wallet create result",
    STRESS_WALLET_CREATE_RESULT_SCHEMA_VERSION,
    [
      "walletDirectory",
      "createdCount",
      "reusedCount",
      "envFilePath",
      "argsFilePath",
      "wallets",
    ],
  );
  const createdCount = artifactInteger(
    raw.createdCount,
    "stress wallet create result createdCount",
  );
  const reusedCount = artifactInteger(
    raw.reusedCount,
    "stress wallet create result reusedCount",
  );
  if (!Array.isArray(raw.wallets)) {
    throw new Error("stress wallet create result wallets must be an array.");
  }
  raw.wallets.forEach((wallet, index) =>
    parseStressWalletSummaryArtifact(
      wallet,
      `stress wallet create result wallets[${index.toString()}]`,
    ),
  );
  artifactExactString(
    raw.walletDirectory,
    "stress wallet create result walletDirectory",
  );
  artifactExactString(
    raw.envFilePath,
    "stress wallet create result envFilePath",
  );
  artifactExactString(
    raw.argsFilePath,
    "stress wallet create result argsFilePath",
  );
  if (createdCount + reusedCount !== raw.wallets.length) {
    throw new Error(
      "stress wallet create result cardinality binding is inconsistent.",
    );
  }
  return raw;
};

export const parseStressWalletPrepareResult = (
  value: unknown,
): Record<string, unknown> => {
  const raw = parseExactVersionedArtifact(
    value,
    "stress wallet prepare result",
    STRESS_WALLET_PREPARE_RESULT_SCHEMA_VERSION,
    [
      "walletDirectory",
      "requestedCount",
      "generatedWalletCount",
      "submittedDepositCount",
      "alreadyFundedCount",
      "verifiedWalletCount",
      "lovelacePerWallet",
      "nodeEndpoint",
      "envFilePath",
      "argsFilePath",
      "wallets",
    ],
  );
  if (!Array.isArray(raw.wallets)) {
    throw new Error("stress wallet prepare result wallets must be an array.");
  }
  const requestedCount = artifactInteger(
    raw.requestedCount,
    "stress wallet prepare result requestedCount",
    1,
  );
  const generatedWalletCount = artifactInteger(
    raw.generatedWalletCount,
    "stress wallet prepare result generatedWalletCount",
  );
  const submittedDepositCount = artifactInteger(
    raw.submittedDepositCount,
    "stress wallet prepare result submittedDepositCount",
  );
  const alreadyFundedCount = artifactInteger(
    raw.alreadyFundedCount,
    "stress wallet prepare result alreadyFundedCount",
  );
  const verifiedWalletCount = artifactInteger(
    raw.verifiedWalletCount,
    "stress wallet prepare result verifiedWalletCount",
  );
  const walletIds = new Set<string>();
  for (const [index, value] of raw.wallets.entries()) {
    const label = `stress wallet prepare result wallets[${index.toString()}]`;
    const entry = asObject(value, label);
    assertExactKeys(
      entry,
      label,
      [
        "wallet",
        "status",
        "beforeUtxoCount",
        "afterUtxoCount",
        "verifiedFundingUtxoCount",
      ],
      ["depositTxHash", "depositEventId"],
    );
    const wallet = parseStressWalletSummaryArtifact(
      entry.wallet,
      `${label}.wallet`,
    );
    if (walletIds.has(wallet.walletId)) {
      throw new Error(
        "stress wallet prepare result wallet IDs must be unique.",
      );
    }
    walletIds.add(wallet.walletId);
    const status = artifactExactString(entry.status, `${label}.status`);
    if (status !== "submitted" && status !== "already_funded") {
      throw new Error(`${label}.status is unsupported.`);
    }
    artifactInteger(entry.beforeUtxoCount, `${label}.beforeUtxoCount`);
    artifactInteger(entry.afterUtxoCount, `${label}.afterUtxoCount`);
    artifactInteger(
      entry.verifiedFundingUtxoCount,
      `${label}.verifiedFundingUtxoCount`,
      1,
    );
    if ((status === "submitted") !== (entry.depositTxHash !== undefined)) {
      throw new Error(`${label} depositTxHash/status binding is inconsistent.`);
    }
    if (entry.depositTxHash !== undefined) {
      artifactHash32(entry.depositTxHash, `${label}.depositTxHash`);
    }
    if (entry.depositEventId !== undefined) {
      artifactExactString(entry.depositEventId, `${label}.depositEventId`);
    }
  }
  [
    ["walletDirectory", raw.walletDirectory],
    ["nodeEndpoint", raw.nodeEndpoint],
    ["envFilePath", raw.envFilePath],
    ["argsFilePath", raw.argsFilePath],
  ].forEach(([name, field]) =>
    artifactExactString(field, `stress wallet prepare result ${String(name)}`),
  );
  artifactDecimal(
    raw.lovelacePerWallet,
    "stress wallet prepare result lovelacePerWallet",
  );
  if (
    requestedCount !== raw.wallets.length ||
    verifiedWalletCount !== raw.wallets.length ||
    generatedWalletCount > requestedCount ||
    submittedDepositCount + alreadyFundedCount !== requestedCount
  ) {
    throw new Error(
      "stress wallet prepare result cardinality binding is inconsistent.",
    );
  }
  return raw;
};

const STRESS_WALLET_FANOUT_RESULT_KEYS = [
  "walletDirectory",
  "requestedCount",
  "generatedWalletCount",
  "branchFactor",
  "maxInFlight",
  "lovelacePerWallet",
  "feeHeadroomLovelace",
  "rootRequiredLovelace",
  "submittedTransferCount",
  "alreadyFundedTransferCount",
  "verifiedWalletCount",
  "nodeEndpoint",
  "envFilePath",
  "argsFilePath",
  "reportPath",
  "levels",
  "wallets",
] as const;

const parseStressWalletFanoutArtifact = (
  value: unknown,
  report: boolean,
): Record<string, unknown> => {
  const label = report
    ? "stress wallet fanout report"
    : "stress wallet fanout result";
  const raw = parseExactVersionedArtifact(
    value,
    label,
    report
      ? STRESS_WALLET_FANOUT_REPORT_SCHEMA_VERSION
      : STRESS_WALLET_FANOUT_RESULT_SCHEMA_VERSION,
    report
      ? [...STRESS_WALLET_FANOUT_RESULT_KEYS, "edges"]
      : STRESS_WALLET_FANOUT_RESULT_KEYS,
  );
  const requestedCount = artifactInteger(
    raw.requestedCount,
    `${label} requestedCount`,
    1,
  );
  const generatedWalletCount = artifactInteger(
    raw.generatedWalletCount,
    `${label} generatedWalletCount`,
  );
  const submittedTransferCount = artifactInteger(
    raw.submittedTransferCount,
    `${label} submittedTransferCount`,
  );
  const alreadyFundedTransferCount = artifactInteger(
    raw.alreadyFundedTransferCount,
    `${label} alreadyFundedTransferCount`,
  );
  const verifiedWalletCount = artifactInteger(
    raw.verifiedWalletCount,
    `${label} verifiedWalletCount`,
  );
  artifactInteger(raw.branchFactor, `${label} branchFactor`, 2);
  artifactInteger(raw.maxInFlight, `${label} maxInFlight`, 1);
  const lovelacePerWallet = artifactDecimal(
    raw.lovelacePerWallet,
    `${label} lovelacePerWallet`,
  );
  const feeHeadroomLovelace = artifactDecimal(
    raw.feeHeadroomLovelace,
    `${label} feeHeadroomLovelace`,
  );
  const rootRequiredLovelace = artifactDecimal(
    raw.rootRequiredLovelace,
    `${label} rootRequiredLovelace`,
  );
  [
    "walletDirectory",
    "nodeEndpoint",
    "envFilePath",
    "argsFilePath",
    "reportPath",
  ].forEach((name) => artifactExactString(raw[name], `${label} ${name}`));
  if (!Array.isArray(raw.levels) || !Array.isArray(raw.wallets)) {
    throw new Error(`${label} levels and wallets must be arrays.`);
  }
  let levelTransferCount = 0;
  const observedLevels = new Set<number>();
  const declaredTransferCountByLevel = new Map<number, number>();
  for (const [index, value] of raw.levels.entries()) {
    const entryLabel = `${label} levels[${index.toString()}]`;
    const entry = asObject(value, entryLabel);
    assertExactKeys(entry, entryLabel, ["level", "transferCount"]);
    const level = artifactInteger(entry.level, `${entryLabel}.level`);
    if (observedLevels.has(level)) {
      throw new Error(`${label} levels must be unique.`);
    }
    observedLevels.add(level);
    const transferCount = artifactInteger(
      entry.transferCount,
      `${entryLabel}.transferCount`,
    );
    levelTransferCount += transferCount;
    declaredTransferCountByLevel.set(level, transferCount);
  }
  const walletIds = new Set<string>();
  for (const [index, value] of raw.wallets.entries()) {
    const entryLabel = `${label} wallets[${index.toString()}]`;
    const entry = asObject(value, entryLabel);
    assertExactKeys(entry, entryLabel, ["wallet", "verifiedFundingUtxoCount"]);
    const wallet = parseStressWalletSummaryArtifact(
      entry.wallet,
      `${entryLabel}.wallet`,
    );
    if (walletIds.has(wallet.walletId)) {
      throw new Error(`${label} wallet IDs must be unique.`);
    }
    walletIds.add(wallet.walletId);
    artifactInteger(
      entry.verifiedFundingUtxoCount,
      `${entryLabel}.verifiedFundingUtxoCount`,
      1,
    );
  }
  if (
    requestedCount !== raw.wallets.length ||
    verifiedWalletCount !== raw.wallets.length ||
    generatedWalletCount > requestedCount ||
    submittedTransferCount + alreadyFundedTransferCount !== requestedCount ||
    levelTransferCount !==
      submittedTransferCount + alreadyFundedTransferCount ||
    BigInt(rootRequiredLovelace) !==
      BigInt(requestedCount) *
        (BigInt(lovelacePerWallet) + BigInt(feeHeadroomLovelace))
  ) {
    throw new Error(`${label} cardinality/value binding is inconsistent.`);
  }
  if (report) {
    if (!Array.isArray(raw.edges)) {
      throw new Error(`${label} edges must be an array.`);
    }
    const childWalletIds = new Set<string>();
    let submittedEdgeCount = 0;
    const edgeCountByLevel = new Map<number, number>();
    for (const [index, value] of raw.edges.entries()) {
      const edgeLabel = `${label} edges[${index.toString()}]`;
      const edge = asObject(value, edgeLabel);
      assertExactKeys(edge, edgeLabel, [
        "level",
        "parentWalletId",
        "childWalletId",
        "lovelace",
        "txHash",
        "acceptedStatus",
        "submitted",
      ]);
      const level = artifactInteger(edge.level, `${edgeLabel}.level`, 1);
      if (!observedLevels.has(level)) {
        throw new Error(`${edgeLabel}.level is not declared by levels.`);
      }
      edgeCountByLevel.set(level, (edgeCountByLevel.get(level) ?? 0) + 1);
      const parentWalletId = artifactExactString(
        edge.parentWalletId,
        `${edgeLabel}.parentWalletId`,
      );
      if (parentWalletId !== "treasury" && !walletIds.has(parentWalletId)) {
        throw new Error(
          `${edgeLabel}.parentWalletId is not in the wallet set.`,
        );
      }
      const childWalletId = artifactExactString(
        edge.childWalletId,
        `${edgeLabel}.childWalletId`,
      );
      if (childWalletIds.has(childWalletId)) {
        throw new Error(`${label} child wallet IDs must be unique.`);
      }
      childWalletIds.add(childWalletId);
      if (
        BigInt(artifactDecimal(edge.lovelace, `${edgeLabel}.lovelace`)) <= 0n
      ) {
        throw new Error(`${edgeLabel}.lovelace must be positive.`);
      }
      artifactHash32(edge.txHash, `${edgeLabel}.txHash`);
      const acceptedStatus = artifactExactString(
        edge.acceptedStatus,
        `${edgeLabel}.acceptedStatus`,
      );
      if (typeof edge.submitted !== "boolean") {
        throw new Error(`${edgeLabel}.submitted must be boolean.`);
      }
      if (edge.submitted) {
        submittedEdgeCount += 1;
        if (!acceptedTxStatuses.has(acceptedStatus)) {
          throw new Error(`${edgeLabel}.acceptedStatus is not accepted.`);
        }
      } else if (acceptedStatus !== "already_funded") {
        throw new Error(
          `${edgeLabel}.acceptedStatus must be already_funded when not submitted.`,
        );
      }
    }
    if (
      raw.edges.length !==
        submittedTransferCount + alreadyFundedTransferCount ||
      submittedEdgeCount !== submittedTransferCount ||
      childWalletIds.size !== walletIds.size ||
      [...childWalletIds].some((walletId) => !walletIds.has(walletId)) ||
      [...observedLevels].some(
        (level) =>
          (edgeCountByLevel.get(level) ?? 0) !==
          declaredTransferCountByLevel.get(level),
      )
    ) {
      throw new Error(`${label} edge cardinality is inconsistent.`);
    }
  }
  return raw;
};

export const parseStressWalletFanoutResult = (
  value: unknown,
): Record<string, unknown> => parseStressWalletFanoutArtifact(value, false);

export const parseStressWalletFanoutReport = (
  value: unknown,
): Record<string, unknown> => parseStressWalletFanoutArtifact(value, true);

const STRESS_WALLET_CONSOLIDATION_RESULT_KEYS = [
  "walletDirectory",
  "requestedCount",
  "reserveLovelace",
  "maxInFlight",
  "nodeEndpoint",
  "treasuryAddress",
  "treasuryBeforeLovelace",
  "treasuryAfterLovelace",
  "treasuryDeltaLovelace",
  "sourceBeforeLovelace",
  "sourceAfterLovelace",
  "inferredFeesLovelace",
  "projectedTreasuryLovelace",
  "submittedTransferCount",
  "resumedTransferCount",
  "alreadyConsolidatedCount",
  "reportPath",
] as const;

const parseStressWalletAccountingArtifact = (
  value: unknown,
  label: string,
): {
  readonly lovelace: string;
  readonly utxoCount: number;
  readonly outrefs: readonly string[];
} => {
  const raw = asObject(value, label);
  assertExactKeys(raw, label, ["lovelace", "utxoCount", "outrefs"]);
  if (!Array.isArray(raw.outrefs)) {
    throw new Error(`${label}.outrefs must be an array.`);
  }
  const outrefs = raw.outrefs.map((outref, index) => {
    const parsed = artifactExactString(
      outref,
      `${label}.outrefs[${index.toString()}]`,
    );
    if (!/^[0-9a-f]{64}#(0|[1-9]\d*)$/.test(parsed)) {
      throw new Error(
        `${label}.outrefs[${index.toString()}] must be canonical.`,
      );
    }
    return parsed;
  });
  const utxoCount = artifactInteger(raw.utxoCount, `${label}.utxoCount`);
  if (
    outrefs.length !== utxoCount ||
    new Set(outrefs).size !== outrefs.length ||
    [...outrefs].sort().join("|") !== outrefs.join("|")
  ) {
    throw new Error(`${label} outref cardinality/order is inconsistent.`);
  }
  return {
    lovelace: artifactDecimal(raw.lovelace, `${label}.lovelace`),
    utxoCount,
    outrefs,
  };
};

const parseStressWalletConsolidationResultFields = (
  raw: Record<string, unknown>,
  label: string,
): {
  readonly requestedCount: number;
  readonly submittedTransferCount: number;
  readonly resumedTransferCount: number;
  readonly alreadyConsolidatedCount: number;
} => {
  ["walletDirectory", "nodeEndpoint", "treasuryAddress", "reportPath"].forEach(
    (name) => artifactExactString(raw[name], `${label} ${name}`),
  );
  const requestedCount = artifactInteger(
    raw.requestedCount,
    `${label} requestedCount`,
    1,
  );
  artifactInteger(raw.maxInFlight, `${label} maxInFlight`, 1);
  const decimals = [
    "reserveLovelace",
    "treasuryBeforeLovelace",
    "treasuryAfterLovelace",
    "treasuryDeltaLovelace",
    "sourceBeforeLovelace",
    "sourceAfterLovelace",
    "inferredFeesLovelace",
    "projectedTreasuryLovelace",
  ] as const;
  const parsedDecimals = Object.fromEntries(
    decimals.map((name) => [
      name,
      artifactDecimal(raw[name], `${label} ${name}`),
    ]),
  ) as Record<(typeof decimals)[number], string>;
  const submittedTransferCount = artifactInteger(
    raw.submittedTransferCount,
    `${label} submittedTransferCount`,
  );
  const resumedTransferCount = artifactInteger(
    raw.resumedTransferCount,
    `${label} resumedTransferCount`,
  );
  const alreadyConsolidatedCount = artifactInteger(
    raw.alreadyConsolidatedCount,
    `${label} alreadyConsolidatedCount`,
  );
  if (
    alreadyConsolidatedCount > requestedCount ||
    submittedTransferCount > requestedCount - alreadyConsolidatedCount ||
    resumedTransferCount > requestedCount - alreadyConsolidatedCount ||
    BigInt(parsedDecimals.treasuryAfterLovelace) -
      BigInt(parsedDecimals.treasuryBeforeLovelace) !==
      BigInt(parsedDecimals.treasuryDeltaLovelace) ||
    BigInt(parsedDecimals.treasuryAfterLovelace) !==
      BigInt(parsedDecimals.projectedTreasuryLovelace) ||
    BigInt(parsedDecimals.sourceBeforeLovelace) -
      BigInt(parsedDecimals.sourceAfterLovelace) -
      BigInt(parsedDecimals.treasuryDeltaLovelace) !==
      BigInt(parsedDecimals.inferredFeesLovelace)
  ) {
    throw new Error(`${label} accounting/cardinality binding is inconsistent.`);
  }
  return {
    requestedCount,
    submittedTransferCount,
    resumedTransferCount,
    alreadyConsolidatedCount,
  };
};

export const parseStressWalletConsolidationResult = (
  value: unknown,
): Record<string, unknown> => {
  const raw = parseExactVersionedArtifact(
    value,
    "stress wallet consolidation result",
    STRESS_WALLET_CONSOLIDATION_RESULT_SCHEMA_VERSION,
    STRESS_WALLET_CONSOLIDATION_RESULT_KEYS,
  );
  parseStressWalletConsolidationResultFields(
    raw,
    "stress wallet consolidation result",
  );
  return raw;
};

export const parseStressWalletConsolidationReport = (
  value: unknown,
): Record<string, unknown> => {
  const label = "stress wallet consolidation report";
  const raw = parseExactVersionedArtifact(
    value,
    label,
    STRESS_WALLET_CONSOLIDATION_REPORT_SCHEMA_VERSION,
    [
      ...STRESS_WALLET_CONSOLIDATION_RESULT_KEYS,
      "statePath",
      "treasury",
      "wallets",
    ],
  );
  const { requestedCount } = parseStressWalletConsolidationResultFields(
    raw,
    label,
  );
  artifactExactString(raw.statePath, `${label} statePath`);
  const treasury = asObject(raw.treasury, `${label} treasury`);
  assertExactKeys(treasury, `${label} treasury`, ["before", "after"]);
  parseStressWalletAccountingArtifact(
    treasury.before,
    `${label} treasury.before`,
  );
  parseStressWalletAccountingArtifact(
    treasury.after,
    `${label} treasury.after`,
  );
  if (!Array.isArray(raw.wallets)) {
    throw new Error(`${label} wallets must be an array.`);
  }
  const walletIds = new Set<string>();
  for (const [index, value] of raw.wallets.entries()) {
    const walletLabel = `${label} wallets[${index.toString()}]`;
    const wallet = asObject(value, walletLabel);
    assertExactKeys(
      wallet,
      walletLabel,
      ["walletId", "address", "before", "after"],
      ["transfer"],
    );
    const walletId = artifactExactString(
      wallet.walletId,
      `${walletLabel}.walletId`,
    );
    if (walletIds.has(walletId)) {
      throw new Error(`${label} wallet IDs must be unique.`);
    }
    walletIds.add(walletId);
    artifactExactString(wallet.address, `${walletLabel}.address`);
    parseStressWalletAccountingArtifact(wallet.before, `${walletLabel}.before`);
    parseStressWalletAccountingArtifact(wallet.after, `${walletLabel}.after`);
    if (wallet.transfer !== undefined) {
      const transfer = asObject(wallet.transfer, `${walletLabel}.transfer`);
      assertExactKeys(
        transfer,
        `${walletLabel}.transfer`,
        [
          "walletId",
          "address",
          "beforeLovelace",
          "beforeOutrefs",
          "requestedLovelace",
        ],
        [
          "txHash",
          "signedTxCbor",
          "selectedInputs",
          "selectedInputLovelace",
          "acceptedStatus",
        ],
      );
      if (transfer.walletId !== walletId) {
        throw new Error(`${walletLabel}.transfer wallet ID is mismatched.`);
      }
    }
  }
  if (raw.wallets.length !== requestedCount) {
    throw new Error(`${label} wallet cardinality is inconsistent.`);
  }
  return raw;
};

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

export const parseStressWalletTerminalDrainReport = (
  value: unknown,
): Record<string, unknown> => {
  const label = "stress wallet terminal drain report";
  const raw = parseExactVersionedArtifact(
    value,
    label,
    STRESS_WALLET_TERMINAL_DRAIN_REPORT_SCHEMA_VERSION,
    [
      "scope",
      "statePath",
      "endpoint",
      "network",
      "treasuryAddress",
      "conservation",
      "wallets",
    ],
  );
  const scope = parseStressWalletOperationScope(raw.scope);
  ["statePath", "endpoint", "treasuryAddress"].forEach((name) =>
    artifactExactString(raw[name], `${label} ${name}`),
  );
  const networkText = artifactExactString(raw.network, `${label} network`);
  parseStressWalletNetwork(networkText, {});
  const conservation = asObject(raw.conservation, `${label} conservation`);
  assertExactKeys(conservation, `${label} conservation`, [
    "grossSourceLovelace",
    "treasuryDeltaLovelace",
    "totalFeesLovelace",
    "residualSourceLovelace",
  ]);
  const gross = artifactDecimal(
    conservation.grossSourceLovelace,
    `${label} conservation.grossSourceLovelace`,
  );
  const delta = artifactDecimal(
    conservation.treasuryDeltaLovelace,
    `${label} conservation.treasuryDeltaLovelace`,
  );
  const fees = artifactDecimal(
    conservation.totalFeesLovelace,
    `${label} conservation.totalFeesLovelace`,
  );
  const residual = artifactDecimal(
    conservation.residualSourceLovelace,
    `${label} conservation.residualSourceLovelace`,
  );
  if (residual !== "0" || BigInt(delta) + BigInt(fees) !== BigInt(gross)) {
    throw new Error(`${label} conservation binding is inconsistent.`);
  }
  if (!Array.isArray(raw.wallets) || raw.wallets.length !== scope.count) {
    throw new Error(`${label} wallet cardinality is inconsistent.`);
  }
  const walletIds = new Set<string>();
  for (const [index, value] of raw.wallets.entries()) {
    const walletLabel = `${label} wallets[${index.toString()}]`;
    const wallet = asObject(value, walletLabel);
    assertExactKeys(wallet, walletLabel, [
      "walletId",
      "address",
      "before",
      "after",
    ]);
    const walletId = artifactExactString(
      wallet.walletId,
      `${walletLabel}.walletId`,
    );
    if (walletIds.has(walletId)) {
      throw new Error(`${label} wallet IDs must be unique.`);
    }
    walletIds.add(walletId);
    artifactExactString(wallet.address, `${walletLabel}.address`);
    const before = asObject(wallet.before, `${walletLabel}.before`);
    assertExactKeys(
      before,
      `${walletLabel}.before`,
      [
        "walletId",
        "address",
        "beforeOutrefs",
        "beforeLovelace",
        "beforeValueSha256",
        "status",
      ],
      [
        "txHash",
        "signedTxCbor",
        "selectedInputs",
        "requestedLovelace",
        "feeLovelace",
        "signedTxBytes",
      ],
    );
    if (before.walletId !== walletId || before.address !== wallet.address) {
      throw new Error(`${walletLabel}.before identity is mismatched.`);
    }
    parseStressWalletAccountingArtifact(wallet.after, `${walletLabel}.after`);
  }
  return raw;
};
