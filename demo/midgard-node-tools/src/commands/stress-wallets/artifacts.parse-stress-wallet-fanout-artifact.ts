import {
  artifactDecimal,
  artifactExactString,
  artifactHash32,
  artifactInteger,
  asObject,
  assertExactKeys,
  parseExactVersionedArtifact,
} from "./artifact-fields.js";
import { STRESS_WALLET_FANOUT_RESULT_KEYS } from "./artifacts.parse-stress-wallet-prepare-result.js";
import {
  STRESS_WALLET_FANOUT_REPORT_SCHEMA_VERSION,
  STRESS_WALLET_FANOUT_RESULT_SCHEMA_VERSION,
} from "./constants.js";
import { acceptedTxStatuses } from "./runtime.js";
import { parseStressWalletSummaryArtifact } from "./wallet-summary.js";

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

export const STRESS_WALLET_CONSOLIDATION_RESULT_KEYS = [
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

export const parseStressWalletAccountingArtifact = (
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
