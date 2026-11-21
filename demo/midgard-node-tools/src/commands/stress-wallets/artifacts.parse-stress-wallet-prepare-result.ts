import {
  artifactDecimal,
  artifactExactString,
  artifactHash32,
  artifactInteger,
  asObject,
  assertExactKeys,
  parseExactVersionedArtifact,
} from "./artifact-fields.js";
import {
  STRESS_WALLET_CREATE_RESULT_SCHEMA_VERSION,
  STRESS_WALLET_PREPARE_RESULT_SCHEMA_VERSION,
} from "./constants.js";
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

export const STRESS_WALLET_FANOUT_RESULT_KEYS = [
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
