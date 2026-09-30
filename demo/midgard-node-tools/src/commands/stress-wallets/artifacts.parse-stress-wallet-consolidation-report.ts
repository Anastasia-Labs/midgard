import {
  artifactDecimal,
  artifactExactString,
  artifactInteger,
  asObject,
  assertExactKeys,
  parseExactVersionedArtifact,
} from "./artifact-fields.js";
import {
  parseStressWalletAccountingArtifact,
  STRESS_WALLET_CONSOLIDATION_RESULT_KEYS,
} from "./artifacts.parse-stress-wallet-fanout-artifact.js";
import {
  STRESS_WALLET_CONSOLIDATION_REPORT_SCHEMA_VERSION,
  STRESS_WALLET_CONSOLIDATION_RESULT_SCHEMA_VERSION,
} from "./constants.js";

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
