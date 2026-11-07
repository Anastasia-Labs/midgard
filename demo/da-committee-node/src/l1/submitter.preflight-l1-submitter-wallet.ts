import {
  type L1SubmitterPreflightLucid,
  type L1SubmitterPreflightOptions,
  type L1SubmitterPreflightResult,
  type L1SubmitterPreflightStatus,
  type L1SubmitterReadinessSummary,
  maxBigInt,
} from "./submitter.classify-l1-submitter-utxos.js";
import {
  pollReadiness,
  readinessErrors,
  selectL1SubmitterWallet,
  submitAutoFundPayment,
} from "./submitter.prune-in-flight-spends.js";

export const preflightL1SubmitterWallet = async (
  lucid: L1SubmitterPreflightLucid,
  options: L1SubmitterPreflightOptions,
): Promise<L1SubmitterPreflightResult> => {
  const readyWithoutFunding = await pollReadiness(lucid, options);
  if (readyWithoutFunding.ready) {
    return preflightResult("ready", readyWithoutFunding);
  }
  if (options.autoFundKeySource === undefined) {
    return preflightResult("failed", readyWithoutFunding);
  }

  const submitterAddress = readyWithoutFunding.address;
  await selectL1SubmitterWallet(lucid, options.autoFundKeySource);
  const funderAddress = await lucid.wallet().address();
  if (funderAddress === submitterAddress) {
    await selectL1SubmitterWallet(lucid, options.submitterKeySource);
    return preflightResult("failed", readyWithoutFunding, {
      errors: ["auto_fund_source_matches_submitter_address"],
    });
  }

  const autoFundLovelace =
    maxBigInt(
      readyWithoutFunding.missingPlainLovelace,
      readyWithoutFunding.missingCollateralLovelace,
    ) + options.autoFundBufferLovelace;
  const fundingTxHash = await submitAutoFundPayment({
    lucid,
    submitterAddress,
    lovelace: autoFundLovelace,
    confirmationPollIntervalMs: options.retryDelayMs,
  });
  await selectL1SubmitterWallet(lucid, options.submitterKeySource);
  const afterFunding = await pollReadiness(lucid, options);
  if (afterFunding.ready) {
    return preflightResult("funded", afterFunding, {
      fundingTxHash,
      autoFundLovelace,
    });
  }
  return preflightResult("failed", afterFunding, {
    fundingTxHash,
    autoFundLovelace,
  });
};

export const assertL1SubmitterWalletPreflight = async (
  lucid: L1SubmitterPreflightLucid,
  options: L1SubmitterPreflightOptions,
): Promise<L1SubmitterPreflightResult> => {
  const result = await preflightL1SubmitterWallet(lucid, options);
  if (result.status === "failed") {
    throw new L1SubmitterPreflightError(result);
  }
  return result;
};

export class L1SubmitterPreflightError extends Error {
  readonly result: L1SubmitterPreflightResult;

  constructor(result: L1SubmitterPreflightResult) {
    super(formatL1SubmitterPreflightFailure(result));
    this.name = "L1SubmitterPreflightError";
    this.result = result;
  }
}

export const formatL1SubmitterPreflightFailure = (
  result: L1SubmitterPreflightResult,
): string => {
  const ignoredOutRefs = result.ignoredOutRefs
    .map((entry) => `${entry.outRef}:${entry.reasons.join("+")}`)
    .join("|");
  return [
    "L1 submitter wallet preflight failed",
    `submitter_address=${result.address}`,
    `required_plain_lovelace=${result.requiredPlainLovelace.toString()}`,
    `available_plain_lovelace=${result.plainAdaLovelace.toString()}`,
    `missing_plain_lovelace=${result.missingPlainLovelace.toString()}`,
    `required_collateral_lovelace=${result.requiredCollateralLovelace.toString()}`,
    `best_collateral_lovelace=${result.collateralCandidateLovelace.toString()}`,
    `missing_collateral_lovelace=${result.missingCollateralLovelace.toString()}`,
    `required_spendable_utxo_count=${result.requiredSpendableUtxoCount.toString()}`,
    `available_spendable_utxo_count=${result.plainAdaUtxoCount.toString()}`,
    `missing_spendable_utxo_count=${result.missingSpendableUtxoCount.toString()}`,
    `ignored_out_refs=${ignoredOutRefs}`,
    `errors=${result.errors.join("|")}`,
  ].join(", ");
};

export const l1SubmitterPreflightResultToJson = (
  result: L1SubmitterPreflightResult,
): Record<string, unknown> => ({
  status: result.status,
  address: result.address,
  totalLiveLovelace: result.totalLiveLovelace.toString(),
  plainAdaLovelace: result.plainAdaLovelace.toString(),
  plainAdaUtxoCount: result.plainAdaUtxoCount,
  collateralCandidateLovelace: result.collateralCandidateLovelace.toString(),
  ...(result.collateralCandidateOutRef === undefined
    ? {}
    : { collateralCandidateOutRef: result.collateralCandidateOutRef }),
  spendableOutRefs: result.spendableOutRefs,
  ignoredOutRefs: result.ignoredOutRefs.map((entry) => ({
    outRef: entry.outRef,
    lovelace: entry.lovelace.toString(),
    reasons: entry.reasons,
  })),
  requiredPlainLovelace: result.requiredPlainLovelace.toString(),
  requiredCollateralLovelace: result.requiredCollateralLovelace.toString(),
  requiredSpendableUtxoCount: result.requiredSpendableUtxoCount,
  missingPlainLovelace: result.missingPlainLovelace.toString(),
  missingCollateralLovelace: result.missingCollateralLovelace.toString(),
  missingSpendableUtxoCount: result.missingSpendableUtxoCount,
  ...(result.fundingTxHash === undefined
    ? {}
    : { fundingTxHash: result.fundingTxHash }),
  ...(result.autoFundLovelace === undefined
    ? {}
    : { autoFundLovelace: result.autoFundLovelace.toString() }),
  errors: result.errors,
});

const preflightResult = (
  status: L1SubmitterPreflightStatus,
  summary: L1SubmitterReadinessSummary,
  extra: {
    readonly fundingTxHash?: string;
    readonly autoFundLovelace?: bigint;
    readonly errors?: readonly string[];
  } = {},
): L1SubmitterPreflightResult => ({
  status,
  address: summary.address,
  totalLiveLovelace: summary.totalLiveLovelace,
  plainAdaLovelace: summary.plainAdaLovelace,
  plainAdaUtxoCount: summary.plainAdaUtxoCount,
  collateralCandidateLovelace: summary.collateralCandidateLovelace,
  ...(summary.collateralCandidateOutRef === undefined
    ? {}
    : { collateralCandidateOutRef: summary.collateralCandidateOutRef }),
  spendableOutRefs: summary.spendableOutRefs,
  ignoredOutRefs: summary.ignoredOutRefs,
  requiredPlainLovelace: summary.requiredPlainLovelace,
  requiredCollateralLovelace: summary.requiredCollateralLovelace,
  requiredSpendableUtxoCount: summary.requiredSpendableUtxoCount,
  missingPlainLovelace: summary.missingPlainLovelace,
  missingCollateralLovelace: summary.missingCollateralLovelace,
  missingSpendableUtxoCount: summary.missingSpendableUtxoCount,
  ...(extra.fundingTxHash === undefined
    ? {}
    : { fundingTxHash: extra.fundingTxHash }),
  ...(extra.autoFundLovelace === undefined
    ? {}
    : { autoFundLovelace: extra.autoFundLovelace }),
  errors: extra.errors ?? readinessErrors(summary),
});
