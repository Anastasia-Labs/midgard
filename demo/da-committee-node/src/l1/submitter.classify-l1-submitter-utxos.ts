import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

export type L1SubmitterCredential =
  | { readonly kind: "seed"; readonly value: string }
  | { readonly kind: "private_key"; readonly value: string };

export type L1SubmitOptions = {
  readonly awaitConfirmation?: boolean;
  readonly confirmationPollIntervalMs?: number;
};

export type L1SubmitterUtxoIgnoreReason =
  | "has_datum"
  | "has_script_ref"
  | "has_non_lovelace_assets"
  | "below_collateral_floor"
  | "stale_out_ref"
  | "spent_in_process";

export type IgnoredL1SubmitterOutRef = {
  readonly outRef: string;
  readonly lovelace: bigint;
  readonly reasons: readonly L1SubmitterUtxoIgnoreReason[];
};

export type L1SubmitterReadinessRequirements = {
  readonly minPlainAdaLovelace: bigint;
  readonly minCollateralLovelace: bigint;
  readonly minSpendableUtxoCount: number;
};

export type L1SubmitterReadinessSummary = {
  readonly address: string;
  readonly totalLiveLovelace: bigint;
  readonly plainAdaLovelace: bigint;
  readonly plainAdaUtxoCount: number;
  readonly collateralCandidateLovelace: bigint;
  readonly collateralCandidateOutRef?: string;
  readonly spendableOutRefs: readonly string[];
  readonly ignoredOutRefs: readonly IgnoredL1SubmitterOutRef[];
  readonly requiredPlainLovelace: bigint;
  readonly requiredCollateralLovelace: bigint;
  readonly requiredSpendableUtxoCount: number;
  readonly missingPlainLovelace: bigint;
  readonly missingCollateralLovelace: bigint;
  readonly missingSpendableUtxoCount: number;
  readonly ready: boolean;
  readonly spendableUtxos: readonly UTxO[];
};

export type L1SubmitterPreflightOptions = L1SubmitterReadinessRequirements & {
  readonly submitterKeySource: string;
  readonly autoFundKeySource?: string;
  readonly autoFundBufferLovelace: bigint;
  readonly retryCount: number;
  readonly retryDelayMs: number;
};

export type L1SubmitterPreflightStatus = "ready" | "funded" | "failed";

export type L1SubmitterPreflightResult = Omit<
  L1SubmitterReadinessSummary,
  "ready" | "spendableUtxos"
> & {
  readonly status: L1SubmitterPreflightStatus;
  readonly fundingTxHash?: string;
  readonly autoFundLovelace?: bigint;
  readonly errors: readonly string[];
};

export type UtxoOverrideLucid = Pick<LucidEvolution, "wallet"> & {
  readonly utxosAt?: (address: string) => Promise<UTxO[]>;
  readonly utxosByOutRef?: (
    outRefs: {
      readonly txHash: string;
      readonly outputIndex: number;
    }[],
  ) => Promise<UTxO[]>;
  readonly overrideUTxOs?: (utxos: UTxO[]) => void;
  readonly currentSlot?: () => number;
  readonly transactionStatus?: (
    txHash: string,
  ) => Promise<{ readonly status: string }>;
};

export type L1SubmitterPreflightLucid = Pick<
  LucidEvolution,
  "awaitTxConfirmation" | "newTx" | "selectWallet"
> &
  UtxoOverrideLucid;

/**
 * A submitted transaction whose inputs refreshes keep out of the spendable
 * set, because the provider may still list them. It is forgotten once its
 * inputs leave the wallet listing (it landed, or they were spent otherwise),
 * or once it can no longer land while they are still listed: past its TTL, or
 * after its confirmation wait gave up and the chain has not seen it.
 */
export type InFlightSpend = {
  readonly outRefs: readonly string[];
  /** The body's TTL: the first slot the transaction is invalid in. */
  readonly ttlSlot?: number;
  /** Set when the confirmation wait ended without seeing the transaction. */
  unconfirmed: boolean;
};

export const inFlightSpendsByLucid = new WeakMap<
  object,
  Map<string, InFlightSpend>
>();

export const DEFAULT_READINESS_REQUIREMENTS: L1SubmitterReadinessRequirements =
  {
    minPlainAdaLovelace: 0n,
    minCollateralLovelace: 0n,
    minSpendableUtxoCount: 0,
  };

export const classifyL1SubmitterUtxos = ({
  address,
  utxos,
  requirements,
  spentOutRefs,
  staleOutRefs,
}: {
  readonly address: string;
  readonly utxos: readonly UTxO[];
  readonly requirements: L1SubmitterReadinessRequirements;
  readonly spentOutRefs?: ReadonlySet<string>;
  readonly staleOutRefs?: ReadonlySet<string>;
}): L1SubmitterReadinessSummary => {
  const sortedUtxos = [...utxos].sort((left, right) =>
    outRefKey(left).localeCompare(outRefKey(right)),
  );
  const spendableUtxos: UTxO[] = [];
  const ignoredOutRefs: IgnoredL1SubmitterOutRef[] = [];
  let totalLiveLovelace = 0n;

  for (const utxo of sortedUtxos) {
    const outRef = outRefKey(utxo);
    const lovelace = utxo.assets.lovelace ?? 0n;
    totalLiveLovelace += lovelace;
    const reasons = ignoredReasonsForUtxo({
      utxo,
      outRef,
      requirements,
      spentOutRefs,
      staleOutRefs,
    });
    const spendableReasons = reasons.filter(
      (reason) => reason !== "below_collateral_floor",
    );
    if (spendableReasons.length === 0) {
      spendableUtxos.push(utxo);
    }
    if (reasons.length > 0) {
      ignoredOutRefs.push({ outRef, lovelace, reasons });
    }
  }

  const plainAdaLovelace = spendableUtxos.reduce(
    (total, utxo) => total + (utxo.assets.lovelace ?? 0n),
    0n,
  );
  const collateralCandidate = spendableUtxos
    .filter(
      (utxo) =>
        (utxo.assets.lovelace ?? 0n) >= requirements.minCollateralLovelace,
    )
    .sort((left, right) =>
      compareBigInt(right.assets.lovelace ?? 0n, left.assets.lovelace ?? 0n),
    )[0];
  const collateralCandidateLovelace =
    collateralCandidate?.assets.lovelace ?? 0n;
  const missingPlainLovelace = maxBigInt(
    0n,
    requirements.minPlainAdaLovelace - plainAdaLovelace,
  );
  const missingCollateralLovelace = maxBigInt(
    0n,
    requirements.minCollateralLovelace - collateralCandidateLovelace,
  );
  const missingSpendableUtxoCount = Math.max(
    0,
    requirements.minSpendableUtxoCount - spendableUtxos.length,
  );
  return {
    address,
    totalLiveLovelace,
    plainAdaLovelace,
    plainAdaUtxoCount: spendableUtxos.length,
    collateralCandidateLovelace,
    ...(collateralCandidate === undefined
      ? {}
      : { collateralCandidateOutRef: outRefKey(collateralCandidate) }),
    spendableOutRefs: spendableUtxos.map(outRefKey),
    ignoredOutRefs,
    requiredPlainLovelace: requirements.minPlainAdaLovelace,
    requiredCollateralLovelace: requirements.minCollateralLovelace,
    requiredSpendableUtxoCount: requirements.minSpendableUtxoCount,
    missingPlainLovelace,
    missingCollateralLovelace,
    missingSpendableUtxoCount,
    ready:
      missingPlainLovelace === 0n &&
      missingCollateralLovelace === 0n &&
      missingSpendableUtxoCount === 0,
    spendableUtxos,
  };
};

/** Whether the chain has not seen `txHash`; a failed lookup is no proof. */
export const transactionAbsent = async (
  lucid: Partial<UtxoOverrideLucid>,
  txHash: string,
): Promise<boolean> => {
  if (typeof lucid.transactionStatus !== "function") {
    return true;
  }
  try {
    return (await lucid.transactionStatus(txHash)).status === "not_found";
  } catch {
    return false;
  }
};

const ignoredReasonsForUtxo = ({
  utxo,
  outRef,
  requirements,
  spentOutRefs,
  staleOutRefs,
}: {
  readonly utxo: UTxO;
  readonly outRef: string;
  readonly requirements: L1SubmitterReadinessRequirements;
  readonly spentOutRefs?: ReadonlySet<string>;
  readonly staleOutRefs?: ReadonlySet<string>;
}): readonly L1SubmitterUtxoIgnoreReason[] => {
  const reasons: L1SubmitterUtxoIgnoreReason[] = [];
  if (spentOutRefs?.has(outRef) === true) {
    reasons.push("spent_in_process");
  }
  if (staleOutRefs?.has(outRef) === true) {
    reasons.push("stale_out_ref");
  }
  if (utxo.datum != null || utxo.datumHash != null) {
    reasons.push("has_datum");
  }
  if (utxo.scriptRef != null) {
    reasons.push("has_script_ref");
  }
  if (
    Object.keys(utxo.assets).length !== 1 ||
    typeof utxo.assets.lovelace !== "bigint"
  ) {
    reasons.push("has_non_lovelace_assets");
  }
  if (
    reasons.length === 0 &&
    (utxo.assets.lovelace ?? 0n) < requirements.minCollateralLovelace
  ) {
    reasons.push("below_collateral_floor");
  }
  return reasons;
};

export const outRefKey = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

export const maxBigInt = (left: bigint, right: bigint): bigint =>
  left > right ? left : right;

const compareBigInt = (left: bigint, right: bigint): number =>
  left < right ? -1 : left > right ? 1 : 0;
