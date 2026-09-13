import type { Assets, UTxO } from "@lucid-evolution/lucid";

import { compareOutRefs, outRefLabel } from "../tx-context.js";

export type WalletHygieneRole =
  | "reference"
  | "operator-main"
  | "operator-merge"
  | "deposit-user"
  | "da-l1-submitter";

export type WalletHygieneIgnoredReason =
  | "has_datum"
  | "has_datum_hash"
  | "has_script_ref"
  | "has_non_lovelace_assets"
  | "missing_lovelace"
  | "stale_out_ref"
  | "reserved_reference_script"
  | "reserved_hub_oracle_nonce";

export type WalletHygieneIgnoredOutRef = {
  readonly outRef: string;
  readonly lovelace: bigint;
  readonly reasons: readonly WalletHygieneIgnoredReason[];
};

export type WalletHygieneCollateralCandidate = {
  readonly outRef: string;
  readonly lovelace: bigint;
};

export type WalletHygieneRequirements = {
  readonly minPlainAdaLovelace: bigint;
  readonly minCollateralLovelace: bigint;
  readonly minPlainAdaUtxoCount: number;
};

export type WalletHygieneStatus =
  | "ready"
  | "needs_consolidation"
  | "needs_collateral"
  | "needs_funding"
  | "blocked";

export type WalletHygieneReport = {
  readonly role: WalletHygieneRole;
  readonly address: string;
  readonly totalLovelace: bigint;
  readonly plainAdaOnlyLovelace: bigint;
  readonly plainAdaOnlyUtxoCount: number;
  readonly collateralCandidates: readonly WalletHygieneCollateralCandidate[];
  readonly selectedFundingOutRefs: readonly string[];
  readonly ignoredOutRefs: readonly WalletHygieneIgnoredOutRef[];
  readonly requirements: WalletHygieneRequirements;
  readonly missing: {
    readonly plainAdaLovelace: bigint;
    readonly collateralLovelace: bigint;
    readonly plainAdaUtxoCount: number;
  };
  readonly status: WalletHygieneStatus;
};

export type ClassifyWalletUtxosOptions = {
  readonly role: WalletHygieneRole;
  readonly address: string;
  readonly utxos: readonly UTxO[];
  readonly requirements?: Partial<WalletHygieneRequirements>;
  readonly staleOutRefs?: ReadonlySet<string>;
  readonly reservedReferenceScriptOutRefs?: ReadonlySet<string>;
  readonly reservedHubOracleNonceOutRefs?: ReadonlySet<string>;
};

export const lovelaceOf = (utxo: UTxO): bigint => utxo.assets.lovelace ?? 0n;

export const positiveAssetEntries = (
  assets: Readonly<Assets>,
): readonly (readonly [string, bigint])[] =>
  Object.entries(assets).filter(([, amount]) => amount > 0n);

export const hasPositiveNonLovelaceAsset = (utxo: UTxO): boolean =>
  positiveAssetEntries(utxo.assets).some(([unit]) => unit !== "lovelace");

export const isPlainAdaOnlyUtxo = (utxo: UTxO): boolean => {
  if (utxo.datum !== undefined || utxo.datumHash !== undefined) {
    return false;
  }
  if (utxo.scriptRef !== undefined) {
    return false;
  }
  const positiveAssets = positiveAssetEntries(utxo.assets);
  return (
    positiveAssets.length === 1 &&
    positiveAssets[0]?.[0] === "lovelace" &&
    positiveAssets[0][1] > 0n
  );
};

export const explainNonPlainAdaOnlyUtxo = (
  utxo: UTxO,
): readonly WalletHygieneIgnoredReason[] => {
  const reasons: WalletHygieneIgnoredReason[] = [];
  if (utxo.datum !== undefined) {
    reasons.push("has_datum");
  }
  if (utxo.datumHash !== undefined) {
    reasons.push("has_datum_hash");
  }
  if (utxo.scriptRef !== undefined) {
    reasons.push("has_script_ref");
  }
  if (hasPositiveNonLovelaceAsset(utxo)) {
    reasons.push("has_non_lovelace_assets");
  }
  if ((utxo.assets.lovelace ?? 0n) <= 0n) {
    reasons.push("missing_lovelace");
  }
  return reasons;
};

const defaultRequirements: WalletHygieneRequirements = {
  minPlainAdaLovelace: 0n,
  minCollateralLovelace: 0n,
  minPlainAdaUtxoCount: 0,
};

const missingBigInt = (required: bigint, available: bigint): bigint =>
  available >= required ? 0n : required - available;

const missingNumber = (required: number, available: number): number =>
  available >= required ? 0 : required - available;

const resolveStatus = ({
  missingPlainAdaLovelace,
  missingCollateralLovelace,
  missingPlainAdaUtxoCount,
}: {
  readonly missingPlainAdaLovelace: bigint;
  readonly missingCollateralLovelace: bigint;
  readonly missingPlainAdaUtxoCount: number;
}): WalletHygieneStatus => {
  if (missingPlainAdaLovelace > 0n) {
    return "needs_funding";
  }
  if (missingPlainAdaUtxoCount > 0) {
    return "needs_consolidation";
  }
  if (missingCollateralLovelace > 0n) {
    return "needs_collateral";
  }
  return "ready";
};

export const classifyWalletUtxos = ({
  role,
  address,
  utxos,
  requirements,
  staleOutRefs = new Set<string>(),
  reservedReferenceScriptOutRefs = new Set<string>(),
  reservedHubOracleNonceOutRefs = new Set<string>(),
}: ClassifyWalletUtxosOptions): WalletHygieneReport => {
  const resolvedRequirements: WalletHygieneRequirements = {
    ...defaultRequirements,
    ...requirements,
  };
  if (
    !Number.isSafeInteger(resolvedRequirements.minPlainAdaUtxoCount) ||
    resolvedRequirements.minPlainAdaUtxoCount < 0
  ) {
    throw new Error("minPlainAdaUtxoCount must be a safe non-negative integer");
  }
  if (
    resolvedRequirements.minPlainAdaLovelace < 0n ||
    resolvedRequirements.minCollateralLovelace < 0n
  ) {
    throw new Error(
      "wallet hygiene lovelace requirements must be non-negative",
    );
  }

  const sorted = [...utxos].sort(compareOutRefs);
  let totalLovelace = 0n;
  let plainAdaOnlyLovelace = 0n;
  const selectedFundingOutRefs: string[] = [];
  const collateralCandidates: WalletHygieneCollateralCandidate[] = [];
  const ignoredOutRefs: WalletHygieneIgnoredOutRef[] = [];

  for (const utxo of sorted) {
    const outRef = outRefLabel(utxo);
    const lovelace = lovelaceOf(utxo);
    totalLovelace += lovelace;
    const reasons = [...explainNonPlainAdaOnlyUtxo(utxo)];
    if (staleOutRefs.has(outRef)) {
      reasons.push("stale_out_ref");
    }
    if (reservedReferenceScriptOutRefs.has(outRef)) {
      reasons.push("reserved_reference_script");
    }
    if (reservedHubOracleNonceOutRefs.has(outRef)) {
      reasons.push("reserved_hub_oracle_nonce");
    }
    if (reasons.length > 0) {
      ignoredOutRefs.push({ outRef, lovelace, reasons });
      continue;
    }
    plainAdaOnlyLovelace += lovelace;
    selectedFundingOutRefs.push(outRef);
    if (lovelace >= resolvedRequirements.minCollateralLovelace) {
      collateralCandidates.push({ outRef, lovelace });
    }
  }

  const bestCollateralLovelace = collateralCandidates[0]?.lovelace ?? 0n;
  const missingPlainAdaLovelace = missingBigInt(
    resolvedRequirements.minPlainAdaLovelace,
    plainAdaOnlyLovelace,
  );
  const missingCollateralLovelace = missingBigInt(
    resolvedRequirements.minCollateralLovelace,
    bestCollateralLovelace,
  );
  const missingPlainAdaUtxoCount = missingNumber(
    resolvedRequirements.minPlainAdaUtxoCount,
    selectedFundingOutRefs.length,
  );

  return {
    role,
    address,
    totalLovelace,
    plainAdaOnlyLovelace,
    plainAdaOnlyUtxoCount: selectedFundingOutRefs.length,
    collateralCandidates,
    selectedFundingOutRefs,
    ignoredOutRefs,
    requirements: resolvedRequirements,
    missing: {
      plainAdaLovelace: missingPlainAdaLovelace,
      collateralLovelace: missingCollateralLovelace,
      plainAdaUtxoCount: missingPlainAdaUtxoCount,
    },
    status: resolveStatus({
      missingPlainAdaLovelace,
      missingCollateralLovelace,
      missingPlainAdaUtxoCount,
    }),
  };
};

export const selectPlainAdaFundingUtxos = (
  utxos: readonly UTxO[],
  targetLovelace: bigint,
): readonly UTxO[] => {
  if (targetLovelace <= 0n) {
    return [];
  }
  const sorted = [...utxos].filter(isPlainAdaOnlyUtxo).sort((left, right) => {
    const leftLovelace = lovelaceOf(left);
    const rightLovelace = lovelaceOf(right);
    if (leftLovelace === rightLovelace) {
      return compareOutRefs(left, right);
    }
    return leftLovelace > rightLovelace ? -1 : 1;
  });
  const selected: UTxO[] = [];
  let covered = 0n;
  for (const utxo of sorted) {
    selected.push(utxo);
    covered += lovelaceOf(utxo);
    if (covered >= targetLovelace) {
      break;
    }
  }
  return selected;
};

export const selectCollateralCandidate = (
  utxos: readonly UTxO[],
  minCollateralLovelace: bigint,
): UTxO | undefined =>
  [...utxos]
    .filter(
      (utxo) =>
        isPlainAdaOnlyUtxo(utxo) && lovelaceOf(utxo) >= minCollateralLovelace,
    )
    .sort((left, right) => {
      const leftLovelace = lovelaceOf(left);
      const rightLovelace = lovelaceOf(right);
      if (leftLovelace === rightLovelace) {
        return compareOutRefs(left, right);
      }
      return leftLovelace < rightLovelace ? -1 : 1;
    })[0];

export const formatWalletHygieneError = (report: WalletHygieneReport): string =>
  [
    `role=${report.role}`,
    `address=${report.address}`,
    `status=${report.status}`,
    `required_plain_lovelace=${report.requirements.minPlainAdaLovelace.toString()}`,
    `available_plain_lovelace=${report.plainAdaOnlyLovelace.toString()}`,
    `missing_plain_lovelace=${report.missing.plainAdaLovelace.toString()}`,
    `required_collateral_lovelace=${report.requirements.minCollateralLovelace.toString()}`,
    `best_collateral_lovelace=${(report.collateralCandidates[0]?.lovelace ?? 0n).toString()}`,
    `missing_collateral_lovelace=${report.missing.collateralLovelace.toString()}`,
    `required_plain_utxo_count=${report.requirements.minPlainAdaUtxoCount.toString()}`,
    `available_plain_utxo_count=${report.plainAdaOnlyUtxoCount.toString()}`,
    `ignored_outrefs=${report.ignoredOutRefs.map(({ outRef, reasons }) => `${outRef}:${reasons.join("+")}`).join(",")}`,
  ].join(",");
