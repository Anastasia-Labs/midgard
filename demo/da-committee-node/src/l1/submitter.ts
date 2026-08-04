import { readFile } from "node:fs/promises";

import {
  CML,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

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

type UtxoOverrideLucid = Pick<LucidEvolution, "wallet"> & {
  readonly utxosAt?: (address: string) => Promise<UTxO[]>;
  readonly utxosByOutRef?: (
    outRefs: {
      readonly txHash: string;
      readonly outputIndex: number;
    }[],
  ) => Promise<UTxO[]>;
  readonly overrideUTxOs?: (utxos: UTxO[]) => void;
};

type L1SubmitterPreflightLucid = Pick<
  LucidEvolution,
  "awaitTxConfirmation" | "newTx" | "selectWallet"
> &
  UtxoOverrideLucid;

const spentOutRefsByLucid = new WeakMap<object, Set<string>>();

const DEFAULT_READINESS_REQUIREMENTS: L1SubmitterReadinessRequirements = {
  minPlainAdaLovelace: 0n,
  minCollateralLovelace: 0n,
  minSpendableUtxoCount: 0,
};

export const selectL1SubmitterWallet = async (
  lucid: Pick<LucidEvolution, "selectWallet"> & Partial<UtxoOverrideLucid>,
  keySource: string,
): Promise<L1SubmitterCredential> => {
  const credential = await readL1SubmitterKeySource(keySource);
  if (credential.kind === "seed") {
    lucid.selectWallet.fromSeed(credential.value);
  } else {
    lucid.selectWallet.fromPrivateKey(credential.value as never);
  }
  await refreshL1SubmitterPlainAdaUtxos(lucid);
  return credential;
};

export const refreshL1SubmitterPlainAdaUtxos = async (
  lucid: Partial<UtxoOverrideLucid>,
  requirements: L1SubmitterReadinessRequirements = DEFAULT_READINESS_REQUIREMENTS,
): Promise<L1SubmitterReadinessSummary | undefined> => {
  if (typeof lucid.overrideUTxOs !== "function") {
    return undefined;
  }
  if (typeof lucid.wallet !== "function") {
    return undefined;
  }
  const wallet = lucid.wallet();
  const address = await wallet.address();
  const utxos =
    typeof lucid.utxosAt === "function"
      ? await lucid.utxosAt(address)
      : await wallet.getUtxos();
  const spentOutRefs = spentOutRefsByLucid.get(lucid);
  const staleOutRefs = await staleCandidateOutRefs(lucid, utxos, spentOutRefs);
  const summary = classifyL1SubmitterUtxos({
    address,
    utxos,
    requirements,
    spentOutRefs,
    staleOutRefs,
  });
  lucid.overrideUTxOs([...summary.spendableUtxos]);
  return summary;
};

export const isPlainAdaUtxo = (utxo: UTxO): boolean =>
  utxo.datum == null &&
  utxo.datumHash == null &&
  utxo.scriptRef == null &&
  Object.keys(utxo.assets).length === 1 &&
  typeof utxo.assets.lovelace === "bigint";

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

export const signSubmitAndConfirm = async (
  lucid: Pick<LucidEvolution, "awaitTxConfirmation"> &
    Partial<UtxoOverrideLucid>,
  tx: TxSignBuilder,
  options: L1SubmitOptions = {},
): Promise<string> => {
  await refreshL1SubmitterPlainAdaUtxos(lucid);
  const signed = await tx.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const txHash = await signed.submit();
  rememberSpentOutRefs(lucid, signedCbor);
  if (options.awaitConfirmation !== false) {
    await lucid.awaitTxConfirmation(txHash, {
      ...(options.confirmationPollIntervalMs === undefined
        ? {}
        : { checkInterval: options.confirmationPollIntervalMs }),
    });
    await refreshL1SubmitterPlainAdaUtxos(lucid);
  }
  return txHash;
};

const staleCandidateOutRefs = async (
  lucid: Partial<UtxoOverrideLucid>,
  utxos: readonly UTxO[],
  spentOutRefs: ReadonlySet<string> | undefined,
): Promise<ReadonlySet<string> | undefined> => {
  if (typeof lucid.utxosByOutRef !== "function") {
    return undefined;
  }
  const candidateOutRefs = utxos
    .filter(
      (utxo) => isPlainAdaUtxo(utxo) && !spentOutRefs?.has(outRefKey(utxo)),
    )
    .map((utxo) => ({ txHash: utxo.txHash, outputIndex: utxo.outputIndex }));
  if (candidateOutRefs.length === 0) {
    return undefined;
  }
  const liveUtxos = await lucid.utxosByOutRef(candidateOutRefs);
  const liveOutRefs = new Set(liveUtxos.map(outRefKey));
  return new Set(
    candidateOutRefs
      .map((outRef) => `${outRef.txHash}#${outRef.outputIndex.toString()}`)
      .filter((outRef) => !liveOutRefs.has(outRef)),
  );
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

const pollReadiness = async (
  lucid: Partial<UtxoOverrideLucid>,
  requirements: L1SubmitterReadinessRequirements & {
    readonly retryCount: number;
    readonly retryDelayMs: number;
  },
): Promise<L1SubmitterReadinessSummary> => {
  let summary = await refreshL1SubmitterPlainAdaUtxos(lucid, requirements);
  if (summary === undefined) {
    throw new Error(
      "L1 submitter wallet preflight requires a selectable wallet",
    );
  }
  for (
    let attempt = 0;
    !summary.ready && attempt < requirements.retryCount;
    attempt += 1
  ) {
    await sleep(requirements.retryDelayMs);
    const nextSummary = await refreshL1SubmitterPlainAdaUtxos(
      lucid,
      requirements,
    );
    if (nextSummary === undefined) {
      throw new Error(
        "L1 submitter wallet preflight requires a selectable wallet",
      );
    }
    summary = nextSummary;
  }
  return summary;
};

const submitAutoFundPayment = async ({
  lucid,
  submitterAddress,
  lovelace,
  confirmationPollIntervalMs,
}: {
  readonly lucid: Pick<LucidEvolution, "awaitTxConfirmation" | "newTx"> &
    Partial<UtxoOverrideLucid>;
  readonly submitterAddress: string;
  readonly lovelace: bigint;
  readonly confirmationPollIntervalMs: number;
}): Promise<string> => {
  const tx = await lucid
    .newTx()
    .pay.ToAddress(submitterAddress, { lovelace })
    .complete();
  return signSubmitAndConfirm(lucid, tx, { confirmationPollIntervalMs });
};

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

const readinessErrors = (
  summary: L1SubmitterReadinessSummary,
): readonly string[] => {
  if (summary.ready) {
    return [];
  }
  const errors: string[] = [];
  if (summary.missingPlainLovelace > 0n) {
    errors.push("missing_plain_lovelace");
  }
  if (summary.missingCollateralLovelace > 0n) {
    errors.push("missing_collateral_lovelace");
  }
  if (summary.missingSpendableUtxoCount > 0) {
    errors.push("missing_spendable_utxo_count");
  }
  return errors;
};

const sleep = (ms: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, ms));

export const readL1SubmitterKeySource = async (
  source: string,
): Promise<L1SubmitterCredential> => {
  const trimmed = source.trim();
  if (trimmed.startsWith("file:")) {
    const fromFile = (
      await readFile(trimmed.slice("file:".length), "utf8")
    ).trim();
    return parseInlineCredential(fromFile);
  }
  return parseInlineCredential(trimmed);
};

const parseInlineCredential = (value: string): L1SubmitterCredential => {
  if (value.startsWith("seed:")) {
    return requiredCredential("seed", value.slice("seed:".length));
  }
  if (value.startsWith("mnemonic:")) {
    return requiredCredential("seed", value.slice("mnemonic:".length));
  }
  if (value.startsWith("private-key:")) {
    return requiredCredential(
      "private_key",
      value.slice("private-key:".length),
    );
  }
  if (value.startsWith("privateKey:")) {
    return requiredCredential("private_key", value.slice("privateKey:".length));
  }
  if (value.trim().split(/\s+/).length >= 12) {
    return requiredCredential("seed", value);
  }
  return requiredCredential("private_key", value);
};

const rememberSpentOutRefs = (lucid: object, txCbor: string): void => {
  const outRefs = spentOutRefsFromTx(txCbor);
  if (outRefs.length === 0) {
    return;
  }
  const spentOutRefs = spentOutRefsByLucid.get(lucid) ?? new Set<string>();
  for (const outRef of outRefs) {
    spentOutRefs.add(outRef);
  }
  spentOutRefsByLucid.set(lucid, spentOutRefs);
};

const spentOutRefsFromTx = (txCbor: string): readonly string[] => {
  const tx = CML.Transaction.from_cbor_hex(txCbor);
  const inputs = tx.body().inputs();
  const outRefs: string[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    outRefs.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return outRefs;
};

const outRefKey = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const maxBigInt = (left: bigint, right: bigint): bigint =>
  left > right ? left : right;

const compareBigInt = (left: bigint, right: bigint): number =>
  left < right ? -1 : left > right ? 1 : 0;

const requiredCredential = <Kind extends L1SubmitterCredential["kind"]>(
  kind: Kind,
  value: string,
): Extract<L1SubmitterCredential, { readonly kind: Kind }> => {
  const trimmed = value.trim();
  if (trimmed === "") {
    throw new Error("L1 submitter key source is empty");
  }
  return { kind, value: trimmed } as Extract<
    L1SubmitterCredential,
    { readonly kind: Kind }
  >;
};
