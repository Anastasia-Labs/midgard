import { readFile } from "node:fs/promises";

import {
  CML,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  classifyL1SubmitterUtxos,
  DEFAULT_READINESS_REQUIREMENTS,
  type InFlightSpend,
  inFlightSpendsByLucid,
  type L1SubmitOptions,
  type L1SubmitterCredential,
  type L1SubmitterReadinessRequirements,
  type L1SubmitterReadinessSummary,
  outRefKey,
  transactionAbsent,
  type UtxoOverrideLucid,
} from "./submitter.classify-l1-submitter-utxos.js";

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
  const spentOutRefs = await pruneInFlightSpends(lucid, utxos);
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
  const inFlightSpend = rememberInFlightSpend(lucid, txHash, signedCbor);
  if (options.awaitConfirmation !== false) {
    try {
      await lucid.awaitTxConfirmation(txHash, {
        ...(options.confirmationPollIntervalMs === undefined
          ? {}
          : { checkInterval: options.confirmationPollIntervalMs }),
      });
    } catch (error) {
      if (inFlightSpend !== undefined) {
        inFlightSpend.unconfirmed = true;
      }
      throw error;
    }
    await refreshL1SubmitterPlainAdaUtxos(lucid);
  }
  return txHash;
};

/**
 * Forgets the in-flight spends that no longer protect anything (see
 * `InFlightSpend`) and returns the inputs of the rest.
 */
const pruneInFlightSpends = async (
  lucid: Partial<UtxoOverrideLucid>,
  utxos: readonly UTxO[],
): Promise<ReadonlySet<string> | undefined> => {
  const inFlight = inFlightSpendsByLucid.get(lucid);
  if (inFlight === undefined) {
    return undefined;
  }
  const listedOutRefs = new Set(utxos.map(outRefKey));
  const currentSlot =
    typeof lucid.currentSlot === "function" ? lucid.currentSlot() : undefined;
  const spentOutRefs = new Set<string>();
  for (const [txHash, spend] of inFlight) {
    if (
      !spend.outRefs.some((outRef) => listedOutRefs.has(outRef)) ||
      (spend.ttlSlot !== undefined &&
        currentSlot !== undefined &&
        currentSlot >= spend.ttlSlot) ||
      (spend.unconfirmed && (await transactionAbsent(lucid, txHash)))
    ) {
      inFlight.delete(txHash);
      continue;
    }
    for (const outRef of spend.outRefs) {
      spentOutRefs.add(outRef);
    }
  }
  return spentOutRefs;
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

export const pollReadiness = async (
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

export const submitAutoFundPayment = async ({
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

export const readinessErrors = (
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

const rememberInFlightSpend = (
  lucid: object,
  txHash: string,
  txCbor: string,
): InFlightSpend | undefined => {
  const body = CML.Transaction.from_cbor_hex(txCbor).body();
  const inputs = body.inputs();
  const outRefs: string[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    outRefs.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  if (outRefs.length === 0) {
    return undefined;
  }
  const ttl = body.ttl();
  const spend: InFlightSpend = {
    outRefs,
    ...(ttl === undefined ? {} : { ttlSlot: Number(ttl) }),
    unconfirmed: false,
  };
  const inFlight =
    inFlightSpendsByLucid.get(lucid) ?? new Map<string, InFlightSpend>();
  inFlight.set(txHash, spend);
  inFlightSpendsByLucid.set(lucid, inFlight);
  return spend;
};

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
