import { type StateQueueTransitionNode } from "@al-ft/midgard-sdk";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  DEFAULT_TX_ORDER_CARRIAGE_TIMEOUT_MS,
  type FetchLike,
  type KupoSpend,
  normalizeKupoHttpUrl,
} from "../l1-tx-order-carriage.js";
import {
  HEX_32,
  type Tip,
} from "./state-queue-correction-observer.parse-state-queue-correction-observer-state.js";
import {
  queryOgmios,
  TIP_READ_ATTEMPTS,
} from "./state-queue-correction-observer.reconcile-state-queue-correction-observer.js";

const fetchTipPoint = async (
  ogmiosUrl: string,
  fetchImpl: FetchLike,
): Promise<Omit<Tip, "blockNo">> => {
  const point = (await queryOgmios(
    ogmiosUrl,
    fetchImpl,
    "queryNetwork/tip",
  )) as { id?: unknown; slot?: unknown } | undefined;
  if (
    typeof point?.id !== "string" ||
    !HEX_32.test(point.id) ||
    typeof point.slot !== "number" ||
    !Number.isSafeInteger(point.slot) ||
    point.slot < 0
  ) {
    throw new Error("Ogmios tip query returned no canonical point");
  }
  return { blockHash: point.id, slot: point.slot };
};

/** Ogmios v6 answers queryNetwork/tip with a point only; the height comes from
 * queryNetwork/blockHeight and is bound to the tip only when two tip reads
 * bracketing it agree. A chain that keeps moving fails after a few tries. */
export const fetchTip = async (
  ogmiosUrl: string,
  fetchImpl: FetchLike,
): Promise<Tip> => {
  for (let attempt = 0; attempt < TIP_READ_ATTEMPTS; attempt += 1) {
    const before = await fetchTipPoint(ogmiosUrl, fetchImpl);
    const height = await queryOgmios(
      ogmiosUrl,
      fetchImpl,
      "queryNetwork/blockHeight",
    );
    if (
      typeof height !== "number" ||
      !Number.isSafeInteger(height) ||
      height < 0
    ) {
      throw new Error("Ogmios block height query returned no block height");
    }
    const after = await fetchTipPoint(ogmiosUrl, fetchImpl);
    if (before.blockHash === after.blockHash && before.slot === after.slot) {
      return { ...before, blockNo: height };
    }
  }
  throw new Error(
    `Ogmios tip moved during each of ${TIP_READ_ATTEMPTS.toString()} block height reads`,
  );
};

/** Bound on each Kupo and Ogmios HTTP read made here. Every one is a point
 * query (the tip, one output, one transaction's outputs), never a ledger
 * scan, so it takes the same bound as the tx-order carriage reads. */
export const STATE_QUEUE_CORRECTION_REQUEST_TIMEOUT_MS =
  DEFAULT_TX_ORDER_CARRIAGE_TIMEOUT_MS;

/** A hung Kupo or Ogmios fails the read after `timeoutMs` instead of wedging
 * the fiber that awaits it; a caller's own signal still applies. */
export const withRequestTimeout =
  (fetchImpl: FetchLike, timeoutMs: number): FetchLike =>
  (url, init) => {
    const timeout = AbortSignal.timeout(timeoutMs);
    return fetchImpl(url, {
      ...init,
      signal:
        init?.signal === undefined || init.signal === null
          ? timeout
          : AbortSignal.any([init.signal, timeout]),
    });
  };

/** Canonical local tip shared by operational transaction reconciliation. */
export const readLocalOgmiosTip = (ogmiosUrl: string): Promise<Tip> =>
  fetchTip(
    ogmiosUrl,
    withRequestTimeout(fetch, STATE_QUEUE_CORRECTION_REQUEST_TIMEOUT_MS),
  );

export const sameSpend = (left: KupoSpend, right: KupoSpend): boolean =>
  left.transactionId === right.transactionId &&
  left.point.headerHash === right.point.headerHash &&
  left.point.slot === right.point.slot;

export type HistoricalQueueOutput = Readonly<{
  node: StateQueueTransitionNode;
  nextHeaderHash: string | null;
}>;

export type HistoricalCorrectionLockOutput = Readonly<{
  outRef: string;
  datum: SDK.CorrectionLockDatum;
}>;

export const decodeKupoCorrectionLockMatch = ({
  candidate,
  expectedTransactionHash,
  expectedOutputIndex,
  correctionLockAddress,
  hubOraclePolicyId,
}: {
  readonly candidate: unknown;
  readonly expectedTransactionHash: string;
  readonly expectedOutputIndex: number;
  readonly correctionLockAddress: string;
  readonly hubOraclePolicyId: string;
}): HistoricalCorrectionLockOutput | null => {
  const match = candidate as {
    transaction_id?: unknown;
    output_index?: unknown;
    address?: unknown;
    datum_type?: unknown;
    datum?: unknown;
    value?: { assets?: unknown };
  };
  if (
    match.transaction_id !== expectedTransactionHash ||
    match.output_index !== expectedOutputIndex ||
    typeof match.value !== "object" ||
    match.value === null ||
    typeof match.value.assets !== "object" ||
    match.value.assets === null ||
    Array.isArray(match.value.assets)
  ) {
    return null;
  }
  const nativeAssets = Object.entries(
    match.value.assets as Record<string, unknown>,
  ).map(
    ([rawUnit, quantity]) => [rawUnit.replaceAll(".", ""), quantity] as const,
  );
  const lockUnit = SDK.correctionLockUnit(hubOraclePolicyId);
  if (
    match.address !== correctionLockAddress ||
    nativeAssets.length !== 1 ||
    nativeAssets[0]?.[0] !== lockUnit ||
    (nativeAssets[0]?.[1] !== 1 && nativeAssets[0]?.[1] !== "1") ||
    match.datum_type !== "inline" ||
    typeof match.datum !== "string"
  ) {
    return null;
  }
  try {
    return {
      outRef: `${expectedTransactionHash}#${expectedOutputIndex.toString()}`,
      datum: Data.from(match.datum, SDK.CorrectionLockDatum),
    };
  } catch {
    return null;
  }
};

export const fetchKupoResolvedOutput = async ({
  kupoUrl,
  reference,
  fetchImpl,
}: {
  readonly kupoUrl: string;
  readonly reference: { readonly txHash: string; readonly outputIndex: number };
  readonly fetchImpl: FetchLike;
}): Promise<unknown> => {
  const url = `${normalizeKupoHttpUrl(kupoUrl).replace(/\/+$/u, "")}/matches/${reference.outputIndex.toString()}@${reference.txHash}?resolve_hashes`;
  const response = await fetchImpl(url);
  const body = await response.text();
  if (!response.ok) {
    throw new Error(
      `Kupo resolved-output query failed with HTTP ${response.status.toString()}: ${body.slice(0, 256)}`,
    );
  }
  let decoded: unknown;
  try {
    decoded = JSON.parse(body) as unknown;
  } catch (cause) {
    throw new Error("Kupo resolved-output query returned malformed JSON", {
      cause,
    });
  }
  if (!Array.isArray(decoded)) {
    throw new Error("Kupo resolved-output query did not return an array");
  }
  const matches = decoded.filter(
    (candidate) =>
      (candidate as { transaction_id?: unknown }).transaction_id ===
        reference.txHash &&
      (candidate as { output_index?: unknown }).output_index ===
        reference.outputIndex,
  );
  if (matches.length !== 1) {
    throw new Error(
      "Kupo resolved-output query did not return one exact match",
    );
  }
  return matches[0];
};

export const fetchKupoTransactionCorrectionLockOutputs = async ({
  kupoUrl,
  transactionHash,
  correctionLockAddress,
  hubOraclePolicyId,
  fetchImpl,
}: {
  readonly kupoUrl: string;
  readonly transactionHash: string;
  readonly correctionLockAddress: string;
  readonly hubOraclePolicyId: string;
  readonly fetchImpl: FetchLike;
}): Promise<readonly HistoricalCorrectionLockOutput[]> => {
  const url = `${normalizeKupoHttpUrl(kupoUrl).replace(/\/+$/u, "")}/matches/*@${transactionHash}?resolve_hashes`;
  const response = await fetchImpl(url);
  const body = await response.text();
  if (!response.ok) {
    throw new Error(
      `Kupo correction-lock output query failed with HTTP ${response.status.toString()}: ${body.slice(0, 256)}`,
    );
  }
  let decoded: unknown;
  try {
    decoded = JSON.parse(body) as unknown;
  } catch (cause) {
    throw new Error(
      "Kupo correction-lock output query returned malformed JSON",
      { cause },
    );
  }
  if (!Array.isArray(decoded)) {
    throw new Error(
      "Kupo correction-lock output query did not return an array",
    );
  }
  return decoded.flatMap((candidate) => {
    const index = (candidate as { output_index?: unknown }).output_index;
    if (
      typeof index !== "number" ||
      !Number.isSafeInteger(index) ||
      index < 0
    ) {
      return [];
    }
    const lock = decodeKupoCorrectionLockMatch({
      candidate,
      expectedTransactionHash: transactionHash,
      expectedOutputIndex: index,
      correctionLockAddress,
      hubOraclePolicyId,
    });
    return lock === null ? [] : [lock];
  });
};

export const fraudProofAssetNameFromResolvedMatch = ({
  candidate,
  fraudProofAddress,
  fraudProofPolicyId,
  targetHeaderHash,
}: {
  readonly candidate: unknown;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly targetHeaderHash: string;
}): string | null => {
  const match = candidate as {
    address?: unknown;
    datum_type?: unknown;
    datum?: unknown;
    value?: { assets?: unknown };
  };
  if (
    match.address !== fraudProofAddress ||
    match.datum_type !== "inline" ||
    typeof match.datum !== "string" ||
    typeof match.value !== "object" ||
    match.value === null ||
    typeof match.value.assets !== "object" ||
    match.value.assets === null ||
    Array.isArray(match.value.assets)
  ) {
    return null;
  }
  const proofAssets = Object.entries(
    match.value.assets as Record<string, unknown>,
  ).flatMap(([rawUnit, quantity]) => {
    const unit = rawUnit.replaceAll(".", "");
    const assetName = unit.startsWith(fraudProofPolicyId)
      ? unit.slice(fraudProofPolicyId.length)
      : null;
    return assetName !== null &&
      /^[0-9a-f]{64}$/u.test(assetName) &&
      assetName.slice(8) === targetHeaderHash &&
      (quantity === 1 || quantity === "1")
      ? [assetName]
      : unit.startsWith(fraudProofPolicyId)
        ? [null]
        : [];
  });
  return proofAssets.length === 1 ? proofAssets[0] : null;
};
