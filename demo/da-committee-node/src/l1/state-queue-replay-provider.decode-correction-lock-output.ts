import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  httpUrl,
  json,
  splitOutRef,
  type StateQueueReplayFetch,
} from "./state-queue-replay-provider.open-rpc.js";
import { type CorrectionLockOutput } from "./state-queue-replay-provider.parse-transaction.js";

export const decodeCorrectionLockOutput = (
  value: unknown,
  transactionHash: string,
  outputIndex: number,
  correctionLockAddress: string,
  hubOraclePolicyId: string,
): CorrectionLockOutput | null => {
  const output = value as {
    transaction_id?: unknown;
    output_index?: unknown;
    address?: unknown;
    datum_type?: unknown;
    datum?: unknown;
    value?: { assets?: unknown };
  };
  if (
    output.transaction_id !== transactionHash ||
    output.output_index !== outputIndex ||
    output.address !== correctionLockAddress ||
    output.datum_type !== "inline" ||
    typeof output.datum !== "string" ||
    typeof output.value?.assets !== "object" ||
    output.value.assets === null ||
    Array.isArray(output.value.assets)
  ) {
    return null;
  }
  const assets = Object.entries(
    output.value.assets as Record<string, unknown>,
  ).map(([unit, quantity]) => [unit.replaceAll(".", ""), quantity] as const);
  if (
    assets.length !== 1 ||
    assets[0]?.[0] !== SDK.correctionLockUnit(hubOraclePolicyId) ||
    (assets[0]?.[1] !== 1 && assets[0]?.[1] !== "1")
  ) {
    return null;
  }
  try {
    return {
      outRef: `${transactionHash}#${outputIndex.toString()}`,
      datum: Data.from(output.datum, SDK.CorrectionLockDatum),
    };
  } catch {
    return null;
  }
};

export const fetchResolvedOutput = async (
  kupoUrl: string,
  reference: string,
  fetchImpl: StateQueueReplayFetch,
): Promise<unknown> => {
  const { txHash, index } = splitOutRef(reference);
  const body = await json(
    fetchImpl,
    `${httpUrl(kupoUrl)}/matches/${index.toString()}@${txHash}?resolve_hashes`,
  );
  if (!Array.isArray(body)) {
    throw new Error("Kupo resolved replay output is not an array");
  }
  const matches = body.filter(
    (item) =>
      (item as { transaction_id?: unknown }).transaction_id === txHash &&
      (item as { output_index?: unknown }).output_index === index,
  );
  if (matches.length !== 1) {
    throw new Error("Kupo resolved replay output is not unique");
  }
  return matches[0];
};

export const fetchCorrectionLockOutputs = async (
  kupoUrl: string,
  transactionHash: string,
  correctionLockAddress: string,
  hubOraclePolicyId: string,
  fetchImpl: StateQueueReplayFetch,
): Promise<readonly CorrectionLockOutput[]> => {
  const body = await json(
    fetchImpl,
    `${httpUrl(kupoUrl)}/matches/*@${transactionHash}?resolve_hashes`,
  );
  if (!Array.isArray(body)) {
    throw new Error("Kupo correction-lock replay outputs are not an array");
  }
  return body.flatMap((item) => {
    const index = (item as { output_index?: unknown }).output_index;
    if (
      typeof index !== "number" ||
      !Number.isSafeInteger(index) ||
      index < 0
    ) {
      return [];
    }
    const lock = decodeCorrectionLockOutput(
      item,
      transactionHash,
      index,
      correctionLockAddress,
      hubOraclePolicyId,
    );
    return lock === null ? [] : [lock];
  });
};

export const fraudProofAssetName = (
  value: unknown,
  fraudProofAddress: string,
  fraudProofPolicyId: string,
  targetHeaderHash: string,
): string | null => {
  const output = value as {
    address?: unknown;
    datum_type?: unknown;
    datum?: unknown;
    value?: { assets?: unknown };
  };
  if (
    output.address !== fraudProofAddress ||
    output.datum_type !== "inline" ||
    typeof output.datum !== "string" ||
    typeof output.value?.assets !== "object" ||
    output.value.assets === null ||
    Array.isArray(output.value.assets)
  ) {
    return null;
  }
  const names = Object.entries(
    output.value.assets as Record<string, unknown>,
  ).flatMap(([unit, quantity]) => {
    const normalized = unit.replaceAll(".", "");
    if (!normalized.startsWith(fraudProofPolicyId)) return [];
    const assetName = normalized.slice(fraudProofPolicyId.length);
    return /^[0-9a-f]{64}$/u.test(assetName) &&
      assetName.slice(8) === targetHeaderHash &&
      (quantity === 1 || quantity === "1")
      ? [assetName]
      : [null];
  });
  return names.length === 1 ? names[0] : null;
};
