import type { LucidEvolution } from "@lucid-evolution/lucid";

import { kupmiosKupoUrlFromLucid } from "@/kupmios.js";
import { slotToUnixTimeForLucid } from "@/lucid-time.js";

export const KUPO_CONFIRMATION_METADATA_TIMEOUT_MS = 1_500;

export type KupoConfirmationMetadata = {
  readonly slotNo: number;
  readonly blockHeaderHash: string;
  readonly confirmedAtMs: number;
};

export type KupoConfirmationMetadataResolution =
  | {
      readonly type: "Available";
      readonly metadata: KupoConfirmationMetadata;
    }
  | {
      readonly type: "Unavailable";
      readonly reason: string;
    };

type FetchResponse = Pick<Response, "ok" | "status" | "statusText" | "json">;

export type KupoMetadataFetch = (
  input: string | URL,
  init?: RequestInit,
) => Promise<FetchResponse>;

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

/**
 * Parses one exact Kupo output-reference match. The row identity and creation
 * point are authenticated structurally before their slot can become metric
 * evidence.
 */
export const parseExactKupoConfirmationMatch = ({
  body,
  txHash,
  outputIndex,
}: {
  readonly body: unknown;
  readonly txHash: string;
  readonly outputIndex: number;
}): Omit<KupoConfirmationMetadata, "confirmedAtMs"> => {
  if (!Array.isArray(body) || body.length !== 1) {
    throw new Error("Kupo exact output-reference query must return one row");
  }
  const row = body[0];
  if (!isRecord(row)) throw new Error("Kupo match row must be an object");
  if (row.transaction_id !== txHash || row.output_index !== outputIndex) {
    throw new Error("Kupo match row does not match the requested output");
  }
  if (!isRecord(row.created_at)) {
    throw new Error("Kupo match row is missing created_at metadata");
  }
  const slotNo = row.created_at.slot_no;
  const blockHeaderHash = row.created_at.header_hash;
  if (
    typeof slotNo !== "number" ||
    !Number.isSafeInteger(slotNo) ||
    slotNo < 0
  ) {
    throw new Error("Kupo match created_at.slot_no is invalid");
  }
  if (
    typeof blockHeaderHash !== "string" ||
    !/^[0-9a-f]{64}$/i.test(blockHeaderHash)
  ) {
    throw new Error("Kupo match created_at.header_hash is invalid");
  }
  return { slotNo, blockHeaderHash: blockHeaderHash.toLowerCase() };
};

const errorMessage = (cause: unknown): string =>
  cause instanceof Error ? cause.message : String(cause);

const bounded = async <A>(
  operation: (signal: AbortSignal) => Promise<A>,
  timeoutMs: number,
): Promise<A> => {
  const controller = new AbortController();
  let timeout: NodeJS.Timeout | undefined;
  const timeoutPromise = new Promise<never>((_, reject) => {
    timeout = setTimeout(() => {
      controller.abort();
      reject(new Error(`Kupo metadata request timed out after ${timeoutMs}ms`));
    }, timeoutMs);
  });
  try {
    return await Promise.race([operation(controller.signal), timeoutPromise]);
  } finally {
    if (timeout !== undefined) clearTimeout(timeout);
  }
};

/**
 * Resolves the exact Cardano block creation time for a canonical UTxO through
 * Kupo. Every unavailable/error case is data, never a confirmation-path
 * failure: callers omit the lag sample and continue confirming the block.
 */
export const resolveKupoConfirmationMetadata = async ({
  lucid,
  txHash,
  outputIndex,
  fetchImpl = fetch,
  timeoutMs = KUPO_CONFIRMATION_METADATA_TIMEOUT_MS,
}: {
  readonly lucid: LucidEvolution;
  readonly txHash: string;
  readonly outputIndex: number;
  readonly fetchImpl?: KupoMetadataFetch;
  readonly timeoutMs?: number;
}): Promise<KupoConfirmationMetadataResolution> => {
  const kupoUrl = kupmiosKupoUrlFromLucid(lucid);
  if (kupoUrl === undefined) {
    return { type: "Unavailable", reason: "kupo_url_unavailable" };
  }
  if (!Number.isSafeInteger(timeoutMs) || timeoutMs <= 0) {
    return { type: "Unavailable", reason: "invalid_timeout" };
  }
  const pattern = encodeURIComponent(`${outputIndex.toString()}@${txHash}`);
  try {
    const body = await bounded(async (signal) => {
      const response = await fetchImpl(`${kupoUrl}/matches/${pattern}`, {
        signal,
        headers: { accept: "application/json" },
      });
      if (!response.ok) {
        throw new Error(
          `Kupo returned HTTP ${response.status.toString()} ${response.statusText}`,
        );
      }
      return response.json() as Promise<unknown>;
    }, timeoutMs);
    const match = parseExactKupoConfirmationMatch({
      body,
      txHash,
      outputIndex,
    });
    const confirmedAtMs = slotToUnixTimeForLucid(lucid, match.slotNo);
    if (
      confirmedAtMs === undefined ||
      !Number.isSafeInteger(confirmedAtMs) ||
      confirmedAtMs < 0
    ) {
      return {
        type: "Unavailable",
        reason: "slot_time_conversion_unavailable",
      };
    }
    return {
      type: "Available",
      metadata: { ...match, confirmedAtMs },
    };
  } catch (cause) {
    return {
      type: "Unavailable",
      reason: `kupo_metadata_error:${errorMessage(cause)}`,
    };
  }
};
