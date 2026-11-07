import type { LucidEvolution } from "@lucid-evolution/lucid";

import { slotToUnixTimeForLucid } from "./lucid-time.js";

export const TRANSACTION_CONFIRMATION_METADATA_TIMEOUT_MS = 1_500;

export type TransactionConfirmationMetadata = {
  readonly slotNo: number;
  readonly blockHeaderHash?: string;
  readonly confirmations?: number;
  readonly confirmedAtMs: number;
};

export type TransactionConfirmationMetadataResolution =
  | {
      readonly type: "Available";
      readonly metadata: TransactionConfirmationMetadata;
    }
  | {
      readonly type: "Unavailable";
      readonly reason: string;
    };

const errorMessage = (cause: unknown): string =>
  cause instanceof Error ? cause.message : String(cause);

/**
 * Resolves provider-neutral inclusion metadata for a confirmed transaction.
 * Metadata unavailability never fails block confirmation itself; callers omit
 * the optional lag sample and preserve the canonical confirmation path.
 */
export const resolveTransactionConfirmationMetadata = async ({
  lucid,
  txHash,
  timeoutMs = TRANSACTION_CONFIRMATION_METADATA_TIMEOUT_MS,
}: {
  readonly lucid: LucidEvolution;
  readonly txHash: string;
  readonly timeoutMs?: number;
}): Promise<TransactionConfirmationMetadataResolution> => {
  if (!Number.isSafeInteger(timeoutMs) || timeoutMs <= 0) {
    return { type: "Unavailable", reason: "invalid_timeout" };
  }

  const controller = new AbortController();
  let timeout: NodeJS.Timeout | undefined;
  try {
    const status = await Promise.race([
      lucid.transactionStatus(txHash, { signal: controller.signal }),
      new Promise<never>((_, reject) => {
        timeout = setTimeout(() => {
          controller.abort();
          reject(
            new Error(
              `transaction status request timed out after ${timeoutMs.toString()}ms`,
            ),
          );
        }, timeoutMs);
      }),
    ]);
    if (status.status !== "confirmed") {
      return {
        type: "Unavailable",
        reason: `transaction_${status.status}`,
      };
    }
    const { slot, blockHash, confirmations } = status.confirmation;
    if (slot === undefined || !Number.isSafeInteger(slot) || slot < 0) {
      return {
        type: "Unavailable",
        reason: "confirmation_slot_unavailable",
      };
    }
    const confirmedAtMs = slotToUnixTimeForLucid(lucid, slot);
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
      metadata: {
        slotNo: slot,
        ...(blockHash === undefined ? {} : { blockHeaderHash: blockHash }),
        ...(confirmations === undefined ? {} : { confirmations }),
        confirmedAtMs,
      },
    };
  } catch (cause) {
    return {
      type: "Unavailable",
      reason: `transaction_status_error:${errorMessage(cause)}`,
    };
  } finally {
    if (timeout !== undefined) clearTimeout(timeout);
  }
};
