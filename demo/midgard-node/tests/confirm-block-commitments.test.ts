import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  resolveConfirmationDetectionLagMs,
  shouldObserveConfirmationDetectionLag,
} from "../src/fibers/block-confirmation.js";
import { resolveTransactionConfirmationMetadata } from "../src/transaction-confirmation-metadata.js";
import { probeSubmittedTx } from "../src/workers/confirm-block-commitments.js";
import {
  decideUnsubmittedPendingBlockRecovery,
  type PendingBlockConfirmation,
  pendingBlockHasSubmittedTx,
  shouldDeferUnsubmittedPendingBlockRecovery,
  shouldRunFullStateQueueConfirmationScan,
} from "../src/workers/utils/confirm-block-commitments.js";

const pendingBlock = (
  submittedTxHash: PendingBlockConfirmation["submittedTxHash"],
): PendingBlockConfirmation => ({
  expectedHeaderHash: "aa".repeat(32),
  submittedTxHash,
  blockEndTimeMs: Date.now() + 60_000,
  updatedAtMs: Date.now(),
});

const confirmationLucid = ({
  status = "confirmed",
  slot = 5,
}: {
  readonly status?: "confirmed" | "not_found" | "pending" | "failed";
  readonly slot?: number;
} = {}): LucidEvolution =>
  ({
    transactionStatus: async (txHash: string) =>
      status === "confirmed"
        ? {
            status,
            txHash,
            confirmation: {
              txHash,
              slot,
              blockHash: "cd".repeat(32),
              confirmations: 7,
            },
          }
        : { status, txHash },
    slotToUnixTime: (value: number) => value * 1_000,
  }) as unknown as LucidEvolution;

describe("confirm-block-commitments utilities", () => {
  it("resolves confirmation lag in milliseconds and clamps negative clock skew", () => {
    expect(
      resolveConfirmationDetectionLagMs({
        confirmationSlotUnixMs: 10_000,
        availableConfirmedSetAtMs: 10_750,
      }),
    ).toBe(750);
    expect(
      resolveConfirmationDetectionLagMs({
        confirmationSlotUnixMs: 10_001,
        availableConfirmedSetAtMs: 10_000,
      }),
    ).toBe(0);
    expect(shouldObserveConfirmationDetectionLag(null)).toBe(true);
    expect(shouldObserveConfirmationDetectionLag(10_000n)).toBe(false);
  });

  it("uses provider-neutral transaction status as confirmation evidence", async () => {
    const txHash = "ab".repeat(32);
    const resolution = await resolveTransactionConfirmationMetadata({
      lucid: confirmationLucid(),
      txHash,
    });
    expect(resolution).toEqual({
      type: "Available",
      metadata: {
        slotNo: 5,
        blockHeaderHash: "cd".repeat(32),
        confirmations: 7,
        confirmedAtMs: 5_000,
      },
    });
  });

  it("returns unavailable for non-confirmed, incomplete, and failed status", async () => {
    const txHash = "ab".repeat(32);
    const notFound = await resolveTransactionConfirmationMetadata({
      lucid: confirmationLucid({ status: "not_found" }),
      txHash,
    });
    expect(notFound).toEqual({
      type: "Unavailable",
      reason: "transaction_not_found",
    });

    const invalid = await resolveTransactionConfirmationMetadata({
      lucid: confirmationLucid({ slot: -1 }),
      txHash,
    });
    expect(invalid).toEqual({
      type: "Unavailable",
      reason: "confirmation_slot_unavailable",
    });

    const failed = await resolveTransactionConfirmationMetadata({
      lucid: {
        transactionStatus: () =>
          Promise.reject(new Error("provider unavailable")),
      } as unknown as LucidEvolution,
      txHash,
    });
    expect(failed).toEqual({
      type: "Unavailable",
      reason: "transaction_status_error:provider unavailable",
    });
  });

  it("executes the provider-neutral targeted confirmation probe and fails closed on provider errors", async () => {
    const observed: Array<
      readonly [
        string,
        { readonly timeout?: number; readonly checkInterval?: number },
      ]
    > = [];
    const confirmed = await Effect.runPromise(
      probeSubmittedTx(
        {
          awaitTxConfirmation: async (txHash, options) => {
            if (options?.checkInterval === undefined) {
              throw new Error("Targeted confirmation probe omitted options");
            }
            observed.push([txHash, options]);
            return { txHash };
          },
        },
        "ab".repeat(32),
      ),
    );
    const providerFailure = await Effect.runPromise(
      probeSubmittedTx(
        {
          awaitTxConfirmation: () =>
            Promise.reject(new Error("provider unavailable")),
        },
        "cd".repeat(32),
      ),
    );
    expect(confirmed).toBe(true);
    expect(providerFailure).toBe(false);
    expect(observed).toEqual([
      ["ab".repeat(32), { timeout: 1_500, checkInterval: 250 }],
    ]);
  });

  it("classifies pending journals without submitted tx hashes as unsubmitted", () => {
    expect(pendingBlockHasSubmittedTx(pendingBlock(""))).toBe(false);
    expect(pendingBlockHasSubmittedTx(pendingBlock("bb".repeat(32)))).toBe(
      true,
    );
  });

  it("uses the targeted tx probe and retains periodic canonical-advance scans", () => {
    expect(
      shouldRunFullStateQueueConfirmationScan({
        targetedTxConfirmed: true,
        pendingAgeMs: 7_000,
        unconfirmedBlockMaxAgeMs: 180_000,
        validityExpired: false,
      }),
    ).toBe(true);
    expect(
      shouldRunFullStateQueueConfirmationScan({
        targetedTxConfirmed: false,
        pendingAgeMs: 7_000,
        unconfirmedBlockMaxAgeMs: 180_000,
        validityExpired: false,
      }),
    ).toBe(false);
    expect(
      shouldRunFullStateQueueConfirmationScan({
        targetedTxConfirmed: false,
        pendingAgeMs: 20_500,
        unconfirmedBlockMaxAgeMs: 180_000,
        validityExpired: false,
      }),
    ).toBe(true);
    expect(
      shouldRunFullStateQueueConfirmationScan({
        targetedTxConfirmed: false,
        pendingAgeMs: 7_000,
        unconfirmedBlockMaxAgeMs: 180_000,
        validityExpired: true,
      }),
    ).toBe(true);
  });

  it("defers unsubmitted pending recovery until the block end plus grace has elapsed", () => {
    const block = pendingBlock("");
    const recoveryGraceMs = 30_000;

    expect(
      shouldDeferUnsubmittedPendingBlockRecovery({
        pendingBlock: block,
        nowMs: block.blockEndTimeMs + recoveryGraceMs,
        recoveryGraceMs,
      }),
    ).toBe(true);
    expect(
      shouldDeferUnsubmittedPendingBlockRecovery({
        pendingBlock: block,
        nowMs: block.blockEndTimeMs + recoveryGraceMs + 1,
        recoveryGraceMs,
      }),
    ).toBe(false);
  });

  it("recovers canonical unsubmitted snapshots before considering stale recovery", () => {
    const block = pendingBlock("");
    const recoveryGraceMs = 30_000;

    expect(
      decideUnsubmittedPendingBlockRecovery({
        canonicalMatchFound: true,
        pendingBlock: block,
        nowMs: block.blockEndTimeMs + recoveryGraceMs + 1,
        recoveryGraceMs,
      }),
    ).toBe("recover_canonical");
    expect(
      decideUnsubmittedPendingBlockRecovery({
        canonicalMatchFound: false,
        pendingBlock: block,
        nowMs: block.blockEndTimeMs + recoveryGraceMs,
        recoveryGraceMs,
      }),
    ).toBe("defer");
    expect(
      decideUnsubmittedPendingBlockRecovery({
        canonicalMatchFound: false,
        pendingBlock: block,
        nowMs: block.blockEndTimeMs + recoveryGraceMs + 1,
        recoveryGraceMs,
      }),
    ).toBe("recover_stale");
  });
});
