import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  resolveConfirmationDetectionLagMs,
  shouldObserveConfirmationDetectionLag,
} from "@/fibers/block-confirmation.js";
import {
  parseExactKupoConfirmationMatch,
  resolveKupoConfirmationMetadata,
} from "@/kupo-confirmation-metadata.js";
import { probeSubmittedTx } from "@/workers/confirm-block-commitments.js";
import {
  decideUnsubmittedPendingBlockRecovery,
  type PendingBlockConfirmation,
  pendingBlockHasSubmittedTx,
  shouldDeferUnsubmittedPendingBlockRecovery,
  shouldRunFullStateQueueConfirmationScan,
} from "@/workers/utils/confirm-block-commitments.js";

const pendingBlock = (
  submittedTxHash: PendingBlockConfirmation["submittedTxHash"],
): PendingBlockConfirmation => ({
  expectedHeaderHash: "aa".repeat(32),
  submittedTxHash,
  blockEndTimeMs: Date.now() + 60_000,
  updatedAtMs: Date.now(),
});

const customKupmiosLucid = ({
  kupoUrl = "http://kupo.test///",
  time = 1_000,
  slot = 1,
}: {
  readonly kupoUrl?: string;
  readonly time?: number;
  readonly slot?: number;
} = {}): LucidEvolution =>
  ({
    config: () => ({
      network: "Custom",
      provider: { kupoUrl, time, slot },
    }),
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

  it("strictly parses one exact Kupo output creation point", () => {
    const txHash = "ab".repeat(32);
    const blockHeaderHash = "cd".repeat(32);
    expect(
      parseExactKupoConfirmationMatch({
        body: [
          {
            transaction_id: txHash,
            output_index: 2,
            created_at: {
              slot_no: 5,
              header_hash: blockHeaderHash.toUpperCase(),
            },
          },
        ],
        txHash,
        outputIndex: 2,
      }),
    ).toEqual({ slotNo: 5, blockHeaderHash });
    expect(() =>
      parseExactKupoConfirmationMatch({
        body: [],
        txHash,
        outputIndex: 2,
      }),
    ).toThrow("must return one row");
    expect(() =>
      parseExactKupoConfirmationMatch({
        body: [
          {
            transaction_id: txHash,
            output_index: 3,
            created_at: { slot_no: 5, header_hash: blockHeaderHash },
          },
        ],
        txHash,
        outputIndex: 2,
      }),
    ).toThrow("does not match the requested output");
  });

  it("uses an injected exact Kupo match as authoritative confirmation time", async () => {
    const txHash = "ab".repeat(32);
    const requested: string[] = [];
    const resolution = await resolveKupoConfirmationMetadata({
      lucid: customKupmiosLucid(),
      txHash,
      outputIndex: 2,
      fetchImpl: async (input) => {
        requested.push(String(input));
        return {
          ok: true,
          status: 200,
          statusText: "OK",
          json: async () => [
            {
              transaction_id: txHash,
              output_index: 2,
              created_at: {
                slot_no: 5,
                header_hash: "cd".repeat(32),
              },
            },
          ],
        };
      },
    });
    expect(requested).toEqual([
      `http://kupo.test/matches/${encodeURIComponent(`2@${txHash}`)}`,
    ]);
    expect(resolution).toEqual({
      type: "Available",
      metadata: {
        slotNo: 5,
        blockHeaderHash: "cd".repeat(32),
        confirmedAtMs: 5_000,
      },
    });
  });

  it("returns unavailable for missing, invalid, and failed Kupo metadata", async () => {
    const txHash = "ab".repeat(32);
    const missingProvider = await resolveKupoConfirmationMetadata({
      lucid: {
        config: () => ({ network: "Custom", provider: {} }),
      } as unknown as LucidEvolution,
      txHash,
      outputIndex: 2,
    });
    expect(missingProvider).toEqual({
      type: "Unavailable",
      reason: "kupo_url_unavailable",
    });

    const invalid = await resolveKupoConfirmationMetadata({
      lucid: customKupmiosLucid(),
      txHash,
      outputIndex: 2,
      fetchImpl: async () => ({
        ok: true,
        status: 200,
        statusText: "OK",
        json: async () => [],
      }),
    });
    expect(invalid.type).toBe("Unavailable");
    if (invalid.type === "Unavailable") {
      expect(invalid.reason).toContain("must return one row");
    }

    const failed = await resolveKupoConfirmationMetadata({
      lucid: customKupmiosLucid(),
      txHash,
      outputIndex: 2,
      fetchImpl: () => Promise.reject(new Error("provider unavailable")),
    });
    expect(failed).toEqual({
      type: "Unavailable",
      reason: "kupo_metadata_error:provider unavailable",
    });
  });

  it("executes the real targeted awaitTx probe and fails closed on provider errors", async () => {
    const observed: Array<readonly [string, number]> = [];
    const confirmed = await Effect.runPromise(
      probeSubmittedTx(
        {
          awaitTx: async (txHash, pollMs) => {
            if (pollMs === undefined) {
              throw new Error("Targeted confirmation probe omitted pollMs");
            }
            observed.push([txHash, pollMs]);
            return true;
          },
        },
        "ab".repeat(32),
      ),
    );
    const providerFailure = await Effect.runPromise(
      probeSubmittedTx(
        {
          awaitTx: () => Promise.reject(new Error("provider unavailable")),
        },
        "cd".repeat(32),
      ),
    );
    expect(confirmed).toBe(true);
    expect(providerFailure).toBe(false);
    expect(observed).toEqual([["ab".repeat(32), 250]]);
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
