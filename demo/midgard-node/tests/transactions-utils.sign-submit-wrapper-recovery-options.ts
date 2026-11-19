import "./transactions-utils.validity-window-submit-recovery.js";

import { Emulator } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  awaitSubmittedTransactionConfirmation,
  handleSignSubmitNoConfirmation,
  signSubmitTransaction,
} from "../src/transactions/utils.await-submitted-transaction-confirmation.js";
import { runWithoutFollower, TEST_INTENT } from "./helpers/intent-journal.js";
import {
  fakeSignBuilder,
  fakeWrapperLucid,
} from "./transactions-utils.fake-sign-builder.js";
import {
  expectNoInlineSubmitDefer,
  signedTxCbor,
} from "./transactions-utils.parse-outside-validity-interval-details.js";

describe("sign/submit wrapper recovery options", () => {
  it("advances the Lucid emulator before exact status confirmation", async () => {
    const provider = new Emulator([]);
    const awaitTx = vi.fn(async () => true);
    const transactionStatus = vi.fn(async (txHash: string) => ({
      status: "confirmed" as const,
      txHash,
      confirmation: { txHash },
    }));

    const txHash = await Effect.runPromise(
      awaitSubmittedTransactionConfirmation(
        {
          config: () => ({ provider }),
          awaitTx,
          transactionStatus,
          wallet: () => ({}),
        } as never,
        { txHash: "tx-emulator-confirmation", signedTxCbor: "00" },
        {
          confirmationTimeoutMs: 120_000,
          confirmationRetries: 0,
          confirmationPollIntervalMs: 100,
        },
      ),
    );

    expect(txHash).toBe("tx-emulator-confirmation");
    expect(awaitTx).toHaveBeenCalledWith("tx-emulator-confirmation", 100);
    expect(awaitTx.mock.invocationCallOrder[0]).toBeLessThan(
      transactionStatus.mock.invocationCallOrder[0] ?? 0,
    );
    expect(provider.slot).toBe(0);
  });

  it("spaces transient provider status reads and recovers the exact submitted tx", async () => {
    vi.useFakeTimers();
    try {
      let attempts = 0;
      const transactionStatus = vi.fn(async (txHash: string) => {
        attempts += 1;
        if (attempts <= 2) {
          throw new Error("transient kupo transport error");
        }
        return { status: "confirmed" as const, txHash, confirmation: {} };
      });
      const confirmation = Effect.runPromise(
        awaitSubmittedTransactionConfirmation(
          {
            config: () => ({ provider: undefined }),
            transactionStatus,
            wallet: () => ({}),
          } as never,
          {
            txHash: "tx-transient-confirmation-provider",
            signedTxCbor: "00",
          },
          {
            confirmationTimeoutMs: 120_000,
            confirmationRetries: 0,
            confirmationPollIntervalMs: 100,
          },
        ),
      );

      await vi.advanceTimersByTimeAsync(0);
      expect(transactionStatus).toHaveBeenCalledTimes(1);
      await vi.advanceTimersByTimeAsync(99);
      expect(transactionStatus).toHaveBeenCalledTimes(1);
      await vi.advanceTimersByTimeAsync(1);
      expect(transactionStatus).toHaveBeenCalledTimes(2);
      await vi.advanceTimersByTimeAsync(99);
      expect(transactionStatus).toHaveBeenCalledTimes(2);
      await vi.advanceTimersByTimeAsync(1);
      expect(transactionStatus).toHaveBeenCalledTimes(3);

      await expect(confirmation).resolves.toBe(
        "tx-transient-confirmation-provider",
      );
      expect(transactionStatus).toHaveBeenCalledTimes(3);
      expect(transactionStatus).toHaveBeenCalledWith(
        "tx-transient-confirmation-provider",
        { signal: expect.any(AbortSignal) },
      );
    } finally {
      vi.useRealTimers();
    }
  });

  it("allows an exact submitted transaction to confirm after the legacy 90-second ceiling", async () => {
    vi.useFakeTimers();
    try {
      const startedAt = Date.now();
      const transactionStatus = vi.fn(async (txHash: string) =>
        Date.now() - startedAt >= 100_000
          ? { status: "confirmed" as const, txHash, confirmation: {} }
          : { status: "not_found" as const, txHash },
      );
      let settled = false;
      const confirmation = Effect.runPromise(
        awaitSubmittedTransactionConfirmation(
          {
            config: () => ({ provider: undefined }),
            transactionStatus,
            wallet: () => ({}),
          } as never,
          { txHash: "tx-long-confirmation", signedTxCbor: "00" },
          {
            confirmationTimeoutMs: 120_000,
            confirmationRetries: 0,
            confirmationPollIntervalMs: 1_000,
          },
        ),
      ).finally(() => {
        settled = true;
      });

      await vi.advanceTimersByTimeAsync(90_001);
      expect(settled).toBe(false);
      await vi.advanceTimersByTimeAsync(10_000);

      await expect(confirmation).resolves.toBe("tx-long-confirmation");
    } finally {
      vi.useRealTimers();
    }
  });

  it("still fails when the configured exact transaction confirmation deadline expires", async () => {
    vi.useFakeTimers();
    try {
      const transactionStatus = vi.fn(async (txHash: string) => ({
        status: "not_found" as const,
        txHash,
      }));
      const confirmation = Effect.runPromise(
        Effect.either(
          awaitSubmittedTransactionConfirmation(
            {
              config: () => ({ provider: undefined }),
              transactionStatus,
              wallet: () => ({}),
            } as never,
            { txHash: "tx-confirmation-timeout", signedTxCbor: "00" },
            {
              confirmationTimeoutMs: 60_000,
              confirmationRetries: 1,
              confirmationPollIntervalMs: 1_000,
            },
          ),
        ),
      );

      await vi.advanceTimersByTimeAsync(120_000);
      const result = await confirmation;

      expect(result._tag).toBe("Left");
      if (result._tag !== "Left") {
        throw new Error("expected confirmation timeout");
      }
      expect(result.left).toMatchObject({
        _tag: "TxConfirmError",
        txHash: "tx-confirmation-timeout",
      });
      expect(String(result.left.cause)).toContain(
        "Timed out waiting for transaction tx-confirmation-timeout",
      );
      expect(transactionStatus).toHaveBeenCalledTimes(121);
    } finally {
      vi.useRealTimers();
    }
  });

  // The forwarded slot source decides: short of the bound plus margin, refused.
  it.each([
    { tipSlot: 7, submitted: false },
    { tipSlot: 12, submitted: true },
  ])(
    "forwards submit recovery options through signSubmitTransaction: tip slot $tipSlot",
    async ({ tipSlot, submitted }) => {
      const sleeps: number[] = [];
      const submitProgram = vi.fn(() => Effect.void);
      const signed = {
        toCBOR: () =>
          signedTxCbor({ invalidBeforeSlot: 10, invalidHereafterSlot: 20 }),
        submitProgram,
      };

      const result = await runWithoutFollower(
        Effect.either(
          signSubmitTransaction(
            fakeWrapperLucid() as never,
            fakeSignBuilder(signed),
            TEST_INTENT,
            {
              label: "operator registration",
              slotSnapshot: () =>
                Effect.succeed({
                  source: "test",
                  currentSlot: tipSlot,
                  observedAtMs: 1_779_150_000_000,
                  slotLengthMs: 1_000,
                }),
              sleep: (milliseconds) =>
                Effect.sync(() => sleeps.push(milliseconds)),
            },
          ),
        ),
      );

      expect(result._tag).toBe(submitted ? "Right" : "Left");
      expect(submitProgram).toHaveBeenCalledTimes(submitted ? 1 : 0);
      expect(sleeps).toEqual([]);
      if (!submitted)
        expect(String(result._tag === "Left" ? result.left : "")).toContain(
          `was built before its lower bound: the L1 ledger tip (slot ${tipSlot.toString()}, test)`,
        );
    },
  );

  it("forwards submit recovery options through no-confirmation wrapper", async () => {
    const submitProgram = vi.fn(() => Effect.void);
    const signed = {
      toCBOR: () =>
        signedTxCbor({ invalidBeforeSlot: 10, invalidHereafterSlot: 20 }),
      submitProgram,
    };

    const result = await runWithoutFollower(
      Effect.either(
        handleSignSubmitNoConfirmation(
          fakeWrapperLucid() as never,
          fakeSignBuilder(signed),
          TEST_INTENT,
          {
            requireSlotForBoundedTx: true,
            slotSnapshot: () => Effect.fail(new Error("slot unavailable")),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    expect(submitProgram).not.toHaveBeenCalled();
  });

  it("preserves no-inline submit defer through signSubmitTransaction", async () => {
    const submitProgram = vi.fn(() => Effect.void);
    const signed = {
      toCBOR: () =>
        signedTxCbor({ invalidBeforeSlot: 10, invalidHereafterSlot: 20 }),
      submitProgram,
    };

    const result = await runWithoutFollower(
      Effect.either(
        signSubmitTransaction(
          fakeWrapperLucid() as never,
          fakeSignBuilder(signed),
          TEST_INTENT,
          {
            inlineWaitPolicy: "defer_positive_wait",
            noInlineSubmitDefer: {
              key: "sign-submit-key",
              dependencyKey: "dep-sign-submit",
              invalidationKey: "inv-sign-submit",
            },
            slotSnapshot: () =>
              Effect.succeed({
                source: "test",
                currentSlot: 7,
                observedAtMs: 1_779_150_000_000,
                slotLengthMs: 1_000,
              }),
            sleep: (milliseconds) =>
              Effect.sync(() => {
                throw new Error(`unexpected sleep ${milliseconds.toString()}`);
              }),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    if (result._tag !== "Left") {
      throw new Error("expected no-inline defer");
    }
    expect(expectNoInlineSubmitDefer(result.left)).toMatchObject({
      kind: "pre_submit_validity",
      key: "sign-submit-key",
      txHash: "tx-wrapper",
      currentSlot: 7,
      targetSlot: 12,
      waitMs: 5_000,
    });
    expect(submitProgram).not.toHaveBeenCalled();
  });

  it("exposes no-inline no-confirmation defers as a result union", async () => {
    const submitProgram = vi.fn(() => Effect.void);
    const signed = {
      toCBOR: () =>
        signedTxCbor({ invalidBeforeSlot: 10, invalidHereafterSlot: 20 }),
      submitProgram,
    };

    const result = await runWithoutFollower(
      handleSignSubmitNoConfirmation(
        fakeWrapperLucid() as never,
        fakeSignBuilder(signed),
        TEST_INTENT,
        {
          inlineWaitPolicy: "defer_positive_wait",
          noInlineSubmitDefer: {
            key: "no-confirm-key",
            dependencyKey: "dep-no-confirm",
            invalidationKey: "inv-no-confirm",
          },
          slotSnapshot: () =>
            Effect.succeed({
              source: "test",
              currentSlot: 7,
              observedAtMs: 1_779_150_000_000,
              slotLengthMs: 1_000,
            }),
          sleep: (milliseconds) =>
            Effect.sync(() => {
              throw new Error(`unexpected sleep ${milliseconds.toString()}`);
            }),
        },
      ),
    );

    expect(result.status).toBe("deferred");
    if (result.status !== "deferred") {
      throw new Error("expected deferred result");
    }
    expect(result.defer).toMatchObject({
      kind: "pre_submit_validity",
      key: "no-confirm-key",
      txHash: "tx-wrapper",
      currentSlot: 7,
      targetSlot: 12,
      waitMs: 5_000,
    });
    expect(submitProgram).not.toHaveBeenCalled();
  });

  it("exposes no-inline no-confirmation submissions as a result union", async () => {
    const submitProgram = vi.fn(() => Effect.void);
    const signed = {
      toCBOR: () =>
        signedTxCbor({ invalidBeforeSlot: 10, invalidHereafterSlot: 20 }),
      submitProgram,
    };

    const result = await runWithoutFollower(
      handleSignSubmitNoConfirmation(
        fakeWrapperLucid() as never,
        fakeSignBuilder(signed),
        TEST_INTENT,
        {
          inlineWaitPolicy: "defer_positive_wait",
          noInlineSubmitDefer: {
            key: "no-confirm-submitted-key",
            dependencyKey: "dep-no-confirm-submitted",
            invalidationKey: "inv-no-confirm-submitted",
          },
          slotSnapshot: () =>
            Effect.succeed({
              source: "test",
              currentSlot: 12,
              observedAtMs: 1_779_150_000_000,
              slotLengthMs: 1_000,
            }),
        },
      ),
    );

    expect(result).toEqual({ status: "submitted", txHash: "tx-wrapper" });
    expect(submitProgram).toHaveBeenCalledTimes(1);
  });
});
