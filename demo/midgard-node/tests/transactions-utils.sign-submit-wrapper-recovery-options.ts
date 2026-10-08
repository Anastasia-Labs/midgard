import "./transactions-utils.validity-window-submit-recovery.js";

import { Emulator } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  awaitSubmittedTransactionConfirmation,
  handleSignSubmitNoConfirmation,
  signSubmitTransaction,
} from "../src/transactions/utils.js";
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
    vi.useFakeTimers();
    try {
      const provider = new Emulator([]);
      const awaitTx = vi.fn(async () => true);
      const awaitTxConfirmation = vi.fn(async (txHash: string) => ({ txHash }));
      const confirmation = Effect.runPromise(
        awaitSubmittedTransactionConfirmation(
          {
            config: () => ({ provider }),
            awaitTx,
            awaitTxConfirmation,
            wallet: () => ({}),
          } as never,
          {
            txHash: "tx-emulator-confirmation",
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

      await expect(confirmation).resolves.toBe("tx-emulator-confirmation");
      expect(awaitTx).toHaveBeenCalledWith("tx-emulator-confirmation", 100);
      expect(awaitTxConfirmation).toHaveBeenCalledWith(
        "tx-emulator-confirmation",
        { timeout: 120_000, checkInterval: 100 },
      );
    } finally {
      vi.useRealTimers();
    }
  });

  it("spaces transient provider confirmation retries and recovers the exact submitted tx", async () => {
    vi.useFakeTimers();
    try {
      let attempts = 0;
      const awaitTxConfirmation = vi.fn(async (txHash: string) => {
        attempts += 1;
        if (attempts <= 2) {
          throw new Error("transient kupo transport error");
        }
        return { txHash };
      });
      const confirmation = Effect.runPromise(
        awaitSubmittedTransactionConfirmation(
          {
            config: () => ({ provider: undefined }),
            awaitTxConfirmation,
            wallet: () => ({}),
          } as never,
          {
            txHash: "tx-transient-confirmation-provider",
            signedTxCbor: "00",
          },
          {
            confirmationTimeoutMs: 120_000,
            confirmationRetries: 2,
            confirmationPollIntervalMs: 100,
          },
        ),
      );

      await vi.advanceTimersByTimeAsync(0);
      expect(awaitTxConfirmation).toHaveBeenCalledTimes(1);
      expect(awaitTxConfirmation).toHaveBeenNthCalledWith(
        1,
        "tx-transient-confirmation-provider",
        { timeout: 120_000, checkInterval: 100 },
      );

      await vi.advanceTimersByTimeAsync(99);
      expect(awaitTxConfirmation).toHaveBeenCalledTimes(1);
      await vi.advanceTimersByTimeAsync(1);
      expect(awaitTxConfirmation).toHaveBeenCalledTimes(2);
      expect(awaitTxConfirmation).toHaveBeenNthCalledWith(
        2,
        "tx-transient-confirmation-provider",
        { timeout: 120_000, checkInterval: 100 },
      );

      await vi.advanceTimersByTimeAsync(99);
      expect(awaitTxConfirmation).toHaveBeenCalledTimes(2);
      await vi.advanceTimersByTimeAsync(1);
      expect(awaitTxConfirmation).toHaveBeenCalledTimes(3);
      expect(awaitTxConfirmation).toHaveBeenNthCalledWith(
        3,
        "tx-transient-confirmation-provider",
        { timeout: 120_000, checkInterval: 100 },
      );

      await vi.advanceTimersByTimeAsync(0);

      await expect(confirmation).resolves.toBe(
        "tx-transient-confirmation-provider",
      );
      expect(awaitTxConfirmation).toHaveBeenCalledTimes(3);
    } finally {
      vi.useRealTimers();
    }
  });

  it("allows an exact submitted transaction to confirm after the legacy 90-second ceiling", async () => {
    vi.useFakeTimers();
    try {
      const awaitTxConfirmation = vi.fn(
        (txHash: string) =>
          new Promise<{ readonly txHash: string }>((resolve) => {
            setTimeout(() => resolve({ txHash }), 100_000);
          }),
      );
      const confirmation = Effect.runPromise(
        awaitSubmittedTransactionConfirmation(
          {
            config: () => ({ provider: undefined }),
            awaitTxConfirmation,
            wallet: () => ({}),
          } as never,
          {
            txHash: "tx-long-confirmation",
            signedTxCbor: "00",
          },
          {
            confirmationTimeoutMs: 120_000,
            confirmationRetries: 0,
            confirmationPollIntervalMs: 1_000,
          },
        ),
      );

      await vi.advanceTimersByTimeAsync(90_001);
      expect(awaitTxConfirmation).toHaveBeenCalledWith("tx-long-confirmation", {
        timeout: 120_000,
        checkInterval: 1_000,
      });
      await vi.advanceTimersByTimeAsync(9_999);

      await expect(confirmation).resolves.toBe("tx-long-confirmation");
    } finally {
      vi.useRealTimers();
    }
  });

  it("still fails when the configured exact transaction confirmation deadline expires", async () => {
    vi.useFakeTimers();
    try {
      const confirmation = Effect.runPromise(
        Effect.either(
          awaitSubmittedTransactionConfirmation(
            {
              config: () => ({ provider: undefined }),
              awaitTxConfirmation: vi.fn(
                (
                  _txHash: string,
                  options: { readonly timeout?: number } = {},
                ) =>
                  new Promise<never>((_resolve, reject) => {
                    setTimeout(
                      () =>
                        reject(
                          new Error(
                            `timed out waiting for tx confirmation after ${options.timeout?.toString()}ms`,
                          ),
                        ),
                      options.timeout,
                    );
                  }),
              ),
              wallet: () => ({}),
            } as never,
            {
              txHash: "tx-confirmation-timeout",
              signedTxCbor: "00",
            },
            {
              confirmationTimeoutMs: 120_000,
              confirmationRetries: 0,
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
        "timed out waiting for tx confirmation after 120000ms",
      );
    } finally {
      vi.useRealTimers();
    }
  });

  it("ends the whole confirmation wait, retries included, at the confirmation deadline", async () => {
    vi.useFakeTimers();
    try {
      // Each provider wait times out after a second; twelve retries alone
      // would keep the caller waiting 25 seconds.
      const awaitTxConfirmation = vi.fn(
        () =>
          new Promise<never>((_resolve, reject) => {
            setTimeout(
              () => reject(new Error("provider wait timed out")),
              1_000,
            );
          }),
      );
      const confirmation = Effect.runPromise(
        Effect.either(
          awaitSubmittedTransactionConfirmation(
            {
              config: () => ({ provider: undefined }),
              awaitTxConfirmation,
              wallet: () => ({}),
            } as never,
            {
              txHash: "tx-confirmation-deadline",
              signedTxCbor: "00",
            },
            {
              confirmationTimeoutMs: 1_000,
              confirmationRetries: 12,
              confirmationPollIntervalMs: 1_000,
              confirmationDeadlineMs: Date.now() + 5_000,
            },
          ),
        ),
      );
      let settled = false;
      void confirmation.then(() => {
        settled = true;
      });

      await vi.advanceTimersByTimeAsync(4_999);
      expect(settled).toBe(false);
      await vi.advanceTimersByTimeAsync(1);
      expect(settled).toBe(true);
      const result = await confirmation;

      expect(result._tag).toBe("Left");
      if (result._tag !== "Left") throw new Error("expected the deadline");
      expect(result.left).toMatchObject({
        _tag: "TxConfirmError",
        message: "Transaction confirmation deadline passed",
        txHash: "tx-confirmation-deadline",
      });
      expect(awaitTxConfirmation).toHaveBeenCalledTimes(3);
    } finally {
      vi.useRealTimers();
    }
  });

  it("forwards submit recovery options through signSubmitTransaction", async () => {
    const slots = [7, 12];
    const waits: number[] = [];
    const submitProgram = vi.fn(() => Effect.void);
    const signed = {
      toCBOR: () =>
        signedTxCbor({ invalidBeforeSlot: 10, invalidHereafterSlot: 20 }),
      submitProgram,
    };
    const signBuilder = fakeSignBuilder(signed);

    const result = await runWithoutFollower(
      Effect.either(
        signSubmitTransaction(
          fakeWrapperLucid() as never,
          signBuilder,
          TEST_INTENT,
          {
            label: "operator registration",
            slotSnapshot: () =>
              Effect.succeed({
                source: "test",
                currentSlot: slots.shift() ?? 12,
                observedAtMs: 1_779_150_000_000,
                slotLengthMs: 1_000,
              }),
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Right");
    expect(submitProgram).toHaveBeenCalledTimes(1);
    expect(waits).toEqual([5_000]);
  });

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
