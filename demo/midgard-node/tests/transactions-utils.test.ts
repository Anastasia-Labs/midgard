import { CML, Emulator, OgmiosJsonRpcError } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  awaitSubmittedTransactionConfirmation,
  handleSignSubmitNoConfirmation,
  inspectSignedTxValidityInterval,
  isNoInlineSubmitDefer,
  isUnknownOutputReferenceSubmitError,
  NoInlineSubmitDefer,
  parseOutsideValidityIntervalDetails,
  resolveEarlyValidityRetryDelayMs,
  signSubmitTransaction,
  submitSignedTxWithRecovery,
} from "../src/transactions/utils.js";

describe("parseOutsideValidityIntervalDetails", () => {
  it("parses typed Kupmios/Ogmios early-validity submit errors", () => {
    const error = new OgmiosJsonRpcError({
      code: 3118,
      message: "The transaction is outside of its validity interval.",
      data: {
        validityInterval: {
          invalidBefore: 123415253,
          invalidAfter: 123415372,
        },
        currentSlot: 123415249,
      },
      method: "submitTransaction",
      id: null,
    });

    expect(parseOutsideValidityIntervalDetails(error)).toEqual({
      invalidBeforeSlot: 123415253,
      invalidHereafterSlot: 123415372,
      currentSlot: 123415249,
    });
  });

  it("parses structured Ogmios 3118 errors through causes", () => {
    expect(
      parseOutsideValidityIntervalDetails(
        new Error("submit failed", {
          cause: {
            error: {
              code: 3118,
              data: {
                validityInterval: {
                  invalidBefore: 126544954,
                  invalidAfter: 126545100,
                },
                currentSlot: 126544938,
              },
            },
          },
        }),
      ),
    ).toEqual({
      invalidBeforeSlot: 126544954,
      invalidHereafterSlot: 126545100,
      currentSlot: 126544938,
    });
  });

  it("parses lower-bound-only Ogmios 3118 errors as early-validity details", () => {
    expect(
      parseOutsideValidityIntervalDetails({
        error: {
          code: 3118,
          data: {
            validityInterval: {
              invalidBefore: 126544954,
            },
            currentSlot: 126544938,
          },
        },
      }),
    ).toEqual({
      invalidBeforeSlot: 126544954,
      currentSlot: 126544938,
    });
  });
});

describe("validity-window submit recovery", () => {
  const outsideValidityError = new Error(
    "OutsideValidityIntervalUTxO (ValidityInterval {invalidBefore = SJust (SlotNo 10), invalidHereafter = SJust (SlotNo 20)}) (SlotNo 7)",
  );

  const ogmiosOutsideValidityError = ({
    invalidBefore,
    invalidAfter,
    currentSlot,
  }: {
    readonly invalidBefore: number;
    readonly invalidAfter?: number;
    readonly currentSlot: number;
  }): OgmiosJsonRpcError =>
    new OgmiosJsonRpcError({
      code: 3118,
      message: "The transaction is outside of its validity interval.",
      data: {
        validityInterval: {
          invalidBefore,
          ...(invalidAfter === undefined ? {} : { invalidAfter }),
        },
        currentSlot,
      },
      method: "submitTransaction",
      id: null,
    });

  const fakeLucid = {
    config: () => ({ provider: undefined }),
    awaitTxConfirmation: vi.fn(() =>
      Promise.reject(new Error("transaction not confirmed")),
    ),
  };

  it("computes bounded retry delays for early-validity provider errors", () => {
    expect(
      resolveEarlyValidityRetryDelayMs(
        {
          invalidBeforeSlot: 10,
          invalidHereafterSlot: 20,
          currentSlot: 7,
        },
        0,
      ),
    ).toBe(5_000);

    expect(
      resolveEarlyValidityRetryDelayMs(
        {
          invalidBeforeSlot: 100,
          invalidHereafterSlot: 102,
          currentSlot: 50,
        },
        0,
      ),
    ).toBeNull();

    expect(
      resolveEarlyValidityRetryDelayMs(
        {
          invalidBeforeSlot: 10,
          invalidHereafterSlot: 20,
          currentSlot: 11,
        },
        0,
      ),
    ).toBe(1_000);

    expect(
      resolveEarlyValidityRetryDelayMs(
        {
          invalidBeforeSlot: 10,
          invalidHereafterSlot: 20,
          currentSlot: 7,
        },
        60,
      ),
    ).toBeNull();
  });

  it("fails stale early-validity provider slots when the bounded recovery budget is exhausted", async () => {
    const waits: number[] = [];
    const submitProgram = vi.fn(() => Effect.fail(outsideValidityError));
    const signed = { submitProgram };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          fakeLucid as never,
          signed as never,
          "tx-validity",
          {
            maxPreSubmitWaitMs: 11_000,
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    expect(submitProgram).toHaveBeenCalledTimes(3);
    expect(waits).toEqual([5_000, 5_000]);
  });

  it("allows a provider-reported wait that fits the configured bounded recovery budget", async () => {
    const waits: number[] = [];
    let calls = 0;
    const providerLagError = ogmiosOutsideValidityError({
      invalidBefore: 40,
      currentSlot: 10,
    });
    const submitProgram = vi.fn(() => {
      calls += 1;
      return calls === 1 ? Effect.fail(providerLagError) : Effect.void;
    });
    const signed = { submitProgram };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          fakeLucid as never,
          signed as never,
          "tx-provider-lag",
          {
            maxPreSubmitWaitMs: 60_000,
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Right");
    expect(submitProgram).toHaveBeenCalledTimes(2);
    expect(waits).toEqual([32_000]);
  });

  it("does not exhaust stale-provider attempts before the configured wait budget", async () => {
    const waits: number[] = [];
    let calls = 0;
    const staleProviderSlotError = ogmiosOutsideValidityError({
      invalidBefore: 10,
      invalidAfter: 100,
      currentSlot: 8,
    });
    const submitProgram = vi.fn(() => {
      calls += 1;
      return calls <= 10 ? Effect.fail(staleProviderSlotError) : Effect.void;
    });
    const signed = { submitProgram };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          fakeLucid as never,
          signed as never,
          "tx-repeated-provider-lag",
          {
            maxPreSubmitWaitMs: 60_000,
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Right");
    expect(submitProgram).toHaveBeenCalledTimes(11);
    expect(waits).toEqual(Array.from({ length: 10 }, () => 4_000));
  });

  it("recovers a longer stale-provider lag when the caller provides a larger bounded budget", async () => {
    const waits: number[] = [];
    let calls = 0;
    const staleProviderSlotError = ogmiosOutsideValidityError({
      invalidBefore: 100,
      invalidAfter: 600,
      currentSlot: 88,
    });
    const submitProgram = vi.fn(() => {
      calls += 1;
      return calls <= 5 ? Effect.fail(staleProviderSlotError) : Effect.void;
    });
    const signed = { submitProgram };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          fakeLucid as never,
          signed as never,
          "tx-long-provider-lag",
          {
            maxPreSubmitWaitMs: 120_000,
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Right");
    expect(submitProgram).toHaveBeenCalledTimes(6);
    expect(waits).toEqual(Array.from({ length: 5 }, () => 14_000));
  });

  it("keeps a single generic provider retry in the submit recovery loop", async () => {
    const waits: number[] = [];
    let calls = 0;
    const submitProgram = vi.fn(() => {
      calls += 1;
      return calls === 1 ? Effect.fail(new Error("fetch failed")) : Effect.void;
    });
    const signed = { submitProgram };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          fakeLucid as never,
          signed as never,
          "tx-provider",
          {
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Right");
    expect(submitProgram).toHaveBeenCalledTimes(2);
    expect(waits).toEqual([2_000]);
  });

  it("recovers lower-bound-only Ogmios 3118 without generic provider retry", async () => {
    const waits: number[] = [];
    let calls = 0;
    const lowerBoundOnlyError = ogmiosOutsideValidityError({
      invalidBefore: 126544954,
      currentSlot: 126544938,
    });
    const submitProgram = vi.fn(() => {
      calls += 1;
      return calls === 1 ? Effect.fail(lowerBoundOnlyError) : Effect.void;
    });
    const signed = { submitProgram };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          fakeLucid as never,
          signed as never,
          "tx-lower-bound-only",
          {
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Right");
    expect(submitProgram).toHaveBeenCalledTimes(2);
    expect(waits).toEqual([18_000]);
  });

  it("recovers repeated stale early-validity provider slots within the bounded retry budget", async () => {
    vi.useFakeTimers();
    vi.setSystemTime(new Date("2026-01-01T00:00:00.000Z"));
    try {
      const waits: number[] = [];
      let calls = 0;
      const submitProgram = vi.fn(() => {
        calls += 1;
        return calls <= 2 ? Effect.fail(outsideValidityError) : Effect.void;
      });
      const signed = { submitProgram };

      const result = await Effect.runPromise(
        Effect.either(
          submitSignedTxWithRecovery(
            fakeLucid as never,
            signed as never,
            "tx-stale-provider-slot",
            {
              sleep: (milliseconds) =>
                Effect.sync(() => {
                  waits.push(milliseconds);
                  vi.setSystemTime(Date.now() + milliseconds);
                }),
            },
          ),
        ),
      );

      expect(result._tag).toBe("Right");
      expect(submitProgram).toHaveBeenCalledTimes(3);
      expect(waits).toEqual([5_000, 5_000]);
    } finally {
      vi.useRealTimers();
    }
  });

  it("uses provider-reported slot lag when local evidence is already ready", async () => {
    const waits: number[] = [];
    const slots = [12, 12, 12];
    let calls = 0;
    const submitProgram = vi.fn(() => {
      calls += 1;
      return calls === 1 ? Effect.fail(outsideValidityError) : Effect.void;
    });
    const signed = {
      toCBOR: () =>
        signedTxCbor({ invalidBeforeSlot: 10, invalidHereafterSlot: 20 }),
      submitProgram,
    };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          { awaitTx: vi.fn() } as never,
          signed as never,
          "tx-local-slot-provider",
          {
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
    expect(submitProgram).toHaveBeenCalledTimes(2);
    expect(waits).toEqual([5_000]);
  });

  it("waits until the signed transaction lower bound before first submit", async () => {
    const waits: number[] = [];
    const slots = [126544938, 126544956];
    const submitProgram = vi.fn(() => Effect.void);
    const signed = {
      toCBOR: () =>
        signedTxCbor({
          invalidBeforeSlot: 126544954,
          invalidHereafterSlot: 126545000,
        }),
      submitProgram,
    };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          { awaitTx: vi.fn() } as never,
          signed as never,
          "tx-pre-submit",
          {
            slotSnapshot: () =>
              Effect.succeed({
                source: "test",
                currentSlot: slots.shift() ?? 126544956,
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
    expect(waits).toEqual([18_000]);
  });

  it("keeps the pre-submit margin after the lower bound has opened", async () => {
    const waits: number[] = [];
    const slots = [126544955, 126544956];
    const submitProgram = vi.fn(() => Effect.void);
    const signed = {
      toCBOR: () =>
        signedTxCbor({
          invalidBeforeSlot: 126544954,
          invalidHereafterSlot: 126545000,
        }),
      submitProgram,
    };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          { awaitTx: vi.fn() } as never,
          signed as never,
          "tx-pre-submit-margin",
          {
            slotSnapshot: () =>
              Effect.succeed({
                source: "test",
                currentSlot: slots.shift() ?? 126544956,
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
    expect(waits).toEqual([1_000]);
  });

  it("defers pre-submit validity waits in no-inline mode without sleeping or submitting", async () => {
    const waits: number[] = [];
    const submitProgram = vi.fn(() => Effect.void);
    const signed = {
      toCBOR: () =>
        signedTxCbor({
          invalidBeforeSlot: 126544954,
          invalidHereafterSlot: 126545000,
        }),
      submitProgram,
    };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          { awaitTx: vi.fn() } as never,
          signed as never,
          "tx-pre-submit-no-inline",
          {
            inlineWaitPolicy: "defer_positive_wait",
            noInlineSubmitDefer: {
              key: "pre-submit-key",
              dependencyKey: "dep-pre-submit",
              invalidationKey: "inv-pre-submit",
            },
            slotSnapshot: () =>
              Effect.succeed({
                source: "test",
                currentSlot: 126544938,
                observedAtMs: 1_779_150_000_000,
                slotLengthMs: 1_000,
              }),
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    if (result._tag !== "Left") {
      throw new Error("expected no-inline defer");
    }
    const defer = expectNoInlineSubmitDefer(result.left);
    expect(defer).toBeInstanceOf(NoInlineSubmitDefer);
    expect(defer).toMatchObject({
      kind: "pre_submit_validity",
      key: "pre-submit-key",
      txHash: "tx-pre-submit-no-inline",
      currentSlot: 126544938,
      targetSlot: 126544956,
      dueSlot: 126544956,
      waitMs: 18_000,
      slotSource: "test",
      dependencyKey: "dep-pre-submit",
      invalidationKey: "inv-pre-submit",
      invalidBeforeSlot: 126544954,
      invalidHereafterSlot: 126545000,
    });
    expect(submitProgram).not.toHaveBeenCalled();
    expect(waits).toEqual([]);
  });

  it("defers early-validity recovery waits in no-inline mode without sleeping", async () => {
    const waits: number[] = [];
    const submitProgram = vi.fn(() => Effect.fail(outsideValidityError));
    const signed = { submitProgram };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          fakeLucid as never,
          signed as never,
          "tx-early-validity-no-inline",
          {
            inlineWaitPolicy: "defer_positive_wait",
            noInlineSubmitDefer: {
              key: "early-validity-key",
              dependencyKey: "dep-early-validity",
              invalidationKey: "inv-early-validity",
            },
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    if (result._tag !== "Left") {
      throw new Error("expected no-inline defer");
    }
    expect(expectNoInlineSubmitDefer(result.left)).toMatchObject({
      kind: "early_validity_recovery",
      key: "early-validity-key",
      txHash: "tx-early-validity-no-inline",
      currentSlot: 7,
      targetSlot: 12,
      dueSlot: 12,
      waitMs: 5_000,
      slotSource: "test",
      dependencyKey: "dep-early-validity",
      invalidationKey: "inv-early-validity",
      invalidBeforeSlot: 10,
      invalidHereafterSlot: 20,
    });
    expect(submitProgram).toHaveBeenCalledTimes(1);
    expect(waits).toEqual([]);
  });

  it("defers provider slot waits in no-inline mode without sleeping", async () => {
    const waits: number[] = [];
    const submitProgram = vi.fn(() => Effect.fail(outsideValidityError));
    const signed = {
      toCBOR: () =>
        signedTxCbor({ invalidBeforeSlot: 10, invalidHereafterSlot: 20 }),
      submitProgram,
    };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          { awaitTx: vi.fn() } as never,
          signed as never,
          "tx-provider-no-inline",
          {
            inlineWaitPolicy: "defer_positive_wait",
            noInlineSubmitDefer: {
              key: "provider-slot-key",
              dependencyKey: "dep-provider-slot",
              invalidationKey: "inv-provider-slot",
            },
            slotSnapshot: () =>
              Effect.succeed({
                source: "test",
                currentSlot: 12,
                observedAtMs: 1_779_150_000_000,
                slotLengthMs: 1_000,
              }),
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    if (result._tag !== "Left") {
      throw new Error("expected no-inline defer");
    }
    expect(expectNoInlineSubmitDefer(result.left)).toMatchObject({
      kind: "provider_slot_wait",
      key: "provider-slot-key",
      txHash: "tx-provider-no-inline",
      currentSlot: 7,
      targetSlot: 12,
      dueSlot: 12,
      waitMs: 5_000,
      slotSource: "provider",
      dependencyKey: "dep-provider-slot",
      invalidationKey: "inv-provider-slot",
      invalidBeforeSlot: 10,
      invalidHereafterSlot: 20,
    });
    expect(submitProgram).toHaveBeenCalledTimes(1);
    expect(waits).toEqual([]);
  });

  it("advances emulator provider slots during default pre-submit waits", async () => {
    const provider = {
      slot: 7,
      time: 7_000,
      now: vi.fn(() => provider.time),
      awaitSlot: vi.fn((length = 1) => {
        provider.slot += length;
        provider.time += length * 1_000;
      }),
    };
    const submitProgram = vi.fn(() => Effect.void);
    const signed = {
      toCBOR: () =>
        signedTxCbor({ invalidBeforeSlot: 10, invalidHereafterSlot: 20 }),
      submitProgram,
    };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          {
            config: () => ({ provider }),
            unixTimeToSlot: (unixTime: number) => Math.floor(unixTime / 1_000),
            currentSlot: () => {
              throw new Error("Expected emulator provider time to be used");
            },
            awaitTx: vi.fn(),
          } as never,
          signed as never,
          "tx-emulator-pre-submit",
        ),
      ),
    );

    expect(result._tag).toBe("Right");
    expect(provider.awaitSlot).toHaveBeenCalledWith(5);
    expect(provider.slot).toBe(12);
    expect(submitProgram).toHaveBeenCalledTimes(1);
  });

  it("does not retry an expired signed transaction", async () => {
    const waits: number[] = [];
    const submitProgram = vi.fn(() => Effect.void);
    const signed = {
      toCBOR: () =>
        signedTxCbor({ invalidBeforeSlot: 10, invalidHereafterSlot: 12 }),
      submitProgram,
    };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          { awaitTx: vi.fn() } as never,
          signed as never,
          "tx-expired",
          {
            slotSnapshot: () =>
              Effect.succeed({
                source: "test",
                currentSlot: 12,
                observedAtMs: 1_779_150_000_000,
                slotLengthMs: 1_000,
              }),
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    expect(submitProgram).not.toHaveBeenCalled();
    expect(waits).toEqual([]);
  });

  it("fails closed before submit when a strict bounded tx has no slot snapshot", async () => {
    const submitProgram = vi.fn(() => Effect.void);
    const signed = {
      toCBOR: () =>
        signedTxCbor({ invalidBeforeSlot: 10, invalidHereafterSlot: 20 }),
      submitProgram,
    };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          { awaitTx: vi.fn() } as never,
          signed as never,
          "tx-strict-no-slot",
          {
            requireSlotForBoundedTx: true,
            slotSnapshot: () => Effect.fail(new Error("ogmios unavailable")),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    expect(String((result as { readonly left: unknown }).left)).toContain(
      "requires a local submit slot",
    );
    expect(submitProgram).not.toHaveBeenCalled();
  });

  it("does not retry the same signed body for unknown input submit errors", async () => {
    const waits: number[] = [];
    const unknownInputError = new OgmiosJsonRpcError({
      code: 3102,
      message: "Unknown inputs",
      data: {
        unknownOutputReferences: [{ transaction: { id: "abc" }, index: 0 }],
      },
      method: "submitTransaction",
      id: null,
    });
    const submitProgram = vi.fn(() => Effect.fail(unknownInputError));
    const signed = { submitProgram };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          fakeLucid as never,
          signed as never,
          "tx-unknown-input",
          {
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    expect(submitProgram).toHaveBeenCalledTimes(1);
    expect(waits).toEqual([]);
    expect(isUnknownOutputReferenceSubmitError(unknownInputError)).toBe(true);
  });

  it("accepts an unknown-input submit race only after exact status confirmation", async () => {
    const txHash = "tx-confirmed-submit-race";
    const unknownInputError = new OgmiosJsonRpcError({
      code: 3102,
      message: "Unknown inputs",
      data: { unknownOutputReferences: [{ index: 0 }] },
      method: "submitTransaction",
      id: null,
    });
    const submitProgram = vi.fn(() => Effect.fail(unknownInputError));
    const awaitTxConfirmation = vi.fn(async () => ({ txHash }));

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          {
            config: () => ({ provider: undefined }),
            awaitTxConfirmation,
          } as never,
          { submitProgram } as never,
          txHash,
        ),
      ),
    );

    expect(result._tag).toBe("Right");
    expect(submitProgram).toHaveBeenCalledTimes(1);
    expect(awaitTxConfirmation).toHaveBeenCalledWith(txHash, {
      timeout: 90_000,
      checkInterval: 5_000,
    });
  });

  it("does not sleep for generic provider retries in no-inline mode", async () => {
    const waits: number[] = [];
    const providerError = new Error("fetch failed");
    const submitProgram = vi.fn(() => Effect.fail(providerError));
    const signed = { submitProgram };

    const result = await Effect.runPromise(
      Effect.either(
        submitSignedTxWithRecovery(
          fakeLucid as never,
          signed as never,
          "tx-no-inline-provider-error",
          {
            inlineWaitPolicy: "defer_positive_wait",
            noInlineSubmitDefer: {
              key: "provider-error-key",
              dependencyKey: "dep-provider-error",
              invalidationKey: "inv-provider-error",
            },
            sleep: (milliseconds) =>
              Effect.sync(() => waits.push(milliseconds)),
          },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    expect(String((result as { readonly left: unknown }).left)).toContain(
      "refusing provider retry sleep under ownership",
    );
    expect(submitProgram).toHaveBeenCalledTimes(1);
    expect(waits).toEqual([]);
  });
});

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
            walletAddress: "addr_test1emulatorconfirmation",
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
            walletAddress: "addr_test1transientconfirmation",
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

      await vi.advanceTimersByTimeAsync(5_000);

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
            walletAddress: "addr_test1longconfirmation",
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
      await vi.advanceTimersByTimeAsync(14_999);

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
              walletAddress: "addr_test1confirmationtimeout",
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

    const result = await Effect.runPromise(
      Effect.either(
        signSubmitTransaction(fakeWrapperLucid() as never, signBuilder, {
          slotSnapshot: () =>
            Effect.succeed({
              source: "test",
              currentSlot: slots.shift() ?? 12,
              observedAtMs: 1_779_150_000_000,
              slotLengthMs: 1_000,
            }),
          sleep: (milliseconds) => Effect.sync(() => waits.push(milliseconds)),
        }),
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

    const result = await Effect.runPromise(
      Effect.either(
        handleSignSubmitNoConfirmation(
          fakeWrapperLucid() as never,
          fakeSignBuilder(signed),
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

    const result = await Effect.runPromise(
      Effect.either(
        signSubmitTransaction(
          fakeWrapperLucid() as never,
          fakeSignBuilder(signed),
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

    const result = await Effect.runPromise(
      handleSignSubmitNoConfirmation(
        fakeWrapperLucid() as never,
        fakeSignBuilder(signed),
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

    const result = await Effect.runPromise(
      handleSignSubmitNoConfirmation(
        fakeWrapperLucid() as never,
        fakeSignBuilder(signed),
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

const expectNoInlineSubmitDefer = (value: unknown): NoInlineSubmitDefer => {
  expect(isNoInlineSubmitDefer(value)).toBe(true);
  return value as NoInlineSubmitDefer;
};

const signedTxCbor = ({
  invalidBeforeSlot,
  invalidHereafterSlot,
}: {
  readonly invalidBeforeSlot?: number;
  readonly invalidHereafterSlot?: number;
}): string => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("11".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  const body = CML.TransactionBody.new(inputs, outputs, 0n);
  if (invalidBeforeSlot !== undefined) {
    body.set_validity_interval_start(BigInt(invalidBeforeSlot));
  }
  if (invalidHereafterSlot !== undefined) {
    body.set_ttl(BigInt(invalidHereafterSlot));
  }
  const tx = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
    undefined,
  );
  const cbor = tx.to_cbor_hex();
  expect(inspectSignedTxValidityInterval(cbor)).toEqual({
    ...(invalidBeforeSlot === undefined ? {} : { invalidBeforeSlot }),
    ...(invalidHereafterSlot === undefined ? {} : { invalidHereafterSlot }),
  });
  return cbor;
};

const fakeWrapperLucid = () => ({
  wallet: () => ({
    address: async () => "addr_test1wrapper",
  }),
  awaitTx: vi.fn(),
});

const fakeSignBuilder = (signed: unknown) =>
  ({
    toHash: () => "tx-wrapper",
    sign: {
      withWallet: () => ({
        completeProgram: () => Effect.succeed(signed),
      }),
    },
  }) as never;
