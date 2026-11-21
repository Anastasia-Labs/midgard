import { describe, expect, it, vi } from "vitest";

import {
  captureEventHistoryWithRetry,
  EventHistoryCaptureDeadlineError,
  eventHistoryCaptureWindow,
  type EventHistoryWitness,
  requireEventHistoryCaptureProtection,
} from "../src/user-events/index.js";

const timing = {
  visibilityBudgetMs: 20_000n,
  submissionBudgetMs: 40_000n,
  remainingProofBudgetMs: 80_000n,
  slotLengthMs: 1_000n,
};
const witness = (txHash: string, protectedUntil = 0n): EventHistoryWitness => ({
  kind: "Absent",
  anchor: {
    key: null,
    node: {
      position: "Root",
      next: null,
      protected_until: protectedUntil,
      payload: "RootContent",
    },
    utxo: { txHash, outputIndex: 0, address: "fixture", assets: {} },
  },
});
const base = { headerEnd: 1_000n, mergeDeadline: 1_000_000n, timing };

describe("history capture under bounded inclusion assumptions", () => {
  it("rejects the two-second fixture as a production liveness assumption", () => {
    expect(() => requireEventHistoryCaptureProtection(2_000n, timing)).toThrow(
      /does not cover/,
    );
    expect(() => requireEventHistoryCaptureProtection(61_000n, timing)).toThrow(
      /does not cover/,
    );
    expect(() =>
      requireEventHistoryCaptureProtection(120_000n, timing),
    ).not.toThrow();
  });
  it("reserves all later stages and refuses the exact deadline", () => {
    expect(() =>
      eventHistoryCaptureWindow({ ...base, now: 879_000n, protectedUntil: 0n }),
    ).toThrow(EventHistoryCaptureDeadlineError);
    expect(
      eventHistoryCaptureWindow({ ...base, now: 878_999n, protectedUntil: 0n })
        .validTo,
    ).toBe(919_999n);
  });
  it("backs off the lower bound and retains slot slack after the header end", () => {
    expect(
      eventHistoryCaptureWindow({
        ...base,
        now: 100_000n,
        protectedUntil: 141_000n,
      }),
    ).toEqual({ validFrom: 40_000n, validTo: 141_000n, protected: true });
    expect(
      eventHistoryCaptureWindow({ ...base, now: 2_000n, protectedUntil: 0n })
        .validFrom,
    ).toBe(2_000n);
    expect(() =>
      eventHistoryCaptureWindow({ ...base, now: 1_999n, protectedUntil: 0n }),
    ).toThrow(/accused interval/);
  });
  it("reselects the authoritative replacement after a known conflict", async () => {
    const old = witness("aa");
    const replacement = witness("bb", 200_000n);
    const fetch = vi
      .fn()
      .mockResolvedValueOnce(old)
      .mockResolvedValueOnce(replacement);
    const submit = vi
      .fn()
      .mockResolvedValueOnce({ kind: "ReferenceConflict" })
      .mockResolvedValueOnce({ kind: "Captured", value: "confirmed-thread" });
    const onConflict = vi.fn().mockResolvedValue(undefined);
    expect(
      await captureEventHistoryWithRetry({
        ...base,
        now: () => 100_000n,
        fetch,
        submit,
        onConflict,
        maxAttempts: 3,
      }),
    ).toBe("confirmed-thread");
    expect(
      submit.mock.calls.map(([w, window]) => [
        w.anchor.utxo.txHash,
        window.protected,
      ]),
    ).toEqual([
      ["aa", false],
      ["bb", true],
    ]);
    expect(onConflict).toHaveBeenCalledTimes(1);
    expect(onConflict).toHaveBeenCalledWith(old, 1);
  });
  it("does not resubmit an ambiguous result or a script refusal", async () => {
    for (const message of [
      "submission timed out",
      "validator refused",
      "confirmed output visibility timeout",
    ]) {
      const fetch = vi.fn().mockResolvedValue(witness("aa"));
      const submit = vi.fn().mockRejectedValue(new Error(message));
      await expect(
        captureEventHistoryWithRetry({
          ...base,
          now: () => 100_000n,
          fetch,
          submit,
          maxAttempts: 3,
        }),
      ).rejects.toThrow(message);
      expect(submit).toHaveBeenCalledTimes(1);
      expect(fetch).toHaveBeenCalledTimes(1);
    }
  });
  it("bounds repeated stale snapshots and stops before fetching after the deadline", async () => {
    const fetch = vi.fn().mockResolvedValue(witness("aa"));
    const submit = vi.fn().mockResolvedValue({ kind: "ReferenceConflict" });
    await expect(
      captureEventHistoryWithRetry({
        ...base,
        now: () => 100_000n,
        fetch,
        submit,
        maxAttempts: 3,
      }),
    ).rejects.toThrow(/exhausted/);
    expect(submit).toHaveBeenCalledTimes(3);
    fetch.mockClear();
    await expect(
      captureEventHistoryWithRetry({
        ...base,
        now: () => 879_000n,
        fetch,
        submit,
        maxAttempts: 3,
      }),
    ).rejects.toThrow(EventHistoryCaptureDeadlineError);
    expect(fetch).not.toHaveBeenCalled();
  });
  it("rechecks the remaining budget after a slow provider fetch", async () => {
    let now = 100_000n;
    const submit = vi.fn();
    await expect(
      captureEventHistoryWithRetry({
        ...base,
        now: () => now,
        fetch: async () => {
          now = 879_000n;
          return witness("aa");
        },
        submit,
        maxAttempts: 3,
      }),
    ).rejects.toThrow(EventHistoryCaptureDeadlineError);
    expect(submit).not.toHaveBeenCalled();
  });
});
