import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";
import type { SlotConfig } from "@lucid-evolution/lucid";
import { Effect, Logger } from "effect";
import { describe, expect, it } from "vitest";

import {
  resolveLucidSlotMapping,
  retryTransientSubmitSlotSnapshot,
} from "../src/custom-slot-mapping.js";
import {
  L1LedgerBehindError,
  ledgerSubmitSlotSnapshot,
  transientL1ReadCause,
} from "../src/services/l1-provider.js";

// Whole seconds in the past, so wall-clock slots are exact.
const GENESIS_START_MS = Math.floor(Date.now() / 1_000) * 1_000 - 3_600_000;
const LEDGER: SlotConfig = {
  zeroTime: GENESIS_START_MS,
  zeroSlot: 0,
  slotLength: 1_000,
};
const slotAt = (ms: number) => Math.floor((ms - GENESIS_START_MS) / 1_000);

type Answer = "ok" | "transport_down" | "two-second-slots" | "malformed";

/** Successive answers of the ledger's slot-configuration read (the last repeats). */
const ledgerReads = (answers: readonly Answer[]) => {
  let calls = 0;
  const read = async (): Promise<SlotConfig> => {
    const answer = answers[Math.min(calls, answers.length - 1)]!;
    calls += 1;
    if (answer === "transport_down")
      throw new L1ProviderTransientError("transport", "node_unreachable");
    if (answer === "malformed")
      throw new Error("era history is not an array of at least 1 items");
    return answer === "two-second-slots"
      ? { ...LEDGER, slotLength: 2_000 }
      : LEDGER;
  };
  return { read, calls: () => calls };
};

const run = <A, E>(effect: Effect.Effect<A, E>) => {
  const logs: string[] = [];
  const logger = Logger.make(({ message }) => {
    logs.push(Array.isArray(message) ? message.join(" ") : String(message));
  });
  return Effect.runPromise(
    Effect.either(effect).pipe(
      Effect.provide(Logger.replace(Logger.defaultLogger, logger)),
    ),
  ).then((result) => ({ result, logs }));
};

const FAST = { baseDelayMs: 1, maxDelayMs: 2 } as const;

describe("the node's Lucid slot mapping, from the ledger", () => {
  it("waits out an unreachable node with a logged reason, then resolves the ledger's mapping", async () => {
    const ledger = ledgerReads(["transport_down", "transport_down", "ok"]);
    const { result, logs } = await run(
      resolveLucidSlotMapping({ read: ledger.read, retry: FAST }),
    );
    expect(result._tag).toBe("Right");
    expect(result._tag === "Right" ? result.right : undefined).toEqual(LEDGER);
    expect(ledger.calls()).toBe(3);
    expect(
      logs.filter((line) =>
        line.includes("L1 slot mapping unready: L1 provider transport"),
      ),
    ).toHaveLength(2);
  });

  it("refuses a ledger slot length other than the profile's at once", async () => {
    const ledger = ledgerReads(["two-second-slots"]);
    const { result, logs } = await run(
      resolveLucidSlotMapping({ read: ledger.read, retry: FAST }),
    );
    expect(result._tag).toBe("Left");
    expect(String(result._tag === "Left" ? result.left : "")).toMatch(
      /Ledger slot length disagreement/u,
    );
    expect(ledger.calls()).toBe(1);
    expect(logs.some((line) => line.includes("unready"))).toBe(false);
  });

  it("refuses an unreadable ledger answer without re-reading", async () => {
    const ledger = ledgerReads(["malformed", "ok"]);
    const { result } = await run(
      resolveLucidSlotMapping({ read: ledger.read, retry: FAST }),
    );
    expect(result._tag).toBe("Left");
    expect(ledger.calls()).toBe(1);
  });
});

describe("the submit-slot snapshot at submit time", () => {
  const BOUND_MS = 200_000;
  /** Successive ledger tips, as milliseconds behind now (the last repeats). */
  const readOnce = (lagsMs: readonly number[]) => {
    let calls = 0;
    const read = () =>
      Effect.try({
        try: () => {
          const now = Date.now();
          const lag = lagsMs[Math.min(calls, lagsMs.length - 1)]!;
          calls += 1;
          return ledgerSubmitSlotSnapshot({
            slotConfig: LEDGER,
            ledgerTipSlot: slotAt(now - lag),
            nowMs: now,
            boundMs: BOUND_MS,
          });
        },
        catch: (cause) =>
          cause instanceof Error ? cause : new Error(String(cause)),
      });
    return { read, calls: () => calls };
  };
  const retry = { maxAttempts: 4, baseDelayMs: 1, maxDelayMs: 2 } as const;

  it("re-reads a tip behind wall time and proceeds once it is within the bound", async () => {
    const ledger = readOnce([300_000, 300_000, 1_000]);
    const { result } = await run(
      retryTransientSubmitSlotSnapshot(ledger.read, retry),
    );
    expect(result._tag).toBe("Right");
    expect(result._tag === "Right" ? result.right.source : "").toBe(
      "l1_node_tip",
    );
    expect(ledger.calls()).toBe(3);
  });

  it("still refuses a tip that stays behind past the bound", async () => {
    const ledger = readOnce([300_000]);
    const { result } = await run(
      retryTransientSubmitSlotSnapshot(ledger.read, retry),
    );
    expect(result._tag).toBe("Left");
    expect(
      transientL1ReadCause(result._tag === "Left" ? result.left : undefined),
    ).toBeInstanceOf(L1LedgerBehindError);
    expect(ledger.calls()).toBe(4);
  });

  it("refuses a non-transient failure without re-reading", async () => {
    let calls = 0;
    const { result } = await run(
      retryTransientSubmitSlotSnapshot(
        () =>
          Effect.suspend(() => {
            calls += 1;
            return Effect.fail(new Error("protocol parameters are malformed"));
          }),
        retry,
      ),
    );
    expect(result._tag).toBe("Left");
    expect(calls).toBe(1);
  });
});
