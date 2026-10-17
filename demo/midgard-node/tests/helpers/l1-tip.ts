/**
 * Test L1 accesses for `l1SlotNow` and the client's view: attaches an L1
 * access (`src/l1-access.ts`) to a Lucid client (a stub or a live-like one)
 * whose tip slot a test sets directly, with a monotonic clock the test owns.
 * A wall clock the test fakes never reaches it. The client's provider then
 * carries the access, exactly as a client built over an adapter does.
 */
import type { SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import type { LucidEvolution, Provider } from "@lucid-evolution/lucid";

import {
  type L1Access,
  type L1ViewPoint,
  openL1Access,
} from "../../src/l1-access.js";

export type TestL1Tip = {
  /** Sets the ledger tip slot the next read returns. */
  setTipSlot(slot: number): void;
  /** Moves the monotonic clock forward. */
  advanceMs(ms: number): void;
  /** How many reads the source has served. */
  reads(): number;
  /** The attached access. */
  access: L1Access;
};

export type TestL1AccessOptions = Readonly<{
  /** The tip read; defaults to the slot `setTipSlot` sets. */
  tipSlot?: () => Promise<number>;
  submitSlotSnapshot?: () => Promise<SubmitSlotSnapshot>;
  viewPoint?: () => Promise<L1ViewPoint>;
  monotonicNowMs?: () => number;
}>;

const L1_ACCESS = Symbol.for("midgard.l1-access");

/**
 * The client's provider, made the one stable object its `config()` returns
 * from now on (a stub's `config()` may build a fresh one per call), with any
 * earlier test access removed.
 */
const stableProvider = (api: LucidEvolution): Provider => {
  const target = api as unknown as {
    config?: () => { provider?: unknown };
  };
  const original = target.config?.bind(api);
  const current = original?.().provider;
  const provider = (
    typeof current === "object" && current !== null ? current : {}
  ) as Provider;
  delete (provider as unknown as Record<symbol, unknown>)[L1_ACCESS];
  target.config = () => ({ ...(original?.() ?? {}), provider });
  return provider;
};

const unused = (what: string) => () =>
  Promise.reject(new Error(`no ${what} in this test`));

/** Attaches a test access over `api`'s provider; the test drives its tip. */
export const attachTestL1Access = (
  api: LucidEvolution,
  tipSlot: number,
  options: TestL1AccessOptions = {},
): TestL1Tip => {
  let slot = tipSlot;
  let nowMs = 0;
  let reads = 0;
  const viewPoint = options.viewPoint ?? unused("view point");
  const access = openL1Access(
    {
      kind: "node",
      provider: stableProvider(api),
      endpoint: "test",
      // The stub clients have no slot mapping: slot s starts at s * 1000 ms.
      slotConfig: async () => ({ zeroTime: 0, zeroSlot: 0, slotLength: 1_000 }),
      tipSlot:
        options.tipSlot ??
        (async () => {
          reads += 1;
          return slot;
        }),
      viewPoint,
      synchronizedViewPoint: viewPoint,
      submitSlotSnapshot:
        options.submitSlotSnapshot ?? unused("submit-slot snapshot"),
      close: async () => undefined,
    },
    {
      slotLengthMs: 1_000,
      monotonicNowMs: options.monotonicNowMs ?? (() => nowMs),
    },
  );
  return {
    setTipSlot: (next) => {
      slot = next;
      // Past the refresh window, so the next `l1SlotNow` reads it.
      nowMs += 1_000;
    },
    advanceMs: (ms) => {
      nowMs += ms;
    },
    reads: () => reads,
    access,
  };
};

/** A wall clock this many ms fast, for the "10 minutes fast" checks. */
export const TEN_MINUTES_MS = 10 * 60_000;
