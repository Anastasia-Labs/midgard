import type { WatcherProverFundingReservationStore } from "../../src/funding/prover-funding-reservation.js";

/** Funding changes a fixture can stop the process around. */
const FUNDING_CHANGES = [
  "prepareTransition",
  "confirmTransition",
  "abandonPendingTransition",
  "reobserveTransition",
  "markConflict",
  "release",
] as const;

export type FundingChange = (typeof FUNDING_CHANGES)[number];

/** The simulated process stop; reopen the store before going on. */
export class FixtureProcessStop extends Error {}

/** Process stops a fixture arms around the funding changes of its stores. */
export const createFixtureProcessStops = () => {
  let crash:
    | { method: FundingChange; boundary: "before" | "after" }
    | undefined;
  return {
    /** The store a permit uses: the process stops at an armed boundary of
     * the next armed change, before its transaction or after its commit. */
    wrap: (
      store: WatcherProverFundingReservationStore,
    ): WatcherProverFundingReservationStore => {
      const wrapped = { ...store } as Record<string, unknown>;
      for (const method of FUNDING_CHANGES) {
        const change = store[method] as
          | ((input: unknown) => Promise<unknown>)
          | undefined;
        if (change === undefined) continue;
        wrapped[method] = async (input: unknown) => {
          const armed = crash?.method === method ? crash : undefined;
          if (armed !== undefined) crash = undefined;
          if (armed?.boundary === "before")
            throw new FixtureProcessStop(`${method} before its transaction`);
          const result = await change(input);
          if (armed?.boundary === "after")
            throw new FixtureProcessStop(`${method} after its commit`);
          return result;
        };
      }
      return wrapped as WatcherProverFundingReservationStore;
    },
    /** Stops the process at `boundary` of the next `method` change a
     * permit makes. */
    stopAt: (method: FundingChange, boundary: "before" | "after") => {
      crash = { method, boundary };
    },
  };
};
