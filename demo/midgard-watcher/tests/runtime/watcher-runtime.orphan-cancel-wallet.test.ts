import type { Provider } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import type { WatcherProcessConfig } from "../../src/runtime/process-config.js";
import { watcherOrphanCancelInput } from "../../src/runtime/watcher-runtime.orphan-cancel-wallet.js";

const HEADER = "aa".repeat(28);
const OTHER = "bb".repeat(28);

/** The runtime's cancel input over the finalized queue `inclusion` holds. */
const cancelInput = (
  inclusion: { current: readonly string[] | null },
  submitted: string[] = [],
) =>
  watcherOrphanCancelInput({
    config: {} as WatcherProcessConfig,
    address: "addr_test1",
    provider: {
      submitTx: async (signed: string) => {
        submitted.push(signed);
        return "00".repeat(32);
      },
    } as unknown as Provider,
    inclusion: () =>
      inclusion.current === null
        ? null
        : ({
            finalizedHeaders: inclusion.current.map((headerHash) => ({
              headerHash,
            })),
          } as unknown as WatcherAuthenticatedStateQueueObservation),
  });

describe("the runtime's orphan cancel input", () => {
  it("names a target gone only once an observation's finalized queue no longer holds its header", async () => {
    const inclusion: { current: readonly string[] | null } = { current: null };
    const { targetGone } = cancelInput(inclusion);
    const target = { category: "doubleSpend", headerHash: HEADER };
    // Before the first observation no target is gone.
    expect(await targetGone(target)).toBe(false);
    inclusion.current = [OTHER, HEADER];
    expect(await targetGone(target)).toBe(false);
    inclusion.current = [OTHER];
    expect(await targetGone(target)).toBe(true);
    inclusion.current = [];
    expect(await targetGone(target)).toBe(true);
    inclusion.current = null;
    expect(await targetGone(target)).toBe(false);
  });

  it("submits the exact signed bytes through the provider", async () => {
    const submitted: string[] = [];
    const { wallet } = cancelInput({ current: null }, submitted);
    expect(wallet.address).toBe("addr_test1");
    await wallet.submit("84a0");
    expect(submitted).toEqual(["84a0"]);
  });
});
