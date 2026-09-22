import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  WatcherLocalKupmios,
  type WatcherNativeRewardAccountQuery,
} from "../../src/l1/native-reward-account.js";

const native: WatcherNativeRewardAccountQuery = {
  binaryPath: "/unused/native-query",
  timeoutMs: 10_000,
  watcherConfig: {
    targetNetwork: "Preprod",
    l1: {
      source: {
        sourceMode: "local_node",
        authorityNodeId: "test-node",
        chainSync: {
          kind: "cardano_node_socket",
          socketPath: "/unused/node.socket",
          nodeConfigPath: "/unused/node.json",
          genesisConfigPath: "/unused/genesis.json",
          genesisIdentitySha256: "ab".repeat(32),
        },
        queryServices: [],
      },
    },
  },
};
const txHash = "12".repeat(32);
const confirmation = {
  transaction_index: 0,
  transaction_id: txHash,
  output_index: 0,
  address: "unused-address",
  value: { coins: 2_000_000, assets: {} },
  datum_hash: null,
  script_hash: null,
  created_at: { slot_no: 42, header_hash: "34".repeat(32) },
  spent_at: null,
};
const provider = (awaitTxTimeoutMs?: number) =>
  new WatcherLocalKupmios("http://kupo.test", "http://ogmios.test", native, {
    awaitTxTimeoutMs,
  });

describe("local Kupmios confirmation deadlines", () => {
  beforeEach(() => {
    vi.useFakeTimers();
    vi.setSystemTime(0);
  });
  afterEach(() => {
    vi.unstubAllGlobals();
    vi.useRealTimers();
  });

  it("keeps polling the same submitted hash beyond the default deadline", async () => {
    const fetch = vi.fn(async () =>
      Response.json(Date.now() >= 180_000 ? [confirmation] : []),
    );
    vi.stubGlobal("fetch", fetch);
    const settled = vi.fn();
    const result = provider(600_000).awaitTx(txHash, 500).then(settled);
    await vi.advanceTimersByTimeAsync(180_000);
    expect(settled).not.toHaveBeenCalled();
    await vi.advanceTimersByTimeAsync(80_000);
    await result;
    expect(settled).toHaveBeenCalledWith(true);
    expect(fetch.mock.calls.length).toBeGreaterThan(1);
    for (const [url, options] of vi.mocked(globalThis.fetch).mock.calls) {
      expect(String(url)).toBe(`http://kupo.test/matches/*@${txHash}`);
      expect(options?.method).toBe("GET");
    }
  });

  it.each([undefined, 600_000])(
    "retains a finite overall deadline with override %s",
    async (timeoutMs) => {
      vi.stubGlobal(
        "fetch",
        vi.fn(async () => Response.json([])),
      );
      const timeout = timeoutMs ?? 160_000;
      const result = provider(timeoutMs).awaitTx(txHash, 500);
      const settled = vi.fn();
      void result.then(settled, settled);
      const rejected = expect(result).rejects.toMatchObject({
        operation: "awaitTx",
        kind: "timeout",
        retryable: true,
      });
      await vi.advanceTimersByTimeAsync(timeout - 1);
      expect(settled).not.toHaveBeenCalled();
      await vi.advanceTimersByTimeAsync(1);
      await rejected;
      expect(settled).toHaveBeenCalledOnce();
    },
  );

  it("keeps the default one-shot request deadline at thirty seconds", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(() => new Promise<Response>(() => {})),
    );
    const result = provider(600_000).getDatum("56".repeat(32));
    const settled = vi.fn();
    void result.then(settled, settled);
    const rejected = expect(result).rejects.toMatchObject({
      operation: "getDatum",
      kind: "timeout",
      retryable: true,
    });
    await vi.advanceTimersByTimeAsync(29_999);
    expect(settled).not.toHaveBeenCalled();
    await vi.advanceTimersByTimeAsync(1);
    await rejected;
    expect(settled).toHaveBeenCalledOnce();
  });
});
