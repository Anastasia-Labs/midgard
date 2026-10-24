import * as SDK from "@al-ft/midgard-sdk";
import { Kupmios, KupmiosError } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  isRetryableProviderError,
  runProviderStepWithRetry,
} from "../src/provider-retry.js";

// These run the installed Lucid Kupmios provider over a stubbed `fetch`, so
// the errors classified are the ones a real Kupo or Ogmios answer produces.

const kupmios = () => new Kupmios("http://kupo.test", "http://ogmios.test");

const stubFetch = (response: () => Response): { calls: () => number } => {
  let calls = 0;
  vi.stubGlobal("fetch", async () => {
    calls += 1;
    return response();
  });
  return { calls: () => calls };
};

const failureOf = async (read: () => Promise<unknown>): Promise<unknown> => {
  try {
    await read();
  } catch (error) {
    return error;
  }
  throw new Error("expected the provider read to fail");
};

// Ogmios's HTTP endpoint answers every JSON-RPC error with status 400.
const ogmiosErrorAnswer = (code: number) => () =>
  new Response(
    JSON.stringify({
      jsonrpc: "2.0",
      method: "queryLedgerState/protocolParameters",
      error: { code, message: `answer ${code.toString()}` },
      id: null,
    }),
    { status: 400, headers: { "content-type": "application/json" } },
  );

afterEach(() => {
  vi.unstubAllGlobals();
});

describe("isRetryableProviderError honours Kupmios's typed retryability", () => {
  it.each([
    [429, true],
    [503, true],
    [400, false],
  ])("a Kupmios HTTP %s error is retryable: %s", (status, retryable) => {
    const error = new KupmiosError({
      protocol: "kupo",
      operation: "getUtxos",
      status,
    });
    expect(error.retryable).toBe(retryable);
    expect(isRetryableProviderError(error)).toBe(retryable);
  });

  it.each([
    [503, true],
    [400, false],
  ])(
    "a Kupo HTTP %s answer through the real provider is retryable: %s",
    async (status, retryable) => {
      const fetch = stubFetch(() => new Response("unavailable", { status }));
      const error = await failureOf(() => kupmios().getUtxos("addr_test1"));
      expect(fetch.calls()).toBe(1);
      expect(isRetryableProviderError(error)).toBe(retryable);
    },
  );

  it.each([
    [503, true],
    [400, false],
  ])(
    'a "Failed to fetch" wrapper around a Kupo HTTP %s answer is retryable: %s',
    async (status, retryable) => {
      stubFetch(() => new Response("answer", { status }));
      const cause = await failureOf(() => kupmios().getUtxos("addr_test1"));
      const wrapped = new SDK.LucidError({
        message: "Failed to fetch hub-oracle witness UTxO(s)",
        cause,
      });
      expect(isRetryableProviderError(wrapped)).toBe(retryable);
      // The reference-script read wraps its failure the same way.
      const referenceScriptRead = new SDK.StateQueueError({
        message: "Failed to fetch reference script UTxOs",
        cause,
      });
      expect(isRetryableProviderError(referenceScriptRead)).toBe(retryable);
    },
  );
});

describe("isRetryableProviderError reads the code of an Ogmios error answer", () => {
  it.each([2000, 2001, 2002, 2003, -32603])(
    "an Ogmios %s answer (the node cannot answer now) is retryable",
    async (code) => {
      stubFetch(ogmiosErrorAnswer(code));
      const error = await failureOf(() => kupmios().getProtocolParameters());
      // The provider's own flag follows the HTTP 400 and says no.
      expect(error).toMatchObject({ kind: "json_rpc", retryable: false });
      expect(isRetryableProviderError(error)).toBe(true);
    },
  );

  it.each([-32700, -32600, -32601, -32602, 1000, 2004, 3005, 4000])(
    "an Ogmios %s answer (a refused request) is not retryable",
    async (code) => {
      stubFetch(ogmiosErrorAnswer(code));
      const error = await failureOf(() => kupmios().getProtocolParameters());
      expect(isRetryableProviderError(error)).toBe(false);
    },
  );

  it("retries a transient Ogmios answer a bounded number of times", async () => {
    const fetch = stubFetch(ogmiosErrorAnswer(2003));
    const outcome = await Effect.runPromise(
      Effect.either(
        runProviderStepWithRetry(
          "protocol parameters",
          Effect.tryPromise(() => kupmios().getProtocolParameters()),
          { maxAttempts: 3, baseDelayMs: 0, maxDelayMs: 0 },
        ),
      ),
    );
    expect(outcome._tag).toBe("Left");
    expect(fetch.calls()).toBe(3);
  });

  it("does not repeat a refused Ogmios request", async () => {
    const fetch = stubFetch(ogmiosErrorAnswer(-32602));
    const outcome = await Effect.runPromise(
      Effect.either(
        runProviderStepWithRetry(
          "protocol parameters",
          Effect.tryPromise(() => kupmios().getProtocolParameters()),
          { maxAttempts: 3, baseDelayMs: 0, maxDelayMs: 0 },
        ),
      ),
    );
    expect(outcome._tag).toBe("Left");
    expect(fetch.calls()).toBe(1);
  });
});
