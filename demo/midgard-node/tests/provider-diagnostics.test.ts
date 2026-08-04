import "./utils.js";

import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { Effect } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import { runL1ProviderPreflight } from "@/commands/l1-provider-preflight.js";
import {
  classifyProviderHttpResponse,
  clearProviderCooldownsForTest,
  parseRetryAfterMs,
  redactEndpoint,
} from "@/provider-diagnostics.js";
import { NodeConfig } from "@/services/config.js";

const config = {
  L1_PROVIDER: "Kupmios" as const,
  L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 1_000,
  L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS: 60_000,
  L1_OGMIOS_KEY: "http://127.0.0.1:1337",
  L1_KUPO_KEY: "http://127.0.0.1:1442",
  NETWORK: "Preprod" as const,
};

const textResponse = (body: string, init?: ResponseInit): Response =>
  new Response(body, {
    status: 200,
    ...init,
  });

describe("provider diagnostics", () => {
  beforeEach(() => {
    clearProviderCooldownsForTest();
  });

  it("classifies quota, HTML, and recent-tx not-found responses distinctly", () => {
    expect(parseRetryAfterMs("60", 1_000)).toEqual(60_000);
    expect(
      classifyProviderHttpResponse({
        status: 429,
        body: '{"message":"Too many requests"}',
        retryAfter: "60",
      }),
    ).toMatchObject({
      kind: "rate_limited",
      retryable: true,
      rateLimitEligible: true,
      retryAfterMs: 60_000,
    });
    expect(
      classifyProviderHttpResponse({
        status: 502,
        body: "<html>bad gateway</html>",
      }),
    ).toMatchObject({
      kind: "html_response",
      retryable: true,
      rateLimitEligible: true,
    });
    expect(
      classifyProviderHttpResponse({
        status: 404,
        body: '{"error":"not found"}',
        knownRecentTx: true,
      }),
    ).toMatchObject({
      kind: "recent_tx_not_found",
      retryable: true,
    });
    expect(
      classifyProviderHttpResponse({
        status: 404,
        body: '{"error":"not found"}',
      }),
    ).toMatchObject({
      kind: "permanent_not_found",
      retryable: false,
    });
  });

  it("redacts route URLs", () => {
    expect(
      redactEndpoint(
        "https://user:secret@example.test/api/v0?project_id=leak#frag",
      ),
    ).toEqual("https://example.test/api/v0");
  });

  it("rejects deprecated L1_PROVIDER_FAILOVER even when empty", async () => {
    const previous = process.env.L1_PROVIDER_FAILOVER;
    process.env.L1_PROVIDER_FAILOVER = "";
    try {
      await expect(
        Effect.runPromise(
          Effect.gen(function* () {
            return yield* NodeConfig;
          }).pipe(Effect.provide(NodeConfig.layer)),
        ),
      ).rejects.toThrow(/L1_PROVIDER_FAILOVER is no longer supported/);
    } finally {
      if (previous === undefined) {
        delete process.env.L1_PROVIDER_FAILOVER;
      } else {
        process.env.L1_PROVIDER_FAILOVER = previous;
      }
    }
  });

  it("passes only when local Kupo and Ogmios health endpoints are reachable", async () => {
    const nowMs = 1_000_000;
    const healthBody = {
      connectionStatus: "connected",
      networkSynchronization: 1,
      lastKnownTip: { slot: 41 },
      lastTipUpdate: new Date(nowMs).toISOString(),
    };
    const calls: string[] = [];
    const report = await runL1ProviderPreflight({
      config,
      nowMs,
      fetchImpl: async (url, init) => {
        calls.push(url);
        if (url === "http://127.0.0.1:1442/health") {
          return textResponse("ok");
        }
        if (url === "http://127.0.0.1:1337/health") {
          return textResponse(JSON.stringify(healthBody));
        }
        expect(init?.method).toBe("POST");
        return textResponse(
          JSON.stringify({
            jsonrpc: "2.0",
            result: { slot: 42 },
            id: "midgard-submit-slot",
          }),
        );
      },
    });

    expect(report.ok).toEqual(true);
    expect(report.degraded).toEqual(false);
    expect(report.route).toEqual({
      primary: "kupmios",
      network: "Preprod",
    });
    expect(report.healthySources).toEqual(["kupmios"]);
    expect(report.unhealthySources).toEqual([]);
    expect(report.sources[0].localLedgerSlot).toMatchObject({
      source: "local_ogmios_tip",
      currentSlot: 42,
    });
    expect(calls).toEqual([
      "http://127.0.0.1:1442/health",
      "http://127.0.0.1:1337/health",
      "http://127.0.0.1:1337/health",
      "http://127.0.0.1:1337",
    ]);
  });

  it("fails when a local Kupmios health endpoint is unhealthy", async () => {
    const report = await runL1ProviderPreflight({
      config,
      fetchImpl: async () =>
        new Response("<html>maintenance</html>", {
          status: 503,
          headers: { "content-type": "text/html" },
        }),
    });

    expect(report.ok).toEqual(false);
    expect(report.healthySources).toEqual([]);
    expect(report.sources[0]).toMatchObject({
      source: "kupmios",
      healthy: false,
      failureKind: "html_response",
    });
  });

  it("preserves the nested network cause needed to diagnose local provider failures", async () => {
    const failure = new TypeError("fetch failed", {
      cause: new Error("getaddrinfo EAI_AGAIN kupo"),
    });
    const report = await runL1ProviderPreflight({
      config,
      fetchImpl: async () => {
        throw failure;
      },
    });

    expect(report.sources[0]).toMatchObject({
      source: "kupmios",
      healthy: false,
      failureKind: "network_error",
      bodySummary:
        "TypeError: fetch failed; cause=Error: getaddrinfo EAI_AGAIN kupo",
    });
  });

  it("keeps forbidden remote provider wiring out of node runtime sources", async () => {
    const runtimeFiles = [
      "src/services/config.ts",
      "src/services/lucid.ts",
      "src/provider-diagnostics.ts",
      "src/commands/l1-provider-preflight.ts",
      "src/transactions/phas-membership-registration.ts",
      "src/commands/l1-utxos.ts",
      "src/commands/address-from-seed.ts",
    ];
    const forbidden = [
      /Koios/,
      /koios/,
      /api\.koios/,
      /preprod\.koios/,
      /preview\.koios/,
      /cardano-preprod\.blockfrost\.io/,
      /cardano-preview\.blockfrost\.io/,
      /blockfrost-fallback-key/,
    ];

    for (const relativePath of runtimeFiles) {
      const text = await readFile(join(process.cwd(), relativePath), "utf8");
      for (const pattern of forbidden) {
        expect(text, `${relativePath} contains ${pattern.source}`).not.toMatch(
          pattern,
        );
      }
    }
  });
});
