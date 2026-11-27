import assert from "node:assert/strict";
import { describe, it } from "node:test";

import {
  formatWebhookPayload,
  parseWallets,
  runChecks,
} from "./preprod-monitor.mjs";

const jsonResponse = (status, body) =>
  new Response(JSON.stringify(body), {
    status,
    headers: { "content-type": "application/json" },
  });

describe("preprod-monitor", () => {
  it("parses wallet lists and rejects malformed entries", () => {
    assert.deepEqual(parseWallets("op1=addr_test1aaa, prover=addr_test1bbb"), [
      { label: "op1", address: "addr_test1aaa" },
      { label: "prover", address: "addr_test1bbb" },
    ]);
    assert.throws(() => parseWallets("broken-entry"), /Invalid/);
  });

  it("formats Discord vs Slack webhook payloads", () => {
    const alerts = [{ id: "node:readyz", ok: false, message: "HTTP 503" }];
    const discord = formatWebhookPayload(
      "https://discord.com/api/webhooks/1/tok",
      alerts,
    );
    assert.equal(typeof discord.content, "string");
    assert.equal("text" in discord, false);

    const slack = formatWebhookPayload(
      "https://hooks.slack.com/services/T/B/X",
      alerts,
    );
    assert.equal(typeof slack.text, "string");
    assert.deepEqual(slack.alerts, alerts);
  });

  it("returns ok when node, watcher, and wallets are healthy and posts no webhook", async () => {
    let webhookPosted = false;
    const mockFetch = async (url) => {
      if (url.endsWith("/readyz")) return jsonResponse(200, { ready: true });
      if (url.endsWith("/pipeline-status"))
        return jsonResponse(200, {
          pendingBlockFinalizations: { oldestActive: { ageMs: 1_000 } },
          stateQueue: { unconfirmedSubmittedBlockAgeMs: 0 },
        });
      if (url.endsWith("/watcher-health"))
        return jsonResponse(200, { ok: true });
      if (url.includes("/addresses/addr_test1op1"))
        return jsonResponse(200, {
          amount: [{ unit: "lovelace", quantity: "80000000" }],
        });
      webhookPosted = true;
      return new Response(null, { status: 204 });
    };

    const result = await runChecks(
      {
        MIDGARD_NODE_URL: "https://preprod.midgard.example",
        MIDGARD_WATCHER_HEALTH_URL: "http://127.0.0.1:7402/watcher-health",
        WALLET_CHECK_ADDRESSES: "op1=addr_test1op1",
        WALLET_MIN_LOVELACE: "50000000",
        ALERT_WEBHOOK_URL: "https://discord.com/api/webhooks/1/tok",
      },
      mockFetch,
    );
    assert.equal(result.ok, true);
    assert.equal(webhookPosted, false);
  });

  it("reports unhealthy readyz, stalled pipeline, low wallet, and posts webhook", async () => {
    const posted = [];
    const mockFetch = async (url, init) => {
      if (init?.method === "POST") {
        posted.push({ url, body: JSON.parse(init.body) });
        return new Response(null, { status: 204 });
      }
      if (url.endsWith("/readyz"))
        return jsonResponse(503, { ready: false, reasons: ["l1_lag"] });
      if (url.endsWith("/pipeline-status"))
        return jsonResponse(200, {
          pendingBlockFinalizations: { oldestActive: { ageMs: 950_000 } },
          stateQueue: { unconfirmedSubmittedBlockAgeMs: 0 },
        });
      if (url.includes("/addresses/addr_test1op1"))
        return jsonResponse(404, { error: "Not Found" });
      return jsonResponse(500, {});
    };

    const result = await runChecks(
      {
        MIDGARD_NODE_URL: "https://preprod.midgard.example",
        WALLET_CHECK_ADDRESSES: "op1=addr_test1op1",
        STALL_THRESHOLD_MS: "900000",
        ALERT_WEBHOOK_URL: "https://discord.com/api/webhooks/1/tok",
      },
      mockFetch,
    );
    assert.equal(result.ok, false);
    assert.equal(result.checks.filter((c) => !c.ok).length, 3);
    assert.equal(posted.length, 1);
    assert.match(posted[0].body.content, /l1_lag/);
    assert.match(posted[0].body.content, /stalled/);
    assert.match(posted[0].body.content, /low balance/);
  });
});
