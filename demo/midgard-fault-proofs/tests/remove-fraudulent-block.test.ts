import { afterEach, describe, expect, it, vi } from "vitest";

import {
  createHttpStateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlockFromFiles,
} from "../src/remove-fraudulent-block.js";

describe("remove-fraudulent-block live-node lease coordinator", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
    delete process.env.TEST_MISSING_MIDGARD_NODE_ADMIN_KEY;
  });

  it("posts acquire, renew, release, and fail actions with admin auth and ttl", async () => {
    const calls: Array<{
      readonly url: string;
      readonly headers: Record<string, string>;
      readonly body: Record<string, unknown>;
    }> = [];
    vi.stubGlobal(
      "fetch",
      vi.fn(async (url: string, init: RequestInit) => {
        const body = JSON.parse(String(init.body)) as Record<string, unknown>;
        calls.push({
          url,
          headers: init.headers as Record<string, string>,
          body,
        });
        const responseBody =
          body.action === "acquire"
            ? { status: "acquired", token: "lease-token" }
            : { status: `${String(body.action)}ed` };
        return new Response(JSON.stringify(responseBody), { status: 200 });
      }),
    );

    const coordinator = createHttpStateQueueMutationLeaseCoordinator({
      midgardNodeUrl: "http://midgard-node.test///",
      adminKey: "secret-admin-key",
      ttlMs: 45_000,
    });
    const lease = await coordinator.acquire();
    await lease.renew();
    await lease.release();
    const failedLease = await coordinator.acquire();
    await failedLease.fail("removal failed");

    expect(lease).toMatchObject({
      token: "lease-token",
      source: "http://midgard-node.test",
    });
    expect(calls.map((call) => call.url)).toEqual([
      "http://midgard-node.test/stateQueueMutationLease",
      "http://midgard-node.test/stateQueueMutationLease",
      "http://midgard-node.test/stateQueueMutationLease",
      "http://midgard-node.test/stateQueueMutationLease",
      "http://midgard-node.test/stateQueueMutationLease",
    ]);
    expect(calls.map((call) => call.headers["x-midgard-admin-key"])).toEqual([
      "secret-admin-key",
      "secret-admin-key",
      "secret-admin-key",
      "secret-admin-key",
      "secret-admin-key",
    ]);
    expect(calls.map((call) => call.body)).toEqual([
      {
        action: "acquire",
        holder: "fault_proof_removal",
        ttlMs: 45_000,
      },
      { action: "renew", token: "lease-token", ttlMs: 45_000 },
      { action: "release", token: "lease-token", ttlMs: 45_000 },
      {
        action: "acquire",
        holder: "fault_proof_removal",
        ttlMs: 45_000,
      },
      {
        action: "fail",
        token: "lease-token",
        ttlMs: 45_000,
        error: "removal failed",
      },
    ]);
  });

  it("fails explicit HTTP lease errors with action, status, and response error", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(
        async () =>
          new Response(JSON.stringify({ error: "lease already held" }), {
            status: 409,
          }),
      ),
    );

    const coordinator = createHttpStateQueueMutationLeaseCoordinator({
      midgardNodeUrl: "http://midgard-node.test",
      adminKey: "secret-admin-key",
    });

    await expect(coordinator.acquire()).rejects.toThrow(
      "POST /stateQueueMutationLease acquire failed with HTTP 409: lease already held",
    );
  });

  it("fails missing admin-key configuration before file or provider work", async () => {
    delete process.env.TEST_MISSING_MIDGARD_NODE_ADMIN_KEY;

    await expect(
      submitRemoveFraudulentBlockFromFiles({
        blueprintPath: "missing-plutus.json",
        deploymentInfoPath: "missing-deployment.json",
        network: "Preprod",
        fraudulentHeaderHash: "33".repeat(28),
        midgardNodeUrl: "http://midgard-node.test",
        midgardNodeAdminKeyEnv: "TEST_MISSING_MIDGARD_NODE_ADMIN_KEY",
      }),
    ).rejects.toThrow(
      "pass --midgard-node-admin-key or set TEST_MISSING_MIDGARD_NODE_ADMIN_KEY",
    );
  });
});
