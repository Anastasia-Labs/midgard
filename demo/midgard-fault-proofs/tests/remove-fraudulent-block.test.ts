import { afterEach, describe, expect, it, vi } from "vitest";

import {
  createHttpStateQueueMutationLeaseCoordinator,
  fraudRemovalUsesWalletCoinSelection,
  fraudSlashEconomicsFromDeploymentManifest,
  resolveFraudSlashEconomics,
  submitRemoveFraudulentBlockFromFiles,
} from "../src/remove-fraudulent-block.js";

const publicEconomics = {
  profile: "public-preprod-launch-v1",
  requiredBondLovelace: 100_000_000_000n,
  slashingPenaltyLovelace: 25_000_000_000n,
  inactivitySlashingPenaltyLovelace: 10_000_000_000n,
  fraudProverRewardLovelace: 75_000_000_000n,
  proverCollateralFloorLovelace: 5_000_000n,
} as const;
const boundedEconomics = {
  profile: "bounded-acceptance-v1",
  requiredBondLovelace: 900_000_000n,
  slashingPenaltyLovelace: 500_000_000n,
  inactivitySlashingPenaltyLovelace: 100_000_000n,
  fraudProverRewardLovelace: 400_000_000n,
  proverCollateralFloorLovelace: 5_000_000n,
} as const;

describe("Q53 exact fraud-slash economics", () => {
  it("disables wallet coin selection only for exact bond-backed slash branches", () => {
    expect(fraudRemovalUsesWalletCoinSelection("SlashActiveOperator")).toBe(
      false,
    );
    expect(fraudRemovalUsesWalletCoinSelection("SlashRetiredOperator")).toBe(
      false,
    );
    expect(fraudRemovalUsesWalletCoinSelection("OperatorAlreadySlashed")).toBe(
      true,
    );
  });

  it("binds the public and testnet full/partially-inactivity-slashed tranches", () => {
    expect(
      resolveFraudSlashEconomics(publicEconomics, 100_000_000_000n),
    ).toEqual({
      requiredBondLovelace: 100_000_000_000n,
      fraudProverRewardLovelace: 75_000_000_000n,
      exactFeeLovelace: 25_000_000_000n,
      tranche: "full",
    });
    expect(
      resolveFraudSlashEconomics(publicEconomics, 90_000_000_000n),
    ).toEqual({
      requiredBondLovelace: 100_000_000_000n,
      fraudProverRewardLovelace: 75_000_000_000n,
      exactFeeLovelace: 15_000_000_000n,
      tranche: "partially-inactivity-slashed",
    });
    expect(resolveFraudSlashEconomics(boundedEconomics, 900_000_000n)).toEqual({
      requiredBondLovelace: 900_000_000n,
      fraudProverRewardLovelace: 400_000_000n,
      exactFeeLovelace: 500_000_000n,
      tranche: "full",
    });
    expect(resolveFraudSlashEconomics(boundedEconomics, 800_000_000n)).toEqual({
      requiredBondLovelace: 900_000_000n,
      fraudProverRewardLovelace: 400_000_000n,
      exactFeeLovelace: 400_000_000n,
      tranche: "partially-inactivity-slashed",
    });
  });

  it.each([
    899_999_999n,
    900_000_001n,
    799_999_999n,
    800_000_001n,
    700_000_000n,
  ])("rejects illegal testnet bond tranche %s", (lovelace) => {
    expect(() =>
      resolveFraudSlashEconomics(boundedEconomics, lovelace),
    ).toThrow(/must be exactly/);
  });

  it("rejects a manifest economics tuple with inconsistent slash relations", () => {
    expect(() =>
      resolveFraudSlashEconomics(
        { ...boundedEconomics, fraudProverRewardLovelace: 399_999_999n },
        900_000_000n,
      ),
    ).toThrow(/violate F04 slash relations/u);
    expect(() =>
      resolveFraudSlashEconomics(
        {
          ...boundedEconomics,
          inactivitySlashingPenaltyLovelace: 500_000_000n,
        },
        900_000_000n,
      ),
    ).toThrow(/violate F04 slash relations/u);
  });

  it("selects economics from the release manifest, never the Cardano network label", () => {
    expect(
      fraudSlashEconomicsFromDeploymentManifest({
        economics: {
          ...publicEconomics,
          requiredBondLovelace: Number(publicEconomics.requiredBondLovelace),
          slashingPenaltyLovelace: Number(
            publicEconomics.slashingPenaltyLovelace,
          ),
          inactivitySlashingPenaltyLovelace: Number(
            publicEconomics.inactivitySlashingPenaltyLovelace,
          ),
          fraudProverRewardLovelace: Number(
            publicEconomics.fraudProverRewardLovelace,
          ),
          proverCollateralFloorLovelace: Number(
            publicEconomics.proverCollateralFloorLovelace,
          ),
        },
      }),
    ).toEqual(publicEconomics);
    expect(() =>
      fraudSlashEconomicsFromDeploymentManifest({
        economics: {
          profile: "public-preprod-launch-v1",
          requiredBondLovelace: 900_000_000,
          slashingPenaltyLovelace: 500_000_000,
          inactivitySlashingPenaltyLovelace: 100_000_000,
          fraudProverRewardLovelace: 400_000_000,
          proverCollateralFloorLovelace: 5_000_000,
        },
      }),
    ).toThrow(/requiredBondLovelace must equal/u);
  });
});

/**
 * Responses mirroring midgard-node's `/stateQueueMutationLease` handler
 * (`resolveStateQueueMutationLeaseRequest` in
 * `demo/midgard-node/src/commands/listen-router.ts`): acquire answers
 * `{status:"acquired", token}` at 200 or `{status:"busy", activeLease}` at
 * 409, and renew/release/fail answer `{status:"renewed"|"released"|"failed"}`.
 *
 * This is a fake, not a conformance harness: midgard-node depends on this
 * package, so the joined check cannot live here without a dependency cycle.
 * What it does establish is that the client speaks the documented protocol and
 * fails closed on every response shape the server can actually return -- the
 * previous `${action}ed` responder invented "releaseed" and "acquireed" and so
 * proved nothing about either side.
 */
const leaseEndpointResponse = (
  body: Record<string, unknown>,
): { readonly status: number; readonly body: unknown } => {
  switch (body.action) {
    case "acquire":
      return {
        status: 200,
        body: { status: "acquired", token: "lease-token" },
      };
    case "renew":
      return { status: 200, body: { status: "renewed" } };
    case "release":
      return { status: 200, body: { status: "released" } };
    case "fail":
      return { status: 200, body: { status: "failed" } };
    default:
      return {
        status: 400,
        body: { error: 'Request body must include an "action" field.' },
      };
  }
};

type RecordedCall = {
  readonly url: string;
  readonly method: string | undefined;
  readonly headers: Record<string, string>;
  readonly body: Record<string, unknown>;
};

const stubLeaseEndpoint = (
  respond: (body: Record<string, unknown>) => {
    readonly status: number;
    readonly body: unknown;
  } = leaseEndpointResponse,
): readonly RecordedCall[] => {
  const calls: RecordedCall[] = [];
  vi.stubGlobal(
    "fetch",
    vi.fn(async (url: string, init: RequestInit) => {
      const body = JSON.parse(String(init.body)) as Record<string, unknown>;
      calls.push({
        url,
        method: init.method,
        headers: init.headers as Record<string, string>,
        body,
      });
      const response = respond(body);
      return new Response(JSON.stringify(response.body), {
        status: response.status,
      });
    }),
  );
  return calls;
};

describe("remove-fraudulent-block live-node lease coordinator", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
    delete process.env.TEST_MISSING_MIDGARD_NODE_ADMIN_KEY;
  });

  it("posts acquire, renew, release, and fail actions with admin auth and ttl", async () => {
    const calls = stubLeaseEndpoint();

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
    expect(new Set(calls.map((call) => call.method))).toEqual(
      new Set(["POST"]),
    );
    expect(new Set(calls.map((call) => call.headers["content-type"]))).toEqual(
      new Set(["application/json"]),
    );
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

  it("fails a busy acquire, naming the action, the status and the held lease", async () => {
    // The server's real 409 for a contended lease carries no `error` field: it
    // is `{status:"busy", activeLease}`. The refusal must still name the
    // action and status and surface the held lease rather than swallowing it.
    stubLeaseEndpoint(() => ({
      status: 409,
      body: {
        status: "busy",
        activeLease: { token: "other-token", holder: "block_commitment" },
      },
    }));
    const coordinator = createHttpStateQueueMutationLeaseCoordinator({
      midgardNodeUrl: "http://midgard-node.test",
      adminKey: "secret-admin-key",
    });

    await expect(coordinator.acquire()).rejects.toThrow(
      'POST /stateQueueMutationLease acquire failed with HTTP 409: {"status":"busy","activeLease":{"token":"other-token","holder":"block_commitment"}}',
    );
  });

  it("fails explicit HTTP lease errors with action, status, and response error", async () => {
    stubLeaseEndpoint(() => ({
      status: 409,
      body: { error: "lease already held" },
    }));

    const coordinator = createHttpStateQueueMutationLeaseCoordinator({
      midgardNodeUrl: "http://midgard-node.test",
      adminKey: "secret-admin-key",
    });

    await expect(coordinator.acquire()).rejects.toThrow(
      "POST /stateQueueMutationLease acquire failed with HTTP 409: lease already held",
    );
  });

  it("names the failing action rather than a constant sentence", async () => {
    // Pairs with the two cases above: the same client must compose a message
    // from whichever action and status actually failed, so the pinned sentence
    // is not a constant.
    stubLeaseEndpoint((body) =>
      body.action === "acquire"
        ? { status: 200, body: { status: "acquired", token: "lease-token" } }
        : { status: 500, body: { error: "db failure with table leases" } },
    );
    const coordinator = createHttpStateQueueMutationLeaseCoordinator({
      midgardNodeUrl: "http://midgard-node.test",
      adminKey: "secret-admin-key",
    });
    const lease = await coordinator.acquire();

    await expect(lease.renew()).rejects.toThrow(
      "POST /stateQueueMutationLease renew failed with HTTP 500: db failure with table leases",
    );
  });

  it("refuses an acquire response that is not an acquired lease", async () => {
    // A 200 that does not carry `status:"acquired"` is server drift. Even
    // when a token-shaped field is present the client must fail closed rather
    // than fence a mutation on a lease it was never granted.
    stubLeaseEndpoint(() => ({
      status: 200,
      body: { status: "busy", token: "lease-token", activeLease: null },
    }));
    const coordinator = createHttpStateQueueMutationLeaseCoordinator({
      midgardNodeUrl: "http://midgard-node.test",
      adminKey: "secret-admin-key",
    });

    await expect(coordinator.acquire()).rejects.toThrow(
      "Unexpected state-queue mutation lease acquire response:",
    );
  });

  it("resumes only the exact journaled coordinator source and fencing token", async () => {
    const calls = stubLeaseEndpoint();
    const coordinator = createHttpStateQueueMutationLeaseCoordinator({
      midgardNodeUrl: "http://midgard-node.test",
      adminKey: "secret-admin-key",
      ttlMs: 45_000,
    });
    await expect(
      coordinator.resume?.({
        token: "lease-token",
        source: "http://substituted-node.test",
      }),
    ).rejects.toThrow("different coordinator");
    // The refusal must happen before any request reaches the node.
    expect(calls).toEqual([]);
    const resumed = await coordinator.resume?.({
      token: "lease-token",
      source: "http://midgard-node.test",
    });
    await resumed?.renew();
    expect(calls.map((call) => call.body)).toEqual([
      { action: "renew", token: "lease-token", ttlMs: 45_000 },
    ]);
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
