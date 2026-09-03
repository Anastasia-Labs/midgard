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

  it("resumes only the exact journaled coordinator source and fencing token", async () => {
    const calls: Record<string, unknown>[] = [];
    vi.stubGlobal(
      "fetch",
      vi.fn(async (_url: string, init: RequestInit) => {
        calls.push(JSON.parse(String(init.body)) as Record<string, unknown>);
        return new Response(JSON.stringify({ status: "renewed" }), {
          status: 200,
        });
      }),
    );
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
    const resumed = await coordinator.resume?.({
      token: "lease-token",
      source: "http://midgard-node.test",
    });
    await resumed?.renew();
    expect(calls).toEqual([
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
