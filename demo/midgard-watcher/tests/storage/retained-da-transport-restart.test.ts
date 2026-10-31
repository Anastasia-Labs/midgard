import { describe, expect, it, vi } from "vitest";

import type { WatcherPublicDaLibp2pTransport } from "../../src/storage/public-da-libp2p-transport.js";
import { watcherRetainedDaTransportRetryDelayMs } from "../../src/storage/retained-da-runtime.create-watcher-retained-da-runtime-owner.js";
import { createWatcherRetainedDaRuntimeOwner } from "../../src/storage/retained-da-runtime.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import {
  PEER_ID,
  rawConfig,
  transportFactory,
} from "./retained-da-runtime.fixtures.js";

const AUTHORITY = makeWatcherDeploymentAuthorityFixture();

const deploymentIdentity = () => AUTHORITY.result;

describe("retained-DA runtime owner transport restart", () => {
  const headerHash = "ab".repeat(28);

  it("starts a failed transport again once, after the retry delay, and keeps the admitted configuration pinned", async () => {
    const fake = transportFactory();
    let failures = 2;
    const factory = vi.fn(async (): Promise<WatcherPublicDaLibp2pTransport> => {
      if (failures > 0) {
        failures -= 1;
        throw new Error("listen EADDRINUSE: address already in use");
      }
      return await fake.factory();
    });
    let monotonic = 10_000;
    const clock = vi
      .spyOn(performance, "now")
      .mockImplementation(() => monotonic);
    const owner = createWatcherRetainedDaRuntimeOwner({
      deploymentIdentity: deploymentIdentity(),
      unsafeTransportFactoryForTest: factory,
    });
    try {
      await expect(owner.createRuntime(rawConfig())).rejects.toThrow(
        "EADDRINUSE",
      );
      // 1 s after the first failure the start is tried again.
      monotonic += 999;
      await expect(owner.createRuntime(rawConfig())).rejects.toThrow(
        "EADDRINUSE",
      );
      expect(factory).toHaveBeenCalledTimes(1);
      monotonic += 1;
      await expect(owner.createRuntime(rawConfig())).rejects.toThrow(
        "EADDRINUSE",
      );
      expect(factory).toHaveBeenCalledTimes(2);
      // The second consecutive failure waits 2 s.
      monotonic += 1_999;
      await expect(owner.createRuntime(rawConfig())).rejects.toThrow(
        "EADDRINUSE",
      );
      expect(factory).toHaveBeenCalledTimes(2);
      // A changed DA configuration is still refused, failure or not.
      await expect(
        owner.createRuntime(
          rawConfig(`/dns4/da-b.example/tcp/443/p2p/${PEER_ID}`),
        ),
      ).rejects.toThrow("retained-DA owner configuration changed");
      monotonic += 1;
      const [first, second] = await Promise.all([
        owner.createRuntime(rawConfig()),
        owner.createRuntime(rawConfig()),
      ]);
      expect(factory).toHaveBeenCalledTimes(3);
      expect(owner.transportStatus()).toEqual({ state: "open", failure: null });
      await first.sources[0]!.fetchPayloadByHeaderHash(headerHash);
      expect(fake.request).toHaveBeenCalledOnce();
      await first.close();
      await second.close();
      await owner.createRuntime(rawConfig());
      expect(factory).toHaveBeenCalledTimes(3);
    } finally {
      clock.mockRestore();
      await owner.close();
    }
    expect(fake.stop).toHaveBeenCalledOnce();
  });

  it("spaces transport start retries from one second, doubling to a one-minute cap", () => {
    expect(
      [1, 2, 3, 7, 50].map(watcherRetainedDaTransportRetryDelayMs),
    ).toEqual([1_000, 2_000, 4_000, 60_000, 60_000]);
  });
});
vi.mock("@al-ft/midgard-core/deployment-profile", async (load) => {
  const actual =
    await load<typeof import("@al-ft/midgard-core/deployment-profile")>();
  return {
    ...actual,
    verifyDeploymentProfileBinding(
      profile: unknown,
      digest: unknown,
      network: unknown,
    ) {
      if (network === "Custom") {
        expect(profile).toEqual(
          actual.DEPLOYMENT_PROFILES["local-devnet-testing"],
        );
        expect(digest).toBe(
          actual.DEPLOYMENT_PROFILE_DIGESTS["local-devnet-testing"],
        );
      } else {
        actual.verifyDeploymentProfileBinding(profile, digest, network);
      }
    },
  };
});
