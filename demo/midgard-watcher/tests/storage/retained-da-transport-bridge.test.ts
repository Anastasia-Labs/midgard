import { describe, expect, it, vi } from "vitest";

import type { WatcherPublicDaLibp2pTransport } from "../../src/storage/public-da-libp2p-transport.js";
import { createWatcherRetainedDaRuntimeOwner } from "../../src/storage/retained-da-runtime.js";
import { harness } from "../fault-proofs/fault-decision-bridge.harness.js";
import {
  headerFixture,
  observation,
} from "../fault-proofs/fault-decision-bridge.observation.js";
import {
  ATTESTED,
  operations,
  records,
} from "../fault-proofs/fault-decision-bridge.released-fixture.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import {
  PEER_ID,
  rawConfig,
  transportFactory,
} from "./retained-da-runtime.fixtures.js";

const AUTHORITY = makeWatcherDeploymentAuthorityFixture();

/** Inside every fixture header's challengeability window (they end at 2 ms). */
const IN_WINDOW = () => 3n;

describe("fault decision bridge over the retained-DA runtime owner", () => {
  it("defers while the public-DA transport cannot start, then classifies and targets the fault exactly once", async () => {
    const fake = transportFactory();
    let failures = 1;
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
      deploymentIdentity: AUTHORITY.result,
      unsafeTransportFactoryForTest: factory,
    });
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [faulty] = current.finalizedHeaders;
    const observability = operations();
    const warn = vi.fn();
    const h = harness({
      current,
      categoryByHeader: { [faulty!.headerHash]: "doubleSpend" },
      operationsSink: observability.sink,
      nowMs: IN_WINDOW,
      monotonicNowMs: () => monotonic,
      warn,
      classifyOverride: async (fresh) => {
        const runtime = await owner.createRuntime(rawConfig());
        await runtime.close();
        return fresh;
      },
    });
    try {
      // The start fails: nothing is journaled, the header waits.
      expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
      expect(factory).toHaveBeenCalledOnce();
      expect(owner.transportStatus().state).toBe("failed");
      // Inside the owner's 1 s delay the lease is refused without a start.
      monotonic += 500;
      await h.bridge.reconcileAndDispatch(current);
      expect(factory).toHaveBeenCalledOnce();
      expect(h.enqueued).toEqual([]);
      expect(records(observability).map(({ outcome }) => outcome)).toEqual([
        "pending_da",
        "pending_da",
      ]);
      expect(warn).toHaveBeenCalledExactlyOnceWith({
        event: "classification_deferred",
        headerHash: faulty!.headerHash,
        cause: "retained_da_transport_unavailable",
      });
      // Once the delay has passed the transport starts and the fault is
      // classified and targeted once.
      monotonic += 500;
      await h.bridge.reconcileAndDispatch(current);
      expect(factory).toHaveBeenCalledTimes(2);
      expect(owner.transportStatus()).toEqual({ state: "open", failure: null });
      expect(h.bridge.status().target?.headerHash).toBe(faulty!.headerHash);
      expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
        faulty!.headerHash,
      ]);
      monotonic += 1_000_000;
      await h.bridge.reconcileAndDispatch(current);
      expect(h.application.classifyHeader).toHaveBeenCalledTimes(3);
      expect(h.enqueuedGenerations).toHaveLength(2);
      expect(new Set(h.enqueuedGenerations).size).toBe(1);
    } finally {
      clock.mockRestore();
      await owner.close();
    }
  });

  it("still fails closed when the owner refuses a changed DA configuration", async () => {
    const fake = transportFactory();
    const owner = createWatcherRetainedDaRuntimeOwner({
      deploymentIdentity: AUTHORITY.result,
      unsafeTransportFactoryForTest: fake.factory,
    });
    await (await owner.createRuntime(rawConfig())).close();
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [header] = current.finalizedHeaders;
    const h = harness({
      current,
      categoryByHeader: { [header!.headerHash]: "doubleSpend" },
      nowMs: IN_WINDOW,
      classifyOverride: async (fresh) => {
        await owner.createRuntime(
          rawConfig(`/dns4/da-b.example/tcp/443/p2p/${PEER_ID}`),
        );
        return fresh;
      },
    });
    try {
      await expect(h.bridge.reconcileAndDispatch(current)).rejects.toThrow(
        "retained-DA owner configuration changed",
      );
      expect(h.enqueued).toEqual([]);
    } finally {
      await owner.close();
    }
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
