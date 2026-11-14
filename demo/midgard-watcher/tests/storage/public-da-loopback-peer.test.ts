import { encodeDaPayloadByHeaderResponseCbor } from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import { parseWatcherConfig } from "../../src/runtime/config.js";
import { createWatcherRetainedDaRuntime } from "../../src/storage/retained-da-runtime.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import {
  libp2pTransportFixture,
  rawConfig,
} from "./retained-da-runtime.fixtures.js";

describe("named-network stack loopback DA peer", () => {
  const peerId = "12D3KooWJzVqLz7QpLdfW6M5G2X1L8L6GQ9QJ3uCHZP8X8J6BC8u";
  const address = `/ip4/127.0.0.1/tcp/4141/p2p/${peerId}`;

  it("dials a named-network stack's own loopback peer its config admits", async () => {
    const common = rawConfig(address);
    const watcherConfig = parseWatcherConfig({
      ...common,
      da: { ...common.da, stackLoopbackPeers: true },
    });
    const f = libp2pTransportFixture(
      peerId,
      encodeDaPayloadByHeaderResponseCbor({
        status: "not_found",
        headerHash: Buffer.from("ab".repeat(28), "hex"),
        payloadHash: null,
        payloadBytes: null,
        chunkManifest: null,
        reasonCode: null,
      }),
    );
    await f.transport.start();
    const runtime = await createWatcherRetainedDaRuntime({
      watcherConfig,
      deploymentIdentity: makeWatcherDeploymentAuthorityFixture().result,
      unsafeTransportFactoryForTest: async () => f.transport,
    });
    try {
      const result = await runtime.sources[0]!.fetchPayloadByHeaderHash(
        "ab".repeat(28),
      );
      expect(result.attempts.map((attempt) => attempt.status)).toEqual(
        result.attempts.map(() => "not_found"),
      );
      expect(f.dialProtocol.mock.calls.length).toBeGreaterThan(0);
    } finally {
      await runtime.close();
    }
  });
});
