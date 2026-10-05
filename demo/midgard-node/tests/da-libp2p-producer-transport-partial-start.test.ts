import { createServer } from "node:net";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { describe, expect, it } from "vitest";

import {
  createDaLibp2pProducerTransport,
  type DaProducerStreamHandler,
  parseDaProducerPublicationManifest,
} from "../src/da/libp2p-producer.js";
import {
  closeServer,
  listenOnLoopback,
  PRODUCER_PRIVATE_KEY_SOURCE,
  runtimeManifestFixture,
  serverPort,
} from "./da-payload-libp2p-producer.manifest-fixture.js";

const freeLoopbackPort = async (): Promise<number> => {
  const server = await listenOnLoopback();
  const port = serverPort(server);
  await closeServer(server);
  return port;
};

/** Resolves to the bind error code, or "free" after binding and releasing. */
const probePort = (port: number): Promise<string> =>
  new Promise((resolve) => {
    const server = createServer();
    server.once("error", (error: NodeJS.ErrnoException) =>
      resolve(error.code ?? error.message),
    );
    server.listen(port, "127.0.0.1", () => {
      server.close(() => resolve("free"));
    });
  });

const bindListenManifest = async (port: number) => {
  const identity = await loadDaLibp2pIdentity(PRODUCER_PRIVATE_KEY_SOURCE);
  const manifest = parseDaProducerPublicationManifest(
    runtimeManifestFixture(identity.peerId, port),
    { DA_LIBP2P_PRIVATE_KEY_SOURCE: PRODUCER_PRIVATE_KEY_SOURCE },
  );
  if (manifest === null) {
    throw new Error("expected libp2p publication manifest");
  }
  return manifest;
};

const noopHandler: DaProducerStreamHandler = async () => {};

/** Yields one protocol twice, so libp2p refuses the second registration
 * after the node has already bound its listen address. */
class DuplicateYieldingHandlers extends Map<string, DaProducerStreamHandler> {
  override *[Symbol.iterator](): MapIterator<
    [string, DaProducerStreamHandler]
  > {
    yield ["/midgard/test/duplicate/1", noopHandler];
    yield ["/midgard/test/duplicate/1", noopHandler];
  }
}

describe("DA libp2p producer transport partial start", () => {
  it("releases the bound listen socket when start fails after the bind, so a retry binds the same port", async () => {
    const port = await freeLoopbackPort();
    const manifest = await bindListenManifest(port);

    await expect(
      createDaLibp2pProducerTransport(manifest, {
        mode: "bind-listen",
        requestHandlers: new DuplicateYieldingHandlers(),
      }),
    ).rejects.toThrow(/already registered/u);
    expect(await probePort(port)).toBe("free");

    const retried = await createDaLibp2pProducerTransport(manifest, {
      mode: "bind-listen",
      requestHandlers: new Map([["/midgard/test/single/1", noopHandler]]),
    });
    await retried.close?.();
  });

  it("keeps exactly one listener on the port for a successful start and frees it on close", async () => {
    const port = await freeLoopbackPort();
    const manifest = await bindListenManifest(port);

    const transport = await createDaLibp2pProducerTransport(manifest, {
      mode: "bind-listen",
      requestHandlers: new Map([["/midgard/test/single/1", noopHandler]]),
    });
    try {
      expect(await probePort(port)).toBe("EADDRINUSE");
      await expect(
        createDaLibp2pProducerTransport(manifest, { mode: "bind-listen" }),
      ).rejects.toThrow(/EADDRINUSE|address already in use/iu);
      expect(await probePort(port)).toBe("EADDRINUSE");
    } finally {
      await transport.close?.();
    }
    expect(await probePort(port)).toBe("free");
  });
});
