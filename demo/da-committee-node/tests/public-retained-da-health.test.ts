import type { AddressInfo } from "node:net";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import {
  DA_PUBLIC_RETAINED_DA_PROTOCOLS,
  DA_TRANSPORT_LIMITS,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import {
  PublicRetainedDaListener,
  type PublicRetainedDaListenerStatus,
} from "../src/da/libp2p/PublicRetainedDaListener.js";
import {
  listenPublicRetainedDaHealth,
  publicRetainedDaReadiness,
} from "../src/public-retained-da-health.js";

const DEPLOYMENT_FINGERPRINT = "a1".repeat(32);

type Handler = (stream: unknown, connection: unknown) => Promise<void> | void;

/** A listener on a fake libp2p node whose handlers the test calls directly. */
const listenerWith = async (
  options: { readonly admissionWaitMs?: number } = {},
) => {
  const identity = await loadDaLibp2pIdentity(`seed:${"5d".repeat(32)}`);
  const handlers = new Map<string, Handler>();
  let nowMs = 1_000;
  const listener = new PublicRetainedDaListener({
    deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
    config: {
      peerId: identity.peerId,
      privateKeySource: `seed:${"5d".repeat(32)}`,
      listenMultiaddrs: ["/ip4/127.0.0.1/tcp/0"],
      announceMultiaddrs: [],
      protocols: DA_PUBLIC_RETAINED_DA_PROTOCOLS,
      limits: {
        maxStreamsPerPeer: 4,
        maxInflightRequests: 1,
        maxInflightRequestsPerPeer: 4,
        maxInflightProofRequests: 1,
        requestTimeoutMs: 100,
      },
    },
    store: {
      getDaPayload: async () => undefined,
      getStateQueueHeader: async () => undefined,
    },
    privateKey: identity.privateKey,
    dataLimits: { ...DA_TRANSPORT_LIMITS, requestTimeoutMs: 100 },
    ...options,
    nowMs: () => nowMs,
    libp2pFactory: async () => ({
      start: async (): Promise<void> => undefined,
      stop: async (): Promise<void> => undefined,
      handle: async (protocol, handler): Promise<void> => {
        handlers.set(protocol, handler);
      },
      unhandle: async (): Promise<void> => undefined,
      getMultiaddrs: () => [{ toString: () => "/ip4/127.0.0.1/tcp/4001" }],
    }),
  });
  await listener.start();
  const capabilities = handlers.get(
    daRequestResponseProtocolId(
      DEPLOYMENT_FINGERPRINT,
      DaRequestResponseProtocol.capabilities,
    ),
  );
  if (capabilities === undefined) throw new Error("missing handler");
  /** A request whose stream never sends: it holds its permit to the deadline. */
  const stalled = (peer: string) => {
    let fail!: (error: Error) => void;
    const read = new Promise<never>((_resolve, reject) => {
      fail = reject;
    });
    // A refused request is aborted before anything reads its stream.
    read.catch(() => undefined);
    return capabilities(
      {
        abort: (error: Error) => fail(error),
        close: async () => undefined,
        async *[Symbol.asyncIterator](): AsyncGenerator<Uint8Array> {
          await read;
        },
      },
      { remotePeer: { toString: () => peer } },
    );
  };
  return {
    listener,
    stalled,
    setNow: (ms: number) => {
      nowMs = ms;
    },
  };
};

describe("public retained-DA admission", () => {
  it("lets a request at a full pool wait for the permit and run once it frees", async () => {
    const h = await listenerWith({ admissionWaitMs: 400 });
    const first = h.stalled("peer-a");
    const second = h.stalled("peer-b");
    // The first holds the only permit to its deadline; the second waited and
    // was admitted, so it fails at its own deadline rather than as overload.
    await expect(first).rejects.toThrow(/deadline/u);
    await expect(second).rejects.toThrow(/deadline/u);
    await h.listener.stop();
  });

  it("still refuses as overloaded once the wait runs out or the wait queue is full", async () => {
    const short = await listenerWith({ admissionWaitMs: 20 });
    const held = short.stalled("peer-a");
    await expect(short.stalled("peer-b")).rejects.toThrow(/overloaded/u);
    await expect(held).rejects.toThrow(/deadline/u);
    await short.listener.stop();

    const queued = await listenerWith({ admissionWaitMs: 400 });
    const holder = queued.stalled("peer-a");
    const waiter = queued.stalled("peer-b");
    // The queue holds as many waiters as the pool has permits: one here.
    await expect(queued.stalled("peer-c")).rejects.toThrow(/overloaded/u);
    await expect(holder).rejects.toThrow(/deadline/u);
    await expect(waiter).rejects.toThrow(/deadline/u);
    await queued.listener.stop();
  });

  it("records the last failed request, but not an overload refusal, in its status", async () => {
    const h = await listenerWith({ admissionWaitMs: 0 });
    expect(h.listener.status()).toEqual({ bound: true });
    h.setNow(5_000);
    const held = h.stalled("peer-a");
    await expect(h.stalled("peer-b")).rejects.toThrow(/overloaded/u);
    expect(h.listener.status().lastServedErrorAtMs).toBeUndefined();
    await expect(held).rejects.toThrow(/deadline/u);
    expect(h.listener.status()).toMatchObject({
      bound: true,
      lastServedErrorAtMs: 5_000,
      lastServedError: expect.stringMatching(/deadline/u),
    });
    await h.listener.stop();
    expect(h.listener.status().bound).toBe(false);
  });
});

describe("public retained-DA readiness", () => {
  const ready = (status: PublicRetainedDaListenerStatus) =>
    publicRetainedDaReadiness({
      listener: { status: () => status },
      probeStore: async () => undefined,
    });

  it("is ready when bound with a store that answers, whatever was served before", async () => {
    await expect(ready({ bound: true })).resolves.toMatchObject({
      ready: true,
      reasons: [],
    });
    await expect(
      ready({
        bound: true,
        lastServedErrorAtMs: 10,
        lastServedOkAtMs: 20,
      }),
    ).resolves.toMatchObject({ ready: true });
    // An anonymous caller's request that just failed, with no success after
    // it, is reported but does not hold the reader unready.
    await expect(
      ready({
        bound: true,
        lastServedErrorAtMs: Date.now(),
        lastServedError: "public retained DA request deadline exceeded",
      }),
    ).resolves.toMatchObject({
      ready: true,
      reasons: [],
      listener: {
        lastServedError: "public retained DA request deadline exceeded",
      },
    });
  });

  it("is not ready while unbound or while the store does not answer, and clears by itself", async () => {
    await expect(ready({ bound: false })).resolves.toMatchObject({
      ready: false,
      reasons: ["listener_not_bound"],
    });
    await expect(
      publicRetainedDaReadiness({
        listener: { status: () => ({ bound: true }) },
        probeStore: () => new Promise<void>(() => undefined),
        storeProbeTimeoutMs: 10,
      }),
    ).resolves.toMatchObject({
      ready: false,
      reasons: ["store_unreachable: store did not answer within 10ms"],
    });
  });

  it("serves /healthz always and /readyz by readiness over HTTP", async () => {
    let probeFails = true;
    const server = await listenPublicRetainedDaHealth({
      port: 0,
      host: "127.0.0.1",
      listener: { status: () => ({ bound: true }) },
      probeStore: async () => {
        if (probeFails) throw new Error("connection terminated");
      },
    });
    try {
      const base = `http://127.0.0.1:${(server.address() as AddressInfo).port.toString()}`;
      const health = await fetch(`${base}/healthz`);
      expect(health.status).toBe(200);
      const notReady = await fetch(`${base}/readyz`);
      expect(notReady.status).toBe(503);
      await expect(notReady.json()).resolves.toMatchObject({
        reasons: ["store_unreachable: connection terminated"],
      });
      probeFails = false;
      expect((await fetch(`${base}/readyz`)).status).toBe(200);
    } finally {
      await server.close();
    }
  });
});
