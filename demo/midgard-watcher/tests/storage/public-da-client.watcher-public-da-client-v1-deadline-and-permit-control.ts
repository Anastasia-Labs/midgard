import "./public-da-client.watcher-public-da-client-v1-peer-failover.js";

import { describe, expect, it } from "vitest";

import {
  hangUntilAborted,
  HEADER_HASH,
  makeVirtualClock,
  PEERS,
  type PeerScript,
  ScriptedTransport,
} from "./public-da-client.raw-config.js";
import {
  capabilitiesBytes,
  clientWith,
  expectClientError,
  fixture,
  honestInlineScript,
  payloadByHeaderBytes,
  statuses,
} from "./public-da-client.watcher-public-da-client-v1-construction.js";

// ---------------------------------------------------------------------------
// 8. Deadline and permit concurrency control
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 deadline and permit control", () => {
  it("bounds a hung content validator by the fetch deadline and releases its permit", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    const client = clientWith(
      transport,
      { maxConcurrency: 1 },
      makeVirtualClock(),
    );
    let validationSignal: AbortSignal | undefined;
    const failure = await expectClientError(
      client.fetchPayloadByHeader({
        headerHash: HEADER_HASH,
        validateInnerPayload: async (_, signal) => {
          validationSignal = signal;
          await new Promise(() => undefined);
        },
      }),
    );
    expect(failure.code).toBe("deadline_exceeded");
    expect(validationSignal?.aborted).toBe(true);
    expect(
      (await client.fetchPayloadByHeader({ headerHash: HEADER_HASH }))
        .headerHash,
    ).toBe(HEADER_HASH);
    client.close();
  });

  it("closing aborts active requests and rejects queued permits without waiting for their deadlines", async () => {
    let activeSignal: AbortSignal | undefined;
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: (request) => {
          activeSignal = request.signal;
          return hangUntilAborted(request);
        },
      },
    });
    const client = clientWith(transport, { maxConcurrency: 1 });
    const first = client
      .fetchPayloadByHeader({ headerHash: HEADER_HASH })
      .catch((error: unknown) => error);
    const second = client
      .fetchPayloadByHeader({ headerHash: HEADER_HASH })
      .catch((error: unknown) => error);
    await new Promise((resolve) => setImmediate(resolve));
    client.close();
    expect(activeSignal?.aborted).toBe(true);
    expect(await first).toBeInstanceOf(Error);
    expect(await second).toBeInstanceOf(Error);
    await expect(
      client.fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    ).rejects.toThrow(/closed/);
    expect(transport.calls).toHaveLength(1);
  });

  type Gate = {
    readonly waitUntilEntered: Promise<void>;
    readonly release: () => void;
    inFlight: number;
    maxInFlight: number;
    completed: string[];
  };

  const gatedScript = (
    gate: Gate,
    enteredResolve: () => void,
  ): Record<string, PeerScript> => ({
    [PEERS[0]!]: {
      capabilities: () => capabilitiesBytes(),
      "payload-by-header": async () => {
        gate.inFlight += 1;
        gate.maxInFlight = Math.max(gate.maxInFlight, gate.inFlight);
        enteredResolve();
        await gate.waitUntilEntered;
        gate.inFlight -= 1;
        return payloadByHeaderBytes({
          status: "found_inline",
          payloadHash: fixture.payloadHash,
          payloadBytes: fixture.envelope,
        });
      },
    },
  });

  const makeGate = (): { gate: Gate; entered: () => void } => {
    let release: () => void = () => undefined;
    const waitUntilEntered = new Promise<void>((resolve) => {
      release = resolve;
    });
    const gate: Gate = {
      waitUntilEntered,
      release,
      inFlight: 0,
      maxInFlight: 0,
      completed: [],
    };
    return { gate, entered: () => undefined };
  };

  it("serializes requests when maxConcurrency is 1", async () => {
    const { gate } = makeGate();
    const transport = new ScriptedTransport(gatedScript(gate, () => undefined));
    const client = clientWith(transport, { maxConcurrency: 1 });

    const first = client.fetchPayloadByHeader({ headerHash: HEADER_HASH });
    const second = client.fetchPayloadByHeader({ headerHash: HEADER_HASH });

    // Let the event loop run: only the first request may reach the transport.
    await new Promise((resolve) => setImmediate(resolve));
    expect(gate.inFlight).toBe(1);
    expect(
      transport.calls.filter((c) => c.protocol === "payload-by-header"),
    ).toHaveLength(1);

    gate.release();
    await Promise.all([first, second]);
    expect(gate.maxInFlight).toBe(1);
    expect(
      transport.calls.filter((c) => c.protocol === "payload-by-header"),
    ).toHaveLength(2);
  });

  it("runs requests in parallel up to maxConcurrency", async () => {
    const { gate } = makeGate();
    const transport = new ScriptedTransport(gatedScript(gate, () => undefined));
    const client = clientWith(transport, { maxConcurrency: 3 });

    const pending = [0, 1, 2].map(() =>
      client.fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    await new Promise((resolve) => setImmediate(resolve));
    await new Promise((resolve) => setImmediate(resolve));
    expect(gate.maxInFlight).toBe(3);

    gate.release();
    await Promise.all(pending);
  });

  it("drains the permit queue in submission order", async () => {
    const { gate } = makeGate();
    const order: number[] = [];
    const transport = new ScriptedTransport(gatedScript(gate, () => undefined));
    const client = clientWith(transport, { maxConcurrency: 1 });

    const pending = [0, 1, 2, 3].map((index) =>
      client
        .fetchPayloadByHeader({ headerHash: HEADER_HASH })
        .then(() => order.push(index)),
    );
    gate.release();
    await Promise.all(pending);

    expect(order).toEqual([0, 1, 2, 3]);
    expect(gate.maxInFlight).toBe(1);
  });

  it("releases the permit even when the fetch fails, so later requests proceed", async () => {
    let attempt = 0;
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "payload-by-header": () => {
          attempt += 1;
          return attempt === 1
            ? payloadByHeaderBytes({ status: "not_found" })
            : payloadByHeaderBytes({
                status: "found_inline",
                payloadHash: fixture.payloadHash,
                payloadBytes: fixture.envelope,
              });
        },
      },
    });
    const client = clientWith(transport, { maxConcurrency: 1 });

    const failure = await expectClientError(
      client.fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(failure.code).toBe("all_peers_failed");

    const result = await client.fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });
    expect(result.payloadHash).toBe(fixture.payloadHash.toString("hex"));
  });

  it("clamps each per-request timeout to the configured request timeout", async () => {
    const transport = new ScriptedTransport(honestInlineScript());
    await clientWith(transport, {
      requestTimeoutMs: 2_500,
      daFetchMs: 60_000,
    }).fetchPayloadByHeader({ headerHash: HEADER_HASH });

    for (const call of transport.calls) {
      expect(call.timeoutMs).toBeLessThanOrEqual(2_500);
      expect(call.timeoutMs).toBeGreaterThan(0);
    }
  });

  it("shrinks the per-request timeout as the fetch deadline is consumed", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: { capabilities: hangUntilAborted },
      [PEERS[1]!]: { capabilities: hangUntilAborted },
      [PEERS[2]!]: { capabilities: hangUntilAborted },
    });
    await expectClientError(
      clientWith(
        transport,
        {
          peerCount: 3,
          requestTimeoutMs: 400,
          daFetchMs: 1_000,
        },
        makeVirtualClock(),
      ).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );

    expect(transport.calls).toHaveLength(3);
    expect(transport.calls[0]!.timeoutMs).toBe(400);
    // Third dial can only receive whatever is left of the 1s fetch budget:
    // two 400ms dials are spent, so exactly 200ms remain.
    expect(transport.calls[2]!.timeoutMs).toBe(200);
  });

  it("aborts the in-flight transport call when a request times out", async () => {
    let aborted = false;
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: async (request) =>
          new Promise<Uint8Array>((_, reject) => {
            request.signal.addEventListener("abort", () => {
              aborted = true;
              reject(new Error("aborted"));
            });
          }),
      },
    });
    const error = await expectClientError(
      clientWith(
        transport,
        {
          requestTimeoutMs: 120,
          daFetchMs: 5_000,
        },
        makeVirtualClock(),
      ).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["timeout"]);
    expect(aborted).toBe(true);
  });

  it("fails the whole fetch with deadline_exceeded once the budget is spent", async () => {
    const transport = new ScriptedTransport(
      Object.fromEntries(
        PEERS.map((peer) => [peer, { capabilities: hangUntilAborted }]),
      ),
    );
    // Virtual time, so the budget is spent by the dials the client itself
    // chose (400 + 400 + 200 = the whole 1s), not by whatever the host
    // scheduler happened to do under load. Without it the last sliver of the
    // budget can be handed to another peer as a clamped sub-millisecond dial,
    // and the fetch reports the peer failure instead of the deadline (#535).
    const error = await expectClientError(
      clientWith(
        transport,
        {
          peerCount: 5,
          requestTimeoutMs: 400,
          daFetchMs: 1_000,
        },
        makeVirtualClock(),
      ).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );

    expect(error.code).toBe("deadline_exceeded");
    expect(statuses(error.attempts)).toEqual([
      "timeout",
      "timeout",
      "timeout",
      "deadline_exceeded",
    ]);
    // The budget is spent before every peer is dialed.
    expect(error.attempts.length).toBeLessThan(PEERS.length + 1);
  });
});
