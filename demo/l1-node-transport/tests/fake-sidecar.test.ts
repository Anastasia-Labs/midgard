import { mkdtemp, realpath, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  chainPoint,
  type ChainSyncEvent,
  IntersectNotFoundError,
  L1NodeTransport,
  ORIGIN,
  queryRewardAccount,
  SidecarExitedError,
  StreamInterruptedError,
  type StreamInterruption,
  TransportProtocolError,
  type TransportReadiness,
  TransportUnavailableError,
} from "../src/index.js";
import { writeFakeSidecar } from "../testing/fake-sidecar.mjs";

// The fake sidecar stands in for the binary in callers' tests; this checks
// it against the same client the binary is checked against.

const MAGIC = 42;
const handlerModule = fileURLToPath(
  new URL("./fixtures/fake-handler.mjs", import.meta.url),
);
const hash = (byte: string): string => byte.repeat(32);

let directory: string;
const owned: L1NodeTransport[] = [];
beforeEach(async () => {
  directory = await realpath(await mkdtemp(join(tmpdir(), "fake-sidecar-")));
});
afterEach(async () => {
  await Promise.all(owned.splice(0).map((transport) => transport.close()));
  await rm(directory, { recursive: true, force: true });
});

const transportWith = async (
  options: Record<string, unknown> = {},
  networkMagic = MAGIC,
): Promise<L1NodeTransport> => {
  const binaryPath = await writeFakeSidecar({
    path: join(directory, `fake-${owned.length}`),
    handlerModule,
    options: { magic: MAGIC, ...options },
  });
  const transport = new L1NodeTransport({
    binaryPath,
    socketPath: join(directory, "node.socket"),
    networkMagic,
    requestTimeoutMs: 10_000,
    restartDelayMs: { initial: 50, max: 200 },
  });
  owned.push(transport);
  return transport;
};

const take = async (
  next: () => Promise<ChainSyncEvent | undefined>,
  count: number,
): Promise<ChainSyncEvent[]> => {
  const events: ChainSyncEvent[] = [];
  while (events.length < count) {
    const event = await next();
    if (event === undefined) throw new Error("stream ended");
    events.push(event);
  }
  return events;
};

describe("fake sidecar", () => {
  it("follows from the origin within the credit and rolls back", async () => {
    const transport = await transportWith({ rollBackAfter: true });
    await transport.whenReady(10_000);
    const stream = transport.openChainSync({
      points: [ORIGIN],
      credit: 2,
      resume: false,
    });
    const opened = await stream.opened;
    expect(opened.intersection).toEqual(ORIGIN);
    const events = await take(() => stream.next(), 2);
    expect(stream.lastSeq).toBe(2n);
    // The window holds the third block until an ack frees credit.
    stream.ack(2n);
    events.push(...(await take(() => stream.next(), 2)));
    expect(events.map((event) => [event.kind, event.seq])).toEqual([
      ["roll_forward", 1n],
      ["roll_forward", 2n],
      ["roll_forward", 3n],
      ["roll_backward", 4n],
    ]);
    const first = events[0]!;
    expect(first.kind === "roll_forward" && first.prevHash).toBe(hash("00"));
    expect(first.kind === "roll_forward" && [...first.block]).toEqual([
      0x82, 1, 1,
    ]);
    expect(events[3]!.point).toEqual(chainPoint(10n, hash("01")));
    await stream.close();
    expect(await stream.ended).toBeNull();
  });

  it("intersects at the first known point, rolls back to it, and refuses an unknown one", async () => {
    const transport = await transportWith();
    const stream = transport.openChainSync({
      points: [chainPoint(99n, hash("99")), chainPoint(20n, hash("02"))],
      credit: 5,
      resume: false,
    });
    expect((await stream.opened).intersection).toEqual(
      chainPoint(20n, hash("02")),
    );
    // As with the sidecar: the intersection is not the first point (the
    // consumer's position), so a rollback to it comes first.
    const [rollback, event] = await take(() => stream.next(), 2);
    expect(rollback).toMatchObject({
      kind: "roll_backward",
      seq: 1n,
      point: chainPoint(20n, hash("02")),
    });
    expect(event!.point).toEqual(chainPoint(30n, hash("03")));
    await stream.close();
    const missing = transport.openChainSync({
      points: [chainPoint(99n, hash("99"))],
      credit: 1,
    });
    await expect(missing.opened).rejects.toBeInstanceOf(IntersectNotFoundError);
  });

  it("ends a stream it fails", async () => {
    const transport = await transportWith({ failAfter: true });
    const stream = transport.openChainSync({
      points: [ORIGIN],
      credit: 10,
      resume: false,
    });
    await take(() => stream.next(), 3);
    await expect(stream.next()).rejects.toBeInstanceOf(StreamInterruptedError);
    expect(await stream.ended).toBeInstanceOf(StreamInterruptedError);
  });

  it("records each failure a resuming stream reopens from, and tells the consumer", async () => {
    const transport = await transportWith({ failAfter: true });
    const told: StreamInterruption[] = [];
    const stream = transport.openChainSync({
      points: [ORIGIN],
      credit: 10,
      onInterrupted: (interruption) => told.push(interruption),
    });
    await take(() => stream.next(), 3);
    // The handler fails every open: the stream keeps reopening, counted.
    const deadline = Date.now() + 10_000;
    while (told.length < 2 && Date.now() < deadline)
      await new Promise((resolve) => setTimeout(resolve, 20));
    expect(told.slice(0, 2)).toEqual([
      {
        consecutive: 1,
        total: 1,
        last: expect.stringContaining("node_connection_lost") as unknown,
      },
      {
        consecutive: 2,
        total: 2,
        last: expect.stringContaining("fake fault") as unknown,
      },
    ]);
    expect(stream.interruptions).toEqual(told.at(-1));
    await stream.close();
    expect(await stream.ended).toBeNull();
  });

  it("fails a stream whose sequence skips a number", async () => {
    const transport = await transportWith({ skipSequenceAt: 1 });
    const stream = transport.openChainSync({
      points: [ORIGIN],
      credit: 5,
      resume: false,
    });
    const [first] = await take(() => stream.next(), 1);
    expect(first!.seq).toBe(1n);
    await expect(stream.next()).rejects.toBeInstanceOf(TransportProtocolError);
    expect(await stream.ended).toBeInstanceOf(TransportProtocolError);
  });

  it("restarts a sidecar whose answer frame is malformed, without throwing", async () => {
    const seen: TransportReadiness[] = [];
    const transport = await transportWith({ malformedAnswer: true });
    transport.onReadiness((readiness) => seen.push(readiness));
    await transport.whenReady(10_000);
    await expect(transport.mempoolSizes()).rejects.toBeInstanceOf(
      SidecarExitedError,
    );
    expect(seen).toContainEqual(
      expect.objectContaining({
        ready: false,
        reason: "sidecar_restarting",
        detail: expect.stringContaining("answer id is not a natural number"),
      }),
    );
    await transport.whenReady(10_000);
    expect(transport.readiness).toMatchObject({ ready: true });
  });

  it("answers ledger queries, submission and the mempool", async () => {
    const transport = await transportWith();
    const account = await queryRewardAccount(transport, {
      type: "Script",
      hash: "cd".repeat(28),
    });
    expect(account).toMatchObject({
      registered: true,
      depositLovelace: 2_000_000n,
      rewardsLovelace: 5n,
      poolIdHash: null,
      blockNo: 3n,
    });
    expect(await transport.submit(Uint8Array.from([1]))).toEqual({
      accepted: true,
    });
    const rejected = await transport.submit(Uint8Array.from([2]));
    expect(rejected.accepted === false && [...rejected.rejection]).toEqual([
      0x80,
    ]);
    expect(await transport.hasTx(hash("ab"))).toBe(true);
    expect(await transport.hasTx(hash("cd"))).toBe(false);
    expect(await transport.mempoolSizes()).toEqual({
      capacity: 100n,
      size: 10n,
      txCount: 1n,
    });
  });

  it("kills a sidecar that keeps running after its input closes", async () => {
    const transport = await transportWith({ ignoreInputEnd: true });
    await transport.whenReady(10_000);
    const started = performance.now();
    await transport.close();
    // The orderly stop waits its bound, then kills; close returns only once
    // the process is gone.
    expect(performance.now() - started).toBeGreaterThanOrEqual(4_900);
    expect(transport.readiness).toMatchObject({
      ready: false,
      reason: "stopped",
    });
  }, 15_000);

  it("refuses a handshake as the node does", async () => {
    const transport = await transportWith({}, MAGIC + 1);
    const failure = await transport.whenReady(2_000).catch((error) => error);
    expect(failure).toBeInstanceOf(TransportUnavailableError);
    expect((failure as TransportUnavailableError).reason).toBe(
      "node_handshake_failed",
    );
  });
});
