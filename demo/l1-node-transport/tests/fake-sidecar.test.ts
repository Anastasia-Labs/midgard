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
  StreamInterruptedError,
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

  it("intersects at the first known point and refuses an unknown one", async () => {
    const transport = await transportWith();
    const stream = transport.openChainSync({
      points: [chainPoint(99n, hash("99")), chainPoint(20n, hash("02"))],
      credit: 5,
      resume: false,
    });
    expect((await stream.opened).intersection).toEqual(
      chainPoint(20n, hash("02")),
    );
    const [event] = await take(() => stream.next(), 1);
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
