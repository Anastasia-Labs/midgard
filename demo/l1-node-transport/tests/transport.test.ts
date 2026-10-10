import { execFileSync, spawn } from "node:child_process";
import { createServer } from "node:net";

import { afterEach, describe, expect, inject, it } from "vitest";

import {
  type ChainSyncEvent,
  type ChainSyncStream,
  decodeCbor,
  encodeFrame,
  type Frame,
  FrameReader,
  L1NodeTransport,
  ORIGIN,
  queryRewardAccount,
  TransportFailedError,
  type TransportReadiness,
} from "../src/index.js";
import { headerHash, hex, MockNode, within } from "./mock-node.js";

const sidecarBinary = inject("sidecarBinary");
const mockNodeBinary = inject("mockNodeBinary");
const MAGIC = 42;

const owned: Array<{ close(): Promise<void> }> = [];
afterEach(async () => {
  for (const resource of owned.splice(0).reverse()) await resource.close();
});

const mockNode = async (): Promise<MockNode> => {
  const node = await MockNode.start(mockNodeBinary, MAGIC);
  owned.push(node);
  return node;
};

const transportFor = (
  socketPath: string,
  onReadiness?: (readiness: TransportReadiness) => void,
  requestTimeoutMs = 20_000,
  networkMagic = MAGIC,
): L1NodeTransport => {
  const transport = new L1NodeTransport({
    binaryPath: sidecarBinary,
    socketPath,
    networkMagic,
    requestTimeoutMs,
    restartDelayMs: { initial: 50, max: 200 },
    ...(onReadiness === undefined ? {} : { onReadiness }),
  });
  owned.push(transport);
  return transport;
};

const take = async (
  stream: ChainSyncStream,
  count: number,
): Promise<ChainSyncEvent[]> => {
  const events: ChainSyncEvent[] = [];
  while (events.length < count) {
    const event = await within(stream.next(), 15_000);
    if (event === "timeout" || event === undefined)
      throw new Error(`stream stalled after ${events.length} events`);
    events.push(event);
  }
  return events;
};

/** Each event's seq follows the last, with no gap and no repeat. */
const expectContiguous = (events: ChainSyncEvent[], from: bigint): void => {
  events.forEach((event, index) =>
    expect(event.seq).toBe(from + BigInt(index)),
  );
};

describe("sidecar handshake", () => {
  it("reports its frame protocol version", () => {
    expect(
      execFileSync(sidecarBinary, ["--protocol-version"], {
        encoding: "utf8",
      }).trim(),
    ).toBe("1");
  });

  it("refuses an unsupported version with a fatal frame and exit 64", async () => {
    const child = spawn(sidecarBinary, [], { stdio: ["pipe", "pipe", "pipe"] });
    const reader = new FrameReader();
    const frames: Frame[] = [];
    child.stdout.on("data", (chunk: Buffer) =>
      frames.push(...reader.push(chunk)),
    );
    child.stderr.resume();
    const exit = new Promise<number | null>((resolve) =>
      child.once("close", resolve),
    );
    child.stdin.write(
      encodeFrame({
        type: "hello",
        version: 2,
        socketPath: "/nonexistent",
        networkMagic: MAGIC,
      }),
    );
    expect(await exit).toBe(64);
    expect(frames.map((frame) => frame.header)).toEqual([
      expect.objectContaining({ type: "fatal", code: "version_unsupported" }),
    ]);
  });
});

describe("supervisor", () => {
  it("stays unready with a named reason, then recovers once the node listens", async () => {
    const socketPath = MockNode.vacantSocket();
    const seen: TransportReadiness[] = [];
    const transport = transportFor(socketPath, (readiness) =>
      seen.push(readiness),
    );
    await expect(transport.whenReady(500)).rejects.toMatchObject({
      reason: "node_unreachable",
    });
    expect(seen.length).toBeGreaterThan(0);
    expect(seen.every((readiness) => !readiness.ready)).toBe(true);
    expect(
      seen.map((readiness) => !readiness.ready && readiness.reason),
    ).toContain("node_unreachable");
    const node = await MockNode.start(mockNodeBinary, MAGIC, socketPath);
    owned.push(node);
    await transport.whenReady(10_000);
    expect(transport.readiness).toMatchObject({ ready: true });
    await node.extend(1);
    expect(
      decodeCbor(await transport.query({ query: "chain_block_no" })),
    ).toEqual([1, 1]);
  });

  it("fails, and stops restarting, once the node refuses the handshake", async () => {
    const node = await mockNode();
    const seen: TransportReadiness[] = [];
    const transport = transportFor(
      node.socketPath,
      (readiness) => seen.push(readiness),
      20_000,
      MAGIC + 1,
    );
    const failure = await transport.whenReady(10_000).catch((e) => e);
    expect(failure).toBeInstanceOf(TransportFailedError);
    expect(failure).toMatchObject({ reason: "node_handshake_failed" });
    expect(transport.readiness).toMatchObject({
      ready: false,
      failed: true,
      reason: "node_handshake_failed",
    });
    // Past many restart delays: no further sidecar, the readiness unchanged.
    await new Promise((resolve) => setTimeout(resolve, 1_000));
    expect(seen).toHaveLength(1);
    // Every call and stream fails at once, and names the fault.
    await expect(
      transport.query({ query: "chain_block_no" }),
    ).rejects.toBeInstanceOf(TransportFailedError);
    expect(() =>
      transport.openChainSync({ points: [ORIGIN], credit: 1 }),
    ).toThrow(TransportFailedError);
  });

  it("restarts with backoff while the node drops the connection during the handshake", async () => {
    const socketPath = MockNode.vacantSocket();
    let accepted = 0;
    const dropping = createServer((socket) => {
      accepted += 1;
      socket.destroy();
    });
    await new Promise<void>((resolve) => dropping.listen(socketPath, resolve));
    owned.push({
      close: () =>
        new Promise<void>((resolve) => dropping.close(() => resolve())),
    });
    const seen: TransportReadiness[] = [];
    const transport = transportFor(socketPath, (readiness) =>
      seen.push(readiness),
    );
    await expect(transport.whenReady(1_500)).rejects.toMatchObject({
      reason: "node_connection_lost",
    });
    expect(accepted).toBeGreaterThan(1);
    expect(seen.every((r) => !r.ready && !("failed" in r))).toBe(true);
    // Once a node answers on the socket, the transport recovers.
    await new Promise<void>((resolve) => dropping.close(() => resolve()));
    const node = await MockNode.start(mockNodeBinary, MAGIC, socketPath);
    owned.push(node);
    await transport.whenReady(10_000);
    expect(transport.readiness).toMatchObject({ ready: true });
  });
});

describe("chain-sync", () => {
  it("intersects at the first known point, rolls back to it, and delivers raw blocks in order", async () => {
    const node = await mockNode();
    const blocks = await node.extend(6);
    const transport = transportFor(node.socketPath);
    const unknown = {
      kind: "point",
      slot: 999n,
      hash: "ab".repeat(32),
    } as const;
    const third = {
      kind: "point",
      slot: BigInt(blocks[2]!.slot),
      hash: blocks[2]!.hash,
    } as const;
    const stream = transport.openChainSync({
      points: [unknown, third, ORIGIN],
      credit: 10,
    });
    owned.push(stream);
    const opened = await stream.opened;
    expect(opened.intersection).toEqual(third);
    expect(opened.tip.blockNo).toBe(6n);
    const [rollback, ...events] = await take(stream, 4);
    expectContiguous([rollback!, ...events], 1n);
    // The intersection is not the first point (the consumer's position), so
    // the consumer is moved back to it before the blocks that follow it.
    expect(rollback).toMatchObject({ kind: "roll_backward", point: third });
    events.forEach((event, index) => {
      const block = blocks[3 + index]!;
      expect(event.kind).toBe("roll_forward");
      if (event.kind !== "roll_forward") return;
      // Raw bytes exactly as the node sent them; the hash is the header's.
      expect(hex(event.block)).toBe(block.raw);
      expect(headerHash(event.block)).toBe(block.hash);
      expect(event.point).toEqual({
        kind: "point",
        slot: BigInt(block.slot),
        hash: block.hash,
      });
      expect(event.blockNo).toBe(BigInt(block.number));
      expect(event.prevHash).toBe(block.prev);
    });
  });

  it("reports an intersection that is not found", async () => {
    const node = await mockNode();
    await node.extend(2);
    const transport = transportFor(node.socketPath);
    const stream = transport.openChainSync({
      points: [{ kind: "point", slot: 5n, hash: "cd".repeat(32) }],
      credit: 1,
    });
    await expect(stream.opened).rejects.toMatchObject({
      name: "IntersectNotFoundError",
    });
    await expect(stream.next()).rejects.toMatchObject({
      name: "IntersectNotFoundError",
    });
  });

  it("never delivers more than the credit beyond the last ack", async () => {
    const node = await mockNode();
    await node.extend(12);
    const transport = transportFor(node.socketPath);
    const before = (await node.stats()).requestNexts;
    const stream = transport.openChainSync({ points: [ORIGIN], credit: 3 });
    owned.push(stream);
    await stream.opened;
    const first = await take(stream, 3);
    const pending = stream.next();
    expect(await within(pending, 500)).toBe("timeout");
    // One extra request answers the intersection rollback.
    expect((await node.stats()).requestNexts - before).toBe(4);
    stream.ack(first[1]!.seq);
    // Acking seq 2 returns two credits: seq 4 and 5, then nothing more.
    const more = [(await pending)!, ...(await take(stream, 1))];
    const blocked = stream.next();
    expect(await within(blocked, 500)).toBe("timeout");
    expect((await node.stats()).requestNexts - before).toBe(6);
    stream.ack(5n);
    const rest = [(await blocked)!, ...(await take(stream, 1))];
    expectContiguous([...first, ...more, ...rest], 1n);
  });

  it("orders a rollback in the sequence and follows the new branch", async () => {
    const node = await mockNode();
    const blocks = await node.extend(5);
    const transport = transportFor(node.socketPath);
    const stream = transport.openChainSync({ points: [ORIGIN], credit: 20 });
    owned.push(stream);
    expectContiguous(await take(stream, 5), 1n);
    await node.rollback(3);
    const branch = await node.extend(2, 1);
    const events = await take(stream, 3);
    expectContiguous(events, 6n);
    expect(events[0]).toMatchObject({
      kind: "roll_backward",
      point: {
        kind: "point",
        slot: BigInt(blocks[2]!.slot),
        hash: blocks[2]!.hash,
      },
    });
    expect(events.slice(1).map((event) => event.point)).toEqual(
      branch.map((block) => ({
        kind: "point",
        slot: BigInt(block.slot),
        hash: block.hash,
      })),
    );
  });

  it("resumes across a sidecar restart with no gap and no duplicate", async () => {
    const node = await mockNode();
    await node.extend(4);
    const reasons: string[] = [];
    const transport = transportFor(node.socketPath, (readiness) => {
      if (!readiness.ready) reasons.push(readiness.reason);
    });
    const stream = transport.openChainSync({ points: [ORIGIN], credit: 5 });
    owned.push(stream);
    const before = await take(stream, 4);
    stream.ack(2n);
    // The node drops every connection: the sidecar ends and is restarted.
    await node.command({ op: "drop" });
    await node.extend(3);
    const after = await take(stream, 3);
    expectContiguous([...before, ...after], 1n);
    const chain = await node.chain();
    expect(
      [...before, ...after].map((event) =>
        event.kind === "roll_forward" ? event.point.hash : "rollback",
      ),
    ).toEqual(chain.map((block) => block.hash));
    expect(reasons).toContain("node_connection_lost");
  });

  it("resumes with a rollback when the last delivered block left the chain", async () => {
    const node = await mockNode();
    const blocks = await node.extend(5);
    const transport = transportFor(node.socketPath);
    const stream = transport.openChainSync({ points: [ORIGIN], credit: 10 });
    owned.push(stream);
    await take(stream, 5);
    stream.ack(5n);
    await node.command({ op: "drop" });
    await node.rollback(3);
    const branch = await node.extend(2, 2);
    const events = await take(stream, 3);
    expectContiguous(events, 6n);
    expect(events[0]).toMatchObject({
      kind: "roll_backward",
      point: { kind: "point", hash: blocks[2]!.hash },
    });
    expect(events.slice(1).map((event) => event.point)).toMatchObject(
      branch.map((block) => ({ hash: block.hash })),
    );
  });
});

describe("local state query, submit and monitor", () => {
  it("answers queries with the node's raw result", async () => {
    const node = await mockNode();
    const blocks = await node.extend(3);
    const transport = transportFor(node.socketPath);
    expect(
      decodeCbor(await transport.query({ query: "system_start" })),
    ).toEqual([2022, 100, 0]);
    await transport.withLedgerState("tip", async (state) => {
      const point = decodeCbor(await state.query({ query: "chain_point" }));
      expect(point).toEqual(
        [blocks[2]!.slot, Buffer.from(blocks[2]!.hash, "hex")].map((item) =>
          typeof item === "number" ? item : Uint8Array.from(item),
        ),
      );
      expect(
        decodeCbor(await state.query({ query: "chain_block_no" })),
      ).toEqual([1, 3]);
      // One acquisition serves every query of the session.
      expect((await node.stats()).acquired).toEqual(["tip", "tip"]);
    });
    const second = blocks[1]!;
    await transport.withLedgerState(
      { kind: "point", slot: BigInt(second.slot), hash: second.hash },
      async (state) => {
        await state.query({ query: "chain_point" });
        await state.query({ query: "system_start" });
      },
    );
    expect((await node.stats()).acquired).toEqual([
      "tip",
      "tip",
      `${second.slot}:${second.hash}`,
    ]);
    // The mock echoes a Shelley query's body: the sidecar's encoding.
    const address = Uint8Array.of(0x60, ...new Uint8Array(28).fill(7));
    expect(
      decodeCbor(
        await transport.query({
          query: "utxo_by_address",
          addresses: [address],
        }),
      ),
    ).toEqual([6, [address]]);
  });

  it("reads a reward account from one acquired state", async () => {
    const node = await mockNode();
    await node.extend(2);
    const hash = "11".repeat(28);
    const key = `8200581c${hash}`;
    // {[0, h]: 2000000}
    await node.command({ op: "answer", tag: 22, raw: `a1${key}1a001e8480` });
    // [{[0, h]: pool}, {[0, h]: 15}]
    await node.command({
      op: "answer",
      tag: 10,
      raw: `82a1${key}581c${"22".repeat(28)}a1${key}0f`,
    });
    const transport = transportFor(node.socketPath);
    const account = await queryRewardAccount(transport, { type: "Key", hash });
    expect(account).toMatchObject({
      registered: true,
      depositLovelace: 2_000_000n,
      rewardsLovelace: 15n,
      poolIdHash: "22".repeat(28),
      blockNo: 2n,
    });
    const other = await queryRewardAccount(transport, {
      type: "Script",
      hash,
    });
    expect(other).toMatchObject({
      registered: false,
      depositLovelace: null,
      rewardsLovelace: 0n,
      poolIdHash: null,
    });
  });

  it("refuses a slow ledger request with node_timeout and keeps the sidecar and its streams", async () => {
    const node = await mockNode();
    await node.extend(2);
    const reasons: string[] = [];
    // The sidecar's own deadline is half the client's bound: 500 ms.
    const transport = transportFor(
      node.socketPath,
      (readiness) => {
        if (!readiness.ready) reasons.push(readiness.reason);
      },
      1_000,
    );
    const stream = transport.openChainSync({ points: [ORIGIN], credit: 10 });
    owned.push(stream);
    expectContiguous(await take(stream, 2), 1n);
    await node.command({ op: "ledgerDelay", ms: 2_000 });
    const slow = transport.query({ query: "system_start" });
    await node.extend(2);
    await expect(slow).rejects.toMatchObject({
      name: "TransportRequestError",
      code: "node_timeout",
    });
    expectContiguous(await take(stream, 2), 3n);
    await node.command({ op: "ledgerDelay", ms: 0 });
    // Once the late answer is in, the next request is answered as usual.
    await new Promise((resolve) => setTimeout(resolve, 2_000));
    expect(
      decodeCbor(await transport.query({ query: "chain_block_no" })),
    ).toEqual([1, 4]);
    expect(reasons).toEqual([]);
  });

  it("returns submit rejections as raw bytes and tracks the mempool", async () => {
    const node = await mockNode();
    await node.extend(1);
    const sample = await node.command({ op: "sampleTx" });
    const tx = Uint8Array.from(Buffer.from(sample.tx as string, "hex"));
    const transport = transportFor(node.socketPath);
    const reason = "8201820203";
    await node.command({ op: "reject", raw: reason });
    const rejected = await transport.submit(tx);
    expect(rejected.accepted).toBe(false);
    if (!rejected.accepted) expect(hex(rejected.rejection)).toBe(reason);
    expect(await transport.hasTx(sample.id as string)).toBe(false);
    await node.command({ op: "reject", raw: null });
    expect(await transport.submit(tx)).toEqual({ accepted: true });
    expect(await transport.hasTx(sample.id as string)).toBe(true);
    expect((await transport.mempoolSizes()).txCount).toBe(1n);
    await node.command({ op: "clearMempool" });
    expect(await transport.hasTx(sample.id as string)).toBe(false);
  });
});
