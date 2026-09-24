import { describe, expect, it } from "vitest";

import {
  type LedgerSnapshotPoint,
  readAcquiredLedgerSnapshot,
} from "../src/l1-ledger-snapshot.js";
import type { WebSocketLike } from "../src/l1-tx-order-carriage.js";

const point = { slot: 123, id: "ab".repeat(32) };
const fork = { slot: 123, id: "cd".repeat(32) };
const policy = "ef".repeat(28);
const addresses = ["deposit", "withdrawal", "retention", "hub"];
const output = {
  transaction: { id: "01".repeat(32) },
  index: 0,
  address: "deposit",
  value: { ada: { lovelace: 2_000_000 }, [policy]: { "": 1 } },
  datum: "d87980",
};

type Request = { id: number; method: string; params: Record<string, unknown> };
type Reply = (request: Request) => unknown;

/** Wire-level transport seam: real request framing, IDs, lossless JSON parsing
 * and session lifecycle; no Cardano node or branch authority is simulated. */
class Socket implements WebSocketLike {
  readonly requests: Request[] = [];
  readonly listeners = new Map<string, ((event: never) => void)[]>();
  closed = false;
  connect = () => this.emit("open");
  rawReply: ((request: Request) => string) | undefined;
  reply: Reply = ({ method }) => {
    switch (method) {
      case "queryLedgerState/tip":
        return point;
      case "acquireLedgerState":
        return { acquired: "ledgerState", point };
      case "queryLedgerState/utxo":
        return [output];
      case "releaseLedgerState":
        return { released: "ledgerState" };
      default:
        throw new Error(`Unexpected method ${method}`);
    }
  };
  emit(type: string, event?: unknown) {
    for (const listener of this.listeners.get(type) ?? [])
      listener(event as never);
  }
  addEventListener(type: string, listener: (event: never) => void) {
    this.listeners.set(type, [...(this.listeners.get(type) ?? []), listener]);
  }
  send(data: string) {
    const request = JSON.parse(data) as Request;
    this.requests.push(request);
    queueMicrotask(() => {
      if (this.closed) return;
      const data =
        this.rawReply?.(request) ??
        JSON.stringify({
          jsonrpc: "2.0",
          id: request.id,
          result: this.reply(request),
        });
      this.emit("message", { data });
    });
  }
  close() {
    if (this.closed) return;
    this.closed = true;
    this.emit("close");
  }
  read(signal?: AbortSignal, timeoutMs = 200, at?: LedgerSnapshotPoint) {
    return readAcquiredLedgerSnapshot({
      ogmiosUrl: "http://localhost:1337/ogmios",
      addresses,
      at,
      timeoutMs,
      signal,
      webSocketFactory: (url) => {
        expect(url).toBe("ws://localhost:1337/ogmios");
        queueMicrotask(() => this.connect());
        return this;
      },
    });
  }
}

describe("acquired node ledger snapshot", () => {
  it("captures every address at one exact acquired point, then releases and closes", async () => {
    const socket = new Socket();
    const snapshot = await socket.read();
    expect(socket.requests.map(({ method }) => method)).toEqual([
      "queryLedgerState/tip",
      "acquireLedgerState",
      "queryLedgerState/utxo",
      "queryLedgerState/tip",
      "releaseLedgerState",
    ]);
    expect(socket.requests[1]!.params).toEqual({ point });
    expect(socket.requests[2]!.params).toEqual({
      addresses: [...addresses].sort(),
    });
    expect(snapshot.point).toEqual(point);
    expect(snapshot.outputs[0]).toMatchObject({
      txHash: output.transaction.id,
      outputIndex: 0,
      assets: { lovelace: 2_000_000n, [policy]: 1n },
      datum: "d87980",
      hasReferenceScript: false,
    });
    expect(Object.isFrozen(snapshot.outputs[0]!.assets)).toBe(true);
    expect(socket.closed).toBe(true);
  });

  it("acquires an explicit historical point without querying or substituting the current tip", async () => {
    const socket = new Socket();
    const snapshot = await socket.read(undefined, 200, point);
    expect(snapshot.point).toEqual(point);
    expect(socket.requests.map(({ method }) => method)).toEqual([
      "acquireLedgerState",
      "queryLedgerState/utxo",
      "queryLedgerState/tip",
      "releaseLedgerState",
    ]);
    expect(socket.requests[0]!.params).toEqual({ point });
    expect(socket.closed).toBe(true);
  });

  it("copies the requested point before opening so caller mutation cannot retarget acquisition", async () => {
    const socket = new Socket();
    const requested = { ...point };
    const pending = socket.read(undefined, 200, requested);
    requested.id = fork.id;
    const snapshot = await pending;
    expect(snapshot.point).toEqual(point);
    expect(socket.requests[0]!.params).toEqual({ point });
  });

  it("refuses unavailable historical state without falling back to any tip", async () => {
    const socket = new Socket();
    socket.rawReply = (request) =>
      JSON.stringify({
        id: request.id,
        error: { code: 2000, message: "point too old" },
      });
    await expect(socket.read(undefined, 200, point)).rejects.toThrow(
      /point too old/,
    );
    expect(socket.requests.map(({ method }) => method)).toEqual([
      "acquireLedgerState",
    ]);
    expect(socket.closed).toBe(true);
  });

  it("refuses substitution of the historical point before reading any outputs", async () => {
    const socket = new Socket();
    await expect(socket.read(undefined, 200, fork)).rejects.toThrow(
      /different ledger point/,
    );
    expect(socket.requests.map(({ method }) => method)).toEqual([
      "acquireLedgerState",
    ]);
    expect(socket.closed).toBe(true);
  });

  it.each([
    { slot: -1, id: point.id },
    { slot: 100, id: "bad" },
    { slot: Number.MAX_SAFE_INTEGER + 1, id: point.id },
  ])(
    "rejects malformed requested historical point %j before opening",
    async (at) => {
      const socket = new Socket();
      await expect(socket.read(undefined, 200, at)).rejects.toThrow();
      expect(socket.requests).toEqual([]);
      expect(socket.closed).toBe(false);
    },
  );

  it("preserves native quantities beyond Number precision and distinguishes script-bearing donations", async () => {
    const socket = new Socket();
    const reply = socket.reply;
    socket.rawReply = (request) =>
      JSON.stringify({
        id: request.id,
        result:
          request.method === "queryLedgerState/utxo"
            ? [
                {
                  ...output,
                  value: {
                    ada: { lovelace: "BIG_QUANTITY" },
                    [policy]: { "": "BIG_QUANTITY" },
                  },
                  script: { language: "plutus:v3", cbor: "00" },
                },
              ]
            : reply(request),
      }).replaceAll('"BIG_QUANTITY"', "9007199254740993");
    const snapshot = await socket.read();
    expect(snapshot.outputs[0]!.assets).toEqual({
      lovelace: 9007199254740993n,
      [policy]: 9007199254740993n,
    });
    expect(snapshot.outputs[0]!.hasReferenceScript).toBe(true);
  });

  it.each([
    [
      "wrong acquired point",
      "acquireLedgerState",
      { acquired: "ledgerState", point: fork },
      /different ledger point/,
    ],
    [
      "partial envelope",
      "queryLedgerState/utxo",
      { outputs: [output], next: "cursor" },
      /complete array/,
    ],
    [
      "duplicate outref",
      "queryLedgerState/utxo",
      [output, output],
      /repeats an output/,
    ],
    [
      "foreign address",
      "queryLedgerState/utxo",
      [{ ...output, address: "elsewhere" }],
      /outside/,
    ],
    [
      "ambiguous datum",
      "queryLedgerState/utxo",
      [{ ...output, datumHash: "ff".repeat(32) }],
      /both inline/,
    ],
    [
      "negative quantity",
      "queryLedgerState/utxo",
      [{ ...output, value: { ada: { lovelace: -1 } } }],
      /natural number/,
    ],
    [
      "invalid release",
      "releaseLedgerState",
      { released: "mempool" },
      /did not release/,
    ],
  ] as const)(
    "refuses %s and closes its state",
    async (_label, method, result, message) => {
      const socket = new Socket();
      const reply = socket.reply;
      socket.reply = (request) =>
        request.method === method ? result : reply(request);
      await expect(socket.read()).rejects.toThrow(message);
      expect(socket.closed).toBe(true);
    },
  );

  it("refuses a changed point during capture", async () => {
    const socket = new Socket();
    const reply = socket.reply;
    let reads = 0;
    socket.reply = (request) =>
      request.method === "queryLedgerState/tip" && ++reads > 1
        ? fork
        : reply(request);
    await expect(socket.read()).rejects.toThrow(/point changed/);
    expect(socket.closed).toBe(true);
  });

  it.each(["null", '{"id":2,"result":[],"result":[]}'])(
    "rejects malformed/ambiguous wire response %s",
    async (wire) => {
      const socket = new Socket();
      socket.rawReply = () => wire;
      await expect(socket.read()).rejects.toThrow(/malformed JSON/);
      expect(socket.closed).toBe(true);
    },
  );

  it("refuses an expired acquired state without publishing partial outputs", async () => {
    const socket = new Socket();
    const reply = socket.reply;
    socket.rawReply = (request) =>
      JSON.stringify(
        request.method === "queryLedgerState/utxo"
          ? {
              id: request.id,
              error: { code: 2001, message: "acquired state expired" },
            }
          : { id: request.id, result: reply(request) },
      );
    await expect(socket.read()).rejects.toThrow(/acquired state expired/);
    expect(socket.closed).toBe(true);
  });

  it("cancels an outstanding query and retires late replies", async () => {
    const socket = new Socket();
    const controller = new AbortController();
    const reply = socket.reply;
    socket.reply = (request) => {
      if (request.method === "queryLedgerState/utxo") controller.abort();
      return reply(request);
    };
    await expect(socket.read(controller.signal)).rejects.toThrow(
      /session aborted/,
    );
    expect(
      socket.requests.some(({ method }) => method === "releaseLedgerState"),
    ).toBe(false);
    expect(socket.closed).toBe(true);
  });

  it("cancels during opening and closes before any request", async () => {
    const socket = new Socket();
    const controller = new AbortController();
    socket.connect = () => controller.abort();
    await expect(socket.read(controller.signal)).rejects.toThrow(
      /session aborted/,
    );
    expect(socket.closed).toBe(true);
    expect(socket.requests).toEqual([]);
  });

  it("closes an opening socket after an error", async () => {
    const socket = new Socket();
    socket.connect = () => socket.emit("error");
    await expect(socket.read()).rejects.toThrow(/socket failed/);
    expect(socket.closed).toBe(true);
    expect(socket.requests).toEqual([]);
  });

  it("bounds the whole acquisition, not only each individual request", async () => {
    const socket = new Socket();
    const send = socket.send.bind(socket);
    socket.send = (data) => {
      setTimeout(() => {
        if (!socket.closed) send(data);
      }, 15);
    };
    await expect(socket.read(undefined, 25)).rejects.toThrow(/session aborted/);
    expect(socket.closed).toBe(true);
    expect(
      socket.requests.some(({ method }) => method === "releaseLedgerState"),
    ).toBe(false);
  });
});
