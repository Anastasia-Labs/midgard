import { expect } from "vitest";

import type { WebSocketLike } from "../../src/l1-kupmios.js";
import {
  type LedgerSnapshotPoint,
  readAcquiredLedgerSnapshot,
} from "../../src/l1-ledger-snapshot.js";

export const point = { slot: 123, id: "ab".repeat(32) };
export const fork = { slot: 123, id: "cd".repeat(32) };
export const policy = "ef".repeat(28);
export const addresses = ["deposit", "withdrawal", "retention", "hub"];
export const output = {
  transaction: { id: "01".repeat(32) },
  index: 0,
  address: "deposit",
  value: { ada: { lovelace: 2_000_000 }, [policy]: { "": 1 } },
  datum: "d87980",
};

export type Request = {
  id: number;
  method: string;
  params: Record<string, unknown>;
};
type Reply = (request: Request) => unknown;

/** Wire-level transport seam: real request framing, IDs, lossless JSON parsing
 * and session lifecycle; no Cardano node or branch authority is simulated. */
export class Socket implements WebSocketLike {
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
  read(
    signal?: AbortSignal,
    timeoutMs = 200,
    at?: LedgerSnapshotPoint,
    outputReferences?: { txHash: string; outputIndex: number }[],
  ) {
    return readAcquiredLedgerSnapshot({
      ogmiosUrl: "http://localhost:1337/ogmios",
      addresses,
      at,
      outputReferences,
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
