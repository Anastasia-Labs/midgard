import { expect } from "vitest";

import type { FetchLike, WebSocketLike } from "../../src/l1-kupmios.js";
import {
  type HistoryTransportPoint,
  type HistoryTransportRecording,
  historyTransportSlots,
  immutableTransportRecord,
  lossless,
  rawOutput,
} from "./history-source-owner-emulator.history-transport-slots.js";
import { hash } from "./history-source-owner-emulator.recorded-history-batch.js";

// Synthetic ancestry/contiguous heights, actual observed slots and complete body
// bytes. This supplies transport only, never ledger-validity/source authority.
export const makeHistoryTransport = (
  recorded: HistoryTransportRecording,
  streaming: boolean,
) => {
  const points: HistoryTransportPoint[] = [];
  const sealedSources: string[] = [];
  const creators = new Map<string, number>();
  const origin = {
    id: hash("source-owner-synthetic-parent"),
    slot: 0,
    height: 0,
  };
  const genesis = immutableTransportRecord(recorded.genesis);
  const genesisBytes = lossless.stringify(genesis);
  let closed = false;
  const append = (initial: boolean) => {
    if (closed) throw new Error("History transport is closed");
    if (lossless.stringify(recorded.genesis) !== genesisBytes)
      throw new Error("History transport genesis changed");
    const slots = historyTransportSlots(recorded);
    if (slots.length < points.length)
      throw new Error("History transport sealed history was removed");
    const next: HistoryTransportPoint[] = [];
    const sources: string[] = [];
    const nextCreators = new Map<string, number>();
    for (const [index, [slot, value]] of slots.entries()) {
      const source = lossless.stringify(value.source);
      if (
        index < points.length &&
        (points[index]!.point.slot !== slot || sealedSources[index] !== source)
      )
        throw new Error("History transport cannot mutate a sealed slot");
      if (!initial && index >= points.length && !value.complete)
        throw new Error(
          "History transport requires a completed observation batch before append",
        );
      const parent = next.at(-1)?.point ?? origin;
      const point =
        points[index] ??
        immutableTransportRecord({
          point: {
            slot,
            height: parent.height + 1,
            id: hash(
              `owner:${slot}:${[...value.transactions.keys()].join(":")}`,
            ),
          },
          parent: parent.id,
          transactions: [...value.transactions.values()],
          outputs: value.outputs,
        });
      for (const tx of point.transactions) {
        const id = String(tx.id);
        if (nextCreators.has(id))
          throw new Error(
            "History transport repeats a creating transaction across slots",
          );
        nextCreators.set(id, index);
      }
      next.push(point);
      sources.push(source);
    }
    if (next.length === 0)
      throw new Error("History transport requires an initial recorded point");
    // Validation above is read-only. Publish all new snapshots/archives together.
    points.push(...next.slice(points.length));
    sealedSources.splice(0, sealedSources.length, ...sources);
    creators.clear();
    for (const [id, index] of nextCreators) creators.set(id, index);
  };
  append(true);
  let visible = points.length - 1;
  let holdIntersection = false;
  const sockets = new Set<RecordedSocket>();
  const requests: { socket: number; method: string; params: unknown }[] = [];
  let sequence = 0;
  const tip = () => points[visible]!.point;
  type Request = {
    id: number;
    method: string;
    params: Record<string, unknown>;
  };
  class RecordedSocket implements WebSocketLike {
    listeners = new Map<string, ((event: never) => void)[]>();
    readonly id = ++sequence;
    closed = false;
    authenticated = false;
    acquired: number | undefined;
    cursor = -1;
    pending: Request | undefined;
    held: Request | undefined;
    addEventListener(type: string, listener: (event: never) => void) {
      this.listeners.set(type, [...(this.listeners.get(type) ?? []), listener]);
    }
    emit(type: string, event?: unknown) {
      for (const listener of this.listeners.get(type) ?? [])
        listener(event as never);
    }
    answer(request: Request, result: unknown) {
      queueMicrotask(() => {
        if (!this.closed)
          this.emit("message", {
            data: lossless.stringify({ id: request.id, result }),
          });
      });
    }
    send(data: string) {
      if (this.closed || closed) throw new Error("History transport is closed");
      const request = lossless.parse(data) as Request;
      requests.push({
        socket: this.id,
        method: request.method,
        params: request.params,
      });
      switch (request.method) {
        case "queryNetwork/genesisConfiguration":
          this.authenticated = true;
          this.answer(request, genesis);
          break;
        case "queryNetwork/tip": {
          // Ogmios v6 carries the height only on queryNetwork/blockHeight.
          const { slot, id } = tip();
          this.answer(request, { slot, id });
          break;
        }
        case "queryNetwork/blockHeight":
          this.answer(request, tip().height);
          break;
        case "queryLedgerState/tip":
          this.answer(
            request,
            this.acquired === undefined ? tip() : points[this.acquired]!.point,
          );
          break;
        case "acquireLedgerState": {
          const requested = request.params.point as { id: string };
          this.acquired = points.findIndex((p) => p.point.id === requested.id);
          expect(this.acquired).toBeGreaterThanOrEqual(0);
          this.answer(request, {
            acquired: "ledgerState",
            point: points[this.acquired]!.point,
          });
          break;
        }
        case "queryLedgerState/utxo": {
          expect(this.acquired).toBeDefined();
          const addresses = request.params.addresses as string[];
          this.answer(
            request,
            points[this.acquired!]!.outputs.filter((o) =>
              addresses.includes(o.address),
            ).map(rawOutput),
          );
          break;
        }
        case "releaseLedgerState":
          this.acquired = undefined;
          this.answer(request, { released: "ledgerState" });
          break;
        case "findIntersection":
          if (holdIntersection && this.authenticated) this.held = request;
          else this.intersect(request);
          break;
        case "nextBlock":
          expect(this.pending).toBeUndefined();
          this.pending = request;
          this.flush();
          break;
        default:
          throw new Error(`Unexpected fixture RPC ${request.method}`);
      }
    }
    intersect(request: Request) {
      const candidates = request.params.points as {
        id: string;
        slot: number;
      }[];
      const selected = candidates.find(
        (p) => p.id === origin.id || points.some((b) => b.point.id === p.id),
      );
      if (!selected)
        throw new Error("Fixture cannot intersect requested branch");
      this.cursor = points.findIndex((p) => p.point.id === selected.id);
      this.answer(request, { intersection: selected, tip: tip() });
    }
    flush() {
      if (this.pending === undefined || this.cursor >= visible || this.closed)
        return;
      const request = this.pending;
      this.pending = undefined;
      const next = points[++this.cursor]!;
      this.answer(request, {
        direction: "forward",
        tip: tip(),
        block: {
          type: "praos",
          era: "conway",
          ...next.point,
          ancestor: next.parent,
          transactions: next.transactions,
        },
      });
    }
    close() {
      if (!this.closed) {
        this.closed = true;
        sockets.delete(this);
        this.pending = undefined;
        this.held = undefined;
        this.emit("close");
      }
    }
  }
  const fetchImpl: FetchLike = async (url) => {
    if (closed) throw new Error("History transport is closed");
    const parsed = new URL(url);
    if (parsed.pathname.startsWith("/checkpoints/")) {
      const slot = Number(parsed.pathname.split("/").at(-1));
      const found =
        [...points].reverse().find((p) => p.point.slot <= slot)?.point ??
        origin;
      return new Response(
        lossless.stringify({ slot_no: found.slot, header_hash: found.id }),
      );
    }
    const match = /\/matches\/(\d+)@([a-f0-9]{64})/u.exec(parsed.pathname);
    if (match === null) throw new Error(`Unexpected fixture HTTP ${url}`);
    const block = points[creators.get(match[2]!)!];
    if (block === undefined)
      throw new Error(`Unknown actual creator ${match[2]}`);
    return new Response(
      lossless.stringify([
        {
          transaction_id: match[2],
          output_index: Number(match[1]),
          datum: null,
          created_at: {
            slot_no: block.point.slot,
            header_hash: block.point.id,
          },
        },
      ]),
    );
  };
  return {
    get points() {
      return Object.freeze([...points]);
    },
    requests,
    appendAccepted: () => {
      if (!streaming)
        throw new Error("Recorded history transport cannot append");
      append(false);
      visible = points.length - 1;
      for (const socket of sockets) socket.flush();
      return tip();
    },
    close: () => {
      if (closed) return;
      closed = true;
      for (const socket of [...sockets]) socket.close();
    },
    indexOf: (hash: string) => {
      const index = creators.get(hash);
      if (index === undefined) throw new Error("Missing accepted transaction");
      return index;
    },
    reveal: (index: number) => {
      expect(index).toBeGreaterThanOrEqual(0);
      expect(index).toBeLessThan(points.length);
      visible = index;
      for (const socket of sockets) socket.flush();
    },
    hold: () => {
      holdIntersection = true;
    },
    release: () => {
      holdIntersection = false;
      for (const socket of sockets)
        if (socket.held) {
          const request = socket.held;
          socket.held = undefined;
          socket.intersect(request);
        }
    },
    heldCount: () => [...sockets].filter((s) => s.held !== undefined).length,
    options: {
      kupoUrl: "http://source-owner-emulator.invalid:1442",
      ogmiosUrl: "http://projection-lifecycle-emulator.invalid:1337",
      timeoutMs: 20_000,
      blockScanLimit: 1024,
      maximumResponseBytes: 16 * 1024 * 1024,
      maximumTransactionBytes: 16_384,
      fetchImpl,
      webSocketFactory: () => {
        if (closed) throw new Error("History transport is closed");
        const socket = new RecordedSocket();
        sockets.add(socket);
        queueMicrotask(() => {
          if (!socket.closed) socket.emit("open");
        });
        return socket;
      },
    },
  };
};
