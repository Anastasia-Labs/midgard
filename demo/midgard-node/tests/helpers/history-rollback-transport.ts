import JSONBig from "json-bigint";

import type { LedgerSnapshotOutput } from "../../src/l1-ledger-snapshot.js";
import type {
  FetchLike,
  WebSocketLike,
} from "../../src/l1-tx-order-carriage.js";
import {
  makeRecordedHistoryTransport,
  makeStreamingHistoryTransport,
  type RecordedHistoryBatch,
} from "./history-source-owner-emulator.js";

type Recording = Parameters<typeof makeStreamingHistoryTransport>[0];
type Point = ReturnType<typeof makeStreamingHistoryTransport>["points"][number];
const lossless = JSONBig({ useNativeBigInt: true, strict: true });
const rawOutput = (output: LedgerSnapshotOutput) => {
  const value: Record<string, Record<string, bigint>> = {
    ada: { lovelace: output.assets.lovelace ?? 0n },
  };
  for (const [unit, quantity] of Object.entries(output.assets)) {
    if (unit !== "lovelace")
      (value[unit.slice(0, 56)] ??= {})[unit.slice(56)] = quantity;
  }
  return {
    transaction: { id: output.txHash },
    index: output.outputIndex,
    address: output.address,
    value,
    ...(output.datum === undefined ? {} : { datum: output.datum }),
    ...(output.datumHash === undefined ? {} : { datumHash: output.datumHash }),
    ...(output.hasReferenceScript ? { script: {} } : {}),
  };
};

/** Controlled branch ancestry and source RPCs, with transaction bodies and
 * point snapshots exclusively obtained from actual accepted emulator batches.
 * This is not a real network consensus/rollback test. It never edits the sealed
 * original recording or asks the operator's SQL/archive to authorize a branch. */
export const makeRollbackHistoryTransport = (recorded: Recording) => {
  const forward = makeStreamingHistoryTransport(recorded);
  let points = [...forward.points];
  const origin = { id: points[0]!.parent, slot: 0, height: 0 };
  let forked = false;
  let branchRecording: Recording | undefined;
  let closed = false;
  let nextSocketId = 0;
  const requests: typeof forward.requests = [];
  const branches: { kind: "backward" | "forward"; point: Point["point"] }[] =
    [];
  const sockets = new Set<Socket>();
  const tip = () => points.at(-1)!.point;
  // A network tip answer that lags ChainSync, as a heartbeat racing a newer
  // block can observe; ChainSync itself always reports the real tip.
  let pinnedNetworkTip: Point["point"] | undefined;
  const networkTip = () => pinnedNetworkTip ?? tip();
  // Runs once, synchronously, before the next address-scope ledger query is
  // answered from the snapshot already acquired.
  let beforeLedgerQuery: (() => void) | undefined;
  type Request = {
    id: number;
    method: string;
    params: Record<string, unknown>;
  };
  class Socket implements WebSocketLike {
    readonly id = ++nextSocketId;
    private listeners = new Map<string, ((event: never) => void)[]>();
    private closed = false;
    private acquired: Point | undefined;
    private cursor = -1;
    private pending: Request | undefined;
    private backward: Point["point"] | undefined;
    addEventListener(type: string, listener: (event: never) => void) {
      this.listeners.set(type, [...(this.listeners.get(type) ?? []), listener]);
    }
    emit(type: string, event?: unknown) {
      for (const listener of this.listeners.get(type) ?? [])
        listener(event as never);
    }
    private answer(request: Request, result: unknown) {
      queueMicrotask(() => {
        if (!this.closed)
          this.emit("message", {
            data: lossless.stringify({ id: request.id, result }),
          });
      });
    }
    send(data: string) {
      if (this.closed || closed) throw new Error("Rollback transport closed");
      const request = lossless.parse(data) as Request;
      requests.push({
        socket: this.id,
        method: request.method,
        params: request.params,
      });
      switch (request.method) {
        case "queryNetwork/genesisConfiguration":
          this.answer(request, recorded.genesis);
          break;
        case "queryNetwork/tip": {
          // Ogmios v6 carries the height only on queryNetwork/blockHeight.
          const { slot, id } = networkTip();
          this.answer(request, { slot, id });
          break;
        }
        case "queryNetwork/blockHeight":
          this.answer(request, networkTip().height);
          break;
        case "queryLedgerState/tip":
          this.answer(request, this.acquired?.point ?? tip());
          break;
        case "acquireLedgerState": {
          const requested = request.params.point as { id: string };
          const point = points.find((p) => p.point.id === requested.id);
          if (point === undefined)
            throw new Error(
              "Cannot acquire an orphaned/unrecorded source snapshot",
            );
          this.acquired = point;
          this.answer(request, { acquired: "ledgerState", point: point.point });
          break;
        }
        case "queryLedgerState/utxo": {
          if (this.acquired === undefined)
            throw new Error("Snapshot was not acquired");
          const hook = beforeLedgerQuery;
          beforeLedgerQuery = undefined;
          hook?.();
          const addresses = request.params.addresses as string[];
          this.answer(
            request,
            this.acquired.outputs
              .filter((o) => addresses.includes(o.address))
              .map(rawOutput),
          );
          break;
        }
        case "releaseLedgerState":
          this.acquired = undefined;
          this.answer(request, { released: "ledgerState" });
          break;
        case "findIntersection": {
          const candidates = request.params.points as {
            id: string;
            slot: number;
          }[];
          const selected = candidates.find(
            (candidate) =>
              candidate.id === origin.id ||
              points.some((p) => p.point.id === candidate.id),
          );
          if (selected === undefined)
            throw new Error("No active recorded intersection");
          this.cursor = points.findIndex((p) => p.point.id === selected.id);
          this.answer(request, { intersection: selected, tip: tip() });
          break;
        }
        case "nextBlock": {
          if (this.pending !== undefined)
            throw new Error("Concurrent chain request on one socket");
          this.pending = request;
          this.flush();
          break;
        }
        default:
          throw new Error(`Unexpected rollback RPC ${request.method}`);
      }
    }
    rollback(index: number) {
      if (this.cursor > index) {
        this.backward = points[index]!.point;
        this.cursor = index;
      }
      this.flush();
    }
    flush() {
      if (this.closed || this.pending === undefined) return;
      if (this.backward !== undefined) {
        const request = this.pending;
        this.pending = undefined;
        const point = this.backward;
        this.backward = undefined;
        this.answer(request, { direction: "backward", point, tip: tip() });
        return;
      }
      if (this.cursor >= points.length - 1) return;
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
      if (this.closed) return;
      this.closed = true;
      this.pending = undefined;
      sockets.delete(this);
      this.emit("close");
    }
  }
  const fetchImpl: FetchLike = async (url) => {
    if (closed) throw new Error("Rollback transport closed");
    const path = new URL(url).pathname;
    if (path.startsWith("/checkpoints/")) {
      const slot = Number(path.split("/").at(-1));
      const found =
        [...points].reverse().find((point) => point.point.slot <= slot)
          ?.point ?? origin;
      return new Response(
        lossless.stringify({ slot_no: found.slot, header_hash: found.id }),
      );
    }
    const match = /\/matches\/(\d+)@([a-f0-9]{64})/u.exec(path);
    if (match === null) throw new Error(`Unexpected rollback HTTP ${url}`);
    const block = points.find((point) =>
      point.transactions.some((tx) => tx.id === match[2]),
    );
    if (block === undefined) return new Response("[]");
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
    branches,
    options: {
      ...forward.options,
      fetchImpl,
      webSocketFactory: (): WebSocketLike => {
        if (closed) throw new Error("Rollback transport closed");
        const socket = new Socket();
        sockets.add(socket);
        queueMicrotask(() => socket.emit("open"));
        return socket;
      },
    },
    indexOf: (hash: string) => {
      const index = points.findIndex((point) =>
        point.transactions.some((tx) => tx.id === hash),
      );
      if (index < 0) throw new Error("Missing active accepted transaction");
      return index;
    },
    appendAccepted: () => {
      if (forked)
        throw new Error(
          "Use actual fork batches after rollback; original recorder is sealed",
        );
      forward.appendAccepted();
      points = [...forward.points];
      for (const socket of sockets) socket.flush();
      return tip();
    },
    rollbackTo: (id: string) => {
      const index = points.findIndex((point) => point.point.id === id);
      if (index < 0 || index === points.length - 1)
        throw new Error("Rollback requires a retained strict ancestor");
      points = points.slice(0, index + 1);
      forked = true;
      // The retained ancestor may itself be a fork block, so cut the current
      // branch's recording rather than the sealed original.
      const current = branchRecording ?? recorded;
      branchRecording = {
        genesis: recorded.genesis,
        publications: new Map(
          [...current.publications].filter(
            ([, publication]) => publication.observedSlot <= tip().slot,
          ),
        ),
        batches: current.batches.filter(
          (batch) => batch.observedSlot <= tip().slot,
        ),
      };
      branches.push({ kind: "backward", point: tip() });
      for (const socket of sockets) socket.rollback(index);
      return tip();
    },
    appendFork: (batch: RecordedHistoryBatch) => {
      if (!forked || batch.observedSlot <= tip().slot)
        throw new Error("Fork must advance the retained ancestor");
      if (branchRecording === undefined)
        throw new Error("Missing retained branch recording");
      const nextRecording = {
        ...branchRecording,
        batches: [...branchRecording.batches, batch],
      };
      const branch = makeRecordedHistoryTransport(nextRecording);
      const next = branch.points;
      branch.close();
      if (
        next.length !== points.length + 1 ||
        next.at(-1)!.parent !== tip().id ||
        next
          .slice(0, -1)
          .some((point, index) => point.point.id !== points[index]!.point.id)
      ) {
        throw new Error("Accepted fork batch changed its retained prefix");
      }
      points = [...next];
      branchRecording = nextRecording;
      branches.push({ kind: "forward", point: tip() });
      for (const socket of sockets) socket.flush();
      return tip();
    },
    pinNetworkTip: (point: Point["point"] | undefined) => {
      pinnedNetworkTip = point;
    },
    beforeLedgerQuery: (hook: () => void) => {
      beforeLedgerQuery = hook;
    },
    close: () => {
      if (closed) return;
      closed = true;
      for (const socket of [...sockets]) socket.close();
      forward.close();
    },
  };
};
