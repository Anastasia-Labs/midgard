import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  assetsToValue,
  CML,
  credentialToAddress,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  type HistoryTransportOptions,
  readEventHistoryCreatingBody,
} from "../src/l1-event-history-transport.js";
import type { WebSocketLike } from "../src/l1-kupmios.js";
import {
  KupoNotYetIndexed,
  L1SourceUnavailable,
} from "../src/l1-source-unavailable.js";

const hash = (n: number) => n.toString(16).padStart(64, "0");
const ancestor = { id: hash(1), slot: 1 };
const target = { id: hash(3), slot: 30, height: 3 };
const body = (() => {
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(
        credentialToAddress("Preprod", { type: "Key", hash: "ab".repeat(28) }),
      ),
      assetsToValue({ lovelace: 3_000_000n }),
    ),
  );
  const bodyCbor = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    outputs,
    100n,
  ).to_cbor_hex();
  const cbor = CML.Transaction.new(
    CML.TransactionBody.from_cbor_hex(bodyCbor),
    CML.TransactionWitnessSet.new(),
    true,
  ).to_cbor_hex();
  const txHash = computeHash32(Buffer.from(bodyCbor, "hex")).toString("hex");
  return { bodyCbor, cbor, ref: { txHash, outputIndex: 0 } };
})();
const match = [
  {
    transaction_id: body.ref.txHash,
    output_index: 0,
    datum: null,
    created_at: { slot_no: target.slot, header_hash: target.id },
  },
];
const checkpoint = { slot_no: ancestor.slot, header_hash: ancestor.id };

type Request = { id: number; method: string };
// Ogmios already serves the creating block: only Kupo lags.
class Socket implements WebSocketLike {
  listeners = new Map<string, ((event: never) => void)[]>();
  requests: Request[] = [];
  responses: unknown[] = [
    { direction: "backward", point: ancestor },
    {
      direction: "forward",
      block: {
        type: "praos",
        ...target,
        ancestor: ancestor.id,
        transactions: [{ id: body.ref.txHash, cbor: body.cbor }],
      },
    },
  ];
  addEventListener(type: string, listener: (event: never) => void) {
    this.listeners.set(type, [...(this.listeners.get(type) ?? []), listener]);
  }
  emit(type: string, event?: unknown) {
    for (const listener of this.listeners.get(type) ?? [])
      listener(event as never);
  }
  send(data: string) {
    const request = JSON.parse(data) as Request;
    this.requests.push(request);
    const result =
      request.method === "findIntersection"
        ? { intersection: ancestor }
        : this.responses.shift();
    queueMicrotask(() =>
      this.emit("message", {
        data: JSON.stringify({ id: request.id, result }),
      }),
    );
  }
  close() {
    this.emit("close");
  }
}

/** Kupo answers each path from its own queue; the last answer repeats. */
const lagging = (answers: {
  readonly matches: readonly (() => Response)[];
  readonly checkpoints: readonly (() => Response)[];
}) => {
  const controller = new AbortController();
  const sockets: Socket[] = [];
  const served = { matches: 0, checkpoints: 0 };
  const fetchImpl = vi.fn(async (url: string) => {
    const path = url.includes("/checkpoints/") ? "checkpoints" : "matches";
    const queue = answers[path];
    return queue[Math.min(served[path]++, queue.length - 1)]!();
  });
  const onIndexLag = vi.fn();
  const options: HistoryTransportOptions = {
    kupoUrl: "http://kupo:1442",
    ogmiosUrl: "http://cardano-node-ogmios:1337",
    signal: controller.signal,
    timeoutMs: 2000,
    blockScanLimit: 10,
    maximumResponseBytes: 65536,
    maximumTransactionBytes: 16384,
    fetchImpl,
    onIndexLag,
    webSocketFactory: () => {
      const socket = new Socket();
      sockets.push(socket);
      queueMicrotask(() => socket.emit("open"));
      return socket;
    },
  };
  return { controller, options, served, sockets, onIndexLag };
};
const json = (value: unknown) => () => new Response(JSON.stringify(value));

describe("history transport waits for a lagging Kupo index", () => {
  it("reads the body exactly once after Kupo reports no match three times", async () => {
    const run = lagging({
      matches: [json([]), json([]), json([]), json(match)],
      checkpoints: [json(checkpoint)],
    });
    await expect(
      readEventHistoryCreatingBody(run.options, body.ref),
    ).resolves.toBe(body.bodyCbor);
    expect(run.served).toEqual({ matches: 4, checkpoints: 1 });
    expect(run.sockets).toHaveLength(1);
    expect(
      run.sockets[0]!.requests.filter(
        ({ method }) => method === "findIntersection",
      ),
    ).toHaveLength(1);
    const lag = run.onIndexLag.mock.calls.map(([waiting]) => waiting);
    expect(lag).toHaveLength(4);
    expect(lag.slice(0, 3).every((w) => w instanceof KupoNotYetIndexed)).toBe(
      true,
    );
    expect(lag[3]).toBeUndefined();
  });

  it("waits for a checkpoint before the creating slot, then reads once", async () => {
    const run = lagging({
      matches: [json(match)],
      checkpoints: [json(null), json(null), json(checkpoint)],
    });
    await expect(
      readEventHistoryCreatingBody(run.options, body.ref),
    ).resolves.toBe(body.bodyCbor);
    expect(run.served).toEqual({ matches: 3, checkpoints: 3 });
    expect(run.sockets).toHaveLength(1);
  });

  it("refuses a checkpoint past the requested slot at once, without waiting", async () => {
    const run = lagging({
      matches: [json(match)],
      checkpoints: [json({ slot_no: target.slot, header_hash: target.id })],
    });
    const failure = await readEventHistoryCreatingBody(
      run.options,
      body.ref,
    ).catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(Error);
    expect(failure).not.toBeInstanceOf(L1SourceUnavailable);
    expect((failure as Error).message).toMatch(/not before its target/u);
    expect(run.served).toEqual({ matches: 1, checkpoints: 1 });
    expect(run.sockets).toHaveLength(0);
    expect(run.onIndexLag).not.toHaveBeenCalled();
  });

  it("reports the source unavailable once Kupo outlives the lag ceiling", async () => {
    const run = lagging({ matches: [json([])], checkpoints: [] });
    const started = performance.now();
    const failure = await readEventHistoryCreatingBody(
      { ...run.options, indexLagCeilingMs: 600 },
      body.ref,
    ).catch((error: unknown) => error);
    expect(performance.now() - started).toBeLessThan(1_500);
    expect(failure).toBeInstanceOf(L1SourceUnavailable);
    expect(failure).not.toBeInstanceOf(KupoNotYetIndexed);
    expect((failure as Error).message).toMatch(
      /Kupo has no match .* did not index it within 600ms/u,
    );
    expect(run.sockets).toHaveLength(0);
    expect(run.onIndexLag.mock.calls.at(-1)).toEqual([undefined]);
  });

  it("classifies an overloaded Kupo as unavailable without retrying it here", async () => {
    const run = lagging({
      matches: [() => new Response("busy", { status: 503 })],
      checkpoints: [],
    });
    const failure = await readEventHistoryCreatingBody(
      run.options,
      body.ref,
    ).catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(L1SourceUnavailable);
    expect((failure as Error).message).toMatch(/^HTTP 503 /u);
    expect(run.served.matches).toBe(1);
  });

  it("keeps a malformed Kupo answer terminal", async () => {
    const run = lagging({
      matches: [json([{ ...match[0], created_at: { slot_no: -1 } }])],
      checkpoints: [],
    });
    const failure = await readEventHistoryCreatingBody(
      run.options,
      body.ref,
    ).catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(Error);
    expect(failure).not.toBeInstanceOf(L1SourceUnavailable);
    expect(run.served.matches).toBe(1);
  });

  it("stops waiting on its own cancellation without calling it unavailable", async () => {
    const run = lagging({ matches: [json([])], checkpoints: [] });
    const reading = readEventHistoryCreatingBody(run.options, body.ref).catch(
      (error: unknown) => error,
    );
    await vi.waitFor(() => expect(run.onIndexLag).toHaveBeenCalled());
    run.controller.abort(new Error("owner closed"));
    const failure = await reading;
    expect(failure).not.toBeInstanceOf(L1SourceUnavailable);
    expect(run.served.matches).toBe(1);
  });
});
