import { CML, credentialToAddress } from "@lucid-evolution/lucid";
import JSONBig from "json-bigint";
import { describe, expect, it } from "vitest";

import type { WebSocketLike } from "../src/l1-tx-order-carriage.js";
import type { AcceptedHistoryObservation } from "./helpers/history-projection-observations.js";
import {
  makeRecordedHistoryTransport,
  makeStreamingHistoryTransport,
  type RecordedHistoryBatch,
} from "./helpers/history-source-owner-emulator.js";

const lossless = JSONBig({ useNativeBigInt: true, strict: true });
const address = credentialToAddress("Preprod", {
  type: "Key",
  hash: "ab".repeat(28),
});

// Transport-model bytes only: these complete CBOR transactions are deliberately
// not submitted to a ledger. Applied acceptance belongs to the owner journey.
const observation = (amount: bigint): AcceptedHistoryObservation => {
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(amount),
    ),
  );
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    outputs,
    200_000n,
  );
  const tx = CML.Transaction.new(body, CML.TransactionWitnessSet.new(), true);
  const txHash = CML.hash_transaction(body).to_hex();
  const signedCbor = tx.to_cbor_hex();
  return {
    signedCbor,
    historical: [],
    measurement: {
      completeSignedBytes: signedCbor.length / 2,
      fee: 200_000n,
      executionMemory: 0n,
      executionSteps: 0n,
    },
    transaction: {
      txHash,
      spends: "inputs",
      inputs: [],
      references: [],
      collaterals: [],
      mint: {},
      withdrawals: [],
      redeemers: [],
      outputs: [
        {
          txHash,
          outputIndex: 0,
          address,
          assets: { lovelace: amount },
          hasReferenceScript: false,
        },
      ],
    },
  };
};
const batch = (
  item: AcceptedHistoryObservation,
  slot: number,
  outputs = item.transaction.outputs,
): RecordedHistoryBatch => ({
  observations: [item],
  observedSlot: slot,
  observedHeight: Math.floor(slot / 20),
  outputs,
});
const recording = () => {
  const publication = observation(1_000_000n);
  const activation = observation(2_000_000n);
  return {
    publication,
    activation,
    recorded: {
      publications: new Map([
        [
          publication.transaction.txHash,
          { signedCbor: publication.signedCbor, observedSlot: 10 },
        ],
      ]),
      batches: [batch(activation, 20)],
      genesis: {
        scope: "synthetic transport-model genesis",
        initializationTxHash: activation.transaction.txHash,
      },
    },
  };
};

const peer = (socket: WebSocketLike) => {
  let sequence = 0;
  const pending = new Map<
    number,
    { resolve: (result: unknown) => void; reject: (cause: Error) => void }
  >();
  socket.addEventListener("message", (event: unknown) => {
    const { id, result } = lossless.parse((event as { data: string }).data) as {
      id: number;
      result: unknown;
    };
    pending.get(id)?.resolve(result);
    pending.delete(id);
  });
  socket.addEventListener("close", () => {
    for (const item of pending.values()) item.reject(new Error("closed"));
    pending.clear();
  });
  return {
    socket,
    request: (method: string, params: Record<string, unknown> = {}) => {
      const id = ++sequence;
      return new Promise<unknown>((resolve, reject) => {
        pending.set(id, { resolve, reject });
        try {
          socket.send(lossless.stringify({ id, method, params }));
        } catch (cause) {
          pending.delete(id);
          reject(cause);
        }
      });
    },
  };
};

describe("streaming history transport model", () => {
  it("appends future complete observations while preserving acquired snapshots and creator bytes", async () => {
    const { recorded, publication, activation } = recording();
    const transport = makeStreamingHistoryTransport(recorded);
    const initial = transport.points;
    expect(initial.map((item) => item.point.slot)).toEqual([10, 20]);
    expect(initial.map((item) => item.point.height)).toEqual([1, 2]);
    const acquired = peer(transport.options.webSocketFactory());
    await acquired.request("acquireLedgerState", { point: initial[1]!.point });
    const oldOutputs = await acquired.request("queryLedgerState/utxo", {
      addresses: [address],
    });
    const chain = peer(transport.options.webSocketFactory());
    await chain.request("findIntersection", { points: [initial[1]!.point] });
    let delivered = false;
    const next = chain.request("nextBlock").then((value) => {
      delivered = true;
      return value;
    });
    expect(transport.appendAccepted()).toEqual(initial[1]!.point);
    await Promise.resolve();
    expect(delivered).toBe(false);

    const future = observation(3_000_000n);
    recorded.publications.set(future.transaction.txHash, {
      signedCbor: future.signedCbor,
      observedSlot: 35,
    });
    recorded.batches.push(batch(future, 35));
    const tip = transport.appendAccepted();
    expect(tip).toMatchObject({ slot: 35, height: 3 });
    expect(await next).toMatchObject({
      direction: "forward",
      tip,
      block: {
        ...tip,
        ancestor: initial[1]!.point.id,
        transactions: [
          { id: future.transaction.txHash, cbor: future.signedCbor },
        ],
      },
    });
    expect(transport.points[0]).toBe(initial[0]);
    expect(transport.points[1]).toBe(initial[1]);
    expect(initial).toHaveLength(2);
    expect(await acquired.request("queryLedgerState/tip")).toEqual(
      initial[1]!.point,
    );
    expect(
      await acquired.request("queryLedgerState/utxo", { addresses: [address] }),
    ).toEqual(oldOutputs);
    expect(await acquired.request("queryNetwork/tip")).toEqual({
      slot: tip.slot,
      id: tip.id,
    });
    expect(await acquired.request("queryNetwork/blockHeight")).toBe(tip.height);
    expect(transport.points[0]!.transactions[0]!.cbor).toBe(
      publication.signedCbor,
    );
    expect(transport.points[1]!.transactions[0]!.cbor).toBe(
      activation.signedCbor,
    );
    expect(transport.indexOf(future.transaction.txHash)).toBe(2);
    const creator = await transport.options.fetchImpl(
      `${transport.options.kupoUrl}/matches/0@${publication.transaction.txHash}`,
    );
    expect(lossless.parse(await creator.text())).toEqual([
      {
        transaction_id: publication.transaction.txHash,
        output_index: 0,
        datum: null,
        created_at: { slot_no: 10, header_hash: initial[0]!.point.id },
      },
    ]);
    const replay = peer(transport.options.webSocketFactory());
    await replay.request("findIntersection", { points: [initial[0]!.point] });
    expect(await replay.request("nextBlock")).toMatchObject({
      block: {
        transactions: [
          { id: activation.transaction.txHash, cbor: activation.signedCbor },
        ],
      },
    });
    expect(transport.appendAccepted()).toEqual(tip);
    expect(transport.points).toHaveLength(3);
    expect(Object.isFrozen(transport.points[1]!.outputs[0]!.assets)).toBe(true);
    expect(Object.isFrozen(transport.points[1]!.transactions[0])).toBe(true);
    expect(() =>
      Object.assign(transport.points[1]!.outputs[0]!.assets, {
        lovelace: 999n,
      }),
    ).toThrow();
    transport.close();
  });

  it.each([10, 19, 20])(
    "refuses new transactions at old or sealed slot %s atomically",
    (slot) => {
      const { recorded } = recording();
      const transport = makeStreamingHistoryTransport(recorded);
      const previous = transport.points;
      const late = observation(4_000_000n);
      recorded.batches.push(batch(late, slot));
      recorded.batches.push(batch(observation(5_000_000n), 40));
      expect(() => transport.appendAccepted()).toThrow(/sealed slot/);
      expect(transport.points).toEqual(previous);
      expect(() => transport.indexOf(late.transaction.txHash)).toThrow(
        /Missing accepted/,
      );
      transport.close();
    },
  );

  it("detaches nested snapshot and archive data, and refuses historical mutation", async () => {
    const { recorded, publication } = recording();
    const transport = makeStreamingHistoryTransport(recorded);
    const previous = transport.points;
    const initialAssets = recorded.batches[0]!.outputs[0]!.assets;
    Object.assign(initialAssets, { lovelace: 999n });
    expect(transport.points[1]!.outputs[0]!.assets.lovelace).toBe(2_000_000n);
    expect(() => transport.appendAccepted()).toThrow(/sealed slot/);
    Object.assign(initialAssets, { lovelace: 2_000_000n });
    const pub = recorded.publications.get(publication.transaction.txHash)!;
    pub.signedCbor = observation(6_000_000n).signedCbor;
    expect(() => transport.appendAccepted()).toThrow(/sealed slot/);
    expect(transport.points[0]!.transactions[0]!.cbor).toBe(
      publication.signedCbor,
    );
    expect(transport.points).toEqual(previous);
    pub.signedCbor = publication.signedCbor;
    recorded.publications.delete(publication.transaction.txHash);
    expect(() => transport.appendAccepted()).toThrow(/removed/);
    transport.close();
  });

  it("does not expose any partial append when a future capture is incomplete", () => {
    const { recorded } = recording();
    const transport = makeStreamingHistoryTransport(recorded);
    const previous = transport.points;
    const first = observation(7_000_000n);
    const incomplete = observation(8_000_000n);
    recorded.batches.push(batch(first, 30));
    recorded.publications.set(incomplete.transaction.txHash, {
      signedCbor: incomplete.signedCbor,
      observedSlot: 40,
    });
    expect(() => transport.appendAccepted()).toThrow(
      /completed observation batch/,
    );
    expect(transport.points).toEqual(previous);
    expect(() => transport.indexOf(first.transaction.txHash)).toThrow(
      /Missing accepted/,
    );
    recorded.batches.push(batch(incomplete, 40));
    expect(transport.appendAccepted()).toMatchObject({ slot: 40, height: 4 });
    expect(transport.points[2]!.parent).toBe(previous[1]!.point.id);
    expect(transport.points[3]!.parent).toBe(transport.points[2]!.point.id);
    transport.close();
  });

  it("closes pending readers and refuses append and new transport IO", async () => {
    const { recorded } = recording();
    const transport = makeStreamingHistoryTransport(recorded);
    const reader = peer(transport.options.webSocketFactory());
    await reader.request("findIntersection", {
      points: [transport.points.at(-1)!.point],
    });
    const pending = expect(reader.request("nextBlock")).rejects.toThrow(
      "closed",
    );
    const unopened = transport.options.webSocketFactory();
    let openedAfterClose = false;
    unopened.addEventListener("open", () => {
      openedAfterClose = true;
    });
    transport.close();
    transport.close();
    await pending;
    expect(openedAfterClose).toBe(false);
    expect(() => transport.appendAccepted()).toThrow("closed");
    expect(() => transport.options.webSocketFactory()).toThrow("closed");
    await expect(
      transport.options.fetchImpl(
        `${transport.options.kupoUrl}/checkpoints/20`,
      ),
    ).rejects.toThrow("closed");
  });

  it("preserves fixed recorded transport reveal and intersection controls", async () => {
    const { recorded } = recording();
    const transport = makeRecordedHistoryTransport(recorded);
    transport.reveal(0);
    const reader = peer(transport.options.webSocketFactory());
    await reader.request("queryNetwork/genesisConfiguration");
    transport.hold();
    const intersect = reader.request("findIntersection", {
      points: [transport.points[0]!.point],
    });
    expect(transport.heldCount()).toBe(1);
    transport.release();
    await intersect;
    const next = reader.request("nextBlock");
    transport.reveal(1);
    expect(await next).toMatchObject({
      block: { ...transport.points[1]!.point },
    });
    transport.close();
  });
});
