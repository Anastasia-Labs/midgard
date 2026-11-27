import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  type L1Recording,
  type L1RecordingName,
  loadL1Recording,
  recordedFetch,
  recordedOgmiosWebSocket,
  recordedTransaction,
} from "@al-ft/midgard-test-support/l1-recordings";
import { afterAll, describe, expect, it } from "vitest";

import {
  readKupoTransactionInclusion,
  stackJourneyTransactions,
  stackSignedCommitRecovery,
} from "./stack-transactions.js";
import { isTransientTransportError } from "./transport-failure.js";

// Real preprod Kupo v2.11.0 and Ogmios v7.0.0 answers, served through the
// reader's own fetch and WebSocket.
const REMOVAL: L1RecordingName = "preprod-state-queue-removal-a2a47d2e";
const SWEEP: L1RecordingName = "preprod-reference-sweep-e1e70271";

const directories: string[] = [];
afterAll(() =>
  Promise.all(
    directories.map((path) => rm(path, { recursive: true, force: true })),
  ),
);

const readerOf = async (recording: L1Recording) => {
  const directory = await mkdtemp(join(tmpdir(), "stack-transactions-"));
  directories.push(directory);
  const ogmios = recordedOgmiosWebSocket(recording);
  return stackJourneyTransactions({
    kupoUrl: "http://kupo.recorded",
    ogmiosUrl: "ws://ogmios.recorded",
    directory,
    fetchImpl: recordedFetch(recording) as typeof fetch,
    webSocketFactory: (url) => new ogmios.WebSocket(url),
  });
};

/** The recorded block's chain-sync answer that carries the transaction. */
const carryingBlock = (recording: L1Recording) =>
  recording.exchanges.find(
    (exchange) =>
      exchange.surface === "ogmios-websocket" &&
      exchange.request.method === "nextBlock" &&
      JSON.stringify(exchange.response).includes(recording.transaction!.id),
  ) as Extract<
    L1Recording["exchanges"][number],
    { surface: "ogmios-websocket" }
  >;

/**
 * The stack's Ogmios, replaying the recording on every connection and
 * counting how many connections are open at once: a connection is open from
 * the moment it is made until the reader or Ogmios closes it. The
 * `dropConnection`-th connection (counting from 1) is closed by Ogmios as
 * soon as the reader sends it a request.
 */
const countedOgmios = (
  recording: L1Recording,
  options: Readonly<{ dropConnection?: number }> = {},
) => {
  const replay = recordedOgmiosWebSocket(recording);
  let opened = 0;
  let open = 0;
  let mostOpenAtOnce = 0;
  const connect = (url: string) => {
    opened += 1;
    open += 1;
    mostOpenAtOnce = Math.max(mostOpenAtOnce, open);
    const socket = new replay.WebSocket(url);
    const drops = opened === options.dropConnection;
    const closeListeners: ((event: never) => void)[] = [];
    let closed = false;
    const settle = () => {
      if (closed) return false;
      closed = true;
      open -= 1;
      return true;
    };
    return {
      addEventListener(
        type: string,
        listener: (event: never) => void,
        listenerOptions?: { once?: boolean },
      ) {
        if (type === "close") closeListeners.push(listener);
        else socket.addEventListener(type, listener, listenerOptions);
      },
      send(raw: string) {
        if (!drops) return socket.send(raw);
        queueMicrotask(() => {
          if (!settle()) return;
          for (const listener of closeListeners) listener({} as never);
        });
      },
      close() {
        if (settle())
          for (const listener of closeListeners) listener({} as never);
      },
    };
  };
  return {
    connect,
    opened: () => opened,
    mostOpenAtOnce: () => mostOpenAtOnce,
  };
};

describe("stack transaction reads", () => {
  it("reads a confirmed transaction's exact bytes, block and the tip they were read against", async () => {
    const recording = loadL1Recording(REMOVAL);
    const reader = await readerOf(recording);
    expect(reader.observedBlockNo()).toBeUndefined();
    const { id, block } = recording.transaction!;
    const observed = await reader.transaction(id);
    expect(observed).toEqual({
      cbor: recordedTransaction(recording).cbor,
      point: {
        blockHash: block.id,
        blockNo: "5220548",
        slot: block.slot.toString(),
      },
    });
    expect(reader.observedBlockNo()).toBe(5_220_935n);
    const evidence = (await readFile(reader.nativeEvidencePath, "utf8"))
      .trim()
      .split("\n")
      .map((line) => JSON.parse(line) as Record<string, unknown>);
    expect(evidence).toMatchObject([
      { txHash: id, point: observed.point, tipBlockNo: 5_220_935 },
    ]);
  });

  it("refuses bytes that hash to another transaction", async () => {
    const recording = loadL1Recording(REMOVAL);
    const other = recordedTransaction(loadL1Recording(SWEEP)).cbor;
    const exchange = carryingBlock(recording);
    const response = exchange.response as { bodyText?: string; body?: unknown };
    const text = response.bodyText ?? JSON.stringify(response.body);
    const answer = JSON.parse(text) as {
      result: { block: { transactions: { cbor: string }[] } };
    };
    answer.result.block.transactions[0]!.cbor = other as string;
    Object.assign(exchange, {
      response: { bodyText: JSON.stringify(answer) },
    });
    const reader = await readerOf(recording);
    await expect(reader.transaction(recording.transaction!.id)).rejects.toThrow(
      /hash to another transaction/u,
    );
  });

  it("refuses a transaction Ogmios serves without bytes", async () => {
    const recording = loadL1Recording(REMOVAL);
    const exchange = carryingBlock(recording);
    const response = exchange.response as { bodyText?: string; body?: unknown };
    const answer = JSON.parse(
      response.bodyText ?? JSON.stringify(response.body),
    ) as { result: { block: { transactions: { cbor?: string }[] } } };
    delete answer.result.block.transactions[0]!.cbor;
    Object.assign(exchange, {
      response: { bodyText: JSON.stringify(answer) },
    });
    const reader = await readerOf(recording);
    await expect(reader.transaction(recording.transaction!.id)).rejects.toThrow(
      /--include-transaction-cbor/u,
    );
  });

  it("refuses outputs Kupo places in two blocks, and a transaction Kupo never indexed", async () => {
    const recording = loadL1Recording(REMOVAL);
    const { id } = recording.transaction!;
    const outputs = recording.exchanges.find(
      (exchange) =>
        exchange.surface === "kupo" &&
        exchange.request.path === `/matches/*@${id}?resolve_hashes`,
    )!;
    const response = outputs.response as { bodyText?: string; body?: unknown };
    const matches = JSON.parse(
      response.bodyText ?? JSON.stringify(response.body),
    ) as { created_at: { slot_no: number } }[];
    matches[1]!.created_at.slot_no += 1;
    Object.assign(outputs, {
      response: { ...outputs.response, body: matches, bodyText: undefined },
    });
    await expect((await readerOf(recording)).transaction(id)).rejects.toThrow(
      /more than one block/u,
    );
    Object.assign(outputs, {
      response: { ...outputs.response, body: [], bodyText: undefined },
    });
    await expect((await readerOf(recording)).transaction(id)).rejects.toThrow(
      /indexes no output/u,
    );
  });

  it("names why Kupo refused a transaction read", async () => {
    const recording = loadL1Recording(REMOVAL);
    const { id } = recording.transaction!;
    const outputs = recording.exchanges.find(
      (exchange) =>
        exchange.surface === "kupo" &&
        exchange.request.path === `/matches/*@${id}?resolve_hashes`,
    )!;
    Object.assign(outputs, {
      response: {
        ...outputs.response,
        status: 503,
        body: { hint: "Kupo is still synchronizing" },
        bodyText: undefined,
      },
    });
    const refusal = /failed with 503: .*Kupo is still synchronizing/u;
    await expect((await readerOf(recording)).transaction(id)).rejects.toThrow(
      refusal,
    );
    await expect(
      readKupoTransactionInclusion(
        "http://kupo.recorded",
        id,
        recordedFetch(recording) as typeof fetch,
      ),
    ).rejects.toThrow(refusal);
    // A 5xx is no answer about the transaction: a wait reads it again.
    expect(
      isTransientTransportError(
        await (await readerOf(recording))
          .transaction(id)
          .catch((error: unknown) => error),
      ),
    ).toBe(true);
  });

  it("never takes Kupo's refusal of a malformed read for a lost answer", async () => {
    const recording = loadL1Recording(REMOVAL);
    const { id } = recording.transaction!;
    const outputs = recording.exchanges.find(
      (exchange) =>
        exchange.surface === "kupo" &&
        exchange.request.path === `/matches/*@${id}?resolve_hashes`,
    )!;
    Object.assign(outputs, {
      response: {
        ...outputs.response,
        status: 400,
        body: { hint: "Invalid pattern" },
        bodyText: undefined,
      },
    });
    const failure = await (await readerOf(recording))
      .transaction(id)
      .catch((error: unknown) => error);
    expect((failure as Error).message).toMatch(
      /failed with 400: .*Invalid pattern/u,
    );
    expect(isTransientTransportError(failure)).toBe(false);
  });

  it("holds at most one Ogmios connection open however many reads are asked for at once", async () => {
    const recording = loadL1Recording(REMOVAL);
    const directory = await mkdtemp(join(tmpdir(), "stack-transactions-"));
    directories.push(directory);
    const stackOgmios = countedOgmios(recording);
    const reader = stackJourneyTransactions({
      kupoUrl: "http://kupo.recorded",
      ogmiosUrl: "ws://ogmios.recorded",
      directory,
      fetchImpl: recordedFetch(recording) as typeof fetch,
      webSocketFactory: stackOgmios.connect,
    });
    const { id } = recording.transaction!;
    const reads = await Promise.all(
      Array.from({ length: 8 }, () => reader.transaction(id)),
    );
    expect(reads.map(({ cbor }) => cbor)).toEqual(
      Array.from({ length: 8 }, () => recordedTransaction(recording).cbor),
    );
    expect(stackOgmios.opened()).toBe(8);
    expect(stackOgmios.mostOpenAtOnce()).toBe(1);
  });

  it("fails only the read whose Ogmios connection drops, and the next read waiting still succeeds", async () => {
    const recording = loadL1Recording(REMOVAL);
    const directory = await mkdtemp(join(tmpdir(), "stack-transactions-"));
    directories.push(directory);
    // The second connection the stack's Ogmios accepts drops as soon as the
    // reader asks it anything.
    const stackOgmios = countedOgmios(recording, { dropConnection: 2 });
    const reader = stackJourneyTransactions({
      kupoUrl: "http://kupo.recorded",
      ogmiosUrl: "ws://ogmios.recorded",
      directory,
      fetchImpl: recordedFetch(recording) as typeof fetch,
      webSocketFactory: stackOgmios.connect,
    });
    const { id } = recording.transaction!;
    const outcomes = await Promise.allSettled(
      Array.from({ length: 3 }, () => reader.transaction(id)),
    );
    expect(outcomes.map(({ status }) => status)).toEqual([
      "fulfilled",
      "rejected",
      "fulfilled",
    ]);
    expect((outcomes[1] as PromiseRejectedResult).reason).toMatchObject({
      message: "Ogmios chain-sync socket closed",
    });
    expect(
      (outcomes[2] as PromiseFulfilledResult<{ cbor: string }>).value,
    ).toMatchObject({ cbor: recordedTransaction(recording).cbor });
    expect(stackOgmios.opened()).toBe(3);
    expect(stackOgmios.mostOpenAtOnce()).toBe(1);
  });

  it("settles a signed commit the stack's Kupo indexed as included, and reports how far Kupo had indexed", async () => {
    const recording = loadL1Recording(REMOVAL);
    const { id } = recording.transaction!;
    const fetchImpl = recordedFetch(recording) as typeof fetch;
    const recover = stackSignedCommitRecovery({
      kupoUrl: "http://kupo.recorded",
      ogmiosUrl: "ws://ogmios.unreachable",
      timeoutMs: 1_000,
      fetchImpl,
    });
    const signedCbor = recordedTransaction(recording).cbor as string;
    await expect(recover({ txHash: id, signedCbor })).resolves.toMatchObject({
      transactionHash: id,
      signedTransactionCborHex: signedCbor,
      status: "included",
    });
    const outputs = recording.exchanges.find(
      (exchange) =>
        exchange.surface === "kupo" &&
        exchange.request.path === `/matches/*@${id}?resolve_hashes`,
    )!;
    const checkpoint = Number(
      (outputs.response as { headers: Record<string, string> }).headers[
        "x-most-recent-checkpoint"
      ],
    );
    Object.assign(outputs, {
      response: { ...outputs.response, body: [], bodyText: undefined },
    });
    await expect(
      readKupoTransactionInclusion("http://kupo.recorded", id, fetchImpl),
    ).resolves.toEqual({ included: false, indexedThroughSlot: checkpoint });
  });
});
