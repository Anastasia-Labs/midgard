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

import { stackJourneyTransactions } from "./stack-transactions.js";

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
});
