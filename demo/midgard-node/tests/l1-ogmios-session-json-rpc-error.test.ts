import { describe, expect, it } from "vitest";

import { openOgmiosSession, type WebSocketLike } from "../src/l1-kupmios.js";
import {
  ogmiosJsonRpcAnswerCode,
  OgmiosJsonRpcUnavailable,
} from "../src/l1-kupmios.open-ogmios-session.js";
import { L1SourceUnavailable } from "../src/l1-source-unavailable.js";
import { isRecoverableHistorySourceFailure } from "../src/services/event-history-owner.source-failure.js";

/** Answers every request with one JSON-RPC error envelope. */
class AnsweringSocket implements WebSocketLike {
  private readonly listeners = new Map<string, ((event: never) => void)[]>();

  constructor(private readonly error: unknown) {
    queueMicrotask(() => this.emit("open"));
  }

  addEventListener(type: string, listener: (event: never) => void) {
    this.listeners.set(type, [...(this.listeners.get(type) ?? []), listener]);
  }

  send(data: string) {
    const { id } = JSON.parse(data) as { id: number };
    queueMicrotask(() =>
      this.emit("message", {
        data: JSON.stringify({ jsonrpc: "2.0", id, error: this.error }),
      }),
    );
  }

  close() {
    this.emit("close");
  }

  private emit(type: string, event?: unknown) {
    for (const listener of this.listeners.get(type) ?? [])
      listener(event as never);
  }
}

const answerFailure = async (error: unknown): Promise<unknown> => {
  const session = await openOgmiosSession({
    url: "ws://ogmios.test",
    timeoutMs: 1_000,
    webSocketFactory: () => new AnsweringSocket(error),
  });
  try {
    await session.request("findIntersection", { points: ["origin"] });
  } catch (failure) {
    return failure;
  } finally {
    session.close();
  }
  throw new Error("expected the request to fail");
};

describe("Ogmios session error answers", () => {
  it.each([
    [2000, "acquire failure"],
    [2001, "era mismatch while syncing or crossing an era"],
    [2002, "ledger still in Byron"],
    [2003, "acquired state expired"],
    [-32000, "node connection lost"],
    [-32603, "server failed a well-formed request"],
  ])("treats %s (%s) as a source outage", async (code, message) => {
    const failure = await answerFailure({ code, message });
    expect(failure).toBeInstanceOf(OgmiosJsonRpcUnavailable);
    expect(failure).toBeInstanceOf(L1SourceUnavailable);
    expect(ogmiosJsonRpcAnswerCode(failure)).toBe(code);
    expect(isRecoverableHistorySourceFailure(failure)).toBe(true);
  });

  it.each([
    [1000, "no intersection found"],
    [1001, "intersection interleaved"],
    [2004, "invalid genesis"],
    [3005, "submit refusal"],
    [4000, "mempool not acquired"],
    [-32700, "parse error"],
    [-32600, "invalid request"],
    [-32601, "unknown method"],
    [-32602, "invalid params"],
  ])("keeps %s (%s) a refusal", async (code, message) => {
    const failure = await answerFailure({ code, message });
    expect(failure).not.toBeInstanceOf(L1SourceUnavailable);
    expect(ogmiosJsonRpcAnswerCode(failure)).toBe(code);
    expect(isRecoverableHistorySourceFailure(failure)).toBe(false);
  });

  it.each([
    ["no code", { message: "x" }],
    ["a string code", { code: "2003", message: "x" }],
    ["a fractional code", { code: 2003.5, message: "x" }],
    ["a non-object error", "unavailable"],
  ])("keeps an answer with %s a refusal", async (_label, error) => {
    const failure = await answerFailure(error);
    expect(failure).not.toBeInstanceOf(L1SourceUnavailable);
    expect(ogmiosJsonRpcAnswerCode(failure)).toBeUndefined();
    expect(isRecoverableHistorySourceFailure(failure)).toBe(false);
  });

  it("keeps the error JSON in the message", async () => {
    const failure = await answerFailure({
      code: 1000,
      message: "No intersection found.",
      data: { tip: "origin" },
    });
    expect((failure as Error).message).toBe(
      'Ogmios chain-sync error: {"code":1000,"message":"No intersection found.","data":{"tip":"origin"}}',
    );
  });
});
