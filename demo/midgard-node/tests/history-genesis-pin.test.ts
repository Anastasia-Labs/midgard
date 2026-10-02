import { createHash } from "node:crypto";

import { describe, expect, it, vi } from "vitest";

import { runHistoryGenesisPin } from "../src/commands/history-genesis-pin.js";
import {
  authenticateEventHistorySession,
  HISTORY_GENESIS_DIGEST_ALGORITHM,
  readEventHistoryGenesisLosslessSha256,
} from "../src/l1-event-history-source.js";
import type { WebSocketLike } from "../src/l1-tx-order-carriage.js";

// 45000000000000001 is not a double: a Number parse rounds it to ...000.
const GENESIS_WIRE =
  '{"slotLength":{"milliseconds":1000},"maxLovelaceSupply":45000000000000001,"era":"shelley"}';
const EXPECTED_PIN = createHash("sha256")
  .update(
    '{"era":"shelley","maxLovelaceSupply":45000000000000001,"slotLength":{"milliseconds":1000}}',
  )
  .digest("hex");

/** Answers each request with raw JSON text, so the session's own parser is the
 * one that decodes the genesis quantities. */
class GenesisSocket implements WebSocketLike {
  readonly sent: { method: string; params: unknown }[] = [];
  readonly urls: string[] = [];
  readonly listeners = new Map<string, ((event: never) => void)[]>();
  closed = false;
  constructor(readonly resultText: string) {}
  factory = (url: string) => {
    this.urls.push(url);
    queueMicrotask(() => this.emit("open"));
    return this;
  };
  addEventListener(type: string, listener: (event: never) => void) {
    this.listeners.set(type, [...(this.listeners.get(type) ?? []), listener]);
  }
  emit(type: string, event?: unknown) {
    for (const listener of this.listeners.get(type) ?? [])
      listener(event as never);
  }
  send(data: string) {
    const request = JSON.parse(data) as {
      id: number;
      method: string;
      params: unknown;
    };
    this.sent.push({ method: request.method, params: request.params });
    queueMicrotask(() =>
      this.emit("message", {
        data: `{"jsonrpc":"2.0","id":${request.id.toString()},"result":${this.resultText}}`,
      }),
    );
  }
  close() {
    if (this.closed) return;
    this.closed = true;
    this.emit("close");
  }
}

describe("history-genesis-pin", () => {
  it("derives the pin over a lossless socket with the runtime's query", async () => {
    const socket = new GenesisSocket(GENESIS_WIRE);
    const pin = await runHistoryGenesisPin({
      env: { L1_OGMIOS_KEY: "http://127.0.0.1:1337" },
      webSocketFactory: socket.factory,
    });
    expect(pin).toEqual({
      variable: "L1_HISTORY_GENESIS_LOSSLESS_SHA256",
      algorithm: HISTORY_GENESIS_DIGEST_ALGORITHM,
      sha256: EXPECTED_PIN,
    });
    expect(socket.urls).toEqual(["ws://127.0.0.1:1337"]);
    expect(socket.sent).toEqual([
      {
        method: "queryNetwork/genesisConfiguration",
        params: { era: "shelley" },
      },
    ]);
    expect(socket.closed).toBe(true);
  });

  it("prefers --ogmios-url and requires some Ogmios URL", async () => {
    const socket = new GenesisSocket(GENESIS_WIRE);
    await runHistoryGenesisPin({
      ogmiosUrl: "https://ogmios.example:443",
      env: { L1_OGMIOS_KEY: "http://127.0.0.1:1337" },
      webSocketFactory: socket.factory,
    });
    expect(socket.urls).toEqual(["wss://ogmios.example"]);
    await expect(runHistoryGenesisPin({ env: {} })).rejects.toThrow(
      /--ogmios-url or set L1_OGMIOS_KEY/,
    );
  });

  it("prints exactly the pin the runtime authenticates against", async () => {
    const socket = new GenesisSocket(GENESIS_WIRE);
    const { sha256 } = await runHistoryGenesisPin({
      env: { L1_OGMIOS_KEY: "http://127.0.0.1:1337" },
      webSocketFactory: socket.factory,
    });
    const request = vi.fn().mockResolvedValue({
      slotLength: { milliseconds: 1000 },
      maxLovelaceSupply: 45000000000000001n,
      era: "shelley",
    });
    await expect(
      authenticateEventHistorySession({ request }, {
        digest: "binding",
        genesisAlgorithm: HISTORY_GENESIS_DIGEST_ALGORITHM,
        genesisSha256: sha256,
      } as Parameters<typeof authenticateEventHistorySession>[1]),
    ).resolves.toEqual({ bindingDigest: "binding", genesisSha256: sha256 });
  });

  it("refuses a Number-rounded genesis as the runtime does", async () => {
    const rounded = {
      slotLength: { milliseconds: 1000 },
      maxLovelaceSupply: Number("45000000000000001"),
      era: "shelley",
    };
    await expect(
      readEventHistoryGenesisLosslessSha256({
        request: () => Promise.resolve(rounded),
      }),
    ).rejects.toThrow(/losslessly/);
    await expect(
      authenticateEventHistorySession(
        { request: () => Promise.resolve(rounded) },
        {
          digest: "binding",
          genesisAlgorithm: HISTORY_GENESIS_DIGEST_ALGORITHM,
          genesisSha256: EXPECTED_PIN,
        } as Parameters<typeof authenticateEventHistorySession>[1],
      ),
    ).rejects.toThrow(/losslessly/);
  });
});
