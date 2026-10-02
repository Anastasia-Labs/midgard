import { OgmiosJsonRpcError } from "@al-ft/midgard-core/ogmios-json-rpc-error";
import { afterEach, describe, expect, it, vi } from "vitest";

import { OgmiosRpcSession } from "../src/l1/provider.ogmios-rpc-session.js";
import { alignedKupmiosTip } from "../src/l1/provider.run-ogmios-session.js";
import { L1SourceIntegrityError } from "../src/l1/source-integrity.js";
import {
  openRpc,
  type StateQueueReplayWebSocket,
} from "../src/l1/state-queue-replay-provider.open-rpc.js";
import { isFatalStartupError } from "../src/startup.js";

// The committee retries every failed tick, so neither kind of Ogmios error
// answer may become an integrity failure (a quarantine) or a fatal startup
// error. The answer is typed so its code and transience reach the logs and
// any caller that needs them.

const TRANSIENT = [2000, 2001, 2002, 2003, -32000, -32603];
const REFUSED = [1000, 2004, -32600, -32601, -32602];

/** The `globalThis.WebSocket` shape the provider sessions use. */
const answeringWebSocket = (error: unknown) =>
  class {
    onopen: ((event: unknown) => void) | null = null;
    onmessage: ((event: { readonly data: unknown }) => void) | null = null;
    onerror: ((event: unknown) => void) | null = null;
    onclose: ((event: unknown) => void) | null = null;

    constructor(_url: string) {
      queueMicrotask(() => this.onopen?.({}));
    }

    send(data: string) {
      const { id } = JSON.parse(data) as { id: unknown };
      queueMicrotask(() =>
        this.onmessage?.({
          data: JSON.stringify({ jsonrpc: "2.0", id, error }),
        }),
      );
    }

    close() {}
  };

/** The factory-injected socket shape the state-queue replay uses. */
const answeringReplaySocket = (error: unknown): StateQueueReplayWebSocket => {
  const listeners = new Map<string, ((event: never) => void)[]>();
  const emit = (type: string, event?: unknown) => {
    for (const listener of listeners.get(type) ?? []) listener(event as never);
  };
  queueMicrotask(() => emit("open"));
  return {
    addEventListener: (type, listener) => {
      listeners.set(type, [...(listeners.get(type) ?? []), listener]);
    },
    send: (data) => {
      const { id } = JSON.parse(data) as { id: number };
      queueMicrotask(() =>
        emit("message", {
          data: JSON.stringify({ jsonrpc: "2.0", id, error }),
        }),
      );
    },
    close: () => {},
  };
};

const failureOf = async (run: () => Promise<unknown>): Promise<unknown> => {
  try {
    await run();
  } catch (failure) {
    return failure;
  }
  throw new Error("expected the Ogmios request to fail");
};

const clients: readonly (readonly [
  string,
  string,
  (error: unknown) => Promise<unknown>,
])[] = [
  [
    "chain-sync session",
    "Ogmios JSON-RPC error: ",
    async (error) => {
      vi.stubGlobal("WebSocket", answeringWebSocket(error));
      const session = await OgmiosRpcSession.open("http://ogmios.test");
      return failureOf(() => session.request("findIntersection", {}));
    },
  ],
  [
    "tip session",
    "Ogmios JSON-RPC error: ",
    (error) => {
      vi.stubGlobal("WebSocket", answeringWebSocket(error));
      // Kupo never answers, so the Ogmios answer decides the read.
      const kupo = (() => new Promise<Response>(() => {})) as typeof fetch;
      return failureOf(() =>
        alignedKupmiosTip(
          "Custom",
          "http://kupo.test",
          "http://ogmios.test",
          kupo,
          42,
        ),
      );
    },
  ],
  [
    "state-queue replay",
    "Ogmios replay error: ",
    async (error) => {
      const rpc = await openRpc("http://ogmios.test", () =>
        answeringReplaySocket(error),
      );
      try {
        return await failureOf(() => rpc.request("queryLedgerState/utxo", {}));
      } finally {
        rpc.close();
      }
    },
  ],
];

afterEach(() => {
  vi.unstubAllGlobals();
});

describe.each(clients)("committee %s error answers", (_name, prefix, fail) => {
  it.each(TRANSIENT)("types %s as a transient answer", async (code) => {
    const failure = await fail({ code, message: "not now" });
    expect(failure).toBeInstanceOf(OgmiosJsonRpcError);
    expect(failure).toMatchObject({ transient: true, answer: { code } });
    expect(failure).not.toBeInstanceOf(L1SourceIntegrityError);
    expect(isFatalStartupError(failure)).toBe(false);
  });

  it.each(REFUSED)("types %s as a refusal the tick retries", async (code) => {
    const failure = await fail({ code, message: "refused" });
    expect(failure).toBeInstanceOf(OgmiosJsonRpcError);
    expect(failure).toMatchObject({ transient: false, answer: { code } });
    expect(failure).not.toBeInstanceOf(L1SourceIntegrityError);
    expect(isFatalStartupError(failure)).toBe(false);
  });

  it("keeps the error JSON in the message", async () => {
    const failure = await fail({ code: 1000, message: "No intersection." });
    expect((failure as Error).message).toBe(
      `${prefix}{"code":1000,"message":"No intersection."}`,
    );
  });
});
