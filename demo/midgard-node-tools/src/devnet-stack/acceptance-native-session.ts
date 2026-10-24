import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import type { WebSocket } from "undici";

import { createAcceptanceNativeTransport } from "./acceptance-native-transport.js";

/** One physically owned, loopback-only read session; no financial RPC surface. */
export const openAcceptanceNativeSession = async (input: {
  endpoint: string;
  scope: DaAvailabilityReadScope;
  parseJson: (text: string) => unknown;
}) => {
  input.scope.assertCurrent();
  const transport = createAcceptanceNativeTransport(input);
  let terminal: Error | undefined;
  let pending:
    | {
        id: number;
        method: string;
        resolve(text: string): void;
        reject(error: Error): void;
      }
    | undefined;
  const fail = (error: Error): void => {
    terminal ??= error;
    pending?.reject(terminal);
    pending = undefined;
    transport.destroy();
  };
  const abort = (): void =>
    fail(
      new Error("acceptance native query was revoked", {
        cause: input.scope.signal.reason,
      }),
    );
  input.scope.signal.addEventListener("abort", abort, { once: true });
  let websocket: WebSocket | undefined;
  let rejectOpening: ((error: Error) => void) | undefined;
  const close = async (): Promise<void> => {
    fail(new Error("acceptance native query was closed"));
    rejectOpening?.(terminal!);
    input.scope.signal.removeEventListener("abort", abort);
    websocket?.close();
    await transport.close();
  };
  try {
    input.scope.assertCurrent();
    websocket = transport.webSocketFactory(
      input.endpoint.replace(/^http:/u, "ws:"),
    );
    websocket.addEventListener("message", (event) => {
      if (terminal !== undefined) return;
      try {
        const text = event.data;
        if (
          typeof text !== "string" ||
          Buffer.byteLength(text) > 8 * 1024 * 1024
        )
          throw new Error(
            "acceptance native query frame is invalid or oversized",
          );
        const frame = input.parseJson(text) as {
          jsonrpc?: unknown;
          id?: unknown;
          method?: unknown;
          result?: unknown;
          error?: unknown;
        };
        if (
          pending === undefined ||
          frame?.jsonrpc !== "2.0" ||
          frame.id !== pending.id ||
          frame.method !== pending.method ||
          frame.error !== undefined ||
          frame.result === undefined
        )
          throw new Error(
            "acceptance native query response is uncorrelated or failed",
          );
        input.scope.assertCurrent();
        const current = pending;
        pending = undefined;
        current.resolve(text);
      } catch (error) {
        fail(
          error instanceof Error
            ? error
            : new Error("acceptance native query failed"),
        );
      }
    });
    websocket.addEventListener("error", () => {
      fail(new Error("acceptance native query socket failed"));
      rejectOpening?.(terminal!);
    });
    websocket.addEventListener("close", () => {
      fail(new Error("acceptance native query socket closed"));
      rejectOpening?.(terminal!);
    });
    await input.scope.read(
      () =>
        new Promise<void>((resolve, reject) => {
          rejectOpening = reject;
          websocket!.addEventListener(
            "open",
            () => {
              rejectOpening = undefined;
              resolve();
            },
            { once: true },
          );
          if (terminal !== undefined) reject(terminal);
        }),
    );
    input.scope.assertCurrent();
    let id = 0;
    return {
      close,
      request: async (
        method:
          | "findIntersection"
          | "acquireLedgerState"
          | "queryLedgerState/utxo",
        params: unknown,
      ): Promise<string> => {
        input.scope.assertCurrent();
        if (terminal !== undefined) throw terminal;
        if (pending !== undefined)
          throw new Error("acceptance native query already has an active read");
        id += 1;
        return await input.scope.read(
          () =>
            new Promise<string>((resolve, reject) => {
              pending = { id, method, resolve, reject };
              try {
                websocket!.send(
                  JSON.stringify({ jsonrpc: "2.0", id, method, params }),
                );
              } catch (error) {
                fail(
                  error instanceof Error
                    ? error
                    : new Error("acceptance native query send failed"),
                );
              }
            }),
        );
      },
    };
  } catch (error) {
    await close();
    throw error;
  }
};
