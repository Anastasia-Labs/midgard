import type { WebSocketLike } from "../../src/l1-kupmios.js";
import { makeStreamingHistoryTransport } from "./history-source-owner-emulator.js";

/**
 * A streaming history transport whose per-request source deadline is
 * `timeoutMs` and whose Ogmios answers to `queryLedgerState/utxo`, once armed,
 * arrive `delayMs` later: the full-UTxO scan Ogmios runs for every
 * address-scope ledger query, slowed by concurrent clients of the same node.
 * Every other request is answered as the recorder serves it. `delays` lists
 * the real time each delayed answer was held.
 */
export const makeSlowLedgerScanTransport = ({
  timeoutMs,
  delayMs,
}: {
  readonly timeoutMs: number;
  readonly delayMs: number;
}) => {
  let armed = false;
  const delays: number[] = [];
  const slowSocket = (socket: WebSocketLike): WebSocketLike => {
    const slowIds = new Set<unknown>();
    const idOf = (data: string): unknown => {
      try {
        return (JSON.parse(data) as { id?: unknown }).id;
      } catch {
        return undefined;
      }
    };
    return {
      send: (data) => {
        if (
          armed &&
          (JSON.parse(data) as { method?: unknown }).method ===
            "queryLedgerState/utxo"
        )
          slowIds.add(idOf(data));
        socket.send(data);
      },
      close: (code, reason) => socket.close(code, reason),
      addEventListener: (type, listener, options) => {
        if (type !== "message") {
          socket.addEventListener(type, listener, options);
          return;
        }
        socket.addEventListener(
          type,
          (event: never) => {
            const { data } = event as { data: string };
            if (!slowIds.has(idOf(data))) {
              listener(event);
              return;
            }
            const heldFrom = performance.now();
            setTimeout(() => {
              delays.push(performance.now() - heldFrom);
              listener(event);
            }, delayMs);
          },
          options,
        );
      },
    };
  };
  return {
    arm: () => {
      armed = true;
    },
    delays,
    transportFactory: (
      recorded: Parameters<typeof makeStreamingHistoryTransport>[0],
    ) => {
      const transport = makeStreamingHistoryTransport(recorded);
      const factory = transport.options.webSocketFactory;
      return {
        get points() {
          return transport.points;
        },
        requests: transport.requests,
        indexOf: transport.indexOf,
        appendAccepted: transport.appendAccepted,
        close: transport.close,
        options: {
          ...transport.options,
          timeoutMs,
          webSocketFactory: (url: string): WebSocketLike =>
            slowSocket((factory as (target: string) => WebSocketLike)(url)),
        },
      };
    },
  };
};
