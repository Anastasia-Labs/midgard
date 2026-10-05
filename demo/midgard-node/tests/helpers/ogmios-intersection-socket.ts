import { vi } from "vitest";

import type { WebSocketFactory } from "../../src/l1-tx-order-carriage.l1-chain-point.js";

export const intersectionSocket = (
  answer: (request: {
    method: string;
    params: { points: { slot: number; id: string }[] };
  }) => { readonly result?: unknown; readonly error?: unknown },
) => {
  const close = vi.fn();
  const requests: unknown[] = [];
  const factory: WebSocketFactory = () => {
    const listeners = new Map<string, ((event: never) => void)[]>();
    return {
      send: (payload) => {
        const request = JSON.parse(payload);
        requests.push(request);
        queueMicrotask(() => {
          const response = { id: request.id, ...answer(request) };
          for (const listener of listeners.get("message") ?? [])
            listener({ data: JSON.stringify(response) } as never);
        });
      },
      close,
      addEventListener: (type, listener) => {
        listeners.set(type, [...(listeners.get(type) ?? []), listener]);
        if (type === "open") queueMicrotask(() => listener({} as never));
      },
    };
  };
  return { factory, close, requests };
};
