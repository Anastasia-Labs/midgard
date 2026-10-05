import { OgmiosJsonRpcError } from "@al-ft/midgard-core/ogmios-json-rpc-error";
import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import type {
  StateQueueReplayWebSocket,
  StateQueueReplayWebSocketFactory,
} from "../l1/state-queue-replay-provider.open-rpc.js";
import { committeeReadOwner } from "./committee-owned-read-transports.js";

/** Explicit runtime limits, not an inferred successful-time allowance. */
export type CommitteeSourceReadLimits = Readonly<{
  requestRefusalMs: number;
  httpResponseBytes: number;
  webSocketMessageBytes: number;
  rawUtxos: number;
}>;
export const assertCommitteeSourceReadLimits = (
  limits: CommitteeSourceReadLimits,
): void => {
  const keys = [
    "requestRefusalMs",
    "httpResponseBytes",
    "webSocketMessageBytes",
    "rawUtxos",
  ] as const;
  if (
    Object.keys(limits).length !== keys.length ||
    !keys.every((key) => Number.isSafeInteger(limits[key]) && limits[key] > 0)
  )
    throw new Error("Committee source read limits are incomplete");
};

/** Application buffering is capped while consuming the HTTP body; native
 * transport buffers are separate. The actual fetch receives cancellation. */
export const committeeScopedFetch = (
  scope: DaAvailabilityReadScope,
  limits: CommitteeSourceReadLimits,
  fetchImpl: typeof fetch = committeeReadOwner(scope)?.fetchFor(scope) ?? fetch,
): typeof fetch => {
  assertCommitteeSourceReadLimits(limits);
  return async (input, init) =>
    scope.read(
      async (signal) => {
        const controller = new AbortController();
        const combined = AbortSignal.any([
          signal,
          controller.signal,
          ...(init?.signal ? [init.signal] : []),
          ...(input instanceof Request && init?.signal === undefined
            ? [input.signal]
            : []),
        ]);
        let reader: ReadableStreamDefaultReader<Uint8Array> | undefined;
        try {
          const response = await fetchImpl(input, {
            ...init,
            signal: combined,
          });
          reader = response.body?.getReader();
          const chunks: Uint8Array[] = [];
          let total = 0;
          while (reader !== undefined) {
            scope.assertCurrent();
            signal.throwIfAborted();
            const result = await reader.read();
            if (result.done) break;
            if (result.value.byteLength > limits.httpResponseBytes - total)
              throw new Error(
                "Committee source HTTP body exceeds its adopted limit",
              );
            chunks.push(result.value);
            total += result.value.byteLength;
          }
          scope.assertCurrent();
          signal.throwIfAborted();
          const headers = new Headers(response.headers);
          headers.delete("content-length");
          headers.delete("content-encoding");
          return new Response(
            response.body === null ? null : Buffer.concat(chunks, total),
            {
              status: response.status,
              statusText: response.statusText,
              headers,
            },
          );
        } finally {
          controller.abort();
          if (reader !== undefined)
            await reader.cancel().catch(() => undefined);
        }
      },
      { timeoutMs: limits.requestRefusalMs },
    );
};

export type CommitteeScopedOgmiosRpc = Readonly<{
  request: (
    method: string,
    params: Record<string, unknown>,
  ) => Promise<unknown>;
  close: () => void;
}>;

/** Native WebSocket buffers frames before delivering them. The byte limit is
 * post-receipt/pre-parse fencing; expiry closes the actual owned socket. */
export const committeeScopedOgmiosRpc = async (
  url: string,
  scope: DaAvailabilityReadScope,
  limits: CommitteeSourceReadLimits,
  factory: StateQueueReplayWebSocketFactory = committeeReadOwner(
    scope,
  )?.webSocketFor(scope, limits.webSocketMessageBytes) ??
    ((endpoint) =>
      new WebSocket(endpoint) as unknown as StateQueueReplayWebSocket),
): Promise<CommitteeScopedOgmiosRpc> => {
  assertCommitteeSourceReadLimits(limits);
  scope.assertCurrent();
  const endpoint = new URL(url);
  if (endpoint.protocol === "http:") endpoint.protocol = "ws:";
  else if (endpoint.protocol === "https:") endpoint.protocol = "wss:";
  if (!["ws:", "wss:"].includes(endpoint.protocol))
    throw new Error("Committee Ogmios endpoint must use HTTP(S) or WS(S)");
  const socket = factory(endpoint.toString());
  let closed = false;
  let nextId = 0;
  let terminal: unknown;
  let pending:
    | {
        id: number;
        resolve: (value: unknown) => void;
        reject: (reason: unknown) => void;
      }
    | undefined;
  let opened = false;
  let resolveOpen!: () => void;
  let rejectOpen!: (reason: unknown) => void;
  const opening = new Promise<void>((resolve, reject) => {
    resolveOpen = resolve;
    rejectOpen = reject;
  });
  // An already-aborted scope can refuse before it awaits the open promise.
  void opening.catch(() => undefined);
  const close = () => {
    if (closed) return;
    closed = true;
    pending?.reject(new Error("Committee Ogmios session was closed"));
    pending = undefined;
    if (!opened) rejectOpen(new Error("Committee Ogmios opening was closed"));
    scope.signal.removeEventListener("abort", abortScope);
    socket.close();
  };
  const fail = (reason: unknown) => {
    terminal ??= reason;
    pending?.reject(terminal);
    pending = undefined;
    if (!opened) rejectOpen(terminal);
    try {
      close();
    } catch {
      /* Preserve the primary read/transport error. */
    }
  };
  const abortScope = () => fail(scope.signal.reason);
  scope.signal.addEventListener("abort", abortScope, { once: true });
  socket.addEventListener("open", (() => {
    if (closed || scope.signal.aborted) return fail(scope.signal.reason);
    opened = true;
    resolveOpen();
  }) as (event: never) => void);
  socket.addEventListener("error", (() =>
    fail(new Error("Committee Ogmios socket failed"))) as (
    event: never,
  ) => void);
  socket.addEventListener("close", (() => {
    if (!closed)
      fail(new Error("Committee Ogmios socket closed during a read"));
  }) as (event: never) => void);
  socket.addEventListener("message", ((event: { data: unknown }) => {
    if (closed) return;
    try {
      scope.assertCurrent();
      if (
        typeof event.data !== "string" ||
        Buffer.byteLength(event.data) > limits.webSocketMessageBytes
      )
        throw new Error(
          "Committee Ogmios frame is invalid or exceeds its adopted limit",
        );
      const reply = JSON.parse(event.data) as {
        id?: unknown;
        result?: unknown;
        error?: unknown;
      };
      if (pending === undefined || reply.id !== pending.id)
        throw new Error(
          "Committee Ogmios response does not match its pending request",
        );
      if (reply.error !== undefined)
        throw new OgmiosJsonRpcError(
          "Committee Ogmios read refused",
          reply.error,
        );
      const waiter = pending;
      pending = undefined;
      waiter.resolve(reply.result);
    } catch (error) {
      fail(error);
    }
  }) as (event: never) => void);
  try {
    await scope.read(
      async (signal) => {
        const abort = () => fail(signal.reason);
        signal.addEventListener("abort", abort, { once: true });
        try {
          await opening;
        } finally {
          signal.removeEventListener("abort", abort);
        }
      },
      { timeoutMs: limits.requestRefusalMs },
    );
  } catch (error) {
    fail(error);
    throw error;
  }
  return {
    close,
    request: (method, params) =>
      scope.read(
        async (signal) => {
          if (terminal !== undefined)
            throw terminal instanceof Error
              ? terminal
              : new Error("Committee Ogmios transport failed", {
                  cause: terminal,
                });
          if (closed || pending !== undefined)
            throw new Error("Committee Ogmios session is closed or busy");
          const abort = () => fail(signal.reason);
          signal.addEventListener("abort", abort, { once: true });
          try {
            return await new Promise<unknown>((resolve, reject) => {
              const id = nextId++;
              pending = { id, resolve, reject };
              try {
                socket.send(
                  JSON.stringify({ jsonrpc: "2.0", id, method, params }),
                );
              } catch (error) {
                fail(error);
              }
            });
          } finally {
            signal.removeEventListener("abort", abort);
          }
        },
        { timeoutMs: limits.requestRefusalMs },
      ),
  };
};

/** Owning socket seam for retained-point readers. The whole proof is scoped
 * by its caller; native frame buffering remains post-receipt bounded. */
export const committeeScopedWebSocketFactory =
  (
    scope: DaAvailabilityReadScope,
    limits: CommitteeSourceReadLimits,
    factory: StateQueueReplayWebSocketFactory = committeeReadOwner(
      scope,
    )?.webSocketFor(scope, limits.webSocketMessageBytes) ??
      ((url) => new WebSocket(url) as unknown as StateQueueReplayWebSocket),
  ): StateQueueReplayWebSocketFactory =>
  (url) => {
    assertCommitteeSourceReadLimits(limits);
    scope.assertCurrent();
    const socket = factory(url);
    let closed = false;
    const close = (code?: number, reason?: string) => {
      if (closed) return;
      closed = true;
      scope.signal.removeEventListener("abort", abort);
      socket.close(code, reason);
    };
    const abort = () => close();
    scope.signal.addEventListener("abort", abort, { once: true });
    socket.addEventListener("close", (() => {
      closed = true;
      scope.signal.removeEventListener("abort", abort);
    }) as (event: never) => void);
    return {
      close,
      send: (data) => {
        scope.assertCurrent();
        socket.send(data);
      },
      addEventListener: (type, listener, options) => {
        if (type !== "message")
          return socket.addEventListener(type, listener, options);
        socket.addEventListener(
          type,
          ((event: { data: unknown }) => {
            if (
              scope.signal.aborted ||
              typeof event.data !== "string" ||
              Buffer.byteLength(event.data) > limits.webSocketMessageBytes
            ) {
              close();
              return;
            }
            listener(event as never);
          }) as (event: never) => void,
          options,
        );
      },
    };
  };
