import {
  acquireOgmiosSession,
  type OgmiosSession,
} from "./local-kupmios-http-ogmios-source.acquire-ogmios-session.js";
import {
  abortSignalAborted,
  debitReferenceResponse,
  MAX_RESPONSE_BYTES,
  type ReferenceReadScope,
  referenceResponseLimit,
  throwIfSourceAborted,
} from "./local-kupmios-http-ogmios-source.parse-ogmios-block.js";
import {
  type FraudProofRawL1WebSocketFactory,
  type FraudProofRawL1WebSocketLike,
  isNetworkFailure,
  type KupoPoint,
  LocalKupmiosTransportUnavailableError,
} from "./local-kupmios-http-ogmios-source.read-admitted-local-kupmios-signed-transaction-recovery.js";
import { type FraudProofRawL1Point } from "./raw-l1-snapshot.js";

export const openOgmiosSession = async ({
  url,
  timeoutMs,
  webSocketFactory,
  signal,
  maxResponseBytes,
  referenceScope,
}: {
  readonly url: string;
  readonly timeoutMs: number;
  readonly webSocketFactory: FraudProofRawL1WebSocketFactory;
  readonly signal: AbortSignal | undefined;
  readonly maxResponseBytes: number | undefined;
  readonly referenceScope?: ReferenceReadScope;
}): Promise<OgmiosSession> => {
  throwIfSourceAborted(signal);
  referenceResponseLimit(
    maxResponseBytes ?? MAX_RESPONSE_BYTES,
    referenceScope,
  );
  const releaseCapacity = await acquireOgmiosSession(url, timeoutMs, signal);
  let socket: FraudProofRawL1WebSocketLike;
  try {
    throwIfSourceAborted(signal);
    socket = webSocketFactory(url);
  } catch (error) {
    releaseCapacity();
    throwIfSourceAborted(signal);
    if (isNetworkFailure(error))
      throw new LocalKupmiosTransportUnavailableError(
        "Ogmios transport is unavailable",
        { cause: error },
      );
    throw error;
  }
  const openedMonotonicMs = performance.now();
  let physicallyClosed = false;
  let resolveClosed!: () => void;
  const closed = new Promise<void>((resolve) => {
    resolveClosed = resolve;
  });
  const pending = new Map<
    number,
    {
      method: string;
      resolve(value: unknown): void;
      reject(error: Error): void;
    }
  >();
  let lastMethod: string | null = null;
  let nextId = 0;
  let terminal: Error | null = null;
  let opening = true;
  let resolveOpening!: () => void;
  let rejectOpening!: (error: Error) => void;
  const opened = new Promise<void>((resolve, reject) => {
    resolveOpening = resolve;
    rejectOpening = reject;
  });
  const listeners: [string, (event: never) => void][] = [];
  const listen = (type: string, listener: (event: never) => void): void => {
    listeners.push([type, listener]);
    socket.addEventListener(type, listener);
  };
  // Every terminal path owns the whole concrete session. In particular an RPC
  // timeout now closes it immediately instead of waiting for the caller's
  // finally block; no later request can reuse a timed-out or failed session.
  const terminate = (error: Error): void => {
    if (terminal !== null) return;
    terminal = error;
    clearTimeout(openingTimer);
    signal?.removeEventListener("abort", onAbort);
    for (const [type, listener] of listeners) {
      socket.removeEventListener(type, listener);
    }
    listeners.length = 0;
    if (opening) {
      opening = false;
      rejectOpening(error);
    }
    for (const waiter of pending.values()) waiter.reject(error);
    pending.clear();
    if (physicallyClosed) return;
    try {
      socket.close();
    } catch {
      // A socket already failing/closing must not replace the terminal error.
    }
  };
  const onAbort = (): void => {
    terminate(
      new DOMException("local Kupmios raw source aborted", "AbortError"),
    );
  };
  listen("message", ((event: { data: unknown }) => {
    if (terminal !== null) return;
    if (typeof event.data !== "string") {
      terminate(new Error("Ogmios sent a non-text frame"));
      return;
    }
    // The platform WebSocket has already buffered this frame. This limit only
    // bounds text accepted for JSON parsing, not transport-frame allocation.
    if (
      maxResponseBytes !== undefined &&
      Buffer.byteLength(event.data, "utf8") > maxResponseBytes
    ) {
      terminate(new Error("Ogmios response exceeds the raw-source byte bound"));
      return;
    }
    try {
      debitReferenceResponse(
        referenceScope,
        Buffer.byteLength(event.data, "utf8"),
      );
    } catch (cause) {
      terminate(
        cause instanceof Error
          ? cause
          : new Error("reference acquisition frame budget exceeded"),
      );
      return;
    }
    let message: { id?: unknown; result?: unknown; error?: unknown };
    try {
      message = JSON.parse(event.data) as typeof message;
    } catch (cause) {
      terminate(new Error(`Ogmios sent malformed JSON: ${String(cause)}`));
      return;
    }
    if (
      typeof message !== "object" ||
      message === null ||
      Array.isArray(message)
    ) {
      terminate(new Error("Ogmios sent a non-object JSON response"));
      return;
    }
    if (typeof message.id !== "number") return;
    const waiter = pending.get(message.id);
    if (waiter === undefined) return;
    pending.delete(message.id);
    if (message.error !== undefined) {
      const text = `Ogmios error: ${JSON.stringify(message.error)}`;
      // Ogmios answers -32000 when it lost its own node connection: the
      // request was never evaluated, so it says nothing about the chain.
      waiter.reject(
        ogmiosLostNode(message.error)
          ? new LocalKupmiosTransportUnavailableError(text)
          : new Error(text),
      );
    } else {
      waiter.resolve(message.result);
    }
  }) as (event: never) => void);
  listen("error", (() =>
    terminate(
      new LocalKupmiosTransportUnavailableError(
        opening ? "Ogmios socket failed while opening" : "Ogmios socket failed",
      ),
    )) as (event: never) => void);
  // Keep the close listener until the physical transport ends, including after
  // local termination removed all RPC listeners. Do not release on close().
  const onClose = ((event: {
    code?: number;
    reason?: string;
    wasClean?: boolean;
  }) => {
    if (physicallyClosed) return;
    physicallyClosed = true;
    socket.removeEventListener("close", onClose);
    releaseCapacity();
    resolveClosed();
    const detail = {
      phase: opening ? "opening" : "active",
      pendingMethods: [...pending.values()].map(({ method }) => method),
      lastMethod,
      elapsedMs: Math.ceil(performance.now() - openedMonotonicMs),
      code: event.code,
      reason: event.reason?.slice(0, 256),
      wasClean: event.wasClean,
    };
    const message = `Ogmios socket closed: ${JSON.stringify(detail)}`;
    // A peer explicitly rejecting protocol, payload, policy or required
    // extensions is not evidence of a transient connection outage.
    terminate(
      event.code !== undefined &&
        [1002, 1003, 1007, 1008, 1009, 1010].includes(event.code)
        ? new Error(message)
        : new LocalKupmiosTransportUnavailableError(message),
    );
  }) as (event: never) => void;
  socket.addEventListener("close", onClose);
  listen("open", (() => {
    if (terminal !== null || !opening) return;
    opening = false;
    clearTimeout(openingTimer);
    resolveOpening();
  }) as (event: never) => void);
  const openingTimer = setTimeout(() => {
    terminate(
      new LocalKupmiosTransportUnavailableError(
        `Ogmios socket did not open within ${timeoutMs.toString()}ms`,
      ),
    );
  }, timeoutMs);
  signal?.addEventListener("abort", onAbort, { once: true });
  if (signal !== undefined && abortSignalAborted.call(signal)) onAbort();
  await opened;
  const assertSessionOpen = (): void => {
    throwIfSourceAborted(signal);
    if (terminal !== null) throw terminal;
  };
  return {
    request: async (method, params) => {
      assertSessionOpen();
      referenceResponseLimit(
        maxResponseBytes ?? MAX_RESPONSE_BYTES,
        referenceScope,
      );
      const id = nextId;
      nextId += 1;
      const encoded = JSON.stringify({ jsonrpc: "2.0", method, params, id });
      const result = await new Promise<unknown>((resolve, reject) => {
        const timer = setTimeout(() => {
          terminate(
            new LocalKupmiosTransportUnavailableError(
              `Ogmios ${method} timed out`,
            ),
          );
        }, timeoutMs);
        lastMethod = method;
        pending.set(id, {
          method,
          resolve: (value) => {
            clearTimeout(timer);
            resolve(value);
          },
          reject: (error) => {
            clearTimeout(timer);
            reject(error);
          },
        });
        try {
          socket.send(encoded);
        } catch (cause) {
          terminate(
            new LocalKupmiosTransportUnavailableError(
              "Ogmios socket send failed",
              { cause },
            ),
          );
        }
      });
      assertSessionOpen();
      return result;
    },
    close: async () => {
      const priorFailure = terminal;
      terminate(new Error("Ogmios session closed"));
      let timer: ReturnType<typeof setTimeout> | undefined;
      try {
        await Promise.race([
          closed,
          new Promise<never>((_resolve, reject) => {
            timer = setTimeout(
              () =>
                reject(
                  priorFailure !== null &&
                    !(
                      priorFailure instanceof
                      LocalKupmiosTransportUnavailableError
                    )
                    ? priorFailure
                    : new LocalKupmiosTransportUnavailableError(
                        "Ogmios physical socket close timed out",
                        { cause: terminal },
                      ),
                ),
              timeoutMs,
            );
          }),
        ]);
      } finally {
        clearTimeout(timer);
      }
    },
  };
};

const ogmiosLostNode = (error: unknown): boolean => {
  if (typeof error !== "object" || error === null) return false;
  const { code, message } = error as { code?: unknown; message?: unknown };
  return (
    code === -32000 ||
    (typeof message === "string" &&
      /connection with the node lost/iu.test(message))
  );
};

export const sameKupoPoint = (left: KupoPoint, right: KupoPoint): boolean =>
  left.slot === right.slot && left.blockHash === right.blockHash;

export const sameRawPoint = (
  left: FraudProofRawL1Point,
  right: FraudProofRawL1Point,
): boolean =>
  left.slot === right.slot &&
  left.blockHash === right.blockHash &&
  left.blockNo === right.blockNo &&
  left.pointId === right.pointId;
