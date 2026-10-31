import {
  decodeOgmiosJsonRpcError,
  formatOgmiosJsonRpcError,
  isTransientOgmiosJsonRpcErrorCode,
  OgmiosJsonRpcError,
  type OgmiosJsonRpcErrorAnswer,
} from "@al-ft/midgard-core/ogmios-json-rpc-error";
import { type OutRefLike } from "@al-ft/midgard-core/out-ref";

import { type OgmiosSession } from "./kupmios-history.fetch-kupo-spend.js";
import {
  HEX_32,
  type ObservedL1Transaction,
  type WebSocketFactory,
} from "./kupmios-history.l1-chain-point.js";
import {
  L1SourceUnavailable,
  OgmiosRequestTimeout,
} from "./kupmios-history.source-unavailable.js";

/**
 * An Ogmios error answer whose code says the node cannot answer now (still
 * syncing, crossing an era, the acquired state expired, its node connection
 * lost): the same outage as a dropped socket, so a history reader may retry
 * on it.
 */
export class OgmiosJsonRpcUnavailable extends L1SourceUnavailable {
  readonly answer: OgmiosJsonRpcErrorAnswer;

  constructor(message: string, answer: OgmiosJsonRpcErrorAnswer) {
    super(message);
    this.name = "OgmiosJsonRpcUnavailable";
    this.answer = answer;
  }
}

/**
 * The typed failure for an Ogmios JSON-RPC error answer: transient codes are
 * an `OgmiosJsonRpcUnavailable`, every other code (a malformed request, an
 * unknown method, a missing intersection, a misconfigured node) a terminal
 * `OgmiosJsonRpcError`. The text keeps the "Ogmios chain-sync error: <error
 * JSON>" form operators grep for.
 */
export const ogmiosJsonRpcAnswerFailure = (
  error: unknown,
): OgmiosJsonRpcUnavailable | OgmiosJsonRpcError => {
  const message = `Ogmios chain-sync error: ${formatOgmiosJsonRpcError(error)}`;
  const answer = decodeOgmiosJsonRpcError(error);
  return isTransientOgmiosJsonRpcErrorCode(answer.code)
    ? new OgmiosJsonRpcUnavailable(message, answer)
    : new OgmiosJsonRpcError(message, error);
};

/** The Ogmios code of a session's error answer; undefined for any other failure. */
export const ogmiosJsonRpcAnswerCode = (error: unknown): number | undefined =>
  error instanceof OgmiosJsonRpcUnavailable ||
  error instanceof OgmiosJsonRpcError
    ? error.answer.code
    : undefined;

export const openOgmiosSession = async ({
  url,
  timeoutMs,
  webSocketFactory,
  parseMessage = JSON.parse,
  signal,
}: {
  readonly url: string;
  readonly timeoutMs: number;
  readonly webSocketFactory: WebSocketFactory;
  readonly parseMessage?: (text: string) => unknown;
  readonly signal?: AbortSignal;
}): Promise<OgmiosSession> => {
  signal?.throwIfAborted();
  const socket = webSocketFactory(url);
  const pending = new Map<
    number,
    { resolve: (value: unknown) => void; reject: (error: Error) => void }
  >();
  let terminal: Error | null = null;
  let nextId = 0;
  let rejectOpening: ((error: Error) => void) | undefined;

  const failAll = (error: Error): void => {
    terminal ??= error;
    rejectOpening?.(error);
    for (const waiter of pending.values()) {
      waiter.reject(error);
    }
    pending.clear();
  };
  const close = () => {
    signal?.removeEventListener("abort", abort);
    failAll(new Error("Ogmios session closed"));
    socket.close();
  };
  const abort = () => {
    failAll(new Error("Ogmios session aborted", { cause: signal?.reason }));
    close();
  };

  socket.addEventListener("message", ((event: { data: unknown }) => {
    if (typeof event.data !== "string") {
      failAll(new Error("Ogmios chain-sync sent a non-text frame"));
      return;
    }
    let message: {
      id?: unknown;
      result?: unknown;
      error?: unknown;
    };
    try {
      const parsed = parseMessage(event.data);
      if (
        typeof parsed !== "object" ||
        parsed === null ||
        Array.isArray(parsed)
      ) {
        throw new Error("Ogmios response is not an object");
      }
      message = parsed as typeof message;
    } catch (cause) {
      failAll(new Error("Ogmios chain-sync sent malformed JSON", { cause }));
      return;
    }
    if (typeof message.id !== "number") {
      // Unsolicited or unmatchable: nothing correlates it to a request, so it
      // cannot be answered and must not be silently treated as one.
      return;
    }
    const waiter = pending.get(message.id);
    if (waiter === undefined) {
      return;
    }
    pending.delete(message.id);
    if (message.error !== undefined) {
      waiter.reject(ogmiosJsonRpcAnswerFailure(message.error));
      return;
    }
    waiter.resolve(message.result);
  }) as (event: never) => void);
  socket.addEventListener("error", (() => {
    failAll(new L1SourceUnavailable("Ogmios chain-sync socket failed"));
    close();
  }) as (event: never) => void);
  socket.addEventListener("close", (() => {
    signal?.removeEventListener("abort", abort);
    failAll(new L1SourceUnavailable("Ogmios chain-sync socket closed"));
  }) as (event: never) => void);

  try {
    signal?.addEventListener("abort", abort, { once: true });
    signal?.throwIfAborted();
    await new Promise<void>((resolve, reject) => {
      const timer = setTimeout(() => {
        failAll(
          new L1SourceUnavailable(
            `Ogmios chain-sync did not open within ${timeoutMs}ms`,
          ),
        );
        close();
      }, timeoutMs);
      rejectOpening = (error) => {
        clearTimeout(timer);
        rejectOpening = undefined;
        reject(error);
      };
      socket.addEventListener(
        "open",
        (() => {
          clearTimeout(timer);
          rejectOpening = undefined;
          if (terminal !== null) reject(terminal);
          else resolve();
        }) as (event: never) => void,
        { once: true },
      );
    });
  } catch (error) {
    close();
    throw error;
  }

  return {
    request: async (method, params, options) => {
      if (terminal !== null) {
        throw terminal;
      }
      const id = nextId;
      nextId += 1;
      // ChainSync nextBlock legitimately waits at tip. Its caller must own a
      // separate transport-health deadline and close this session on failure.
      const requestTimeout =
        options === undefined ? timeoutMs : options.timeoutMs;
      if (
        requestTimeout !== null &&
        (!Number.isSafeInteger(requestTimeout) || requestTimeout <= 0)
      )
        throw new Error("Ogmios request timeout must be positive or null");
      return await new Promise<unknown>((resolve, reject) => {
        const timer =
          requestTimeout === null
            ? undefined
            : setTimeout(() => {
                pending.delete(id);
                reject(
                  new OgmiosRequestTimeout(
                    `Ogmios ${method} did not answer within ${requestTimeout}ms`,
                  ),
                );
              }, requestTimeout);
        pending.set(id, {
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
          socket.send(JSON.stringify({ jsonrpc: "2.0", method, params, id }));
        } catch (cause) {
          clearTimeout(timer);
          pending.delete(id);
          reject(
            new L1SourceUnavailable(`Failed to send Ogmios ${method}`, {
              cause,
            }),
          );
        }
      });
    },
    close,
  };
};

const exactOutRef = (value: unknown, label: string): OutRefLike => {
  const record = value as { transaction?: { id?: unknown }; index?: unknown };
  const txHash = record.transaction?.id;
  if (typeof txHash !== "string" || !HEX_32.test(txHash)) {
    throw new Error(`${label}.transaction.id is not a transaction id`);
  }
  const outputIndex = record.index;
  if (
    typeof outputIndex !== "number" ||
    !Number.isSafeInteger(outputIndex) ||
    outputIndex < 0
  ) {
    throw new Error(`${label}.index is not an output index`);
  }
  return { txHash, outputIndex };
};

/**
 * Ogmios's JSON transaction view, narrowed to what the history readers use:
 * the id, the spent inputs in ledger order and the reference inputs. These
 * fields are always there; the raw transaction CBOR is not, since Ogmios
 * emits a transaction's `cbor` only when the server was started with
 * `--include-transaction-cbor`.
 */
export const parseObservedTransaction = (
  value: unknown,
  label: string,
): ObservedL1Transaction => {
  const record = value as {
    id?: unknown;
    inputs?: unknown;
    references?: unknown;
  };
  const txHash = record.id;
  if (typeof txHash !== "string" || !HEX_32.test(txHash)) {
    throw new Error(`${label}.id is not a transaction id`);
  }
  const spentInputs =
    record.inputs === undefined
      ? []
      : Array.isArray(record.inputs)
        ? record.inputs.map((entry, index) =>
            exactOutRef(entry, `${label}.inputs[${index.toString()}]`),
          )
        : (() => {
            throw new Error(`${label}.inputs is not an array`);
          })();
  const references =
    record.references === undefined
      ? []
      : Array.isArray(record.references)
        ? record.references.map((entry, index) =>
            exactOutRef(entry, `${label}.references[${index.toString()}]`),
          )
        : (() => {
            throw new Error(`${label}.references is not an array`);
          })();
  return {
    txHash,
    spentInputs,
    referenceInputs: references,
  };
};
