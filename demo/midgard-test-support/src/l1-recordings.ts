import { readFileSync } from "node:fs";
import { isDeepStrictEqual } from "node:util";

/**
 * Real Kupo and Ogmios responses, recorded off a live preprod stack by
 * `scripts/capture-l1-recordings.mjs`, and replays that serve them back to the
 * production readers.
 *
 * A hand-written double of Kupo or Ogmios states what its author believes the
 * wire carries, so a wrong belief passes CI on both sides of the seam. A
 * recording states what the service answered. Serving it through the reader's
 * own transport — its `fetch`, its WebSocket — means the parsers under test meet
 * the real shapes: Kupo v2.11.0's mirrored `spent_at`, an Ogmios tip with no
 * `height`, the chain-sync handshake echo.
 *
 * **What a replay changes, and nothing else.** A JSON-RPC answer is sent back
 * under the id of the request it answers, because the reader picks its own ids;
 * the rest of the envelope is the recorded bytes. Kupo answers are served for
 * the exact path and query that was recorded, as often as they are asked for:
 * a `GET` on an index that has not moved answers the same. Ogmios answers are
 * consumed in the order they were recorded — per method for state queries, and
 * as one stream for chain-sync, whose answers depend on what came before. A
 * request the recording does not hold fails the test loudly rather than being
 * answered with something invented, with one exception that *is* the recorded
 * behaviour: a `nextBlock` past the end of the recorded stream is left
 * unanswered, which is what Ogmios does at the tip until the node adopts the
 * next block (the network recording shows it waiting 34 s for one).
 *
 * `loadL1Recording` parses the file afresh on every call, so a test may mutate
 * its copy — a field Kupo never sends, a redeemer the transaction never carried
 * — to prove a reader refuses or ignores it.
 */

export type L1RecordingName =
  | "preprod-state-queue-removal-a2a47d2e"
  | "preprod-reference-sweep-e1e70271"
  | "preprod-ogmios-network"
  | "preprod-ogmios-follow-tip";

/** A response body: parsed when that reproduces the bytes, raw otherwise. */
export type RecordedBody =
  | { readonly body: unknown; readonly bodyText?: never }
  | { readonly bodyText: string; readonly body?: never };

type HttpResponse = RecordedBody & {
  readonly status: number;
  readonly headers: Readonly<Record<string, string>>;
};

export type JsonRpcRequest = {
  readonly jsonrpc: "2.0";
  readonly method: string;
  readonly params?: unknown;
  readonly id: string | null;
};

export type KupoExchange = {
  readonly surface: "kupo";
  readonly request: {
    readonly method: "GET";
    readonly path: string;
    readonly accept: string;
  };
  readonly response: HttpResponse;
  readonly elapsedMs: number;
};

export type OgmiosHttpExchange = {
  readonly surface: "ogmios-http";
  readonly request: JsonRpcRequest;
  readonly response: HttpResponse;
  readonly elapsedMs: number;
};

export type OgmiosWebSocketExchange = {
  readonly surface: "ogmios-websocket";
  readonly request: JsonRpcRequest;
  readonly response: RecordedBody;
  readonly elapsedMs: number;
};

export type L1Exchange =
  | KupoExchange
  | OgmiosHttpExchange
  | OgmiosWebSocketExchange;

export type L1Recording = {
  readonly name: L1RecordingName;
  readonly description: string;
  readonly provenance: {
    readonly network: string;
    readonly networkMagic: number;
    readonly kupo: { readonly url: string; readonly version: string };
    readonly ogmios: { readonly url: string; readonly version: string };
    readonly capturedAt: string;
    readonly tool: string;
  };
  /** Present on a transaction recording: the transaction and its block. */
  readonly transaction?: {
    readonly id: string;
    readonly block: { readonly slot: number; readonly id: string };
  };
  readonly exchanges: L1Exchange[];
};

export const loadL1Recording = (name: L1RecordingName): L1Recording =>
  JSON.parse(
    readFileSync(
      new URL(`../fixtures/l1/${name}.json`, import.meta.url),
      "utf8",
    ),
  ) as L1Recording;

/** The response text exactly as the service sent it. */
export const recordedText = (response: RecordedBody): string =>
  response.bodyText ?? JSON.stringify(response.body);

/** The recorded Kupo exchange for `path`; a missing one is a test bug. */
export const kupoExchange = (
  recording: L1Recording,
  path: string,
): KupoExchange => {
  const exchange = recording.exchanges.find(
    (candidate): candidate is KupoExchange =>
      candidate.surface === "kupo" && candidate.request.path === path,
  );
  if (exchange === undefined) {
    throw new Error(`${recording.name} holds no Kupo GET ${path}`);
  }
  return exchange;
};

/** The first recorded Kupo match for `txHash#outputIndex`. */
export const kupoMatch = (
  recording: L1Recording,
  outRef: { readonly txHash: string; readonly outputIndex: number },
): Record<string, unknown> => {
  const [match] = kupoExchange(
    recording,
    `/matches/${outRef.outputIndex.toString()}@${outRef.txHash}?resolve_hashes`,
  ).response.body as Record<string, unknown>[];
  if (match === undefined) {
    throw new Error(
      `${recording.name} has no Kupo match for ${outRef.txHash}#${outRef.outputIndex.toString()}`,
    );
  }
  return match;
};

/** The recorded Ogmios WebSocket exchanges for `method`, in capture order. */
export const ogmiosExchanges = (
  recording: L1Recording,
  method: string,
): readonly OgmiosWebSocketExchange[] =>
  recording.exchanges.filter(
    (candidate): candidate is OgmiosWebSocketExchange =>
      candidate.surface === "ogmios-websocket" &&
      candidate.request.method === method,
  );

/** The JSON-RPC `result` of a recorded Ogmios exchange. */
export const ogmiosResult = (exchange: OgmiosWebSocketExchange): unknown =>
  (JSON.parse(recordedText(exchange.response)) as { result?: unknown }).result;

/**
 * The transaction a transaction recording is about, as chain-sync delivered it
 * in the recorded block.
 */
export const recordedTransaction = (
  recording: L1Recording,
): Record<string, unknown> => {
  const id = recording.transaction?.id;
  for (const exchange of ogmiosExchanges(recording, "nextBlock")) {
    const result = ogmiosResult(exchange) as {
      direction?: string;
      block?: { transactions?: Record<string, unknown>[] };
    };
    const transaction = result.block?.transactions?.find(
      (candidate) => candidate.id === id,
    );
    if (transaction !== undefined) return transaction;
  }
  throw new Error(`${recording.name} delivered no transaction ${String(id)}`);
};

const withId = (response: RecordedBody, id: unknown): string =>
  JSON.stringify({
    ...(JSON.parse(recordedText(response)) as Record<string, unknown>),
    id,
  });

const sameParams = (recorded: unknown, sent: unknown): boolean =>
  isDeepStrictEqual(recorded ?? {}, sent ?? {});

/**
 * A `fetch` that answers from the recording: Kupo `GET`s by exact path and
 * query, Ogmios HTTP JSON-RPC posts from each method's recorded answers in
 * order. The host part of the URL is not consulted, so a reader may be
 * configured with any Kupo and Ogmios URL.
 */
export const recordedFetch = (
  recording: L1Recording,
): ((
  input: string | URL | Request,
  init?: RequestInit,
) => Promise<Response>) => {
  const httpQueues = new Map<string, OgmiosHttpExchange[]>();
  for (const exchange of recording.exchanges) {
    if (exchange.surface !== "ogmios-http") continue;
    const queue = httpQueues.get(exchange.request.method) ?? [];
    queue.push(exchange);
    httpQueues.set(exchange.request.method, queue);
  }
  return async (input, init) => {
    const url = new URL(input instanceof Request ? input.url : input);
    if ((init?.method ?? "GET") === "POST") {
      if (typeof init?.body !== "string") {
        throw new Error("an Ogmios HTTP request must carry a JSON body");
      }
      const sent = JSON.parse(init.body) as JsonRpcRequest;
      const exchange = httpQueues.get(sent.method)?.shift();
      if (
        exchange === undefined ||
        !sameParams(exchange.request.params, sent.params)
      ) {
        throw new Error(
          `${recording.name} holds no further Ogmios HTTP ${sent.method} ${JSON.stringify(sent.params ?? {})}`,
        );
      }
      return new Response(withId(exchange.response, sent.id ?? null), {
        status: exchange.response.status,
        headers: exchange.response.headers,
      });
    }
    const { response } = kupoExchange(
      recording,
      `${url.pathname}${url.search}`,
    );
    return new Response(recordedText(response), {
      status: response.status,
      headers: response.headers,
    });
  };
};

type Listener = (event: never) => void;

/** The WebSocket surface a replayed socket offers, in both listener styles. */
export type RecordedOgmiosSocket = {
  onopen: ((event: unknown) => void) | null;
  onmessage: ((event: { readonly data: unknown }) => void) | null;
  onerror: ((event: unknown) => void) | null;
  onclose: ((event: unknown) => void) | null;
  addEventListener(
    type: string,
    listener: Listener,
    options?: { once?: boolean },
  ): void;
  send(raw: string): void;
  close(): void;
};

export type RecordedOgmiosReplay = {
  readonly WebSocket: new (url: string) => RecordedOgmiosSocket;
  /** How many connections were opened. */
  readonly sockets: () => number;
  /** Every JSON-RPC request sent, across connections, in order. */
  readonly requests: () => readonly JsonRpcRequest[];
};

/**
 * A WebSocket class that replays the recorded Ogmios session. Each socket
 * replays it from the start, as a fresh connection to Ogmios would begin a
 * fresh session. It speaks both styles the workspace's readers use —
 * `addEventListener` and the `on*` properties — so the same replay can be a
 * `webSocketFactory` result or a stubbed global `WebSocket`.
 */
export const recordedOgmiosWebSocket = (
  recording: L1Recording,
): RecordedOgmiosReplay => {
  const exchanges = recording.exchanges.filter(
    (candidate): candidate is OgmiosWebSocketExchange =>
      candidate.surface === "ogmios-websocket",
  );
  const requests: JsonRpcRequest[] = [];
  let sockets = 0;

  class RecordedOgmiosWebSocket implements RecordedOgmiosSocket {
    onopen: ((event: unknown) => void) | null = null;
    onmessage: ((event: { readonly data: unknown }) => void) | null = null;
    onerror: ((event: unknown) => void) | null = null;
    onclose: ((event: unknown) => void) | null = null;
    private readonly listeners = new Map<string, Listener[]>();
    private readonly chainSync = exchanges.filter(({ request }) =>
      ["findIntersection", "nextBlock"].includes(request.method),
    );
    private readonly queries = exchanges.filter(
      ({ request }) =>
        !["findIntersection", "nextBlock"].includes(request.method),
    );

    constructor(_url: string) {
      sockets += 1;
      queueMicrotask(() => this.emit("open", {}));
    }

    addEventListener(
      type: string,
      listener: Listener,
      _options?: { once?: boolean },
    ): void {
      this.listeners.set(type, [...(this.listeners.get(type) ?? []), listener]);
    }

    send(raw: string): void {
      const sent = JSON.parse(raw) as JsonRpcRequest;
      requests.push(sent);
      const isChainSync = ["findIntersection", "nextBlock"].includes(
        sent.method,
      );
      const source = isChainSync ? this.chainSync : this.queries;
      const index = isChainSync
        ? 0
        : source.findIndex(({ request }) => request.method === sent.method);
      const exchange = index === -1 ? undefined : source[index];
      if (exchange === undefined && sent.method === "nextBlock") {
        // Past the recorded stream: Ogmios holds a nextBlock at the tip.
        return;
      }
      if (
        exchange === undefined ||
        exchange.request.method !== sent.method ||
        !sameParams(exchange.request.params, sent.params)
      ) {
        throw new Error(
          `${recording.name} holds no further Ogmios ${sent.method} ${JSON.stringify(sent.params ?? {})}`,
        );
      }
      source.splice(index, 1);
      queueMicrotask(() =>
        this.emit("message", { data: withId(exchange.response, sent.id) }),
      );
    }

    close(): void {}

    private emit(type: string, event: unknown): void {
      const property = this[`on${type}` as "onopen"];
      property?.(event);
      for (const listener of this.listeners.get(type) ?? []) {
        listener(event as never);
      }
    }
  }

  return {
    WebSocket: RecordedOgmiosWebSocket,
    sockets: () => sockets,
    requests: (): readonly JsonRpcRequest[] => requests,
  };
};
