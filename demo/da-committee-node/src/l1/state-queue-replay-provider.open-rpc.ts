import { createHash } from "node:crypto";

import {
  formatOgmiosJsonRpcError,
  OgmiosJsonRpcError,
} from "@al-ft/midgard-core/ogmios-json-rpc-error";
import * as SDK from "@al-ft/midgard-sdk";

import {
  L1SourceIntegrityError,
  StateQueueHistoryNotExtendingAnchorError,
} from "./source-integrity.js";

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export type Queue = readonly SDK.StateQueueTransitionNode[];

export type Point = Readonly<{ slot: number; blockHash: string }>;

export type Spend = Readonly<{ transactionHash: string; point: Point }>;

export type Transaction = Readonly<{
  transactionHash: string;
  blockHash: string;
  slot: number;
  blockNo: number;
  transactionIndex: number;
  selectedChainTip: Readonly<{ id: string; slot: number; height: number }>;
  mintPolicyIds: readonly string[];
  redeemers: readonly SDK.StateQueueTransitionRedeemer[];
  spentInputOutRefs: readonly string[];
  referenceInputOutRefs: readonly string[];
  /** The raw transaction, when Ogmios serves it; the replay ignores it. */
  cbor?: string;
}>;

export type HistoricalOutput = Readonly<{
  node: SDK.StateQueueTransitionNode;
  nextHeaderHash: string | null;
}>;

export type StateQueueReplayFetch = (
  input: string,
  init?: RequestInit,
) => Promise<Response>;

export type StateQueueReplayWebSocket = Readonly<{
  send(data: string): void;
  close(code?: number, reason?: string): void;
  addEventListener(
    type: string,
    listener: (event: never) => void,
    options?: { once?: boolean },
  ): void;
}>;

export type StateQueueReplayWebSocketFactory = (
  url: string,
) => StateQueueReplayWebSocket;

const stableJson = (value: unknown): string => {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(stableJson).join(",")}]`;
  return `{${Object.entries(value as Record<string, unknown>)
    .sort(([left], [right]) => left.localeCompare(right))
    .map(([key, member]) => `${JSON.stringify(key)}:${stableJson(member)}`)
    .join(",")}}`;
};

export const digest = (value: unknown): string =>
  createHash("sha256").update(stableJson(value)).digest("hex");

export const sameQueue = (left: Queue, right: Queue): boolean =>
  left.length === right.length &&
  left.every(
    (node, index) =>
      node.headerHash === right[index]?.headerHash &&
      node.outRef === right[index]?.outRef,
  );

export const splitOutRef = (
  value: string,
): { txHash: string; index: number } => {
  if (!OUT_REF.test(value))
    throw new Error("state-queue replay outref is invalid");
  const [txHash, index] = value.split("#") as [string, string];
  return { txHash, index: Number(index) };
};

export const httpUrl = (value: string): string => {
  const url = new URL(value);
  if (url.protocol === "ws:") url.protocol = "http:";
  if (url.protocol === "wss:") url.protocol = "https:";
  url.hash = "";
  return url.toString().replace(/\/$/u, "");
};

const wsUrl = (value: string): string => {
  const url = new URL(value);
  if (url.protocol === "http:") url.protocol = "ws:";
  if (url.protocol === "https:") url.protocol = "wss:";
  url.hash = "";
  return url.toString().replace(/\/$/u, "");
};

/** Bound on each Kupo and Ogmios HTTP read of the replay, the same as its
 * Ogmios socket requests. Every read is a point query (a checkpoint, one
 * output, one transaction's outputs, the tip height), never a ledger scan. */
export const STATE_QUEUE_REPLAY_REQUEST_TIMEOUT_MS = 20_000;

export const json = async (
  fetchImpl: StateQueueReplayFetch,
  url: string,
  init?: RequestInit,
  timeoutMs = STATE_QUEUE_REPLAY_REQUEST_TIMEOUT_MS,
): Promise<unknown> => {
  // A hung Kupo or Ogmios fails the read instead of wedging the replay; a
  // caller's own signal still aborts it sooner.
  const timeout = AbortSignal.timeout(timeoutMs);
  const response = await fetchImpl(url, {
    ...init,
    signal:
      init?.signal === undefined || init.signal === null
        ? timeout
        : AbortSignal.any([init.signal, timeout]),
  });
  const body = await response.text();
  if (!response.ok) {
    throw new Error(
      `state-queue replay HTTP ${response.status.toString()}: ${body.slice(0, 256)}`,
    );
  }
  try {
    return JSON.parse(body) as unknown;
  } catch (cause) {
    throw new Error("state-queue replay source returned malformed JSON", {
      cause,
    });
  }
};

export const point = (value: unknown, label: string): Point => {
  const candidate = value as { slot_no?: unknown; header_hash?: unknown };
  if (
    typeof candidate.slot_no !== "number" ||
    !Number.isSafeInteger(candidate.slot_no) ||
    candidate.slot_no < 0 ||
    typeof candidate.header_hash !== "string" ||
    !HEX_32.test(candidate.header_hash)
  ) {
    throw new Error(`${label} is not a canonical chain point`);
  }
  return { slot: candidate.slot_no, blockHash: candidate.header_hash };
};

export const fetchSpend = async (
  kupoUrl: string,
  reference: string,
  fetchImpl: StateQueueReplayFetch,
): Promise<Spend | null> => {
  const { txHash, index } = splitOutRef(reference);
  const body = await json(
    fetchImpl,
    `${httpUrl(kupoUrl)}/matches/${index.toString()}@${txHash}?resolve_hashes`,
  );
  if (!Array.isArray(body))
    throw new Error("Kupo spend lookup is not an array");
  const matches = body.filter(
    (item) =>
      (item as { transaction_id?: unknown }).transaction_id === txHash &&
      (item as { output_index?: unknown }).output_index === index,
  );
  // Replay walks outputs of the history it has just read from its anchor,
  // and Kupo keeps spent outputs. An output it does not know is not on this
  // chain: from a final anchor it was rolled back deeper than finality, from
  // a bootstrap candidate the candidate was rolled back. One it knows more
  // than once means a corrupt index. A rollback during the replay itself is
  // told apart by the held-chain check around it.
  if (matches.length !== 1) {
    const message = `Kupo does not know state-queue output ${reference} exactly once (${matches.length.toString()} matches)`;
    throw matches.length === 0
      ? new StateQueueHistoryNotExtendingAnchorError(message)
      : new L1SourceIntegrityError(message);
  }
  const match = matches[0] as { datum?: unknown; spent_at?: unknown };
  if (!("datum" in match)) {
    throw new Error("Kupo replay requires resolve_hashes support");
  }
  if (match.spent_at === null) return null;
  if (typeof match.spent_at !== "object" || match.spent_at === undefined) {
    throw new Error("Kupo spend lookup omitted spent_at");
  }
  const spent = match.spent_at as { transaction_id?: unknown };
  if (
    typeof spent.transaction_id !== "string" ||
    !HEX_32.test(spent.transaction_id)
  ) {
    throw new Error("Kupo spent_at transaction id is invalid");
  }
  return {
    transactionHash: spent.transaction_id,
    point: point(spent, "spent_at"),
  };
};

export const fetchAncestor = async (
  kupoUrl: string,
  slot: number,
  fetchImpl: StateQueueReplayFetch,
): Promise<Point> => {
  if (slot < 1) throw new Error("genesis has no replay ancestor");
  return point(
    await json(
      fetchImpl,
      `${httpUrl(kupoUrl)}/checkpoints/${(slot - 1).toString()}`,
    ),
    "Kupo replay ancestor",
  );
};

type Rpc = Readonly<{
  request(method: string, params: Record<string, unknown>): Promise<unknown>;
  close(): void;
}>;

export const openRpc = async (
  url: string,
  factory: StateQueueReplayWebSocketFactory,
): Promise<Rpc> => {
  const socket = factory(wsUrl(url));
  const pending = new Map<
    number,
    { resolve(value: unknown): void; reject(error: Error): void }
  >();
  let nextId = 0;
  let terminal: Error | null = null;
  const fail = (error: Error): void => {
    terminal ??= error;
    for (const waiter of pending.values()) waiter.reject(error);
    pending.clear();
  };
  socket.addEventListener("message", ((event: { data: unknown }) => {
    if (typeof event.data !== "string")
      return fail(new Error("Ogmios returned binary data"));
    let response: { id?: unknown; result?: unknown; error?: unknown };
    try {
      response = JSON.parse(event.data) as typeof response;
    } catch (cause) {
      return fail(new Error("Ogmios returned malformed JSON", { cause }));
    }
    if (typeof response.id !== "number") return;
    const waiter = pending.get(response.id);
    if (waiter === undefined) return;
    pending.delete(response.id);
    if (response.error !== undefined) {
      waiter.reject(
        new OgmiosJsonRpcError(
          `Ogmios replay error: ${formatOgmiosJsonRpcError(response.error)}`,
          response.error,
        ),
      );
    } else {
      waiter.resolve(response.result);
    }
  }) as (event: never) => void);
  socket.addEventListener("error", (() =>
    fail(new Error("Ogmios replay socket failed"))) as (event: never) => void);
  socket.addEventListener("close", (() =>
    fail(new Error("Ogmios replay socket closed"))) as (event: never) => void);
  await new Promise<void>((resolve, reject) => {
    const timer = setTimeout(
      () => reject(new Error("Ogmios replay socket open timed out")),
      STATE_QUEUE_REPLAY_REQUEST_TIMEOUT_MS,
    );
    socket.addEventListener(
      "open",
      (() => {
        clearTimeout(timer);
        resolve();
      }) as (event: never) => void,
      { once: true },
    );
    socket.addEventListener(
      "error",
      (() => {
        clearTimeout(timer);
        reject(new Error("Ogmios replay socket failed while opening"));
      }) as (event: never) => void,
      { once: true },
    );
  });
  return {
    request: async (method, params) => {
      if (terminal !== null) throw terminal;
      const id = nextId++;
      return await new Promise<unknown>((resolve, reject) => {
        const timer = setTimeout(() => {
          pending.delete(id);
          reject(new Error(`Ogmios ${method} replay timed out`));
        }, STATE_QUEUE_REPLAY_REQUEST_TIMEOUT_MS);
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
        socket.send(JSON.stringify({ jsonrpc: "2.0", method, params, id }));
      });
    },
    close: () => socket.close(),
  };
};
