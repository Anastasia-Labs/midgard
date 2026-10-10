/**
 * Resolution of outputs the follower does not hold, by outref (plan §12.3
 * steps 2 to 4). An outref names its creating transaction, and that
 * transaction's id is the hash of its body, so an output's bytes are fixed by
 * its outref: whichever step finds them finds the same bytes. The steps
 * differ only in what they can still reach.
 *
 * 2. The ledger state at a recent point (LocalStateQuery `GetUTxOByTxIn`
 *    acquired there), exact while the point is in the node's volatile window
 *    even if the output was spent after it.
 * 3. The ledger state at the tip, while the output is unspent.
 * 4. The creating transaction fetched by id from a content source, accepted
 *    only when blake2b-256 of its body equals the id.
 *
 * Step 1 (the same block) is the caller's: it holds the block. Nothing here
 * throws for an unreachable or untrusted answer: every step that cannot
 * answer falls through, and what no step found is returned as pending with
 * the reasons, for the caller to retry.
 */
import {
  chainPoint,
  type L1NodeTransport,
  TransportRequestError,
} from "@al-ft/l1-node-transport";

import { outRefKey } from "../codec.js";
import { decodeTransaction, transactionOutputAt } from "../decode/tx.js";
import { decodeLedgerUtxos, type LedgerUtxo } from "../decode/utxo.js";
import type { FactStore } from "../store/fact-store.js";
import type { OutputSummary, OutRef, Point } from "../types.js";

/** The ledger cannot be acquired at that point (older than its volatile window, or off its chain). */
export type LedgerPointUnavailable = Readonly<{
  kind: "point_unavailable";
  detail: string;
}>;

/**
 * The unspent outputs among `outRefs` in the ledger state at `at`; the
 * outrefs it does not return are not unspent there.
 */
export type LedgerOutputs = (
  at: Point | "tip",
  outRefs: readonly OutRef[],
) => Promise<readonly LedgerUtxo[] | LedgerPointUnavailable>;

/** The acquire refusals that mean "this point cannot be read here" (`native/lsq.go`). */
const POINT_UNAVAILABLE = new Set([
  "acquire_point_too_old",
  "acquire_point_not_on_chain",
]);

/** The ledger state query takes at most this many items per request. */
const MAX_QUERY_ITEMS = 4_096;

/** `LedgerOutputs` over the node's transport: one acquired state per call. */
export const transportLedgerOutputs =
  (transport: Pick<L1NodeTransport, "withLedgerState">): LedgerOutputs =>
  async (at, outRefs) => {
    if (outRefs.length === 0) return [];
    try {
      return await transport.withLedgerState(
        at === "tip"
          ? "tip"
          : chainPoint(BigInt(at.slot), at.hash.toString("hex")),
        async (state) => {
          const found: LedgerUtxo[] = [];
          for (let start = 0; start < outRefs.length; start += MAX_QUERY_ITEMS)
            found.push(
              ...decodeLedgerUtxos(
                await state.query({
                  query: "utxo_by_txin",
                  txIns: outRefs
                    .slice(start, start + MAX_QUERY_ITEMS)
                    .map((outRef) => ({
                      txId: outRef.txHash.toString("hex"),
                      index: outRef.index,
                    })),
                }),
              ),
            );
          return found;
        },
      );
    } catch (error) {
      if (
        error instanceof TransportRequestError &&
        POINT_UNAVAILABLE.has(error.code)
      )
        return { kind: "point_unavailable", detail: error.code };
      throw error;
    }
  };

/**
 * A source of transaction bytes by id (§12.3 step 4): a peer operator, a
 * public indexer, the order's creator. Answers null when it does not hold
 * the transaction. Its answer is never trusted: the resolver checks the
 * body's hash against the id before reading an output.
 */
export type TxContentSource = Readonly<{
  name: string;
  fetchTx: (txHash: Buffer) => Promise<Uint8Array | null>;
}>;

/** The follower's own facts: the body of a qualifying transaction it stored. */
export const storeTxContentSource = (store: FactStore): TxContentSource => ({
  name: "follower_facts",
  fetchTx: async (txHash) => (await store.txByHash(txHash))?.bodyCbor ?? null,
});

const HEX = /^(?:[0-9a-fA-F]{2})+$/u;

/** The bytes of an HTTP answer: raw CBOR, hex text, or JSON `{ "cbor": hex }`. */
const answerBytes = (body: Buffer, contentType: string): Uint8Array => {
  if (contentType.includes("json")) {
    const parsed: unknown = JSON.parse(body.toString("utf8"));
    const cbor =
      typeof parsed === "object" && parsed !== null
        ? (parsed as Record<string, unknown>).cbor
        : undefined;
    if (typeof cbor !== "string" || !HEX.test(cbor))
      throw new Error("the JSON answer has no hex `cbor` field");
    return Buffer.from(cbor, "hex");
  }
  const text = body.toString("latin1").trim();
  if (contentType.startsWith("text/") && HEX.test(text))
    return Buffer.from(text, "hex");
  return body;
};

export type HttpTxContentSourceOptions = Readonly<{
  /** The URL, with `{txId}` replaced by the transaction id in hex. */
  urlTemplate: string;
  name?: string;
  timeoutMs?: number;
  headers?: Readonly<Record<string, string>>;
  fetch?: typeof fetch;
}>;

const DEFAULT_SOURCE_TIMEOUT_MS = 10_000;

/**
 * A content source over HTTP GET. 404 answers null; any other failure
 * throws, and the resolver records it and tries the next source.
 */
export const httpTxContentSource = (
  options: HttpTxContentSourceOptions,
): TxContentSource => {
  if (!options.urlTemplate.includes("{txId}"))
    throw new Error("a content source URL template must contain {txId}");
  const get = options.fetch ?? fetch;
  return {
    name: options.name ?? options.urlTemplate,
    fetchTx: async (txHash) => {
      const response = await get(
        options.urlTemplate.replaceAll("{txId}", txHash.toString("hex")),
        {
          headers: { ...options.headers },
          signal: AbortSignal.timeout(
            options.timeoutMs ?? DEFAULT_SOURCE_TIMEOUT_MS,
          ),
        },
      );
      if (response.status === 404) return null;
      if (!response.ok) throw new Error(`HTTP ${response.status.toString()}`);
      return answerBytes(
        Buffer.from(await response.arrayBuffer()),
        response.headers.get("content-type") ?? "",
      );
    },
  };
};

export type ResolveStep = "ledger_at_parent" | "ledger_at_tip" | "content";

export type ResolvedOutput = Readonly<{
  outRef: OutRef;
  output: OutputSummary;
  step: ResolveStep;
  /** The content source that supplied it (step 4). */
  source?: string;
}>;

export type ResolveOutcome = Readonly<{
  resolved: readonly ResolvedOutput[];
  /** Outrefs no step resolved: retry later. */
  pending: readonly OutRef[];
  /** Why steps fell through and which answers were refused. */
  notes: readonly string[];
}>;

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/**
 * Resolves `outRefs` through steps 2 to 4: the ledger at `parent` (skipped
 * when null), the ledger at the tip, then each content source in order.
 */
export const resolveOutputs = async (input: {
  readonly outRefs: readonly OutRef[];
  readonly parent: Point | null;
  readonly ledger?: LedgerOutputs;
  readonly sources?: readonly TxContentSource[];
}): Promise<ResolveOutcome> => {
  const wanted = new Map(input.outRefs.map((o) => [outRefKey(o), o]));
  const resolved: ResolvedOutput[] = [];
  const notes: string[] = [];
  const take = (entries: readonly LedgerUtxo[], step: ResolveStep): void => {
    for (const entry of entries) {
      const key = outRefKey(entry.outRef);
      if (!wanted.has(key)) continue;
      wanted.delete(key);
      resolved.push({ outRef: entry.outRef, output: entry.output, step });
    }
  };
  const ledger = input.ledger;
  const steps: readonly [Point | "tip", ResolveStep][] = [
    ...(input.parent === null
      ? []
      : [[input.parent, "ledger_at_parent"] as [Point, ResolveStep]]),
    ["tip", "ledger_at_tip"],
  ];
  if (ledger !== undefined)
    for (const [at, step] of steps) {
      if (wanted.size === 0) break;
      try {
        const answer = await ledger(at, [...wanted.values()]);
        if ("kind" in answer) notes.push(`${step}: ${answer.detail}`);
        else take(answer, step);
      } catch (error) {
        notes.push(`${step}: ${message(error)}`);
      }
    }
  const byTx = new Map<string, OutRef[]>();
  for (const outRef of wanted.values()) {
    const hash = outRef.txHash.toString("hex");
    byTx.set(hash, [...(byTx.get(hash) ?? []), outRef]);
  }
  for (const [hash, outRefs] of byTx)
    for (const source of input.sources ?? []) {
      let bytes: Uint8Array | null;
      try {
        bytes = await source.fetchTx(Buffer.from(hash, "hex"));
      } catch (error) {
        notes.push(`content ${source.name} ${hash}: ${message(error)}`);
        continue;
      }
      if (bytes === null) continue;
      let tx: ReturnType<typeof decodeTransaction>;
      try {
        tx = decodeTransaction(bytes);
      } catch (error) {
        notes.push(
          `content ${source.name} ${hash}: refused, ${message(error)}`,
        );
        continue;
      }
      if (tx.hash.toString("hex") !== hash) {
        notes.push(
          `content ${source.name} ${hash}: refused, the body hashes to ${tx.hash.toString("hex")}`,
        );
        continue;
      }
      const outputs = outRefs.map((outRef) => ({
        outRef,
        output: transactionOutputAt(tx, outRef.index),
      }));
      const absent = outputs.find(({ output }) => output === null);
      if (absent !== undefined) {
        notes.push(
          `content ${source.name} ${hash}: the transaction has no output ${absent.outRef.index.toString()}`,
        );
        continue;
      }
      for (const { outRef, output } of outputs) {
        wanted.delete(outRefKey(outRef));
        resolved.push({
          outRef,
          output: output as OutputSummary,
          step: "content",
          source: source.name,
        });
      }
      break;
    }
  return { resolved, pending: [...wanted.values()], notes };
};
