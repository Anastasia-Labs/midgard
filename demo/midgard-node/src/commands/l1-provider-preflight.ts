/**
 * The node's L1 preflight: the local node transport's readiness and one
 * submit-slot snapshot from the node's ledger tip. Its one source is the local
 * node (`l1_node`); a failure is named by the transport's unready reason
 * (`transport:<reason>`) or by the read's (`l1_node_behind`,
 * `<source>:<reason>` of a transient provider error, else `l1_read_failed`).
 */
import type { TransportReadiness } from "@al-ft/l1-node-transport";
import { type SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import type { Network } from "@lucid-evolution/lucid";

import { submitSlotEvidence } from "../l1-provider-view.js";
import { l1ReadFailureKind } from "../services/l1-provider.js";

export type L1ProviderSource = "l1_node";

export type L1ProviderPreflightConfig = Readonly<{
  network: Network;
  /** The local node's socket, for the report. */
  endpoint: string;
  timeoutMs: number;
  transportReadiness: () => TransportReadiness;
  readSubmitSlotSnapshot: () => Promise<SubmitSlotSnapshot>;
}>;

export type L1ProviderHealth = {
  readonly source: L1ProviderSource;
  readonly endpoint: string;
  readonly healthy: boolean;
  readonly degraded: boolean;
  readonly latencyMs?: number;
  readonly failureKind?: string;
  readonly bodySummary?: string;
  readonly localLedgerSlot?: SubmitSlotSnapshot;
};

export type L1ProviderPreflightReport = {
  readonly ok: boolean;
  readonly degraded: boolean;
  readonly route: {
    readonly primary: L1ProviderSource;
    readonly network: Network;
  };
  readonly checkedAtMs: number;
  readonly healthySources: readonly L1ProviderSource[];
  readonly unhealthySources: readonly L1ProviderSource[];
  readonly sources: readonly L1ProviderHealth[];
};

const SUMMARY_MAX_CHARS = 240;

/**
 * One bounded line for a thrown value: `name: message`, with its cause's.
 * `JSON.stringify` of an Error is `{}`, so an Error is never serialized.
 */
export const summarizeL1Failure = (value: unknown): string => {
  const describe = (thrown: unknown): string =>
    thrown instanceof Error
      ? `${thrown.name}: ${thrown.message}`
      : String(thrown);
  const nested = value instanceof Error ? value.cause : undefined;
  const text = (
    nested === undefined
      ? describe(value)
      : `${describe(value)}; cause=${describe(nested)}`
  )
    .replace(/\s+/g, " ")
    .trim();
  return text.length <= SUMMARY_MAX_CHARS
    ? text
    : `${text.slice(0, SUMMARY_MAX_CHARS)}...`;
};

const withTimeout = async <T>(
  run: () => Promise<T>,
  timeoutMs: number,
  signal: AbortSignal | undefined,
): Promise<T> => {
  let timer: ReturnType<typeof setTimeout> | undefined;
  let onAbort: (() => void) | undefined;
  try {
    return await Promise.race([
      run(),
      new Promise<never>((_, reject) => {
        timer = setTimeout(
          () =>
            reject(new Error(`the L1 read exceeded ${timeoutMs.toString()}ms`)),
          timeoutMs,
        );
        if (signal !== undefined) {
          onAbort = () => reject(new Error("the L1 read was aborted"));
          if (signal.aborted) onAbort();
          else signal.addEventListener("abort", onAbort, { once: true });
        }
      }),
    ]);
  } finally {
    if (timer !== undefined) clearTimeout(timer);
    if (onAbort !== undefined) signal?.removeEventListener("abort", onAbort);
  }
};

const checkLocalNode = async (
  config: L1ProviderPreflightConfig,
  signal: AbortSignal | undefined,
): Promise<L1ProviderHealth> => {
  const base = {
    source: "l1_node",
    endpoint: config.endpoint,
    degraded: false,
  } as const;
  const startedAt = Date.now();
  try {
    const snapshot = await withTimeout(
      config.readSubmitSlotSnapshot,
      config.timeoutMs,
      signal,
    );
    return {
      ...base,
      healthy: true,
      latencyMs: Date.now() - startedAt,
      localLedgerSlot: snapshot,
      bodySummary: submitSlotEvidence(snapshot),
    };
  } catch (cause) {
    // A read that failed while the transport is not ready is named by the
    // transport's reason: the node or its sidecar is not reachable.
    const transport = config.transportReadiness();
    return {
      ...base,
      healthy: false,
      latencyMs: Date.now() - startedAt,
      ...(transport.ready
        ? {
            failureKind: l1ReadFailureKind(cause),
            bodySummary: summarizeL1Failure(cause),
          }
        : {
            failureKind: `transport:${transport.reason}`,
            bodySummary: summarizeL1Failure(transport.detail),
          }),
    };
  }
};

export const runL1ProviderPreflight = async ({
  config,
  nowMs = Date.now(),
  signal,
}: {
  readonly config: L1ProviderPreflightConfig;
  readonly nowMs?: number;
  readonly signal?: AbortSignal;
}): Promise<L1ProviderPreflightReport> => {
  const sources = [await checkLocalNode(config, signal)];
  const healthySources = sources
    .filter((source) => source.healthy)
    .map((source) => source.source);
  const unhealthySources = sources
    .filter((source) => !source.healthy)
    .map((source) => source.source);
  return {
    ok: healthySources.length > 0,
    degraded: false,
    route: { primary: "l1_node", network: config.network },
    checkedAtMs: nowMs,
    healthySources,
    unhealthySources,
    sources,
  };
};
