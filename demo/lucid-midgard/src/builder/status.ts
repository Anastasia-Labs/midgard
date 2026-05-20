import { isSubmitAdmissionStatus } from "../core/types.js";
import {
  BuilderInvariantError,
  ProviderError,
  ProviderPayloadError,
} from "../core/errors.js";
import { normalizeTxHash } from "../core/out-ref.js";
import type {
  MidgardProtocolParameters,
  SubmitTxResult,
  TxStatus,
} from "../core/types.js";
import type { MidgardProvider } from "../provider.js";
import type { AwaitTxOptions, TxStatusKind } from "../builder.js";
import { validateProtocolInfo } from "./context.js";

type SubmitContext = {
  readonly provider?: MidgardProvider;
  readonly maxSubmitTxCborBytes?: number;
};

const DEFAULT_AWAIT_TX_STATUSES: readonly TxStatusKind[] = [
  "accepted",
  "committed",
  "rejected",
];
const TX_STATUS_KINDS: ReadonlySet<string> = new Set([
  "queued",
  "validating",
  "accepted",
  "committed",
  "pending_commit",
  "awaiting_local_recovery",
  "not_found",
  "rejected",
]);
const DEFAULT_POLL_INTERVAL_MS = 1_000;
const DEFAULT_AWAIT_TIMEOUT_MS = 60_000;

const normalizeProviderTxIdHex = (txId: string, endpoint: string): string => {
  try {
    return normalizeTxHash(txId);
  } catch {
    throw new ProviderPayloadError(
      endpoint,
      "Provider returned invalid tx id",
      txId,
    );
  }
};

export const assertSubmitSizeWithinLimit = (
  txBytes: Uint8Array,
  maxSubmitTxCborBytes: number | undefined,
): void => {
  if (maxSubmitTxCborBytes === undefined) {
    return;
  }
  if (
    !Number.isSafeInteger(maxSubmitTxCborBytes) ||
    maxSubmitTxCborBytes <= 0
  ) {
    throw new ProviderPayloadError(
      "/protocol-info",
      "maxSubmitTxCborBytes must be a positive safe integer",
    );
  }
  if (txBytes.length > maxSubmitTxCborBytes) {
    throw new ProviderPayloadError(
      "/submit",
      "Midgard native transaction exceeds provider submit size limit",
      `size=${txBytes.length.toString()} max=${maxSubmitTxCborBytes.toString()}`,
    );
  }
};

export const resolveSubmitSizeLimit = async ({
  provider,
  providerParams,
  context,
}: {
  readonly provider: MidgardProvider;
  readonly providerParams: MidgardProtocolParameters;
  readonly context: SubmitContext | undefined;
}): Promise<number | undefined> => {
  if (providerParams.maxSubmitTxCborBytes !== undefined) {
    return providerParams.maxSubmitTxCborBytes;
  }
  if (
    context?.provider === provider &&
    context.maxSubmitTxCborBytes !== undefined
  ) {
    return context.maxSubmitTxCborBytes;
  }
  return validateProtocolInfo(await provider.getProtocolInfo()).submissionLimits
    .maxSubmitTxCborBytes;
};

export const assertTxStatusMatches = (
  requestedTxId: string,
  status: TxStatus,
): TxStatus => {
  if (typeof status !== "object" || status === null) {
    throw new ProviderPayloadError(
      "/tx-status",
      "Provider returned malformed transaction status",
      status,
    );
  }
  const rawStatus = status as { readonly kind?: unknown };
  if (
    typeof rawStatus.kind !== "string" ||
    !TX_STATUS_KINDS.has(rawStatus.kind)
  ) {
    throw new ProviderPayloadError(
      "/tx-status",
      "Provider returned unsupported transaction status",
      String(rawStatus.kind),
    );
  }
  if (
    rawStatus.kind === "rejected" &&
    (typeof (status as { readonly code?: unknown }).code !== "string" ||
      ((status as { readonly detail?: unknown }).detail !== null &&
        typeof (status as { readonly detail?: unknown }).detail !== "string"))
  ) {
    throw new ProviderPayloadError(
      "/tx-status",
      "Provider returned malformed rejection status",
    );
  }
  const normalizedRequested = normalizeTxHash(requestedTxId);
  const normalizedActual = normalizeProviderTxIdHex(status.txId, "/tx-status");
  if (normalizedActual !== normalizedRequested) {
    throw new ProviderPayloadError(
      "/tx-status",
      "Provider returned status for a different tx id",
      `expected=${normalizedRequested} actual=${status.txId}`,
    );
  }
  return status.txId === normalizedActual
    ? status
    : ({ ...status, txId: normalizedActual } as TxStatus);
};

export const assertSubmitAdmissionMatches = (
  requestedTxId: string,
  admission: SubmitTxResult,
): SubmitTxResult => {
  if (typeof admission !== "object" || admission === null) {
    throw new ProviderPayloadError(
      "/submit",
      "Provider returned malformed submit admission",
      admission,
    );
  }
  const raw = admission as {
    readonly txId?: unknown;
    readonly status?: unknown;
    readonly httpStatus?: unknown;
    readonly firstSeenAt?: unknown;
    readonly lastSeenAt?: unknown;
    readonly duplicate?: unknown;
  };
  if (typeof raw.txId !== "string") {
    throw new ProviderPayloadError(
      "/submit",
      "Submit response tx id must be a string",
    );
  }
  const normalizedRequested = normalizeTxHash(requestedTxId);
  const normalizedActual = normalizeProviderTxIdHex(raw.txId, "/submit");
  if (normalizedActual !== normalizedRequested) {
    throw new ProviderPayloadError(
      "/submit",
      "Submit response returned a different tx id than the submitted tx",
      `expected=${normalizedRequested} actual=${raw.txId}`,
    );
  }
  if (typeof raw.status !== "string" || !isSubmitAdmissionStatus(raw.status)) {
    throw new ProviderPayloadError(
      "/submit",
      "Submit response status is not a supported durable admission status",
      String(raw.status),
    );
  }
  if (raw.httpStatus !== 200 && raw.httpStatus !== 202) {
    throw new ProviderPayloadError(
      "/submit",
      "Submit response httpStatus must be 200 or 202",
      String(raw.httpStatus),
    );
  }
  if (raw.firstSeenAt !== undefined && typeof raw.firstSeenAt !== "string") {
    throw new ProviderPayloadError(
      "/submit",
      "Submit response firstSeenAt must be a string",
    );
  }
  if (raw.lastSeenAt !== undefined && typeof raw.lastSeenAt !== "string") {
    throw new ProviderPayloadError(
      "/submit",
      "Submit response lastSeenAt must be a string",
    );
  }
  if (typeof raw.duplicate !== "boolean") {
    throw new ProviderPayloadError(
      "/submit",
      "Submit response duplicate flag must be boolean",
    );
  }
  if (raw.httpStatus === 202 && raw.duplicate) {
    throw new ProviderPayloadError(
      "/submit",
      "HTTP 202 submit admission must not be marked duplicate",
    );
  }
  if (raw.httpStatus === 200 && !raw.duplicate) {
    throw new ProviderPayloadError(
      "/submit",
      "HTTP 200 submit admission must be marked duplicate",
    );
  }
  if (raw.httpStatus === 202 && raw.status !== "queued") {
    throw new ProviderPayloadError(
      "/submit",
      "HTTP 202 submit admission must start queued",
    );
  }
  return {
    ...admission,
    txId: normalizedActual,
    status: raw.status,
    httpStatus: raw.httpStatus,
    firstSeenAt: raw.firstSeenAt,
    lastSeenAt: raw.lastSeenAt,
    duplicate: raw.duplicate,
  };
};

const normalizePositiveSafeInteger = (
  value: number | undefined,
  fieldName: string,
  defaultValue: number,
): number => {
  if (value === undefined) {
    return defaultValue;
  }
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new BuilderInvariantError(
      `${fieldName} must be a positive safe integer`,
      `${fieldName}=${String(value)}`,
    );
  }
  return value;
};

const normalizeAwaitTargets = (
  until: AwaitTxOptions["until"],
): ReadonlySet<TxStatusKind> => {
  const targets = Array.isArray(until)
    ? until
    : until === undefined
      ? DEFAULT_AWAIT_TX_STATUSES
      : [until];
  if (targets.length === 0) {
    throw new BuilderInvariantError(
      "await status target set must not be empty",
    );
  }
  return new Set(targets);
};

const throwIfPollingAborted = (signal: AbortSignal | undefined): void => {
  if (signal?.aborted) {
    throw new ProviderError({
      endpoint: "/tx-status",
      message: "Transaction status polling aborted",
      detail:
        signal.reason === undefined
          ? "aborted"
          : signal.reason instanceof Error
            ? signal.reason.message
            : String(signal.reason),
      retryable: false,
    });
  }
};

const waitForPollInterval = (
  milliseconds: number,
  signal: AbortSignal | undefined,
): Promise<void> =>
  new Promise((resolve, reject) => {
    throwIfPollingAborted(signal);
    let timeout: ReturnType<typeof setTimeout> | undefined;
    const onAbort = () => {
      if (timeout !== undefined) {
        clearTimeout(timeout);
      }
      reject(
        new ProviderError({
          endpoint: "/tx-status",
          message: "Transaction status polling aborted",
          detail:
            signal?.reason === undefined
              ? "aborted"
              : signal.reason instanceof Error
                ? signal.reason.message
                : String(signal.reason),
          retryable: false,
        }),
      );
    };
    timeout = setTimeout(() => {
      signal?.removeEventListener("abort", onAbort);
      resolve();
    }, milliseconds);
    signal?.addEventListener("abort", onAbort, { once: true });
  });

export const resolveProvider = (
  provider: MidgardProvider | undefined,
  context: SubmitContext | undefined,
): MidgardProvider => {
  const resolved = provider ?? context?.provider;
  if (resolved === undefined) {
    throw new BuilderInvariantError("No Midgard provider available");
  }
  return resolved;
};

export const pollTxStatus = async (
  provider: MidgardProvider,
  txId: string,
  options: AwaitTxOptions = {},
): Promise<TxStatus> => {
  const normalizedTxId = normalizeTxHash(txId);
  const targets = normalizeAwaitTargets(options.until);
  const pollIntervalMs = normalizePositiveSafeInteger(
    options.pollIntervalMs,
    "pollIntervalMs",
    DEFAULT_POLL_INTERVAL_MS,
  );
  const timeoutMs = normalizePositiveSafeInteger(
    options.timeoutMs,
    "timeoutMs",
    DEFAULT_AWAIT_TIMEOUT_MS,
  );
  const deadline = Date.now() + timeoutMs;
  let lastStatus: TxStatus | undefined;

  for (;;) {
    throwIfPollingAborted(options.signal);
    lastStatus = assertTxStatusMatches(
      normalizedTxId,
      await provider.getTxStatus(normalizedTxId),
    );
    if (targets.has(lastStatus.kind)) {
      return lastStatus;
    }
    if (Date.now() >= deadline) {
      throw new ProviderError({
        endpoint: "/tx-status",
        message: "Timed out waiting for transaction status",
        detail: `tx_id=${normalizedTxId} last_status=${lastStatus.kind}`,
        retryable: true,
      });
    }
    await waitForPollInterval(
      Math.min(pollIntervalMs, Math.max(1, deadline - Date.now())),
      options.signal,
    );
  }
};
