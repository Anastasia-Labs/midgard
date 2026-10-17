import { Cause, HashMap, List, Logger } from "effect";

import { PublicHttpConfigError } from "./listen-public-http.js";

const REDACTED = "[redacted]";
const sensitiveNames =
  /seed|password|secret|private|credential|admin.*key|token|body|cbor|headers|query|url|provider|response|address|payload|request/iu;
const knownTags: ReadonlySet<string> = new Set([
  "ConfigError",
  "DatabaseError",
  "ValidationWorkerError",
  "TxSubmitError",
  "TxConfirmError",
  "StateQueueError",
  "RequestError",
  "HttpBodyError",
  "StartupStepFailedError",
  "TransientBudgetExhaustedError",
]);

/** Bounded rendering only: preserve the original Effect failure, interruption and exit status. */
export const createPublicLogRedactor = (env: NodeJS.ProcessEnv) => {
  const secrets = Object.entries(env)
    .filter(
      ([name, value]) =>
        sensitiveNames.test(name) && value !== undefined && value.length > 0,
    )
    .flatMap(([, value]) => [value!, value!.trim()])
    .filter((value) => value.length > 0)
    .sort((a, b) => b.length - a.length);
  const text = (value: string): string => {
    let result = value;
    for (const secret of secrets) result = result.split(secret).join(REDACTED);
    return result
      .slice(0, 8_192)
      .replace(/(?:https?|wss?):\/\/[^\s"']+/giu, "[redacted-url]")
      .replace(
        /\b(?:addr|stake)(?:_test)?1[a-z0-9]+\b/giu,
        "[redacted-address]",
      )
      .replace(/\b[0-9a-f]{16,}\b/giu, "[redacted-hex]");
  };
  const value = (input: unknown, depth = 0, key = ""): unknown => {
    if (typeof input === "string")
      return /^(?:header_hash|headerHash|block_hash|blockHash)$/u.test(key) &&
        /^(?:[0-9a-f]{56}|[0-9a-f]{64})$/u.test(input) &&
        !secrets.includes(input)
        ? input
        : text(input);
    if (
      typeof input === "number" ||
      typeof input === "boolean" ||
      input === null ||
      input === undefined
    )
      return input;
    if (typeof input === "bigint") return input.toString(10);
    if (input instanceof PublicHttpConfigError)
      return { error_code: "E_PUBLIC_HTTP_CONFIG", reason: text(input.reason) };
    if (input instanceof Error) {
      const descriptor = Object.getOwnPropertyDescriptor(input, "_tag");
      return descriptor !== undefined &&
        "value" in descriptor &&
        typeof descriptor.value === "string" &&
        knownTags.has(descriptor.value)
        ? { failure_type: descriptor.value }
        : REDACTED;
    }
    if (depth > 3 || input instanceof Uint8Array) return REDACTED;
    if (Array.isArray(input))
      return input.slice(0, 32).map((entry) => value(entry, depth + 1));
    if (typeof input !== "object") return REDACTED;
    // Do not invoke getters or toJSON while formatting untrusted diagnostic objects.
    return Object.fromEntries(
      Object.entries(Object.getOwnPropertyDescriptors(input))
        .slice(0, 32)
        .map(([key, descriptor]) => [
          text(key),
          sensitiveNames.test(key) || !("value" in descriptor)
            ? REDACTED
            : value(descriptor.value, depth + 1, key),
        ]),
    );
  };
  return { text, value };
};

// Codes are declared by local producers; upstream text is never a code authority.
const knownCodes: ReadonlySet<string> = new Set([
  "E_HTTP_CURSOR_INVALID",
  "E_HTTP_CURSOR_STALE",
  "E_HTTP_INTERNAL",
  "E_HTTP_READ_UNAVAILABLE",
  "E_HTTP_READ_DATA_LIMIT",
  "E_INVALID_CANONICAL_TX",
  "E_INVALID_NATIVE_TX",
  "E_TX_ID_BYTES_CONFLICT",
  "E_ADMISSION_WRITER_UNAVAILABLE",
  "E_PUBLIC_HTTP_CONFIG",
  "E_PUBLIC_READ_MIGRATING",
  "E_READINESS_DATABASE_UNAVAILABLE",
  "E_READINESS_DIAGNOSTIC_REDACTED",
  "E_READINESS_L1_PROVIDER_UNAVAILABLE",
  "E_READINESS_NATIVE_OWNER_UNAVAILABLE",
  "E_STATE_QUEUE_UNAVAILABLE",
  "E_TX_EVALUATION_FAILED",
  "E_TX_EVALUATION_UNAVAILABLE",
  "E_WITHDRAWAL_READ_AUTHORITY_CONFLICT",
  "E_WITHDRAWAL_READ_BACKFILL_BUDGET",
  "E_WITHDRAWAL_READ_BACKFILL_INPUT_LIMIT",
  "E_MERGE_BLOCK_TX_DECODE_FAILED",
  "E_MERGE_MISSING_BLOCK_TXS",
  "E_MERGE_LAYOUT_DERIVATION_FAILED",
  "E_MERGE_REDEEMER_INDEX_MISMATCH",
  "E_MERGE_UPLC_EVAL_FAILED",
  "E_MERGE_PRODUCER_UNAVAILABLE",
]);
const knownFields: ReadonlySet<string> = new Set([
  "error_code",
  "code",
  "reason",
  "failure_type",
  "failure_types",
  "count",
  "slot",
  "height",
  "generation",
  "ageMs",
  "durationMs",
  "observedAtMs",
  "workerCount",
  "header_hash",
  "headerHash",
  "block_hash",
  "blockHash",
]);
const diagnosticKey = (key: string): string =>
  knownFields.has(key) ? key : "[redacted-key]";
const knownReasons: ReadonlySet<string> = new Set([
  "database_unreachable",
  "database_connection_failed",
  "l1_follower_view_stale",
  "l1_driver_recompute_pending",
  "l1_follower_view_unapplied",
  "operator_membership",
  "local_finalization_pending",
  "reset_in_progress",
  "retained_da_payload_missing",
  "validation_capacity_unavailable",
]);
const safeDiagnostic = (input: unknown, key = "", depth = 0): unknown => {
  if (typeof input === "string") {
    if (
      /^(?:header_hash|headerHash|block_hash|blockHash)$/u.test(key) &&
      /^(?:[0-9a-f]{56}|[0-9a-f]{64})$/u.test(input)
    )
      return input;
    if (
      knownCodes.has(input) ||
      knownReasons.has(input) ||
      (key === "failure_type" && knownTags.has(input))
    )
      return input;
    return REDACTED;
  }
  if (
    typeof input === "number" ||
    typeof input === "boolean" ||
    input === null ||
    input === undefined
  )
    return input;
  if (typeof input === "bigint") return input.toString(10);
  if (depth > 3 || input instanceof Uint8Array || typeof input !== "object")
    return REDACTED;
  if (Array.isArray(input))
    return input
      .slice(0, 32)
      .map((entry) => safeDiagnostic(entry, key, depth + 1));
  if (input instanceof PublicHttpConfigError)
    return { error_code: "E_PUBLIC_HTTP_CONFIG" };
  return Object.fromEntries(
    Object.entries(Object.getOwnPropertyDescriptors(input))
      .slice(0, 32)
      .map(([name, descriptor]) => [
        diagnosticKey(name),
        !knownFields.has(name) ||
        sensitiveNames.test(name) ||
        !("value" in descriptor)
          ? REDACTED
          : safeDiagnostic(descriptor.value, name, depth + 1),
      ]),
  );
};
/** This changes Logger output only; it never catches, replaces or retries a runtime failure. */
export const redactPublicLogger = <O>(
  logger: Logger.Logger<unknown, O>,
  env: NodeJS.ProcessEnv,
): Logger.Logger<unknown, O> => {
  return Logger.mapInputOptions(logger, (options) => {
    const redact = createPublicLogRedactor(env);
    const tags = [
      ...Cause.failures(options.cause),
      ...Cause.defects(options.cause),
    ].flatMap((error) => {
      if (typeof error !== "object" || error === null) return [];
      const descriptor = Object.getOwnPropertyDescriptor(error, "_tag");
      return descriptor !== undefined &&
        "value" in descriptor &&
        typeof descriptor.value === "string" &&
        knownTags.has(descriptor.value)
        ? [descriptor.value]
        : [];
    });
    return {
      ...options,
      message: safeDiagnostic(redact.value(options.message)),
      cause: Cause.empty,
      annotations: HashMap.fromIterable(
        [...options.annotations].map(
          ([key, entry]) =>
            [
              diagnosticKey(key),
              !knownFields.has(key) || sensitiveNames.test(key)
                ? REDACTED
                : safeDiagnostic(redact.value(entry, 0, key), key),
            ] as const,
        ),
      ).pipe(
        HashMap.set<string, unknown>(
          "failure_types",
          tags.length === 0 ? "unknown" : tags.join(","),
        ),
      ),
      spans: List.map(options.spans, (span) => ({
        ...span,
        label: "runtime",
      })),
    };
  });
};
