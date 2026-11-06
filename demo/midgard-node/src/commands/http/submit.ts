import { decodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { Duration, Effect, Metric, Option } from "effect";

import { NodeConfig } from "../../services/index.js";
import { publicSubmissionDiagnostic } from "../listen-public-error-presentation.js";
import { failWith500 } from "../listen-response.js";
import {
  readSubmitBodyWithProtocolLimit,
  requestMediaType,
  resolveSubmitIngressReservation,
  SUBMIT_HTTP_BODY_MAX_BYTES,
  V1_SUBMISSION_MEDIA_TYPE,
  withSubmitIngressPermit,
} from "./submission-capacity.js";
import {
  submitBodyReadDurationTimer,
  submitDurableAdmissionDurationTimer,
  submitHandlerLatencyTimer,
  submitNormalizeDurationTimer,
  submitQueueOfferFailureCounter,
  submitResponseDurationTimer,
  txCounter,
} from "./submission-metrics.js";
import {
  admitCheckedTransaction,
  checkSubmittedTransaction,
} from "./transaction-admission.js";

/**
 * A full submission queue answers 503 with this Retry-After, about one
 * cap-sized script verdict on a worker.
 */
export const SUBMIT_RETRY_AFTER_SECONDS = 5;

const SUBMIT_UNAVAILABLE = {
  status: 503,
  headers: { "retry-after": SUBMIT_RETRY_AFTER_SECONDS.toString() },
};

/**
 * `POST /submit`: validates, normalizes, and enqueues a submitted L2
 * transaction.
 */
export const postSubmitHandler = <R>(
  withMonitoring: boolean | undefined,
  wakeTxQueueProcessor: Effect.Effect<void, never, R>,
) =>
  Effect.gen(function* () {
    const startedAt = withMonitoring === true ? Date.now() : 0;
    const recordLatency = () =>
      withMonitoring === true
        ? submitHandlerLatencyTimer(
            Effect.succeed(Duration.millis(Date.now() - startedAt)),
          )
        : Effect.void;
    return yield* Effect.gen(function* () {
      const nodeConfig = yield* NodeConfig;
      const request = yield* HttpServerRequest.HttpServerRequest;

      if (
        requestMediaType(request.headers["content-type"]) !==
        V1_SUBMISSION_MEDIA_TYPE
      ) {
        yield* Effect.logInfo(
          `▫️ Invalid submit payload: expected ${V1_SUBMISSION_MEDIA_TYPE}`,
        );
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          {
            error: `V1 requests must be a canonical proof submission envelope with Content-Type ${V1_SUBMISSION_MEDIA_TYPE}`,
          },
          { status: 415 },
        );
      }

      const ingressReservation = resolveSubmitIngressReservation(
        request.headers["content-length"],
      );
      if (ingressReservation.kind === "invalid_content_length") {
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          { error: "Invalid Content-Length for V1 submission" },
          { status: 400 },
        );
      }
      if (ingressReservation.kind === "too_large") {
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          {
            error: `V1 submission exceeds the DA proof envelope (${ingressReservation.declaredBytes.toString()} > ${SUBMIT_HTTP_BODY_MAX_BYTES.toString()})`,
          },
          { status: 413 },
        );
      }

      const permittedResponse = yield* withSubmitIngressPermit({
        maxConcurrency: nodeConfig.SUBMIT_INGRESS_MAX_CONCURRENCY,
        maxInFlightBytes: nodeConfig.SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES,
        permitBytes: ingressReservation.permitBytes,
        effect: Effect.gen(function* () {
          const bodyReadStartedAt = Date.now();
          const bodyBytes = yield* Effect.either(
            readSubmitBodyWithProtocolLimit(request),
          );
          yield* submitBodyReadDurationTimer(
            Effect.succeed(Duration.millis(Date.now() - bodyReadStartedAt)),
          );
          if (bodyBytes._tag === "Left") {
            yield* Effect.logInfo(
              `▫️ Submit rejected: request body exceeded or failed the bounded HTTP read`,
            );
            yield* recordLatency();
            return HttpServerResponse.json(
              {
                error: `V1 submission exceeds or could not be read within the DA proof envelope (${SUBMIT_HTTP_BODY_MAX_BYTES.toString()} bytes)`,
              },
              { status: 413 },
            );
          }

          const requestBody = Buffer.from(bodyBytes.right);
          if (
            ingressReservation.declaredBytes !== null &&
            requestBody.length !== ingressReservation.declaredBytes
          ) {
            yield* recordLatency();
            return HttpServerResponse.json(
              { error: "V1 submission Content-Length mismatch" },
              { status: 400 },
            );
          }
          if (requestBody.length > SUBMIT_HTTP_BODY_MAX_BYTES) {
            yield* recordLatency();
            return HttpServerResponse.json(
              {
                error: `V1 submission exceeds the DA proof envelope (${requestBody.length.toString()} > ${SUBMIT_HTTP_BODY_MAX_BYTES.toString()})`,
              },
              { status: 413 },
            );
          }
          const decodedSubmission = yield* Effect.either(
            Effect.try({
              try: () => decodeMidgardProofSubmission(requestBody),
              catch: (cause) => cause,
            }),
          );
          if (decodedSubmission._tag === "Left") {
            yield* Effect.logInfo(
              `▫️ Submit rejected: malformed V1 submission envelope`,
            );
            yield* recordLatency();
            return HttpServerResponse.json(
              { error: "Invalid canonical V1 submission envelope" },
              { status: 400 },
            );
          }
          const transactionBytes = decodedSubmission.right.transactionCbor;

          const normalizeStartedAt = Date.now();
          const checked = checkSubmittedTransaction(
            transactionBytes,
            nodeConfig.MAX_SUBMIT_TX_CBOR_BYTES,
          );
          yield* submitNormalizeDurationTimer(
            Effect.succeed(Duration.millis(Date.now() - normalizeStartedAt)),
          );
          if (!checked.ok) {
            for (const line of checked.log) yield* Effect.logInfo(`▫️ ${line}`);
            yield* recordLatency();
            return HttpServerResponse.json(checked.body, {
              status: checked.status,
            });
          }
          const normalized = checked.normalized;

          // Return the durable work as a suspended effect so both ingress
          // permits are released immediately after verification. PostgreSQL's
          // direct/microbatch admission quotas remain the durable queue bound.
          return Effect.gen(function* () {
            const durableAdmissionStartedAt = Date.now();
            const admitted = yield* admitCheckedTransaction({
              normalized,
              submitSource: normalized.source,
              wakeTxQueueProcessor,
            });
            yield* submitDurableAdmissionDurationTimer(
              Effect.succeed(
                Duration.millis(Date.now() - durableAdmissionStartedAt),
              ),
            );

            Effect.runSync(Metric.increment(txCounter));
            yield* recordLatency();
            const responseStartedAt = Date.now();
            const response = yield* HttpServerResponse.json(
              {
                txId: normalized.txIdHex,
                status: admitted.entry.status,
                firstSeenAt: admitted.entry.first_seen_at.toISOString(),
                lastSeenAt: admitted.entry.last_seen_at.toISOString(),
                duplicate: admitted.kind === "duplicate",
              },
              { status: admitted.kind === "new" ? 202 : 200 },
            );
            yield* submitResponseDurationTimer(
              Effect.succeed(Duration.millis(Date.now() - responseStartedAt)),
            );
            return response;
          });
        }),
      });
      if (Option.isNone(permittedResponse)) {
        yield* Metric.increment(submitQueueOfferFailureCounter);
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          {
            error: "Submit ingress capacity is full; retry later",
          },
          SUBMIT_UNAVAILABLE,
        );
      }
      return yield* permittedResponse.value;
    }).pipe(
      Effect.catchTag("TxAdmissionConflictError", (e) =>
        Effect.gen(function* () {
          yield* recordLatency();
          return yield* HttpServerResponse.json(
            {
              error: "E_TX_ID_BYTES_CONFLICT",
              message: publicSubmissionDiagnostic(
                e.message,
                "E_TX_ID_BYTES_CONFLICT",
              ),
              txId: e.txIdHex,
            },
            { status: 409 },
          );
        }),
      ),
      Effect.catchTag("TxAdmissionBacklogFullError", (e) =>
        Effect.gen(function* () {
          yield* Metric.increment(submitQueueOfferFailureCounter);
          yield* recordLatency();
          if (e.unit === "bytes") {
            return yield* HttpServerResponse.json(
              {
                error: "Durable submission admission byte backlog is full",
                backlogBytes: e.backlog.toString(),
                maxBacklogBytes: e.maxBacklog.toString(),
              },
              SUBMIT_UNAVAILABLE,
            );
          }
          return yield* HttpServerResponse.json(
            {
              error: "Durable submission admission backlog is full",
              backlog: e.backlog.toString(),
              maxBacklog: e.maxBacklog.toString(),
            },
            SUBMIT_UNAVAILABLE,
          );
        }),
      ),
      Effect.catchTag("AdmissionWriterShutdownError", (e) =>
        Effect.gen(function* () {
          yield* Metric.increment(submitQueueOfferFailureCounter);
          yield* recordLatency();
          return yield* HttpServerResponse.json(
            {
              error: "Durable submission admission writer is unavailable",
              message: publicSubmissionDiagnostic(
                e.message,
                "E_ADMISSION_WRITER_UNAVAILABLE",
              ),
            },
            SUBMIT_UNAVAILABLE,
          );
        }),
      ),
      Effect.catchTag("DatabaseError", (e) =>
        Effect.gen(function* () {
          yield* recordLatency();
          return yield* failWith500(
            "POST",
            "submit",
            e.cause,
            "durable transaction admission failed",
          );
        }),
      ),
      Effect.catchTag("HttpBodyError", (e) =>
        failWith500("POST", "submit", e, "▫️ L2 transaction failed"),
      ),
    );
  });
