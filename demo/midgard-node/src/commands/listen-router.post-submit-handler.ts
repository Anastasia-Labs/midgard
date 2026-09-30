import {
  decodeMidgardCekProgramMaterialSidecar,
  decodeMidgardProofSubmission,
  encodeMidgardCekProgramMaterialSidecar,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusTxCbor } from "@al-ft/midgard-core/consensus-validation";
import { collectMidgardAttachedProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql/SqlClient";
import { Cause, Duration, Effect, Exit, Metric, Option } from "effect";

import { TxAdmissionsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import {
  admissionFailureDefinitelyDidNotInsert,
  commitAdmissionBacklogSlot,
  releaseAdmissionBacklogSlot,
  reserveAdmissionBacklogSlot,
} from "../fibers/index.js";
import {
  AdmissionWriter,
  type AdmissionWriterShutdownError,
} from "../services/index.js";
import { NodeConfig } from "../services/index.js";
import { failWith500 } from "./listen-response.js";
import {
  readSubmitBodyWithProtocolLimit,
  requestMediaType,
  resolveSubmitIngressReservation,
  SUBMIT_HTTP_BODY_MAX_BYTES,
  submitBodyReadDurationTimer,
  submitDurableAdmissionDurationTimer,
  submitHandlerLatencyTimer,
  submitNormalizeDurationTimer,
  submitQueueOfferFailureCounter,
  submitResponseDurationTimer,
  txCounter,
  V1_SUBMISSION_MEDIA_TYPE,
  withSubmitIngressPermit,
} from "./listen-router.get-tx-handler.js";
import {
  normalizeSubmitTxCanonicalCborToNative,
  validateSubmitTxCanonicalCbor,
} from "./listen-utils.js";

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
          const programMaterial = decodedSubmission.right.programMaterial;
          const programMaterialSidecarCbor =
            encodeMidgardCekProgramMaterialSidecar(programMaterial);
          // Re-decode the independently stored representation at the admission
          // boundary; database replay must never depend on the HTTP wrapper.
          decodeMidgardCekProgramMaterialSidecar(programMaterialSidecarCbor);

          const normalizeStartedAt = Date.now();
          const validation = validateSubmitTxCanonicalCbor(
            transactionBytes,
            Math.min(
              nodeConfig.MAX_SUBMIT_TX_CBOR_BYTES,
              MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes,
            ),
          );
          if (!validation.ok) {
            yield* submitNormalizeDurationTimer(
              Effect.succeed(Duration.millis(Date.now() - normalizeStartedAt)),
            );
            yield* Effect.logInfo(`▫️ Submit rejected: ${validation.error}`);
            yield* recordLatency();
            return HttpServerResponse.json(
              { error: validation.error },
              { status: validation.status },
            );
          }

          const normalized = normalizeSubmitTxCanonicalCborToNative(
            validation.txCanonicalCbor,
          );
          if (!normalized.ok) {
            yield* submitNormalizeDurationTimer(
              Effect.succeed(Duration.millis(Date.now() - normalizeStartedAt)),
            );
            yield* Effect.logInfo(`▫️ ${normalized.error}`);
            yield* Effect.logInfo(`▫️ ${normalized.detail}`);
            yield* recordLatency();
            return HttpServerResponse.json(
              { error: normalized.error },
              { status: 400 },
            );
          }
          const proofViolation = validateMidgardConsensusTxCbor(
            normalized.txCanonicalCbor,
          );
          if (proofViolation !== null) {
            yield* submitNormalizeDurationTimer(
              Effect.succeed(Duration.millis(Date.now() - normalizeStartedAt)),
            );
            yield* recordLatency();
            return HttpServerResponse.json(
              {
                error: proofViolation.code,
                feature: proofViolation.featureId,
                detail: proofViolation.detail,
              },
              { status: 400 },
            );
          }
          const materialValidation = yield* Effect.either(
            Effect.try({
              try: () => {
                const tx = decodeMidgardNativeTxFullFromCanonicalCbor(
                  normalized.txCanonicalCbor,
                );
                const envelopes = collectMidgardAttachedProgramEnvelopes(tx);
                const hasUnresolvedReferenceInputs =
                  decodeMidgardNativeByteListPreimage(
                    tx.body.referenceInputsPreimageCbor,
                    "reference_inputs_preimage",
                  ).length > 0;
                // Reference-input program envelopes are ledger-state dependent and
                // become authoritative in Phase B. The one bundle traversal still
                // proves every attached envelope and preserves envelope position.
                verifyMidgardCekProgramMaterialBundle(
                  envelopes,
                  programMaterial,
                  hasUnresolvedReferenceInputs
                    ? { allowUnreachable: true }
                    : undefined,
                );
              },
              catch: (cause) => cause,
            }),
          );
          if (materialValidation._tag === "Left") {
            yield* submitNormalizeDurationTimer(
              Effect.succeed(Duration.millis(Date.now() - normalizeStartedAt)),
            );
            yield* recordLatency();
            return HttpServerResponse.json(
              {
                error: "E_CEK_PROGRAM_MATERIAL",
                detail:
                  "V1 program material does not cover every attached program envelope",
              },
              { status: 400 },
            );
          }
          yield* submitNormalizeDurationTimer(
            Effect.succeed(Duration.millis(Date.now() - normalizeStartedAt)),
          );

          // Return the durable work as a suspended effect so both ingress
          // permits are released immediately after verification. PostgreSQL's
          // direct/microbatch admission quotas remain the durable queue bound.
          return Effect.gen(function* () {
            const durableAdmissionStartedAt = Date.now();
            const reservation = yield* reserveAdmissionBacklogSlot(
              nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
            );
            const admissionWriter = yield* AdmissionWriter;
            const admissionEffect: Effect.Effect<
              TxAdmissionsDB.AdmitResult,
              | DatabaseError
              | TxAdmissionsDB.TxAdmissionConflictError
              | TxAdmissionsDB.TxAdmissionBacklogFullError
              | AdmissionWriterShutdownError,
              SqlClient
            > = reservation.reserved
              ? admissionWriter.admitReserved({
                  txId: normalized.txId,
                  txCanonicalCbor: normalized.txCanonicalCbor,
                  programMaterialSidecarCbor,
                  submitSource: normalized.source,
                  maxBacklogBytes:
                    nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG_BYTES,
                })
              : TxAdmissionsDB.admit({
                  txId: normalized.txId,
                  txCanonicalCbor: normalized.txCanonicalCbor,
                  programMaterialSidecarCbor,
                  submitSource: normalized.source,
                  currentBacklog: reservation.currentBacklog,
                  maxBacklog: nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
                  maxBacklogBytes:
                    nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG_BYTES,
                });
            const admitted = yield* admissionEffect.pipe(
              Effect.onExit((exit) => {
                if (!reservation.reserved) return Effect.void;
                if (Exit.isSuccess(exit)) {
                  return exit.value.kind === "new"
                    ? commitAdmissionBacklogSlot
                    : releaseAdmissionBacklogSlot;
                }
                const failure = Option.getOrUndefined(
                  Cause.failureOption(exit.cause),
                );
                // Conflict/backlog errors prove no row was inserted. A SqlError,
                // interruption, or defect can be observed after PostgreSQL committed,
                // so retain the slot conservatively until the next live-count refresh.
                return admissionFailureDefinitelyDidNotInsert(failure)
                  ? releaseAdmissionBacklogSlot
                  : commitAdmissionBacklogSlot;
              }),
            );
            yield* submitDurableAdmissionDurationTimer(
              Effect.succeed(
                Duration.millis(Date.now() - durableAdmissionStartedAt),
              ),
            );
            if (admitted.kind === "new") {
              if (!reservation.reserved) {
                return yield* Effect.dieMessage(
                  "Durable admission inserted without a reserved backlog slot",
                );
              }
              yield* wakeTxQueueProcessor;
            }

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
          { status: 503 },
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
              message: e.message,
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
              { status: 503 },
            );
          }
          return yield* HttpServerResponse.json(
            {
              error: "Durable submission admission backlog is full",
              backlog: e.backlog.toString(),
              maxBacklog: e.maxBacklog.toString(),
            },
            { status: 503 },
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
              message: e.message,
            },
            { status: 503 },
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
