/**
 * `POST /submit` as a raw Node request handler, outside the Effect router
 * (design D3). It runs the ingress checks every route runs
 * (`admitHttpRequest`), reserves a slot in the submit check pool, reads the
 * body, has a pool worker check it (design D4) and hands the checked tx to
 * the admitter (`makeCheckedTransactionsAdmitter`), a plain call that
 * answers from memory. The main thread neither decodes nor hashes the
 * body. Every other route stays on the Effect stack.
 *
 * A refusal costs a counter increment by reason; its log line is sampled to
 * the first per reason per second. The handler records one latency
 * histogram and runs no Effect fiber on the way to its answer.
 */
import type { IncomingMessage, ServerResponse } from "node:http";

import { Effect, Either, Option, Runtime } from "effect";

import { AdmissionWriter, Globals, NodeConfig } from "../../services/index.js";
import type {
  SubmitCheckOutcome,
  SubmitCheckPool,
} from "../../services/submit-check-pool.js";
import {
  admitHttpRequest,
  type createHttpIngress,
  httpIngressFor,
  type HttpRequestHeaders,
  keepsConnection,
} from "../listen-http-ingress.js";
import { publicSubmissionDiagnostic } from "../listen-public-error-presentation.js";
import {
  type AdmissionRefusal,
  type AdmissionResult,
  makeCheckedTransactionsAdmitter,
} from "./checked-transaction-admission.js";
import {
  requestMediaType,
  SUBMIT_HTTP_BODY_MAX_BYTES,
  V1_SUBMISSION_MEDIA_TYPE,
} from "./submission-capacity.js";
import {
  createRefusalLogSampler,
  recordSubmitAdmitted,
  recordSubmitLatency,
  recordSubmitRefusal,
} from "./submission-metrics.js";
import type { CheckedSubmittedTx } from "./transaction-admission.js";

/**
 * A full submission queue answers 503 with this Retry-After, about one
 * cap-sized script verdict on a worker.
 */
export const SUBMIT_RETRY_AFTER_SECONDS = 5;

const RETRY_LATER: Readonly<Record<string, string>> = {
  "retry-after": SUBMIT_RETRY_AFTER_SECONDS.toString(),
};

export type SubmitRequestHandler = (
  request: IncomingMessage,
  response: ServerResponse,
) => void;

/** Whether the raw handler answers `request`. */
export const isSubmitRequest = (request: IncomingMessage): boolean =>
  request.method === "POST" &&
  (request.url === "/submit" || request.url?.startsWith("/submit?") === true);

type Answer = {
  readonly status: number;
  readonly body: Readonly<Record<string, unknown>>;
  readonly headers?: Readonly<Record<string, string>>;
};

/** An admission refusal's counter reason and HTTP answer. */
const admissionRefusal = (
  refusal: AdmissionRefusal,
): { readonly reason: string; readonly answer: Answer } => {
  switch (refusal._tag) {
    case "TxAdmissionConflictError":
      return {
        reason: "tx_id_bytes_conflict",
        answer: {
          status: 409,
          body: {
            error: "E_TX_ID_BYTES_CONFLICT",
            message: publicSubmissionDiagnostic(
              refusal.message,
              "E_TX_ID_BYTES_CONFLICT",
            ),
            txId: refusal.txIdHex,
          },
        },
      };
    // First seen wins (design D6): the tx is never queued behind the
    // claim. The client's own tx may be the one that holds it, so the
    // answer sends it to `/tx-status` before it treats the tx as failed.
    case "AdmissionInputSpentError":
      return {
        reason: "input_spent",
        answer: {
          status: 409,
          body: {
            error: "E_INPUT_SPENT",
            reason: refusal.reason,
            message: refusal.message,
            txId: refusal.txIdHex,
            outRef: refusal.outRefHex,
            ...(refusal.spentByTxIdHex === undefined
              ? {}
              : { spentBy: refusal.spentByTxIdHex }),
          },
        },
      };
    case "TxAdmissionBacklogFullError":
      return {
        reason: "admission_backlog_full",
        answer: {
          status: 503,
          headers: RETRY_LATER,
          body:
            refusal.unit === "bytes"
              ? {
                  error: "Durable submission admission byte backlog is full",
                  backlogBytes: refusal.backlog.toString(),
                  maxBacklogBytes: refusal.maxBacklog.toString(),
                }
              : {
                  error: "Durable submission admission backlog is full",
                  backlog: refusal.backlog.toString(),
                  maxBacklog: refusal.maxBacklog.toString(),
                },
        },
      };
    case "AdmissionWriterFullError":
      return {
        reason: "admission_writer_full",
        answer: {
          status: 503,
          headers: RETRY_LATER,
          body: {
            error: "Durable submission admission writer is full",
            unit: refusal.unit,
          },
        },
      };
    case "AdmissionWriterStalledError":
      return {
        reason: "admission_writer_stalled",
        answer: {
          status: 503,
          headers: RETRY_LATER,
          body: {
            error: "Durable submission admission writes are stalled",
            oldestPendingAgeMs: refusal.oldestPendingAgeMs,
            stallMs: refusal.stallMs,
          },
        },
      };
    case "AdmissionWriterShutdownError":
      return {
        reason: "admission_writer_unavailable",
        answer: {
          status: 503,
          headers: RETRY_LATER,
          body: {
            error: "Durable submission admission writer is unavailable",
            message: publicSubmissionDiagnostic(
              refusal.message,
              "E_ADMISSION_WRITER_UNAVAILABLE",
            ),
          },
        },
      };
  }
};

const INTERNAL_ERROR: Answer = {
  status: 500,
  body: {
    error: "Request could not be completed",
    error_code: "E_HTTP_INTERNAL",
  },
};

/**
 * Builds the raw `/submit` handler from the services in context. Bodies are
 * checked on `checkPool`; admission is a synchronous call on the services
 * captured here. `ingress` is the ingress controller the handler charges; by
 * default the one the node configuration shares with every other route.
 */
export const makeSubmitRequestHandler = <R>({
  wakeTxQueueProcessor,
  checkPool,
  ingress,
}: {
  readonly wakeTxQueueProcessor: Effect.Effect<void, never, R>;
  readonly checkPool: SubmitCheckPool;
  readonly ingress?: ReturnType<typeof createHttpIngress>;
}): Effect.Effect<
  SubmitRequestHandler,
  never,
  AdmissionWriter | NodeConfig | Globals | R
> =>
  Effect.gen(function* () {
    const runtime = yield* Effect.runtime<
      AdmissionWriter | NodeConfig | Globals | R
    >();
    const config = yield* NodeConfig;
    const controller = ingress ?? httpIngressFor(config);
    const runSync = Runtime.runSync(runtime);
    const admitChecked =
      yield* makeCheckedTransactionsAdmitter(wakeTxQueueProcessor);
    const logRefusal = createRefusalLogSampler((line) =>
      runSync(Effect.logInfo(line)),
    );
    const logFailure = createRefusalLogSampler((line) =>
      runSync(Effect.logError(line)),
    );

    return (request, response) => {
      const startedAt = performance.now();
      const releases: (() => void)[] = [];
      const releaseAll = () => {
        for (const release of releases.splice(0)) release();
      };
      // A client that goes away frees whatever its request still holds.
      response.once("close", releaseAll);
      let declared = 0;

      const answer = ({ status, body, headers }: Answer) => {
        releaseAll();
        if (response.headersSent || response.destroyed) return;
        const payload = JSON.stringify(body);
        response.writeHead(status, {
          "content-type": "application/json",
          "content-length": Buffer.byteLength(payload).toString(),
          ...(keepsConnection(status, declared) ? {} : { connection: "close" }),
          ...headers,
        });
        response.end(payload);
        recordSubmitLatency(performance.now() - startedAt);
      };
      const refuse = (
        reason: string,
        refusal: Answer,
        detail: () => string,
      ) => {
        recordSubmitRefusal(reason);
        logRefusal(reason, detail);
        answer(refusal);
      };
      const fail = (detail: () => string) => {
        recordSubmitRefusal("internal_error");
        logFailure("internal_error", detail);
        answer(INTERNAL_ERROR);
      };

      const decision = admitHttpRequest(
        {
          method: "POST",
          url: request.url ?? "",
          remoteAddress: Option.fromNullable(request.socket.remoteAddress),
          // Node hands the headers the ingress reads over as single strings.
          headers: request.headers as HttpRequestHeaders,
        },
        config,
        controller,
      );
      if (!decision.admitted) {
        // The gate already decided whether the connection stays open.
        refuse(decision.reason, decision, () => String(decision.body.error));
        return;
      }
      releases.push(decision.release);
      declared = decision.declared;

      if (
        requestMediaType(request.headers["content-type"]) !==
        V1_SUBMISSION_MEDIA_TYPE
      ) {
        refuse(
          "unsupported_media_type",
          {
            status: 415,
            body: {
              error: `V1 requests must be a canonical proof submission envelope with Content-Type ${V1_SUBMISSION_MEDIA_TYPE}`,
            },
          },
          () => `expected ${V1_SUBMISSION_MEDIA_TYPE}`,
        );
        return;
      }
      // The slot holds the body's bytes from the read to the verdict.
      const ticket = checkPool.reserve(Math.max(1, declared));
      if (ticket === undefined) {
        refuse(
          "submit_capacity",
          {
            status: 503,
            headers: RETRY_LATER,
            body: { error: "Submit ingress capacity is full; retry later" },
          },
          () => "the submit check queue is full",
        );
        return;
      }
      releases.push(ticket.release);

      const admit = (normalized: CheckedSubmittedTx) => {
        let result: AdmissionResult;
        try {
          result = admitChecked([normalized], normalized.source)[0]!;
        } catch (error) {
          fail(() => `admission failed: ${String(error)}`);
          return;
        }
        if (Either.isRight(result)) {
          const ack = result.right;
          recordSubmitAdmitted();
          answer({
            status: ack.kind === "new" ? 202 : 200,
            body: {
              txId: normalized.txIdHex,
              status: ack.status,
              firstSeenAt: ack.firstSeenAt.toISOString(),
              lastSeenAt: ack.lastSeenAt.toISOString(),
              duplicate: ack.kind === "duplicate",
            },
          });
          return;
        }
        const refusal = result.left;
        const refused = admissionRefusal(refusal);
        refuse(refused.reason, refused.answer, () => refusal.message);
      };

      const checked = (outcome: SubmitCheckOutcome) => {
        switch (outcome.kind) {
          case "unavailable":
            refuse(
              "submit_check_unavailable",
              {
                status: 503,
                headers: RETRY_LATER,
                body: { error: "Submit check is unavailable; retry later" },
              },
              () => outcome.detail,
            );
            return;
          case "failed":
            fail(() => `submit check failed: ${outcome.detail}`);
            return;
          case "checked": {
            const check = outcome.check;
            if (!check.ok) {
              refuse(
                check.reason,
                // Only an unavailable signature verifier answers 503, and
                // the sender retries it.
                check.status === 503
                  ? { status: 503, headers: RETRY_LATER, body: check.body }
                  : { status: check.status, body: check.body },
                () =>
                  check.log.length > 0
                    ? check.log.join("; ")
                    : String(check.body.error),
              );
              return;
            }
            admit(check.normalized);
          }
        }
      };

      // The ingress admitted at most `declared` bytes; Node's parser
      // delivers exactly the declared length unless the client goes away.
      const body = Buffer.allocUnsafeSlow(declared);
      let received = 0;
      let unreadable = false;
      const bodyUnreadable = () => {
        if (unreadable) return;
        unreadable = true;
        refuse(
          "body_unreadable",
          {
            status: 413,
            body: {
              error: `V1 submission exceeds or could not be read within the DA proof envelope (${SUBMIT_HTTP_BODY_MAX_BYTES.toString()} bytes)`,
            },
          },
          () => "request body exceeded or failed the bounded HTTP read",
        );
      };
      request.on("data", (chunk: Buffer) => {
        if (unreadable) return;
        if (received + chunk.length > declared) {
          bodyUnreadable();
          return;
        }
        chunk.copy(body, received);
        received += chunk.length;
      });
      request.once("error", bodyUnreadable);
      request.once("end", () => {
        if (unreadable) return;
        if (received !== declared) {
          refuse(
            "content_length_mismatch",
            {
              status: 400,
              body: { error: "V1 submission Content-Length mismatch" },
            },
            () => "body length differs from Content-Length",
          );
          return;
        }
        // The check slot is freed before `checked` runs; only the HTTP
        // ingress slot is held to the response.
        ticket.check(body, checked);
      });
    };
  });
