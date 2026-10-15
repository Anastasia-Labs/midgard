import {
  decodeMidgardProofSubmission,
  encodeMidgardCekProgramMaterialSidecar,
} from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  evaluateMidgardTxRedeemers,
  type MidgardTxRedeemerEvaluation,
} from "@al-ft/midgard-validation";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { Effect, Option } from "effect";

import { canonicalSlotConfigForLucid } from "../lucid-time.js";
import {
  Lucid,
  MempoolLedgerCache,
  NodeConfig,
  ValidationPool,
  type ValidationPoolService,
  ValidationWorkerError,
} from "../services/index.js";
import {
  readSubmitBodyWithProtocolLimit,
  requestMediaType,
  resolveSubmitIngressReservation,
  V1_SUBMISSION_MEDIA_TYPE,
} from "./listen-router.get-tx-handler.js";
import { SUBMIT_RETRY_AFTER_SECONDS } from "./listen-router.post-submit-handler.js";

export const TX_EVALUATE_ENDPOINT = "tx/evaluate";

const UNAVAILABLE = {
  status: 503,
  headers: { "retry-after": SUBMIT_RETRY_AFTER_SECONDS.toString() },
};

/**
 * Evaluations use at most half the pool and only idle slots. Pools smaller
 * than two workers cannot reserve admission capacity and refuse advisory work.
 */
export const txEvaluateMaxConcurrency = (poolSize: number): number =>
  Math.max(0, Math.floor(poolSize / 2));

const evaluatePermits = new Map<number, Effect.Semaphore>();
const evaluatePermit = (poolSize: number): Effect.Semaphore => {
  const permits = txEvaluateMaxConcurrency(poolSize);
  let semaphore = evaluatePermits.get(permits);
  if (semaphore === undefined) {
    semaphore = Effect.unsafeMakeSemaphore(permits);
    evaluatePermits.set(permits, semaphore);
  }
  return semaphore;
};

/** Pool failures are not retried in process: evaluation is advisory. */
const poolEvaluator =
  (pool: ValidationPoolService) =>
  (request: Parameters<ValidationPoolService["evaluateCekVerdict"]>[0]) =>
    pool.tryEvaluateCekVerdict(request).pipe(
      Effect.flatMap((result) =>
        result === undefined
          ? Effect.fail(
              new ValidationWorkerError({
                message: "advisory evaluation capacity is full",
              }),
            )
          : Effect.succeed(result),
      ),
    );

const evaluationBody = (evaluation: MidgardTxRedeemerEvaluation) =>
  evaluation.kind === "evaluated"
    ? {
        redeemers: evaluation.redeemers.map((redeemer) => ({
          tag: redeemer.tag,
          index: redeemer.index.toString(10),
          memory: redeemer.memory.toString(10),
          steps: redeemer.steps.toString(10),
        })),
      }
    : {
        error: "E_TX_EVALUATION_FAILED",
        detail: "Transaction scripts could not be evaluated",
      };

/**
 * `POST /tx/evaluate`: the per-redeemer ex-units a draft transaction's
 * scripts spend under Phase B's cost accounting, against the node's current
 * ledger state. The body is a V1 submission envelope, as for `/submit`; the
 * draft's declared ex-units, fee and signatures are not checked.
 */
export const postTxEvaluateHandler = Effect.gen(function* () {
  const request = yield* HttpServerRequest.HttpServerRequest;
  if (
    requestMediaType(request.headers["content-type"]) !==
    V1_SUBMISSION_MEDIA_TYPE
  ) {
    return yield* HttpServerResponse.json(
      { error: `Content-Type must be ${V1_SUBMISSION_MEDIA_TYPE}` },
      { status: 415 },
    );
  }
  const reservation = resolveSubmitIngressReservation(
    request.headers["content-length"],
  );
  if (reservation.kind !== "ready") {
    return yield* HttpServerResponse.json(
      { error: "Invalid or oversized Content-Length" },
      { status: reservation.kind === "too_large" ? 413 : 400 },
    );
  }
  const pool = yield* ValidationPool;
  if (pool.poolSize < 2)
    return yield* HttpServerResponse.json(
      { error: "Transaction evaluation requires reserved worker capacity" },
      UNAVAILABLE,
    );
  const permitted = yield* evaluatePermit(pool.poolSize).withPermitsIfAvailable(
    1,
  )(
    Effect.gen(function* () {
      const body = yield* Effect.either(
        readSubmitBodyWithProtocolLimit(request),
      );
      if (body._tag === "Left") {
        return yield* HttpServerResponse.json(
          { error: "Request body exceeded or failed the bounded read" },
          { status: 413 },
        );
      }
      const submission = yield* Effect.either(
        Effect.try(() => decodeMidgardProofSubmission(Buffer.from(body.right))),
      );
      if (submission._tag === "Left") {
        return yield* HttpServerResponse.json(
          { error: "Invalid canonical V1 submission envelope" },
          { status: 400 },
        );
      }
      const nodeConfig = yield* NodeConfig;
      const { api: lucid } = yield* Lucid;
      const ledgerCache = yield* MempoolLedgerCache;
      const evaluation = yield* Effect.either(
        evaluateMidgardTxRedeemers({
          txCbor: submission.right.transactionCbor,
          programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar(
            submission.right.programMaterial,
          ),
          readState: (outRefHexes) =>
            ledgerCache.withPhaseBLock(
              ledgerCache.currentState.pipe(
                Effect.map(
                  (state) =>
                    new Map(
                      outRefHexes.flatMap((outRefHex) => {
                        const output = state.get(outRefHex);
                        return output === undefined
                          ? []
                          : [[outRefHex, output] as const];
                      }),
                    ),
                ),
              ),
            ),
          expectedNetworkId: nodeConfig.NETWORK === "Mainnet" ? 1n : 0n,
          limits: MIDGARD_CONSENSUS_PROFILE.limits,
          config: {
            l1SlotConfig: canonicalSlotConfigForLucid(lucid),
            evaluateCekVerdict: poolEvaluator(pool),
          },
        }),
      );
      if (evaluation._tag === "Left") {
        yield* Effect.logWarning(
          "tx evaluation unavailable: E_TX_EVALUATION_UNAVAILABLE",
        );
        return yield* HttpServerResponse.json(
          { error: "Transaction evaluation is unavailable" },
          UNAVAILABLE,
        );
      }
      return yield* HttpServerResponse.json(evaluationBody(evaluation.right), {
        status: evaluation.right.kind === "evaluated" ? 200 : 422,
      });
    }),
  );
  return Option.isSome(permitted)
    ? permitted.value
    : yield* HttpServerResponse.json(
        { error: "Transaction evaluation capacity is full" },
        UNAVAILABLE,
      );
});
