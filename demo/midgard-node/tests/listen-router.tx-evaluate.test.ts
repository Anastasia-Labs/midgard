import { encodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  evaluateMidgardCekVerdict,
  evaluateMidgardTxRedeemers,
  type MidgardCekVerdictRequest,
} from "@al-ft/midgard-validation";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { Deferred, Effect, Fiber } from "effect";
import { describe, expect, it } from "vitest";

import { makeEvaluateDraft } from "../../midgard-validation/tests/phase-b.evaluate-tx-redeemers.fixture.js";
import { postTxEvaluateHandler } from "../src/commands/listen-router.post-tx-evaluate-handler.js";
import {
  Lucid,
  MempoolLedgerCache,
  type MempoolLedgerCacheService,
  NodeConfig,
  ValidationPool,
  type ValidationPoolService,
  ValidationWorkerError,
} from "../src/services/index.js";

const SLOT_CONFIG = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;

const envelope = (draft: ReturnType<typeof makeEvaluateDraft>) =>
  encodeMidgardProofSubmission({
    transactionCbor: draft.candidate.submission.txCbor,
  });

const post = (
  draft: ReturnType<typeof makeEvaluateDraft>,
  pool: Pick<ValidationPoolService, "poolSize" | "tryEvaluateCekVerdict">,
  contentType = "application/vnd.midgard.v1+cbor",
) =>
  Effect.gen(function* () {
    const body = envelope(draft);
    const response = yield* postTxEvaluateHandler.pipe(
      Effect.provideService(
        HttpServerRequest.HttpServerRequest,
        HttpServerRequest.fromWeb(
          new Request("http://midgard.test/tx/evaluate", {
            method: "POST",
            headers: {
              "content-type": contentType,
              "content-length": body.length.toString(),
            },
            body: new Uint8Array(body),
          }),
        ),
      ),
      Effect.provideService(ValidationPool, pool as ValidationPoolService),
      Effect.provideService(MempoolLedgerCache, {
        withPhaseBLock: (effect) => effect,
        currentState: Effect.succeed(new Map(draft.state)),
      } as Partial<MempoolLedgerCacheService> as MempoolLedgerCacheService),
      Effect.provideService(NodeConfig, { NETWORK: "Preprod" } as never),
      Effect.provideService(Lucid, {
        api: { config: () => ({ slotConfig: SLOT_CONFIG }) },
      } as never),
    );
    const web = HttpServerResponse.toWeb(response);
    return {
      status: web.status,
      retryAfter: web.headers.get("retry-after"),
      body: (yield* Effect.promise(() => web.json())) as Record<
        string,
        unknown
      >,
    };
  });

const idlePool = {
  poolSize: 2,
  tryEvaluateCekVerdict: (request: MidgardCekVerdictRequest) =>
    Effect.sync(() => evaluateMidgardCekVerdict(request)),
};

describe("POST /tx/evaluate", () => {
  it.each([0, 1])(
    "refuses pool size %i without reading or evaluating the draft",
    async (poolSize) => {
      let invoked = false;
      const response = await Effect.runPromise(
        post(makeEvaluateDraft([0n, 0n]), {
          poolSize,
          tryEvaluateCekVerdict: () => {
            invoked = true;
            return Effect.die("must not run");
          },
        }),
      );
      expect(response.status).toBe(503);
      expect(response.retryAfter).toBe("5");
      expect(invoked).toBe(false);
    },
  );
  it("refuses an advisory slot rejection without in-process fallback", async () => {
    const response = await Effect.runPromise(
      post(makeEvaluateDraft([0n, 0n]), {
        poolSize: 2,
        tryEvaluateCekVerdict: () => Effect.succeed(undefined),
      }),
    );
    expect(response.status).toBe(503);
    expect(response.retryAfter).toBe("5");
  });
  it("returns the per-redeemer budgets the evaluator computes", async () => {
    const draft = makeEvaluateDraft([0n, 0n]);
    const expected = await Effect.runPromise(
      evaluateMidgardTxRedeemers({
        txCbor: draft.candidate.submission.txCbor,
        readState: () => Effect.succeed(draft.state),
        expectedNetworkId: 0n,
        limits: MIDGARD_CONSENSUS_LIMITS,
        config: { l1SlotConfig: SLOT_CONFIG },
      }),
    );
    if (expected.kind !== "evaluated") throw new Error(expected.detail);
    const [budget] = expected.redeemers;
    const response = await Effect.runPromise(post(draft, idlePool));
    expect(response).toEqual({
      status: 200,
      retryAfter: null,
      body: {
        redeemers: [
          {
            tag: 0,
            index: "0",
            memory: budget!.memory.toString(),
            steps: budget!.steps.toString(),
          },
        ],
      },
    });
  });

  it("answers 422 for a failing script", async () => {
    const response = await Effect.runPromise(
      post(makeEvaluateDraft([0n, 0n], true), idlePool),
    );
    expect(response.status).toBe(422);
    expect(response.body.error).toBe("E_TX_EVALUATION_FAILED");
  });

  it("answers 415 for another media type", async () => {
    const response = await Effect.runPromise(
      post(makeEvaluateDraft([0n, 0n]), idlePool, "application/json"),
    );
    expect(response.status).toBe(415);
  });

  it("runs scripts on the pool and refuses with Retry-After when its share is busy", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const entered = yield* Deferred.make<void>();
        const release = yield* Deferred.make<void>();
        // Two workers leave evaluation one concurrent request.
        const pool = {
          poolSize: 2,
          tryEvaluateCekVerdict: (request: MidgardCekVerdictRequest) =>
            Deferred.succeed(entered, undefined).pipe(
              Effect.zipRight(Deferred.await(release)),
              Effect.zipRight(
                Effect.sync(() => evaluateMidgardCekVerdict(request)),
              ),
            ),
        };
        const draft = makeEvaluateDraft([0n, 0n]);
        const first = yield* Effect.fork(post(draft, pool));
        yield* Deferred.await(entered);
        const busy = yield* post(draft, pool);
        expect(busy.status).toBe(503);
        expect(busy.retryAfter).toBe("5");
        yield* Deferred.succeed(release, undefined);
        const done = yield* Fiber.join(first);
        expect(done.status).toBe(200);
        const again = yield* post(draft, {
          poolSize: 2,
          tryEvaluateCekVerdict: (request) =>
            Effect.sync(() => evaluateMidgardCekVerdict(request)),
        });
        expect(again.status).toBe(200);
      }),
    );
  });

  it("answers 503 when the pool fails, without evaluating in process", async () => {
    const response = await Effect.runPromise(
      post(makeEvaluateDraft([0n, 0n]), {
        poolSize: 2,
        tryEvaluateCekVerdict: () =>
          Effect.fail(new ValidationWorkerError({ message: "worker crashed" })),
      }),
    );
    expect(response.status).toBe(503);
    expect(response.retryAfter).toBe("5");
  });
});
