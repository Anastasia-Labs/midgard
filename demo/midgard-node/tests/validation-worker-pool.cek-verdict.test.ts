import { encodeCborBytes } from "@al-ft/midgard-core/codec/cbor";
import {
  evaluateMidgardCekVerdict,
  type MidgardCekVerdictRequest,
} from "@al-ft/midgard-validation";
import { encodeMidgardCekPlutusData } from "@al-ft/midgard-validation/cek-constant";
import { DataI } from "@al-ft/midgard-validation/uplc";
import {
  Application,
  ErrorUPLC,
  Lambda,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  type UPLCTerm,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { pooledCekVerdictEvaluator } from "../src/fibers/tx-queue-processor.pooled-cek-verdict.js";
import {
  FixedValidationWorkerPool,
  type ValidationPoolService,
  ValidationWorkerError,
} from "../src/services/validation-pool.js";
import { packPhaseAJob } from "../src/workers/utils/validation-pool.js";
import {
  init,
  workerEntry,
} from "./validation-worker-pool.build-adversarial-corpus.js";

const request = (
  body: UPLCTerm,
  executionBudget?: { readonly cpu: bigint; readonly memory: bigint },
): MidgardCekVerdictRequest => {
  const scriptBytes = encodeCborBytes(
    UPLCEncoder.compile(new UPLCProgram([1, 1, 0], body)),
  );
  return {
    scriptBytes,
    scriptLanguage: "PlutusV3",
    contextCbor: encodeMidgardCekPlutusData(new DataI(42n)),
    executionBudget,
    executionIndex: 0n,
    maxSteps: 1_000,
  };
};

const withCause = (
  result: ReturnType<typeof evaluateMidgardCekVerdict>,
): ReturnType<typeof evaluateMidgardCekVerdict> & {
  readonly cause?: unknown;
} => result;

const causeMessage = (cause: unknown): string | undefined =>
  cause instanceof Error ? cause.message : undefined;

/** `(lam ctx [(lam _ ()) ctx])`: a Plutus V3 script must return Unit. */
const unitProgram = () =>
  new Lambda(new Application(new Lambda(UPLCConst.unit), new UPLCVar(0)));

const REQUESTS = {
  success: request(unitProgram()),
  halt_error: request(new Lambda(new ErrorUPLC())),
  budget_stop: request(new Lambda(UPLCConst.unit), { cpu: 1n, memory: 1n }),
  malformed: {
    ...request(new Lambda(UPLCConst.unit)),
    scriptBytes: new Uint8Array([0xde, 0xad]),
  },
};

describe("validation pool CEK verdicts", () => {
  it("dispatches one advisory job on two workers while production progresses without waiting", async () => {
    const pool = new FixedValidationWorkerPool(2, 8, 30_000, workerEntry, init);
    try {
      await pool.start();
      const advisory = pool.trySubmitAdvisory({
        kind: "cek_verdict",
        jobId: pool.allocateJobId(),
        request: REQUESTS.success,
      });
      expect(advisory).toBeDefined();
      for (let index = 0; index < 32; index += 1)
        expect(
          pool.trySubmitAdvisory({
            kind: "cek_verdict",
            jobId: pool.allocateJobId(),
            request: REQUESTS.success,
          }),
        ).toBeUndefined();
      expect(pool.capacityWaiters).toHaveLength(0);
      expect(pool.queue).toHaveLength(0);
      const production = pool.submit(packPhaseAJob(pool.allocateJobId(), []));
      expect(pool.stats().busyWorkers).toBe(2);
      const [advisoryResult, productionResult] = await Promise.all([
        advisory!,
        production,
      ]);
      expect(advisoryResult.kind).toBe("cek_verdict");
      if (advisoryResult.kind === "cek_verdict")
        expect(advisoryResult.result.kind).toBe("accepted");
      expect(productionResult.kind).toBe("phase_a");
      expect(pool.capacityWaiters).toHaveLength(0);
    } finally {
      await pool.close();
    }
  });
  it.each([0, 1])("refuses advisory work with %i workers", async (size) => {
    const pool = new FixedValidationWorkerPool(
      size,
      8,
      30_000,
      workerEntry,
      init,
    );
    try {
      await pool.start();
      expect(
        pool.trySubmitAdvisory({
          kind: "cek_verdict",
          jobId: pool.allocateJobId(),
          request: REQUESTS.success,
        }),
      ).toBeUndefined();
      expect(pool.queue).toHaveLength(0);
      expect(pool.capacityWaiters).toHaveLength(0);
    } finally {
      await pool.close();
    }
  });
  it("computes on a worker exactly the verdict Phase B computes in process", async () => {
    const pool = new FixedValidationWorkerPool(2, 8, 30_000, workerEntry, init);
    try {
      await pool.start();
      const kinds = [];
      for (const [label, cekRequest] of Object.entries(REQUESTS)) {
        const response = await pool.submit({
          kind: "cek_verdict",
          jobId: pool.allocateJobId(),
          request: cekRequest,
        });
        expect(response.kind, label).toBe("cek_verdict");
        if (response.kind !== "cek_verdict") continue;
        // A cause crosses the worker boundary by structured clone: its message
        // and built-in error class survive, its own fields do not.
        const { cause: workerCause, ...workerVerdict } = withCause(
          response.result,
        );
        const { cause: localCause, ...localVerdict } = withCause(
          evaluateMidgardCekVerdict(cekRequest),
        );
        expect(workerVerdict, label).toEqual(localVerdict);
        expect(causeMessage(workerCause), label).toBe(causeMessage(localCause));
        kinds.push(response.result.kind);
      }
      expect(kinds).toEqual([
        "accepted",
        "script_invalid",
        "script_invalid",
        "indeterminate",
      ]);
    } finally {
      await pool.close();
    }
  });

  it("falls back to the in-process verdict when the pool fails", async () => {
    const failing = {
      poolSize: 1,
      evaluateCekVerdict: () =>
        Effect.fail(new ValidationWorkerError({ message: "worker down" })),
    } as unknown as ValidationPoolService;
    const evaluate = pooledCekVerdictEvaluator(failing)!;
    for (const cekRequest of Object.values(REQUESTS)) {
      await expect(Effect.runPromise(evaluate(cekRequest))).resolves.toEqual(
        evaluateMidgardCekVerdict(cekRequest),
      );
    }
  });

  it("evaluates in process when the pool is disabled", () => {
    expect(
      pooledCekVerdictEvaluator({
        poolSize: 0,
      } as unknown as ValidationPoolService),
    ).toBeUndefined();
  });
});
