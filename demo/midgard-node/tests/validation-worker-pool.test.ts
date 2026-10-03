import "node:fs";
import "node:path";
import "node:url";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-validation";
import "effect";
import "vitest";
import "../../midgard-validation/tests/validation-fixtures.js";
import "../src/services/validation-pool.js";
import "../src/workers/utils/validation-pool.js";
import "./validation-worker-pool.build-adversarial-corpus.js";

import { existsSync } from "node:fs";

import {
  deserializePhaseACandidate,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  makeNativeTx,
  makeOutput,
  makeQueued,
  outRefFromByte,
} from "../../midgard-validation/tests/validation-fixtures.js";
import {
  FixedValidationWorkerPool,
  ValidationWorkerError,
} from "../src/services/validation-pool.js";
import { packPhaseAJob } from "../src/workers/utils/validation-pool.js";
import {
  buildAdversarialCorpus,
  init,
  injectDefensiveCycle,
  invalidTxs,
  nodeVerifierInit,
  normalizeVerdict,
  runWorkerPhaseA,
  workerEntry,
} from "./validation-worker-pool.build-adversarial-corpus.js";

describe("long-lived validation worker pool", () => {
  it("runs real bundled workers and keeps response order", async () => {
    expect(existsSync(workerEntry)).toBe(true);
    const pool = new FixedValidationWorkerPool(2, 8, 30_000, workerEntry, init);
    try {
      await pool.start();
      const [left, right] = await Promise.all([
        pool.submit(packPhaseAJob(pool.allocateJobId(), invalidTxs(17))),
        pool.submit(packPhaseAJob(pool.allocateJobId(), invalidTxs(19))),
      ]);
      expect(left.kind).toBe("phase_a");
      expect(right.kind).toBe("phase_a");
      if (left.kind === "phase_a" && right.kind === "phase_a") {
        expect(left.results).toHaveLength(17);
        expect(right.results).toHaveLength(19);
        expect(left.results.every((result) => !result.ok)).toBe(true);
        expect(right.results.every((result) => !result.ok)).toBe(true);
      }
      const memory = await pool.workerMemoryStatistics();
      expect(memory).toHaveLength(2);
      expect(new Set(memory.map((sample) => sample.workerIndex)).size).toBe(2);
      expect(new Set(memory.map((sample) => sample.threadId)).size).toBe(2);
      for (const sample of memory) {
        expect(sample.usedHeapBytes).toBeGreaterThan(0);
        expect(sample.externalBytes).toBeGreaterThanOrEqual(0);
        expect(sample.comparableFootprintBytes).toBe(
          sample.usedHeapBytes + sample.externalBytes,
        );
      }
    } finally {
      await pool.close();
    }
  });

  it.each(["node", "cml"] as const)(
    "uses the explicit %s signature verifier and reuses its bounded key cache",
    async (signatureVerifier) => {
      const fixture = makeNativeTx();
      const queued = makeQueued(fixture.txId, fixture.txCbor);
      const pool = new FixedValidationWorkerPool(1, 4, 30_000, workerEntry, {
        ...init,
        signatureVerifier,
      });
      try {
        await pool.start();
        const response = await pool.submit(
          packPhaseAJob(pool.allocateJobId(), [queued, queued]),
        );
        expect(response.kind).toBe("phase_a");
        if (response.kind === "phase_a") {
          expect(response.results.every((result) => result.ok)).toBe(true);
          expect(response.publicKeyCache).toMatchObject({
            size: 1,
            maxEntries: 4_096,
            hits: 1,
            misses: 1,
            evictions: 0,
          });
        }
      } finally {
        await pool.close();
      }
    },
  );

  it.each([2, 6])(
    "matches inline Phase A verdicts and ordering with a %i-worker pool",
    async (poolSize) => {
      const queued = Array.from({ length: 64 }, (_, index) => {
        const fixture = makeNativeTx({
          spendInputs: [outRefFromByte(index + 1)],
          outputs: [makeOutput(10n)],
        });
        return makeQueued(fixture.txId, fixture.txCbor, BigInt(index));
      });
      const invalidSignature = makeNativeTx({ invalidVkeyWitness: true });
      queued.push(
        makeQueued(invalidSignature.txId, invalidSignature.txCbor, 64n),
      );
      queued.push(
        makeQueued(Buffer.alloc(32, 0xff), Buffer.from("80", "hex"), 65n),
      );
      const inline = await Effect.runPromise(
        runPhaseAValidation(queued, {
          ...init.config,
          concurrency: 1,
        }),
      );
      const pool = new FixedValidationWorkerPool(
        poolSize,
        poolSize * 4,
        30_000,
        workerEntry,
        nodeVerifierInit,
      );
      try {
        await pool.start();
        const responses = await Promise.all(
          Array.from({ length: Math.ceil(queued.length / 16) }, (_, chunk) =>
            pool.submit(
              packPhaseAJob(
                pool.allocateJobId(),
                queued.slice(chunk * 16, chunk * 16 + 16),
              ),
            ),
          ),
        );
        const accepted: string[] = [];
        const rejected: string[] = [];
        for (const response of responses) {
          expect(response.kind).toBe("phase_a");
          if (response.kind !== "phase_a") continue;
          for (const result of response.results) {
            if (result.ok) {
              accepted.push(
                deserializePhaseACandidate(
                  result.candidate,
                ).ledgerTx.txId.toString("hex"),
              );
            } else {
              rejected.push(result.code);
            }
          }
        }
        expect(accepted).toStrictEqual(
          inline.accepted.map((candidate) =>
            candidate.ledgerTx.txId.toString("hex"),
          ),
        );
        expect(rejected).toStrictEqual(
          inline.rejected.map((rejection) => rejection.code),
        );
      } finally {
        await pool.close();
      }
    },
  );

  it.each([2, 6])(
    "matches the full inline verdict and state patch on the adversarial corpus with %i workers",
    async (poolSize) => {
      const corpus = buildAdversarialCorpus();
      const inlinePhaseA = await Effect.runPromise(
        runPhaseAValidation(corpus.queued, {
          ...init.config,
          concurrency: 1,
        }),
      );
      const inlinePhaseB = await Effect.runPromise(
        runPhaseBValidationWithPatch(
          injectDefensiveCycle(inlinePhaseA.accepted, corpus.cycleTxIds),
          corpus.preState,
          {
            nowCardanoSlotNo: 0n,
            bucketConcurrency: 1,
            enforceScriptBudget: true,
          },
        ),
      );
      const inlineVerdict = normalizeVerdict(inlinePhaseA, inlinePhaseB);

      const pool = new FixedValidationWorkerPool(
        poolSize,
        poolSize * 4,
        30_000,
        workerEntry,
        nodeVerifierInit,
      );
      try {
        await pool.start();
        const workerPhaseA = await runWorkerPhaseA(pool, corpus.queued);
        const workerPhaseB = await Effect.runPromise(
          runPhaseBValidationWithPatch(
            injectDefensiveCycle(workerPhaseA.accepted, corpus.cycleTxIds),
            corpus.preState,
            {
              nowCardanoSlotNo: 0n,
              bucketConcurrency: poolSize,
              enforceScriptBudget: true,
            },
          ),
        );

        expect(normalizeVerdict(workerPhaseA, workerPhaseB)).toStrictEqual(
          inlineVerdict,
        );
      } finally {
        await pool.close();
      }
    },
  );

  it("fails an in-flight chunk on worker crash and serves the next job after respawn", async () => {
    const pool = new FixedValidationWorkerPool(
      1,
      4,
      30_000,
      workerEntry,
      nodeVerifierInit,
    );
    try {
      await pool.start();
      const inFlight = pool.submit(
        packPhaseAJob(pool.allocateJobId(), invalidTxs(20_000)),
      );
      await pool.terminateWorker(0);
      await expect(inFlight).rejects.toBeInstanceOf(ValidationWorkerError);

      const fixture = makeNativeTx();
      const afterRespawn = await pool.submit(
        packPhaseAJob(pool.allocateJobId(), [
          makeQueued(fixture.txId, fixture.txCbor),
        ]),
      );
      expect(afterRespawn).toMatchObject({
        kind: "phase_a",
        results: [{ ok: true }],
        publicKeyCache: { size: 1, misses: 1 },
      });
    } finally {
      await pool.close();
    }
  });

  it("blocks enqueue beyond the bounded queue until capacity returns", async () => {
    const pool = new FixedValidationWorkerPool(1, 1, 30_000, workerEntry, init);
    try {
      await pool.start();
      const first = pool.submit(
        packPhaseAJob(pool.allocateJobId(), invalidTxs(20_000)),
      );
      const second = pool.submit(
        packPhaseAJob(pool.allocateJobId(), invalidTxs(1)),
      );
      let thirdResolved = false;
      const third = pool
        .submit(packPhaseAJob(pool.allocateJobId(), invalidTxs(1)))
        .then((value) => {
          thirdResolved = true;
          return value;
        });
      await new Promise((resolveWait) => setTimeout(resolveWait, 5));
      expect(pool.stats().queueDepth).toBe(1);
      expect(thirdResolved).toBe(false);
      await Promise.all([first, second, third]);
    } finally {
      await pool.close();
    }
  });

  it("terminates a worker whose job exceeds its timeout", async () => {
    const pool = new FixedValidationWorkerPool(1, 4, 1, workerEntry, init);
    try {
      await expect(pool.start()).rejects.toBeInstanceOf(ValidationWorkerError);
      expect(pool.isClosed()).toBe(true);
      expect(pool.stats()).toStrictEqual({
        busyWorkers: 0,
        queueDepth: 0,
        oldestInFlightAgeMs: 0,
        liveWorkers: 0,
        restartingWorkers: 0,
      });
    } finally {
      await pool.close();
    }
  });
});
