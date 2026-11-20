import { existsSync, readFileSync } from "node:fs";
import { resolve } from "node:path";
import { pathToFileURL } from "node:url";

import {
  deserializePhaseACandidate,
  encodeScriptContextCbor,
  evaluateScriptWithHarmonic,
  MidgardRedeemerTag,
  type PhaseAResult,
  type PhaseAValidatedTx,
  type PhaseBResultWithPatch,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { Constr } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  FixedValidationWorkerPool,
  ValidationWorkerError,
} from "@/services/validation-pool.js";
import {
  copyToTransferable,
  packPhaseAJob,
} from "@/workers/utils/validation-pool.js";

import {
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makeProtectedScriptOutput,
  makeQueued,
  makeRedeemersCbor,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
} from "../../midgard-validation/tests/validation-fixtures.js";

const workerEntry = pathToFileURL(resolve("dist/validation.js"));
const init = {
  config: {
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    strictnessProfile: "phase2_worker_test",
  },
} as const;
const nodeVerifierInit = {
  ...init,
  signatureVerifier: "node",
} as const;

const alwaysSucceedsBlueprint = JSON.parse(
  readFileSync(resolve("blueprints/always-succeeds/plutus.json"), "utf8"),
) as {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
  }[];
};
const alwaysSucceedsScriptBytes = Buffer.from(
  alwaysSucceedsBlueprint.validators.find(
    (validator) => validator.title === "midgard.deposit_spend.else",
  )?.compiledCode ?? "",
  "hex",
);

const invalidTxs = (length: number) =>
  Array.from({ length }, (_, index) => ({
    txId: Buffer.alloc(32, index & 0xff),
    txCbor: Buffer.from("80", "hex"),
    arrivalSeq: BigInt(index),
    createdAt: new Date(index),
  }));

const runWorkerPhaseA = async (
  pool: FixedValidationWorkerPool,
  queued: readonly ReturnType<typeof makeQueued>[],
): Promise<PhaseAResult> => {
  const responses = await Promise.all(
    Array.from({ length: Math.ceil(queued.length / 4) }, (_, chunk) =>
      pool.submit(
        packPhaseAJob(
          pool.allocateJobId(),
          queued.slice(chunk * 4, chunk * 4 + 4),
        ),
      ),
    ),
  );
  const accepted: PhaseAValidatedTx[] = [];
  const rejected: PhaseAResult["rejected"][number][] = [];
  for (const response of responses) {
    if (response.kind !== "phase_a") {
      throw new Error(`expected phase_a response, got ${response.kind}`);
    }
    for (const result of response.results) {
      if (result.ok) {
        accepted.push(deserializePhaseACandidate(result.candidate));
      } else {
        rejected.push({
          txId: Buffer.from(result.txId),
          code: result.code,
          detail: result.detail,
        });
      }
    }
  }
  return { accepted, rejected };
};

const buildAdversarialCorpus = () => {
  if (alwaysSucceedsScriptBytes.length === 0) {
    throw new Error("missing always-succeeds Plutus spend fixture");
  }
  const shared = outRefFromByte(0x31);
  const parentInput = outRefFromByte(0x32);
  const invalidSignatureInput = outRefFromByte(0x33);
  const scriptInput = outRefFromByte(0x34);
  const budgetInput = outRefFromByte(0x35);
  const cycleLeftInput = outRefFromByte(0x36);
  const cycleRightInput = outRefFromByte(0x37);
  const script = plutusV3ScriptWitness(alwaysSucceedsScriptBytes);
  const scriptHash = hashScriptWitness(script);
  const parent = makeNativeTx({ spendInputs: [parentInput] });
  const fixtures = [
    makeNativeTx({ spendInputs: [shared] }),
    makeNativeTx({ spendInputs: [shared], validityIntervalEnd: 10n }),
    parent,
    makeNativeTx({ spendInputs: [outRefFromTxId(parent.txId)] }),
    makeNativeTx({
      spendInputs: [invalidSignatureInput],
      invalidVkeyWitness: true,
    }),
    makeNativeTx({
      spendInputs: [scriptInput],
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        { tag: MidgardRedeemerTag.Spend, index: 0n },
      ]),
      scriptLanguages: ["PlutusV3"],
    }),
    makeNativeTx({
      spendInputs: [budgetInput],
      scriptWitnesses: [script],
      redeemerTxWitsPreimageCbor: makeRedeemersCbor([
        {
          tag: MidgardRedeemerTag.Spend,
          index: 0n,
          exUnits: [0n, 0n],
        },
      ]),
      scriptLanguages: ["PlutusV3"],
    }),
    makeNativeTx({ spendInputs: [cycleLeftInput] }),
    makeNativeTx({ spendInputs: [cycleRightInput] }),
  ];
  return {
    queued: fixtures.map((fixture, index) =>
      makeQueued(fixture.txId, fixture.txCbor, BigInt(index)),
    ),
    preState: new Map<string, Buffer>([
      [shared.toString("hex"), makeOutput(10n)],
      [parentInput.toString("hex"), makeOutput(10n)],
      [invalidSignatureInput.toString("hex"), makeOutput(10n)],
      [scriptInput.toString("hex"), makeProtectedScriptOutput(scriptHash, 10n)],
      [budgetInput.toString("hex"), makeProtectedScriptOutput(scriptHash, 10n)],
      [cycleLeftInput.toString("hex"), makeOutput(10n)],
      [cycleRightInput.toString("hex"), makeOutput(10n)],
    ]),
    cycleTxIds: [
      fixtures[7]!.txId.toString("hex"),
      fixtures[8]!.txId.toString("hex"),
    ] as const,
  };
};

const injectDefensiveCycle = (
  candidates: readonly PhaseAValidatedTx[],
  cycleTxIds: readonly [string, string],
): readonly PhaseAValidatedTx[] => {
  const byId = new Map(
    candidates.map((candidate) => [
      candidate.ledgerTx.txId.toString("hex"),
      candidate,
    ]),
  );
  const left = byId.get(cycleTxIds[0]);
  const right = byId.get(cycleTxIds[1]);
  if (left === undefined || right === undefined) {
    throw new Error("cycle fixtures did not survive Phase A");
  }
  const leftConsumes = right.graph.produced[0]!.outref.toString("hex");
  const rightConsumes = left.graph.produced[0]!.outref.toString("hex");
  return candidates.map((candidate) => {
    const txIdHex = candidate.ledgerTx.txId.toString("hex");
    if (txIdHex === cycleTxIds[0]) {
      return {
        ...candidate,
        graph: {
          ...candidate.graph,
          spentOutRefHexes: [leftConsumes],
          referenceOutRefHexes: [],
        },
      };
    }
    if (txIdHex === cycleTxIds[1]) {
      return {
        ...candidate,
        graph: {
          ...candidate.graph,
          spentOutRefHexes: [rightConsumes],
          referenceOutRefHexes: [],
        },
      };
    }
    return candidate;
  });
};

const normalizeVerdict = (
  phaseA: PhaseAResult,
  phaseB: PhaseBResultWithPatch,
) => ({
  acceptedTxIds: phaseB.accepted.map((candidate) =>
    candidate.ledgerTx.txId.toString("hex"),
  ),
  rejected: [...phaseA.rejected, ...phaseB.rejected].map((rejection) => ({
    txId: rejection.txId.toString("hex"),
    code: rejection.code,
    detail: rejection.detail,
  })),
  statePatch: phaseB.statePatch,
});

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

  it("returns the same UPLC verdict and detail as the inline evaluator", async () => {
    const pool = new FixedValidationWorkerPool(1, 4, 30_000, workerEntry, init);
    try {
      await pool.start();
      const scriptBytes = Buffer.from("010203", "hex");
      const context = new Constr(0, []);
      const inline = evaluateScriptWithHarmonic(scriptBytes, context);
      const response = await pool.submit({
        kind: "uplc",
        jobId: pool.allocateJobId(),
        scriptBytes: copyToTransferable(scriptBytes),
        contextCbor: copyToTransferable(encodeScriptContextCbor(context)),
      });
      expect(response.kind).toBe("uplc");
      if (response.kind === "uplc") {
        expect(response.result).toStrictEqual(
          inline.kind === "accepted"
            ? {
                ok: true,
                cpu: inline.budget.cpu,
                memory: inline.budget.memory,
              }
            : { ok: false, detail: inline.detail },
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
              evaluateScript: (scriptBytes, contextCbor) =>
                Effect.tryPromise(() =>
                  pool.submit({
                    kind: "uplc",
                    jobId: pool.allocateJobId(),
                    scriptBytes: copyToTransferable(scriptBytes),
                    contextCbor: copyToTransferable(contextCbor),
                  }),
                ).pipe(
                  Effect.flatMap((response) => {
                    if (response.kind !== "uplc") {
                      return Effect.fail(
                        new Error(
                          `expected uplc response, got ${response.kind}`,
                        ),
                      );
                    }
                    return Effect.succeed(
                      response.result.ok
                        ? {
                            kind: "accepted" as const,
                            budget: {
                              cpu: response.result.cpu,
                              memory: response.result.memory,
                            },
                          }
                        : {
                            kind: "script_invalid" as const,
                            detail: response.result.detail,
                          },
                    );
                  }),
                ),
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
