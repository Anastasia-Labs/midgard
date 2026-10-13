import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core";
import {
  buildMidgardCanonicalCekProgram,
  evaluateMidgardCekVerdict,
  type MidgardCekVerdictRequest,
} from "@al-ft/midgard-validation";
import { encodeMidgardCekPlutusData } from "@al-ft/midgard-validation/cek-constant";
import { DataI } from "@harmoniclabs/plutus-data";
import {
  ErrorUPLC,
  Lambda,
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
import {
  init,
  workerEntry,
} from "./validation-worker-pool.build-adversarial-corpus.js";

const request = (
  body: UPLCTerm,
  executionBudget?: { readonly cpu: bigint; readonly memory: bigint },
): MidgardCekVerdictRequest => {
  const program = buildMidgardCanonicalCekProgram(
    Buffer.from(UPLCEncoder.compile(new UPLCProgram([1, 1, 0], body))),
  );
  return {
    scriptBytes: program.envelopeCbor,
    programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
      ...program.material.values(),
    ]),
    contextCbor: encodeMidgardCekPlutusData(new DataI(42n)),
    executionBudget,
    executionIndex: 0n,
    maxSteps: 1_000,
  };
};

const REQUESTS = {
  success: request(new Lambda(new UPLCVar(0))),
  halt_error: request(new Lambda(new ErrorUPLC())),
  budget_stop: request(new Lambda(new UPLCVar(0)), { cpu: 1n, memory: 1n }),
  malformed: {
    ...request(new Lambda(new UPLCVar(0))),
    scriptBytes: new Uint8Array([0xde, 0xad]),
  },
};

describe("validation pool CEK verdicts", () => {
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
        expect(response.result, label).toEqual(
          evaluateMidgardCekVerdict(cekRequest),
        );
        kinds.push(response.result.kind);
      }
      expect(kinds).toEqual([
        "accepted",
        "script_invalid",
        "accepted",
        "script_invalid",
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
