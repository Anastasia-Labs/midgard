/**
 * Node admission of a Plutus spend whose script takes a builtin step past
 * the proof evaluator's 9,215-byte payload bounds (equalsByteString,
 * appendByteString, decodeUtf8, serialiseData). Admission decides its batch
 * in verdict mode on the validation pool's workers, which run the PV11
 * evaluator: Cardano accepts each script, so admission accepts it, and
 * rejects one that then fails on its own. Neither fails the batch for a
 * retry.
 */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { RejectCodes } from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { type ProgramSpend } from "../../midgard-validation/tests/phase-b.indeterminate.fixture.js";
import { payloadBoundRows } from "../../midgard-validation/tests/phase-b.payload-bound.fixture.js";
import { decideAdmissionBatch } from "../src/fibers/tx-queue-processor.js";
import { pooledCekVerdictEvaluator } from "../src/fibers/tx-queue-processor.pooled-cek-verdict.js";
import {
  FixedValidationWorkerPool,
  type ValidationPoolService,
} from "../src/services/validation-pool.js";
import {
  init,
  workerEntry,
} from "./validation-worker-pool.build-adversarial-corpus.js";

const ROWS = payloadBoundRows(0x61);

let pool: FixedValidationWorkerPool;
/** Verdicts the pool's workers computed. */
let workerVerdicts = 0;

/** The admission pool service, every verdict job on a worker. */
const poolService = () =>
  ({
    poolSize: 1,
    evaluateCekVerdict: (request) =>
      Effect.promise(() =>
        pool.submit({
          kind: "cek_verdict",
          jobId: pool.allocateJobId(),
          request,
        }),
      ).pipe(
        Effect.map((response) => {
          if (response.kind !== "cek_verdict")
            throw new Error(`expected a CEK verdict, got ${response.kind}`);
          workerVerdicts += 1;
          return response.result;
        }),
      ),
  }) as Pick<
    ValidationPoolService,
    "poolSize" | "evaluateCekVerdict"
  > as ValidationPoolService;

/** One admission batch of `spends`, as the tx-queue processor decides it. */
const admit = (spends: readonly ProgramSpend[]) =>
  Effect.runPromise(
    Effect.either(
      decideAdmissionBatch({
        phaseA: {
          accepted: spends.map((spend) => spend.candidate),
          rejected: [],
        },
        pendingWithdrawalOutRefHexes: new Set(),
        ledgerState: new Map(
          spends.flatMap((spend) => [...spend.preState.entries()]),
        ),
        phaseBConfig: {
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          nowCardanoSlotNo: 100n,
          bucketConcurrency: 1,
          enforceScriptBudget: true,
          evaluateCekVerdict: pooledCekVerdictEvaluator(poolService()),
        },
      }),
    ),
  );

const txIdHex = (spend: ProgramSpend) =>
  Buffer.from(spend.candidate.ledgerTx.txId).toString("hex");

describe("admission of a builtin step past the proof evaluator's payload bounds", () => {
  beforeAll(async () => {
    pool = new FixedValidationWorkerPool(1, 8, 60_000, workerEntry, init);
    await pool.start();
  });
  afterAll(async () => {
    await pool.close();
  });

  it.each(ROWS)(
    "$label: admission accepts the script and rejects it failing on its own",
    async ({ over, overFailing }) => {
      const before = workerVerdicts;
      const decided = await admit([over, overFailing]);
      expect(
        decided._tag,
        decided._tag === "Left" ? String(decided.left) : "",
      ).toBe("Right");
      if (decided._tag === "Left") return;
      const { phaseB, allRejected } = decided.right;
      expect(
        phaseB.accepted.map((tx) =>
          Buffer.from(tx.ledgerTx.txId).toString("hex"),
        ),
      ).toEqual([txIdHex(over)]);
      expect(
        allRejected.map((rejected) => [
          rejected.txId.toString("hex"),
          rejected.code,
        ]),
      ).toEqual([[txIdHex(overFailing), RejectCodes.PlutusScriptInvalid]]);
      // Both verdicts came from the pool's workers, not the in-process
      // fallback.
      expect(workerVerdicts - before).toBe(2);
    },
    120_000,
  );
});
