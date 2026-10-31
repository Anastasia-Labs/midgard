/**
 * Node admission of Plutus spends whose redeemer, or whose spent output's
 * datum, is Data nested thousands of levels deep, up to the deepest a
 * transaction can carry. Admission decides its batch in verdict mode on the
 * validation pool's workers: each spend gets its script's own verdict, the
 * batch never fails for a retry, and a shallow spend admitted next on the same
 * worker still gets its verdict. A worker and the main thread, with their
 * different stacks, compute the same verdict for the same deep request.
 */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  evaluateMidgardCekVerdict,
  type MidgardCekVerdictRequest,
  RejectCodes,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  deepCanonicalData,
  deepestRedeemerDepth,
  deepRows,
  type DeepShape,
  deepSpend,
  shallowSpend,
  walkingProgram,
} from "../../midgard-validation/tests/phase-b.deep-data.fixture.js";
import { type ProgramSpend } from "../../midgard-validation/tests/phase-b.indeterminate.fixture.js";
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

let pool: FixedValidationWorkerPool;
/** Verdicts the pool's workers computed. */
let workerVerdicts = 0;
/** Every request admission sent to the pool. */
const requests: MidgardCekVerdictRequest[] = [];

/** The admission pool service, every verdict job on a worker. */
const poolService = () =>
  ({
    poolSize: 1,
    evaluateCekVerdict: (request) => {
      requests.push(request);
      return Effect.promise(() =>
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
      );
    },
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

/** Admits `accepted` and rejects `rejected`, all verdicts from the workers. */
const expectAdmission = async (
  accepted: readonly ProgramSpend[],
  rejected: readonly ProgramSpend[],
) => {
  const before = workerVerdicts;
  const decided = await admit([...accepted, ...rejected]);
  expect(
    decided._tag,
    decided._tag === "Left" ? String(decided.left) : "",
  ).toBe("Right");
  if (decided._tag === "Left") return;
  const { phaseB, allRejected } = decided.right;
  expect(
    phaseB.accepted.map((tx) => Buffer.from(tx.ledgerTx.txId).toString("hex")),
  ).toEqual(accepted.map(txIdHex));
  expect(
    allRejected.map((entry) => [entry.txId.toString("hex"), entry.code]),
  ).toEqual(
    rejected.map((spend) => [txIdHex(spend), RejectCodes.PlutusScriptInvalid]),
  );
  // Every verdict came from the pool's workers, not the in-process fallback.
  expect(workerVerdicts - before).toBe(accepted.length + rejected.length);
};

describe("admission of deeply nested script Data", () => {
  beforeAll(async () => {
    pool = new FixedValidationWorkerPool(1, 8, 60_000, workerEntry, init);
    await pool.start();
  });
  afterAll(async () => {
    await pool.close();
  });

  it.each(deepRows())(
    "$label: admission accepts the script and rejects it failing on its own, then admits a shallow spend",
    async ({ accepting, failing }) => {
      await expectAdmission([accepting(0x61)], [failing(0x62)]);
      await expectAdmission([shallowSpend(0x63)], []);
    },
    120_000,
  );

  it.each([
    { shape: "list", depth: 3_000 },
    { shape: "map", depth: 3_000 },
    { shape: "list", depth: deepestRedeemerDepth("list") },
    { shape: "constr", depth: deepestRedeemerDepth("constr") },
    { shape: "map", depth: deepestRedeemerDepth("map") },
  ] as readonly { shape: DeepShape; depth: number }[])(
    "a worker and the main thread compute the same verdict for a $shape redeemer $depth deep",
    async ({ shape, depth }) => {
      const redeemer = deepCanonicalData(shape, depth);
      const spend = deepSpend(0x64, walkingProgram(), { redeemer });
      requests.length = 0;
      await expectAdmission([spend], []);
      expect(requests).toHaveLength(1);
      const request = requests[0]!;
      const response = await pool.submit({
        kind: "cek_verdict",
        jobId: pool.allocateJobId(),
        request,
      });
      expect(response.kind).toBe("cek_verdict");
      if (response.kind !== "cek_verdict") return;
      expect(response.result.kind).toBe("accepted");
      expect(evaluateMidgardCekVerdict(request)).toStrictEqual(response.result);
    },
    120_000,
  );
});
