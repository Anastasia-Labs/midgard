import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  MIDGARD_CEK_GATED_SET_V1,
  RejectCodes,
} from "@al-ft/midgard-validation";
import { Builtin } from "@harmoniclabs/uplc";
import { Effect, Logger } from "effect";
import { describe, expect, it } from "vitest";

import {
  batchPreState,
  plainSpend,
  plutusCountdownSpend,
  type WallCapCandidate,
} from "../../midgard-validation/tests/phase-b.script-wall-cap.fixture.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeOutput,
  makePhaseBCandidate,
  outRefFromTxId,
} from "../../midgard-validation/tests/validation-fixtures.js";
import {
  ADMISSION_REJECT_CODE_SCRIPT_OUTSIDE_GATED_SET,
  decideAdmissionBatch,
} from "../src/fibers/tx-queue-processor.classify-plutus-evaluation-failure.js";

/**
 * Local refusals at admission are decided in the same pass as the rest of
 * the batch: every script of the batch runs once, however many candidates
 * the node refuses, and the other candidates are decided in their own order
 * as if the refused ones were absent.
 */

const ITERATIONS = 20;
const MULTIPLY_INTEGER = 2;

/** Gated set v1 without `multiplyInteger`. */
const GATED_SET = {
  ...MIDGARD_CEK_GATED_SET_V1,
  builtins: MIDGARD_CEK_GATED_SET_V1.builtins.filter(
    (tag) => tag !== MULTIPLY_INTEGER,
  ),
};

/**
 * A transaction spending one output locked by a script that counts down,
 * applying `step` at each count, plus the plain inputs the candidates in
 * `alsoSpending` spend. The script runs inside the gated set unless `step` is
 * `multiplyInteger`.
 */
const scriptSpend = (
  seed: number,
  step: Builtin,
  alsoSpending: readonly WallCapCandidate[] = [],
): WallCapCandidate =>
  plutusCountdownSpend(seed, ITERATIONS, { step, alsoSpending });

/** A plain transaction spending the first output of `parent`. */
const childOf = (parent: WallCapCandidate): WallCapCandidate => ({
  candidate: makePhaseBCandidate({
    spent: [outRefFromTxId(parent.candidate.ledgerTx.txId)],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
  }),
  preState: [],
});

const hex = (entry: WallCapCandidate) =>
  entry.candidate.ledgerTx.txId.toString("hex");

const decide = (entries: readonly WallCapCandidate[]) => {
  const runs = new Map<string, number>();
  return Effect.runPromise(
    decideAdmissionBatch({
      phaseA: {
        accepted: entries.map((entry) => entry.candidate),
        rejected: [],
      },
      pendingWithdrawalOutRefHexes: new Set(),
      ledgerState: batchPreState(entries),
      phaseBConfig: {
        nowCardanoSlotNo: 100n,
        l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
        bucketConcurrency: 1,
        enforceScriptBudget: true,
        cekGatedSet: GATED_SET,
        onScriptEvaluated: (txId) => {
          const key = txId.toString("hex");
          runs.set(key, (runs.get(key) ?? 0) + 1);
        },
      },
    }).pipe(
      Effect.map((decision) => ({
        accepted: decision.phaseB.accepted.map((tx) =>
          tx.ledgerTx.txId.toString("hex"),
        ),
        rejected: decision.allRejected.map((rejection) => [
          rejection.txId.toString("hex"),
          rejection.code,
        ]),
        runs,
      })),
      Effect.provide(Logger.remove(Logger.defaultLogger)),
    ),
  );
};

describe("admission work under local refusals", () => {
  it("runs each script of the batch once however many candidates are refused", async () => {
    const admitted = [0x10, 0x11, 0x12, 0x13].map((seed) =>
      scriptSpend(seed, Builtin.addInteger),
    );
    const refused = [0x20, 0x21, 0x22, 0x23].map((seed) =>
      scriptSpend(seed, Builtin.multiplyInteger),
    );
    const decision = await decide([...admitted, ...refused]);
    expect([...decision.runs.values()]).toStrictEqual(
      [...admitted, ...refused].map(() => 1),
    );
    expect(decision.accepted.sort()).toStrictEqual(admitted.map(hex).sort());
    expect(decision.rejected.sort()).toStrictEqual(
      refused
        .map((entry) => [
          hex(entry),
          ADMISSION_REJECT_CODE_SCRIPT_OUTSIDE_GATED_SET,
        ])
        .sort(),
    );
  });

  it("admits a transaction that spends an input of a refused one as if the refused one were absent", async () => {
    const shared = plainSpend(0x30);
    const refused = scriptSpend(0x31, Builtin.multiplyInteger, [shared]);
    const decision = await decide([refused, shared]);
    expect(decision.accepted).toStrictEqual([hex(shared)]);
    expect(decision.rejected).toStrictEqual([
      [hex(refused), ADMISSION_REJECT_CODE_SCRIPT_OUTSIDE_GATED_SET],
    ]);
  });

  it("rejects a transaction spending an output of a refused one as depending on it", async () => {
    const refused = scriptSpend(0x40, Builtin.multiplyInteger);
    const child = childOf(refused);
    const admitted = scriptSpend(0x41, Builtin.addInteger);
    const decision = await decide([refused, child, admitted]);
    expect(decision.accepted).toStrictEqual([hex(admitted)]);
    expect(decision.rejected.sort()).toStrictEqual(
      [
        [hex(refused), ADMISSION_REJECT_CODE_SCRIPT_OUTSIDE_GATED_SET],
        [hex(child), RejectCodes.DependsOnRejectedTx],
      ].sort(),
    );
    expect(decision.runs.get(hex(admitted))).toBe(1);
  });
});
