import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  MIDGARD_CEK_GATED_SET_V1,
  type MidgardCekGatedSet,
} from "@al-ft/midgard-validation";
import { Effect, Logger } from "effect";
import { describe, expect, it } from "vitest";

import {
  batchPreState,
  plainSpend,
  plutusCountdownSpend,
  type WallCapCandidate,
} from "../../midgard-validation/tests/phase-b.script-wall-cap.fixture.js";
import {
  ADMISSION_REJECT_CODE_SCRIPT_OUTSIDE_GATED_SET,
  decideAdmissionBatch,
} from "../src/fibers/tx-queue-processor.classify-plutus-evaluation-failure.js";

/**
 * At admission, a transaction whose scripts reach a builtin or machine arm
 * outside the gated set is refused under the node's own code, and the rest
 * of the batch is decided without it.
 */

const ITERATIONS = 20;
const ADD_INTEGER = 0;

const hex = (entry: WallCapCandidate) =>
  entry.candidate.ledgerTx.txId.toString("hex");

const run = <A, E>(effect: Effect.Effect<A, E>) =>
  Effect.runPromise(
    effect.pipe(Effect.provide(Logger.remove(Logger.defaultLogger))),
  );

describe("submit gated set", () => {
  const script = plutusCountdownSpend(0xa1, ITERATIONS);
  const plain = plainSpend(0xa2);
  const decide = (cekGatedSet: MidgardCekGatedSet) =>
    run(
      decideAdmissionBatch({
        phaseA: {
          accepted: [script.candidate, plain.candidate],
          rejected: [],
        },
        pendingWithdrawalOutRefHexes: new Set(),
        ledgerState: batchPreState([script, plain]),
        phaseBConfig: {
          nowCardanoSlotNo: 100n,
          l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
          bucketConcurrency: 1,
          enforceScriptBudget: true,
          cekGatedSet,
        },
      }),
    );

  it("admits both transactions under gated set v1", async () => {
    const { phaseB, allRejected } = await decide(MIDGARD_CEK_GATED_SET_V1);
    expect(allRejected).toStrictEqual([]);
    expect(
      phaseB.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")).sort(),
    ).toStrictEqual([hex(script), hex(plain)].sort());
  });

  it("refuses the transaction outside the set and admits the rest", async () => {
    const { phaseB, allRejected } = await decide({
      ...MIDGARD_CEK_GATED_SET_V1,
      builtins: MIDGARD_CEK_GATED_SET_V1.builtins.filter(
        (tag) => tag !== ADD_INTEGER,
      ),
    });
    expect(
      allRejected.map((rejection) => [
        rejection.txId.toString("hex"),
        rejection.code,
      ]),
    ).toStrictEqual([
      [hex(script), ADMISSION_REJECT_CODE_SCRIPT_OUTSIDE_GATED_SET],
    ]);
    expect(
      phaseB.accepted.map((tx) => tx.ledgerTx.txId.toString("hex")),
    ).toStrictEqual([hex(plain)]);
  });
});
