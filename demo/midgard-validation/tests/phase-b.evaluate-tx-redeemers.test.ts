import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  evaluateMidgardCekVerdict,
  evaluateMidgardTxRedeemers,
  type LocalScriptEvalResult,
  MidgardRedeemerTag,
  RejectCodes,
  runPhaseBValidationWithPatch,
} from "../src/index.js";
import {
  EVALUATE_DRAFT_SLOT as SLOT,
  EVALUATE_DRAFT_SPENT,
  makeEvaluateDraft as draft,
} from "./phase-b.evaluate-tx-redeemers.fixture.js";

// The evaluate path must report exactly the budget Phase B spends once the
// transaction declares it.

const L1_SLOT_CONFIG = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;

const stateReads: (readonly string[])[] = [];

const evaluate = (
  tx: ReturnType<typeof draft>,
  hooks: {
    readonly evaluateCekVerdict?: Parameters<
      typeof evaluateMidgardTxRedeemers
    >[0]["config"]["evaluateCekVerdict"];
  } = {},
) =>
  Effect.runPromise(
    evaluateMidgardTxRedeemers({
      txCbor: tx.candidate.submission.txCbor,
      programMaterialSidecarCbor:
        tx.candidate.submission.programMaterialSidecarCbor!,
      readState: (outRefHexes) =>
        Effect.sync(() => {
          stateReads.push(outRefHexes);
          return tx.state;
        }),
      expectedNetworkId: 0n,
      limits: MIDGARD_CONSENSUS_LIMITS,
      config: { l1SlotConfig: L1_SLOT_CONFIG, ...hooks },
    }),
  );

const phaseB = async (tx: ReturnType<typeof draft>) => {
  const spent: LocalScriptEvalResult[] = [];
  const result = await Effect.runPromise(
    runPhaseBValidationWithPatch([tx.candidate], tx.state, {
      l1SlotConfig: L1_SLOT_CONFIG,
      nowCardanoSlotNo: SLOT + 100n,
      bucketConcurrency: 1,
      onScriptEvaluated: (_txId, evaluation) => spent.push(evaluation.result),
    }),
  );
  return { result, spent };
};

describe("evaluateMidgardTxRedeemers", () => {
  it("reports the budget Phase B spends once it is declared", async () => {
    stateReads.length = 0;
    const evaluation = await evaluate(draft([0n, 0n]));
    expect(stateReads).toEqual([[EVALUATE_DRAFT_SPENT.toString("hex")]]);
    expect(evaluation.kind).toBe("evaluated");
    if (evaluation.kind !== "evaluated") return;
    expect(evaluation.redeemers).toHaveLength(1);
    const [budget] = evaluation.redeemers;
    expect(budget).toMatchObject({ tag: MidgardRedeemerTag.Spend, index: 0n });
    expect(budget!.memory).toBeGreaterThan(0n);
    expect(budget!.steps).toBeGreaterThan(0n);

    const declared = await phaseB(draft([budget!.memory, budget!.steps]));
    expect(declared.result.accepted).toHaveLength(1);
    expect(declared.spent).toEqual([
      {
        kind: "accepted",
        budget: { memory: budget!.memory, cpu: budget!.steps },
      },
    ]);

    const short = await phaseB(draft([budget!.memory - 1n, budget!.steps]));
    expect(short.result.rejected[0]?.code).toBe(
      RejectCodes.PlutusScriptInvalid,
    );
  });

  it("gives the same budgets through an out-of-process evaluator", async () => {
    let calls = 0;
    const tx = draft([0n, 0n]);
    const pooled = await evaluate(tx, {
      evaluateCekVerdict: (request) =>
        Effect.sync(() => {
          calls += 1;
          return evaluateMidgardCekVerdict(request);
        }),
    });
    expect(calls).toBe(1);
    expect(pooled).toEqual(await evaluate(tx));
  });

  it("reports a failing script", async () => {
    const evaluation = await evaluate(draft([0n, 0n], true));
    expect(evaluation.kind).toBe("failed");
  });

  it("reports an input missing from the state", async () => {
    const tx = draft([0n, 0n]);
    const evaluation = await evaluate({ ...tx, state: new Map() });
    expect(evaluation).toMatchObject({ kind: "failed" });
    if (evaluation.kind === "failed") {
      expect(evaluation.detail).toContain(RejectCodes.InputNotFound);
    }
  });
});
