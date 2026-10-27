import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.js";
import {
  evaluateMidgardCekVerdict,
  evaluateMidgardTxRedeemers,
  type LocalScriptEvalResult,
  MidgardRedeemerTag,
  RejectCodes,
  runPhaseBValidationWithPatch,
} from "../src/index.js";
import {
  CEK_CONSTR_DATA_INDEX_PROGRAMS,
  compileMidgardCekGoldenProgram,
} from "./fixtures/cek-core-step-v1.vectors.mjs";
import {
  EVALUATE_DRAFT_SLOT as SLOT,
  EVALUATE_DRAFT_SPENT,
  makeEvaluateDraft as draft,
  makeEvaluateProgramDraft,
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

  it("charges Cardano's budget, startup cost included, and no unit less", async () => {
    // Cardano's budget for this program, measured with `aiken uplc eval` (the
    // pinned evaluator, PlutusV3 cost model) on the program applied to
    // `Constr 0 []`. The program's cost does not depend on the context beyond
    // its constructor tag, and that budget includes the 100 cpu / 100 memory
    // Cardano spends when an evaluation starts.
    const cardano = { memory: 2_868n, cpu: 817_918n };
    const evaluation = await evaluate(draft([0n, 0n]));
    expect(evaluation.kind).toBe("evaluated");
    if (evaluation.kind !== "evaluated") return;
    expect(evaluation.redeemers).toEqual([
      {
        tag: MidgardRedeemerTag.Spend,
        index: 0n,
        memory: cardano.memory,
        steps: cardano.cpu,
      },
    ]);

    const exact = await phaseB(draft([cardano.memory, cardano.cpu]));
    expect(exact.result.accepted).toHaveLength(1);
    expect(exact.spent).toEqual([{ kind: "accepted", budget: cardano }]);

    for (const short of [
      [cardano.memory - 1n, cardano.cpu],
      [cardano.memory, cardano.cpu - 1n],
    ] as const) {
      const refused = await phaseB(draft(short));
      expect(refused.result.accepted).toHaveLength(0);
      expect(refused.result.rejected[0]?.code).toBe(
        RejectCodes.PlutusScriptInvalid,
      );
    }
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

  it.each([
    ["zero", true],
    ["maximum", true],
    ["overflow", false],
    ["negative", false],
  ] as const)(
    "pins the constrData %s index verdict",
    async (boundary, accepts) => {
      const definition = CEK_CONSTR_DATA_INDEX_PROGRAMS.find(
        (program) => program.label === `constr_data_index_${boundary}`,
      );
      if (definition === undefined)
        throw new Error(`missing ${boundary} program`);
      const scriptFlat = compileMidgardCekGoldenProgram(
        definition.term(),
        encodeMidgardCekCardanoFlatProgram,
      );
      const checked = await phaseB(
        makeEvaluateProgramDraft([20_000n, 5_000_000n], scriptFlat),
      );
      expect(checked.result.accepted).toHaveLength(accepts ? 1 : 0);
      expect(checked.result.rejected).toHaveLength(accepts ? 0 : 1);
      if (!accepts) {
        expect(checked.result.rejected[0]?.code).toBe(
          RejectCodes.PlutusScriptInvalid,
        );
        expect(checked.spent).toMatchObject([{ kind: "script_invalid" }]);
      }
    },
  );

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
