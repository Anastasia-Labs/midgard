import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core";
import {
  encodeMidgardTxOutput,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  Application,
  Builtin,
  Delay,
  ErrorUPLC,
  Force,
  getNRequiredForces,
  Lambda,
  UPLCBuiltinTag,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  type UPLCTerm,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildMidgardCanonicalCekProgram,
  evaluateMidgardCekVerdict,
  evaluateMidgardTxRedeemers,
  type LocalScriptEvalResult,
  MidgardRedeemerTag,
  RejectCodes,
  runPhaseBValidationWithPatch,
} from "../src/index.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeOutput,
  makePhaseBCandidate,
  makeRedeemersCbor,
  outRefFromByte,
  plutusV3ScriptWitness,
} from "./validation-fixtures.js";

// The evaluate path must report exactly the budget Phase B spends once the
// transaction declares it.

const builtin = (tag: UPLCBuiltinTag): UPLCTerm => {
  let term: UPLCTerm = new Builtin(tag);
  for (let force = 0; force < getNRequiredForces(tag); force += 1) {
    term = new Force(term);
  }
  return term;
};
const apply = (fn: UPLCTerm, ...args: UPLCTerm[]): UPLCTerm =>
  args.reduce<UPLCTerm>((term, arg) => new Application(term, arg), fn);

/** Succeeds when the context's constructor tag is 0, after some arithmetic. */
const contextTagIsZero = (fails: boolean) => {
  const context = new UPLCVar(0);
  const tag = apply(
    builtin(UPLCBuiltinTag.fstPair),
    apply(builtin(UPLCBuiltinTag.unConstrData), context),
  );
  const body = new Lambda(
    new Force(
      apply(
        builtin(UPLCBuiltinTag.ifThenElse),
        apply(
          builtin(UPLCBuiltinTag.equalsInteger),
          apply(
            builtin(UPLCBuiltinTag.multiplyInteger),
            tag,
            UPLCConst.int(fails ? 1n : 0n),
          ),
          UPLCConst.int(fails ? 1n : 0n),
        ),
        new Delay(UPLCConst.unit),
        new Delay(new ErrorUPLC()),
      ),
    ),
  );
  return Buffer.from(UPLCEncoder.compile(new UPLCProgram([1, 1, 0], body)));
};

const SLOT = 200_000n;
const L1_SLOT_CONFIG = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;

const draft = (exUnits: readonly [bigint, bigint], fails = false) => {
  const program = buildMidgardCanonicalCekProgram(contextTagIsZero(fails));
  const script = plutusV3ScriptWitness(program.envelopeCbor);
  const spent = outRefFromByte(0x5d);
  const spentOutput = encodeMidgardTxOutput({
    address: protectMidgardAddress(
      Buffer.from(
        CML.EnterpriseAddress.new(
          0,
          CML.Credential.new_script(
            CML.ScriptHash.from_hex(hashScriptWitness(script)),
          ),
        )
          .to_address()
          .to_raw_bytes(),
      ),
    ),
    value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
  });
  const candidate = makePhaseBCandidate({
    spent: [spent],
    outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE)],
    validityIntervalStart: SLOT,
    scriptWitnesses: [script],
    redeemerTxWitsPreimageCbor: makeRedeemersCbor([
      { tag: MidgardRedeemerTag.Spend, index: 0n, exUnits: [...exUnits] },
    ]),
    scriptLanguages: ["PlutusV3"],
    programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
      ...program.material.values(),
    ]),
  });
  const state = new Map([[spent.toString("hex"), spentOutput]]);
  return { candidate, state };
};

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
      stateValue: (outRefHex) => tx.state.get(outRefHex),
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
    const evaluation = await evaluate(draft([0n, 0n]));
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
