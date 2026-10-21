/** The validation-trace dispute runner at a CEK context step, on the
 * emulator, over a deployment that installs the context step resolver, its
 * control, settle and the stages the runner drives. The successor's thread
 * disputes the step that begins the selected execution's redeemer scan: the
 * runner plays it through the CEK resolver to the context step resolver,
 * which binds the step's witness and evidence, through the context control,
 * which routes it to the redeemer scan's beginning, through that stage,
 * which verifies the step, and through the context settle to the award; the
 * awarded thread closes into our case. A move whose fee fits under the
 * per-move cap is paid by its thread, and one over the cap by our wallet.
 * The stage refuses a successor other than the one it derives. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { CML, coreToTxOutput, Data, type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { validationTraceMoveWindow } from "../src/validation-trace-dispute/move-builders.js";
import {
  completeValidationTraceMove,
  VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE,
} from "../src/validation-trace-dispute/move-fee.js";
import {
  buildValidationTraceHop,
  validationTraceHop,
} from "../src/validation-trace-dispute/one-step-builders.js";
import {
  constr,
  stepDatum,
} from "../src/validation-trace-dispute/one-step-data.js";
import { validationTraceOneStepHolder } from "../src/validation-trace-dispute/one-step-holders.js";
import {
  planValidationTraceOneStep,
  type ValidationTraceContextStepPlan,
} from "../src/validation-trace-dispute/one-step-plan.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  defendTwoThreads,
  REDEEMER_BEGIN_STEP_FROM,
  threadClosed,
  type TwoThreadCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import {
  playThreads,
  type PreparedMove,
  threadPlace,
} from "./support/validation-trace-dispute-one-step-play.js";
import { reportFit } from "./support/validation-trace-dispute-scripted-defender.js";

const STAGE = "contextRedeemerBegin";

let dispute: TwoThreadCase;
/** The successor thread's moves past the game, and its awarded close. */
let moves: PreparedMove[];
let close: PreparedMove | undefined;

const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The body of a transaction the emulator accepted. */
const bodyOf = (hash: string) =>
  CML.Transaction.from_cbor_hex(dispute.scene.l1.signedCbors.get(hash)!).body();

/** The lovelace of output `outRef` of a transaction the emulator accepted. */
const lovelaceAt = (outRef: string) => {
  const [hash, index] = outRef.split("#");
  return bodyOf(hash!).outputs().get(Number(index)).amount().coin();
};

/** The index of the output of `cbor` that carries the successor's thread. */
const threadOutput = (cbor: string) => {
  const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
  for (let index = 0; index < outputs.len(); index += 1)
    if (coreToTxOutput(outputs.get(index)).assets[dispute.b.unit] === 1n)
      return index;
  return undefined;
};

/** The successor's thread where it stands. */
const placeOfB = async () => {
  const found = await threadPlace(
    dispute.scene,
    dispute.deployment,
    dispute.b.unit,
  );
  if (found === undefined) throw new Error("the thread is not live");
  return found;
};

/** Our plan of the context step, by the successor hash the stage keeps. */
const contextPlan = async (
  utxo: UTxO,
): Promise<ValidationTraceContextStepPlan> => {
  const planning = await planValidationTraceOneStep({
    material: dispute.material,
    challengerHighHash: Data.from<string>(
      plutusConstrFieldCbor(utxo.datum!, [1, 0, 0, 0, 1, 3]),
    ),
    contexts: true,
  });
  if (planning.kind !== "plan" || planning.plan.kind !== "context")
    throw new Error("the thread's step is not a CEK context step");
  return planning.plan;
};

describe("validation-trace dispute runner: driving a thread at a CEK context step", () => {
  beforeAll(async () => {
    dispute = await defendTwoThreads({
      withRemoval: false,
      successorFrom: REDEEMER_BEGIN_STEP_FROM,
      contexts: true,
    });
    const { scene, runner, a, b } = dispute;
    await playThreads({
      scene,
      runner,
      sides: [a, b],
      until: async () => (await placeOfB()).holder === STAGE,
      limit: 80,
    });
  }, 1_800_000);

  it("routes the step that begins the redeemer scan to its stage", async () => {
    const plan = await contextPlan((await placeOfB()).utxo);
    expect(plan.stateIndex).toBe(REDEEMER_BEGIN_STEP_FROM);
    expect(plan.context.stage).toBe("redeemerBegin");
  }, 300_000);

  it("refuses a successor other than the one the stage derives, at the stage", async () => {
    const { scene, deployment, b } = dispute;
    const before = await placeOfB();
    const plan = await contextPlan(before.utxo);
    const lucid = scene.h.proverLucid;
    const prover = scene.h.proverSigner.paymentKeyHash;
    const honest = validationTraceHop({
      plan,
      holder: STAGE,
      utxo: before.utxo,
      prover,
    });
    const staged = plutusConstrFieldCbor(before.utxo.datum!, [1, 0]);
    // The honest move's successor carries the context the step derives; this
    // one carries the step's own context, unchanged.
    const unchanged = constr(0, [plan.context.contextCbor]);
    expect(unchanged).not.toBe(plan.context.successorCbor);
    expect(honest.datum).toBe(
      stepDatum(prover, constr(0, [staged, plan.context.successorCbor])),
    );
    const hop = {
      ...honest,
      datum: stepDatum(prover, constr(0, [staged, unchanged])),
    };
    const reference = validationTraceOneStepHolder(
      deployment,
      STAGE,
    ).reference!;
    const window = validationTraceMoveWindow(lucid);
    await expectOnchainRefusal(
      async () =>
        await completeValidationTraceMove({
          lucid,
          stage: "resolve",
          referenced: [reference],
          build: (fee) =>
            buildValidationTraceHop({
              lucid,
              thread: before.utxo,
              unit: b.unit,
              reference,
              hop,
              address: validationTraceOneStepHolder(deployment, "contextSettle")
                .address,
              prover,
              fee,
              ...window,
            }),
        }),
      {
        refusedBy: "fraud_proofs/validation_trace/cek_context_redeemer_begin",
        check: /^Validator returned false$/u,
      },
    );
    expect(outRefOf((await placeOfB()).utxo)).toBe(outRefOf(before.utxo));
  }, 600_000);

  it("moves the thread through the context step's resolution to the award, and closes it", async () => {
    const { scene, deployment, runner, prepared, a, b } = dispute;
    await playThreads({
      scene,
      runner,
      sides: [a, b],
      until: async () =>
        (await threadClosed(dispute, a.unit)) &&
        (await threadClosed(dispute, b.unit)),
      limit: 40,
    });
    moves = prepared.filter(
      ({ stage, cbor }) =>
        (stage === "enter_resolution" || stage === "resolve") &&
        threadOutput(cbor) !== undefined,
    );
    expect(moves.map(({ stage }) => stage)).toStrictEqual([
      "enter_resolution",
      ...Array<string>(6).fill("resolve"),
    ]);
    const holders = [
      "prepare",
      "context",
      "contextControl",
      STAGE,
      "contextSettle",
    ] as const;
    // Each resolution move sends the thread to the next holder, the last to
    // the award.
    for (const [index, holder] of holders.entries()) {
      const move = moves[index + 1]!;
      expect(
        coreToTxOutput(
          bodyOf(move.hash).outputs().get(threadOutput(move.cbor)!),
        ).address,
      ).toBe(validationTraceOneStepHolder(deployment, holder).address);
    }
    const last = moves[6]!;
    expect(
      coreToTxOutput(bodyOf(last.hash).outputs().get(threadOutput(last.cbor)!))
        .address,
    ).toBe(scene.chain.award.spendingScriptAddress);
    close = prepared.find(
      ({ stage, consumes }) =>
        stage === "close_awarded" &&
        consumes === `${last.hash}#${threadOutput(last.cbor)!.toString()}`,
    );
    expect(close).toBeDefined();
    for (const [index, label] of [
      "EnterResolution",
      "PrepareResolution",
      "PrepareSelected context",
      "ContextBound",
      "ContextStaged",
      "ContextVerified redeemerBegin",
      "ContextSettle",
    ].entries())
      reportFit(label, [moves[index]!.cbor]);
    reportFit("CloseAwardedThread", [close!.cbor]);
  }, 1_200_000);

  it("pays each move from its thread when its fee fits under the cap, and from our wallet when it does not", () => {
    for (const move of moves) {
      const body = bodyOf(move.hash);
      const fee = body.fee();
      const thread = lovelaceAt(move.consumes!);
      const kept = body.outputs().get(threadOutput(move.cbor)!).amount().coin();
      if (fee > VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE) {
        expect(body.inputs().len()).toBeGreaterThan(1);
        expect(kept).toBe(thread);
      } else {
        expect(body.inputs().len()).toBe(1);
        expect(kept).toBe(thread - fee);
      }
    }
  });
});
