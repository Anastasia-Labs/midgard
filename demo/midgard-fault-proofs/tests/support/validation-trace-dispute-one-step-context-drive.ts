/** The validation-trace dispute runner at a CEK context step, on the
 * emulator, over a deployment that installs the context step resolver, its
 * control, settle and the stages the runner drives. The successor's thread
 * disputes a context step of our trace that a single stage verifies: the
 * runner plays it through the CEK resolver to the context step resolver,
 * which binds the step's witness and evidence, through the context control,
 * which routes it to the step's stage, through that stage, which verifies
 * the step, and through the context settle to the award; the awarded thread
 * closes into our case. A move whose fee fits under the per-move cap is paid
 * by its thread, and one over the cap by our wallet. Each stage's test file
 * pins the stage's refusal of a successor other than the one it derives. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { CML, coreToTxOutput, Data, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { validationTraceMoveWindow } from "../../src/validation-trace-dispute/move-builders.js";
import {
  completeValidationTraceMove,
  VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE,
} from "../../src/validation-trace-dispute/move-fee.js";
import {
  buildValidationTraceHop,
  validationTraceHop,
} from "../../src/validation-trace-dispute/one-step-builders.js";
import type { DrivenCekContextStage } from "../../src/validation-trace-dispute/one-step-context.js";
import {
  constr,
  stepDatum,
} from "../../src/validation-trace-dispute/one-step-data.js";
import {
  CEK_CONTEXT_STAGE_HOLDERS,
  validationTraceOneStepHolder,
} from "../../src/validation-trace-dispute/one-step-holders.js";
import {
  planValidationTraceOneStep,
  type ValidationTraceContextStepPlan,
} from "../../src/validation-trace-dispute/one-step-plan.js";
import {
  defendTwoThreads,
  threadClosed,
  type TwoThreadCase,
} from "./validation-trace-dispute-one-step-case.js";
import {
  playThreads,
  type PreparedMove,
  threadPlace,
} from "./validation-trace-dispute-one-step-play.js";
import { reportFit } from "./validation-trace-dispute-scripted-defender.js";

/** The last agreed state before the first step of the fixture's trace that
 * each driven stage verifies. */
export const CONTEXT_STAGE_STEP_FROM = Object.freeze({
  redeemerBegin: 94,
  reference: 97,
  spend: 98,
  output: 100,
  signer: 102,
  mintInit: 105,
  assemble: 113,
  txInfo: 114,
  seed: 115,
} as const satisfies Readonly<Record<DrivenCekContextStage, number>>);

const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The drive of the successor's thread through the context step at
 * `stage`'s first step of our trace. */
export const contextStageDrive = (stage: DrivenCekContextStage) => {
  const from = CONTEXT_STAGE_STEP_FROM[stage];
  const holder = CEK_CONTEXT_STAGE_HOLDERS[stage];
  let dispute: TwoThreadCase;
  /** The successor thread's moves past the game, and its awarded close. */
  let moves: PreparedMove[];

  /** The body of a transaction the emulator accepted. */
  const bodyOf = (hash: string) =>
    CML.Transaction.from_cbor_hex(
      dispute.scene.l1.signedCbors.get(hash)!,
    ).body();

  /** The index of the output of `cbor` that carries the successor's thread. */
  const threadOutput = (cbor: string) => {
    const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
    for (let index = 0; index < outputs.len(); index += 1)
      if (coreToTxOutput(outputs.get(index)).assets[dispute.b.unit] === 1n)
        return index;
    return undefined;
  };

  /** The address the successor's thread is paid to by `move`. */
  const paidTo = (move: PreparedMove) =>
    coreToTxOutput(bodyOf(move.hash).outputs().get(threadOutput(move.cbor)!))
      .address;

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

  return {
    /** Plays both threads until the successor's stands at the stage. */
    setUp: async () => {
      dispute = await defendTwoThreads({
        withRemoval: false,
        successorFrom: from,
        contexts: true,
      });
      const { scene, runner, a, b } = dispute;
      await playThreads({
        scene,
        runner,
        sides: [a, b],
        until: async () => (await placeOfB()).holder === holder,
        limit: 80,
      });
    },

    /** Our plan routes the thread's step to the stage. */
    routes: async () => {
      const plan = await contextPlan((await placeOfB()).utxo);
      expect(plan.stateIndex).toBe(from);
      expect(plan.context.stage).toBe(stage);
    },

    /**
     * The stage's hop with a successor that carries the step's own context,
     * unchanged, in place of the one the stage derives; and a check that the
     * thread has not moved.
     */
    unchangedSuccessor: async () => {
      const { scene, deployment, b } = dispute;
      const before = await placeOfB();
      const plan = await contextPlan(before.utxo);
      const lucid = scene.h.proverLucid;
      const prover = scene.h.proverSigner.paymentKeyHash;
      const honest = validationTraceHop({
        plan,
        holder,
        utxo: before.utxo,
        prover,
      });
      const staged = plutusConstrFieldCbor(before.utxo.datum!, [1, 0]);
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
        holder,
      ).reference!;
      const window = validationTraceMoveWindow(lucid);
      return {
        build: async () =>
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
                address: validationTraceOneStepHolder(
                  deployment,
                  "contextSettle",
                ).address,
                prover,
                fee,
                ...window,
              }),
          }),
        unmoved: async () =>
          expect(outRefOf((await placeOfB()).utxo)).toBe(outRefOf(before.utxo)),
      };
    },

    /** Plays the thread through the resolution to the award and its close. */
    awards: async () => {
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
        ({ stage: moved, cbor }) =>
          (moved === "enter_resolution" || moved === "resolve") &&
          threadOutput(cbor) !== undefined,
      );
      expect(moves.map(({ stage: moved }) => moved)).toStrictEqual([
        "enter_resolution",
        ...Array<string>(6).fill("resolve"),
      ]);
      // Each resolution move sends the thread to the next holder, the last
      // to the award.
      for (const [index, next] of (
        [
          "prepare",
          "context",
          "contextControl",
          holder,
          "contextSettle",
        ] as const
      ).entries())
        expect(paidTo(moves[index + 1]!)).toBe(
          validationTraceOneStepHolder(deployment, next).address,
        );
      const last = moves[6]!;
      expect(paidTo(last)).toBe(scene.chain.award.spendingScriptAddress);
      const close = prepared.find(
        ({ stage: moved, consumes }) =>
          moved === "close_awarded" &&
          consumes === `${last.hash}#${threadOutput(last.cbor)!.toString()}`,
      );
      expect(close).toBeDefined();
      for (const [index, label] of [
        "EnterResolution",
        "PrepareResolution",
        "PrepareSelected context",
        "ContextBound",
        "ContextStaged",
        `ContextVerified ${stage}`,
        "ContextSettle",
      ].entries())
        reportFit(label, [moves[index]!.cbor]);
      reportFit("CloseAwardedThread", [close!.cbor]);
    },

    /** Each move's fee is paid by its thread when it fits under the cap, and
     * by our wallet when it does not. */
    pays: () => {
      for (const move of moves) {
        const body = bodyOf(move.hash);
        const fee = body.fee();
        const [hash, index] = move.consumes!.split("#");
        const thread = bodyOf(hash!)
          .outputs()
          .get(Number(index))
          .amount()
          .coin();
        const kept = body
          .outputs()
          .get(threadOutput(move.cbor)!)
          .amount()
          .coin();
        if (fee > VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE) {
          expect(body.inputs().len()).toBeGreaterThan(1);
          expect(kept).toBe(thread);
        } else {
          expect(body.inputs().len()).toBe(1);
          expect(kept).toBe(thread - fee);
        }
      }
    },
  };
};
