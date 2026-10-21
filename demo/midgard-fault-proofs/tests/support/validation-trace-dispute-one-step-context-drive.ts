/** The validation-trace dispute runner at a CEK context step, on the
 * emulator, over a deployment that installs the context step resolver, its
 * control, settle, the stages the runner drives and the holders of the
 * redeemer item step. The successor's thread disputes a context step of our
 * trace: the runner plays it through the CEK resolver to the context step
 * resolver, which binds the step's witness and evidence, through the context
 * control, which routes it to the step's stage, through that stage, which
 * verifies the step, alone or through the redeemer item step's holders, and
 * through the context settle to the award; the awarded thread closes into
 * our case. A move whose fee fits under the per-move cap is paid by its
 * thread, and one over the cap by our wallet. Each single-hop stage's test
 * file pins the stage's refusal of a successor other than the one it
 * derives; the multi-hop stages pin their own
 * (validation-trace-dispute-one-step-context-item-drive.ts,
 * validation-trace-dispute-one-step-context-observer-drive.ts,
 * validation-trace-dispute-one-step-context-redeemer-select-drive.ts,
 * validation-trace-dispute-one-step-context-finalize-drive.ts). */
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
  type ValidationTraceHop,
  validationTraceHop,
} from "../../src/validation-trace-dispute/one-step-builders.js";
import type { DrivenCekContextStage } from "../../src/validation-trace-dispute/one-step-context.js";
import {
  constr,
  stepDatum,
} from "../../src/validation-trace-dispute/one-step-data.js";
import {
  CEK_CONTEXT_STAGE_HOLDERS,
  type ValidationTraceOneStepHolder,
  validationTraceOneStepHolder,
} from "../../src/validation-trace-dispute/one-step-holders.js";
import {
  planValidationTraceOneStep,
  type ValidationTraceContextStepPlan,
} from "../../src/validation-trace-dispute/one-step-plan.js";
import { expectWithinWalletMoveShape } from "./validation-trace-dispute-move-fees.js";
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

/** A driven stage that verifies its step in one hop. */
export type SingleHopCekContextStage = Exclude<
  DrivenCekContextStage,
  | "itemBind"
  | "observerAuthenticate"
  | "redeemerSelectAuthenticate"
  | "finalizeAuthenticate"
>;

/** The last agreed state before the first step of the fixture's trace that
 * each single-hop stage verifies. */
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
} as const satisfies Readonly<Record<SingleHopCekContextStage, number>>);

const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * The drive of the successor's thread through the context step after our
 * trace's state `from`, to `holder`, past the context control: the thread
 * goes on along `path` to the context settle, each resolution move labelled
 * by `labels` in its fit.
 */
export const contextStepDrive = ({
  from,
  holder,
  path,
  labels,
}: {
  readonly from: number;
  readonly holder: ValidationTraceOneStepHolder;
  /** The holders the thread visits from `holder` up to the context settle. */
  readonly path: readonly ValidationTraceOneStepHolder[];
  /** The fit label of each move from `holder` up to the context settle. */
  readonly labels: readonly string[];
}) => {
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

  /**
   * The move of the successor's thread, standing at `holder`, by `hop` to
   * `next`, and a check that the thread has not moved.
   */
  const hopFromPlace = async (
    hop: (
      before: Awaited<ReturnType<typeof placeOfB>>,
      plan: ValidationTraceContextStepPlan,
    ) => ValidationTraceHop,
    next: ValidationTraceOneStepHolder,
  ) => {
    const { scene, deployment, b } = dispute;
    const before = await placeOfB();
    const plan = await contextPlan(before.utxo);
    const lucid = scene.h.proverLucid;
    const prover = scene.h.proverSigner.paymentKeyHash;
    const reference = validationTraceOneStepHolder(
      deployment,
      holder,
    ).reference!;
    expect(before.holder).toBe(holder);
    const built = hop(before, plan);
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
              hop: built,
              address: validationTraceOneStepHolder(deployment, next).address,
              prover,
              fee,
              ...window,
            }),
        }),
      unmoved: async () =>
        expect(outRefOf((await placeOfB()).utxo)).toBe(outRefOf(before.utxo)),
    };
  };

  return {
    dispute: () => dispute,
    placeOfB,
    contextPlan,
    hopFromPlace,
    /** Plays both threads until the successor's stands at `holder`. */
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
        limit: 40 + 2 * path.length,
      });
      moves = prepared.filter(
        ({ stage: moved, cbor }) =>
          (moved === "enter_resolution" || moved === "resolve") &&
          threadOutput(cbor) !== undefined,
      );
      const visits: readonly ValidationTraceOneStepHolder[] = [
        "prepare",
        "context",
        "contextControl",
        holder,
        ...path,
        "contextSettle",
      ];
      expect(moves.map(({ stage: moved }) => moved)).toStrictEqual([
        "enter_resolution",
        ...Array<string>(visits.length + 1).fill("resolve"),
      ]);
      // Each resolution move sends the thread to the next holder, the last
      // to the award.
      for (const [index, next] of visits.entries())
        expect(paidTo(moves[index + 1]!)).toBe(
          validationTraceOneStepHolder(deployment, next).address,
        );
      const last = moves.at(-1)!;
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
        ...labels,
        "ContextSettle",
      ].entries())
        reportFit(label, [moves[index]!.cbor]);
      reportFit("CloseAwardedThread", [close!.cbor]);
    },

    /** Each move's fee is paid by its thread when it fits under the cap, and
     * by our wallet when it does not; every move the runner landed fits the
     * move the wallet-move reserve is priced at. */
    pays: async () => {
      await expectWithinWalletMoveShape(
        dispute.scene,
        dispute.prepared
          .map(({ hash }) => hash)
          .filter((hash) => dispute.scene.l1.signedCbors.has(hash)),
      );
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

/** The drive of the successor's thread through the context step of `stage`
 * after our trace's state `from`, by default the stage's first. */
export const contextStageDrive = (
  stage: SingleHopCekContextStage,
  from: number = CONTEXT_STAGE_STEP_FROM[stage],
) => {
  const holder = CEK_CONTEXT_STAGE_HOLDERS[stage];
  const drive = contextStepDrive({
    from,
    holder,
    path: [],
    labels: [`ContextVerified ${stage}`],
  });
  return {
    setUp: drive.setUp,
    /** Our plan routes the thread's step to the stage. */
    routes: async () => {
      const plan = await drive.contextPlan((await drive.placeOfB()).utxo);
      expect(plan.stateIndex).toBe(from);
      expect(plan.context.stage).toBe(stage);
    },

    /**
     * The stage's hop with a successor that carries the step's own context,
     * unchanged, in place of the one the stage derives; and a check that the
     * thread has not moved.
     */
    unchangedSuccessor: async () => {
      const prover = drive.dispute().scene.h.proverSigner.paymentKeyHash;
      return await drive.hopFromPlace((before, plan) => {
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
        return {
          ...honest,
          datum: stepDatum(prover, constr(0, [staged, unchanged])),
        };
      }, "contextSettle");
    },

    awards: drive.awards,
    pays: drive.pays,
  };
};
