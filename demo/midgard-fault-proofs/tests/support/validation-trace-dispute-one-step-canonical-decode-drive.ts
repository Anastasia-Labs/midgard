/** The validation-trace dispute runner at a canonical-decode step, on the
 * emulator, over a deployment that installs the canonical-decode resolver,
 * its empty and item semantic resolvers and the item's stages. The
 * successor's thread disputes a canonical-decode step of our trace: the
 * runner plays it through the canonical-decode resolver, which commits the
 * transition alone, to the empty semantic resolver, which awards it, or to
 * the item semantic resolver and on through the item's source, observe,
 * proof and settlement stages to the award; the awarded thread closes into
 * our case. A move whose fee fits under the per-move cap is paid by its
 * thread, and one over the cap by our wallet. */
import { hashMidgardValidationMachineState } from "@al-ft/midgard-core";
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
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
import {
  CANONICAL_DECODE_CHALLENGER_SUCCESSOR_HASH,
  type ValidationTraceCanonicalDecodeHolder,
} from "../../src/validation-trace-dispute/one-step-canonical-decode.js";
import { integer } from "../../src/validation-trace-dispute/one-step-data.js";
import {
  type ValidationTraceOneStepHolder,
  validationTraceOneStepHolder,
} from "../../src/validation-trace-dispute/one-step-holders.js";
import {
  planValidationTraceOneStep,
  type ValidationTraceCanonicalDecodePlan,
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

/** The holders a thread visits past the boundary, and each move's fit
 * label, by the semantic resolver the step selects. */
const ROUTES = Object.freeze({
  empty: {
    path: ["canonicalDecodePrepare", "canonicalDecodeEmpty"],
    labels: ["PrepareSelected canonicalDecode", "VerifyEmpty canonicalDecode"],
  },
  item: {
    path: [
      "canonicalDecodePrepare",
      "canonicalDecodeItem",
      "canonicalDecodeItemSource",
      "canonicalDecodeItemObserve",
      "canonicalDecodeItemProof",
      "canonicalDecodeItemSettlement",
    ],
    labels: [
      "PrepareSelected canonicalDecode",
      "Verify canonicalDecode item",
      "Bind canonicalDecode item source",
      "Observe canonicalDecode item",
      "Verify canonicalDecode item proof",
      "Settle canonicalDecode item",
    ],
  },
} as const satisfies Readonly<
  Record<
    string,
    Readonly<{
      path: readonly ValidationTraceCanonicalDecodeHolder[];
      labels: readonly string[];
    }>
  >
>);

export type CanonicalDecodeKind = keyof typeof ROUTES;

/** The kind of each canonical-decode step of the fixture's trace, by the
 * state it starts from; the step from state 9 is the compact binding's. */
export const CANONICAL_DECODE_STEPS: readonly CanonicalDecodeKind[] =
  Object.freeze([
    "item",
    "empty",
    "item",
    "empty",
    "empty",
    "empty",
    "item",
    "item",
    "item",
  ]);

const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * The drive of the successor's thread through the canonical-decode step
 * after our trace's state `from`, a step of `kind`, set up with the thread
 * standing at `at`.
 */
export const canonicalDecodeDrive = ({
  from,
  kind,
  at,
}: {
  readonly from: number;
  readonly kind: CanonicalDecodeKind;
  readonly at: ValidationTraceCanonicalDecodeHolder;
}) => {
  const { path, labels } = ROUTES[kind];
  let dispute: TwoThreadCase;
  /** The successor thread's moves past the game. */
  let moves: PreparedMove[];

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

  const paidTo = (move: PreparedMove) =>
    coreToTxOutput(bodyOf(move.hash).outputs().get(threadOutput(move.cbor)!))
      .address;

  const placeOfB = async () => {
    const found = await threadPlace(
      dispute.scene,
      dispute.deployment,
      dispute.b.unit,
    );
    if (found === undefined) throw new Error("the thread is not live");
    return found;
  };

  /** Our plan of the step, by the successor hash the holder keeps. */
  const planAt = async (
    holder: ValidationTraceCanonicalDecodeHolder,
    utxo: UTxO,
  ): Promise<ValidationTraceCanonicalDecodePlan> => {
    const planning = await planValidationTraceOneStep({
      material: dispute.material,
      challengerHighHash: Data.from<string>(
        plutusConstrFieldCbor(utxo.datum!, [
          ...CANONICAL_DECODE_CHALLENGER_SUCCESSOR_HASH[holder],
        ]),
      ),
      canonicalDecode: true,
    });
    if (planning.kind !== "plan" || planning.plan.kind !== "canonicalDecode")
      throw new Error("the thread's step is not a canonical-decode step");
    return planning.plan;
  };

  /**
   * The hop of the successor's thread at `at` with its successor datum or
   * its redeemer rewritten by `rewrite`, and a check that the thread has not
   * moved.
   */
  const rewrittenHop = async (
    rewrite: (
      honest: ValidationTraceHop,
      plan: ValidationTraceCanonicalDecodePlan,
    ) => Partial<Pick<ValidationTraceHop, "datum" | "redeemer">>,
  ) => {
    const { scene, deployment, b } = dispute;
    const before = await placeOfB();
    expect(before.holder).toBe(at);
    const plan = await planAt(at, before.utxo);
    const lucid = scene.h.proverLucid;
    const prover = scene.h.proverSigner.paymentKeyHash;
    const reference = validationTraceOneStepHolder(deployment, at).reference!;
    const honest = validationTraceHop({
      plan,
      holder: at,
      utxo: before.utxo,
      prover,
    });
    const rewritten = { ...honest, ...rewrite(honest, plan) };
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
              hop: rewritten,
              address: validationTraceOneStepHolder(deployment, honest.next)
                .address,
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
    /** Plays both threads until the successor's stands at `at`. */
    setUp: async () => {
      dispute = await defendTwoThreads({
        withRemoval: false,
        successorFrom: from,
        canonicalDecode: true,
      });
      const { scene, runner, a, b } = dispute;
      await playThreads({
        scene,
        runner,
        sides: [a, b],
        until: async () => (await placeOfB()).holder === at,
        limit: 80,
      });
    },

    /** Our plan sends the thread's step to the semantic resolver of
     * `kind`, committing the transition alone. */
    routes: async () => {
      const plan = await planAt(at, (await placeOfB()).utxo);
      expect(plan.stateIndex).toBe(from);
      expect(plan.preState.phase).toBe("canonicalDecode");
      expect(plan.semanticResolverIndex).toBe(kind === "empty" ? 0 : 1);
      expect(plan.canonicalDecode.item === undefined).toBe(kind === "empty");
      // An item step's evidence carries the field item, which the resolver
      // does not commit; an empty step's carries none.
      if (kind === "empty")
        expect(plan.canonicalDecode.preparedEvidenceHash).toBe(
          plan.evidenceHash,
        );
      else
        expect(plan.canonicalDecode.preparedEvidenceHash).not.toBe(
          plan.evidenceHash,
        );
    },

    /** The empty semantic resolver's hop with a transition other than the
     * one the canonical-decode resolver committed: its claimed successor's
     * program counter one higher. */
    otherTransition: async () =>
      await rewrittenHop((honest) => ({
        redeemer: (inputIndex, outputIndex, context) => {
          const redeemer = honest.redeemer(inputIndex, outputIndex, context);
          const path = [0, 2, 1, 8];
          const counter = Data.from<bigint>(
            plutusConstrFieldCbor(redeemer, path),
          );
          return replacePlutusConstrFieldCbor(
            redeemer,
            path,
            integer(counter + 1n),
          );
        },
      })),

    /** The hop at `at` with a successor whose staged record keeps one more
     * at its field `field` than the stage derives. */
    oneMore: async (field: number) =>
      await rewrittenHop((honest) => {
        const path = [1, 0, 2, field];
        const length = Data.from<bigint>(
          plutusConstrFieldCbor(honest.datum, path),
        );
        return {
          datum: replacePlutusConstrFieldCbor(
            honest.datum,
            path,
            integer(length + 1n),
          ),
        };
      }),

    /** Our plan of each canonical-decode step of our trace, and of none
     * past them. */
    plansEvery: async () => {
      const { states } = await dispute.material.trace();
      const plan = async (stateIndex: number) =>
        await planValidationTraceOneStep({
          material: dispute.material,
          stateIndex,
          challengerHighHash: hashMidgardValidationMachineState(
            states[stateIndex + 1]!,
          ).toString("hex"),
          canonicalDecode: true,
        });
      for (const [stateIndex, kind] of CANONICAL_DECODE_STEPS.entries()) {
        const planning = await plan(stateIndex);
        if (
          planning.kind !== "plan" ||
          planning.plan.kind !== "canonicalDecode"
        )
          throw new Error(`step ${stateIndex.toString()} is not planned`);
        expect(planning.plan.canonicalDecode.item === undefined).toBe(
          kind === "empty",
        );
      }
      expect(await plan(CANONICAL_DECODE_STEPS.length)).toMatchObject({
        kind: "unsupported",
        reason: expect.stringMatching(/ is a compactBinding step, /u),
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
      const visits: readonly ValidationTraceOneStepHolder[] = path;
      expect(moves.map(({ stage: moved }) => moved)).toStrictEqual([
        "enter_resolution",
        ...Array<string>(visits.length + 1).fill("resolve"),
      ]);
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
        ...labels,
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
