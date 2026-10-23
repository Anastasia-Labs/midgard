/** The validation-trace dispute runner driving the successor's thread
 * through a one-step resolution past the boundary, on the emulator: the
 * successor's thread disputes a step of our trace, and the runner plays it
 * along `path` to the award; the awarded thread closes into our case. A
 * move whose fee fits under the per-move cap is paid by its thread, and one
 * over the cap by our wallet. Each resolution's drive
 * (validation-trace-dispute-one-step-*-drive.ts) names the step, its path
 * and how our plan of it is read from the thread. */
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
  type ValidationTraceHopHolder,
} from "../../src/validation-trace-dispute/one-step-builders.js";
import { integer } from "../../src/validation-trace-dispute/one-step-data.js";
import {
  type ValidationTraceOneStepHolder,
  validationTraceOneStepHolder,
} from "../../src/validation-trace-dispute/one-step-holders.js";
import {
  phaseYieldListReads,
  phaseYieldReads,
} from "../../src/validation-trace-dispute/one-step-phase.js";
import type { ValidationTraceOneStepPlan } from "../../src/validation-trace-dispute/one-step-plan.js";
import { scriptSourcesRedeemerReads } from "../../src/validation-trace-dispute/one-step-script-sources-redeemer.js";
import { VALIDATION_TRACE_ONE_STEP_REQUIRED } from "../../src/validation-trace-dispute/threads.js";
import type { FraudProofSignedTransactionRecovery } from "../../src/workflow/l1-source.js";
import type { FraudProofFamilyWorkflowAdapter } from "../../src/workflow/orchestrator.js";
import { emulatorSignedTransactionRecovery } from "./emulator-signed-recovery.js";
import { expectWithinWalletMoveShape } from "./validation-trace-dispute-move-fees.js";
import {
  defendTwoThreads,
  threadClosed,
  type TwoThreadCase,
} from "./validation-trace-dispute-one-step-case.js";
import {
  capturePrepared,
  isPendingFor,
  playThreads,
  type PreparedMove,
  threadPlace,
  withLedgerParameters,
} from "./validation-trace-dispute-one-step-play.js";
import { makeRunner } from "./validation-trace-dispute-runner.js";
import { reportFit } from "./validation-trace-dispute-scripted-defender.js";

const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * The drive of the successor's thread through the step after our trace's
 * state `from`, over a deployment installed with `installs`, set up with
 * the thread standing at `at`; the thread visits `path` past the boundary,
 * each move reported under its fit label in `labels`, and `planAt` reads
 * our plan of the step from the thread's datum at a holder.
 */
export const resolutionDrive = <
  Holder extends ValidationTraceHopHolder,
  Plan extends ValidationTraceOneStepPlan,
>({
  from,
  at,
  path,
  labels,
  installs,
  minFeeA,
  planAt,
}: {
  readonly from: NonNullable<
    Parameters<typeof defendTwoThreads>[0]["successorFrom"]
  >;
  readonly at: Holder;
  readonly path: readonly ValidationTraceOneStepHolder[];
  readonly labels: readonly string[];
  readonly installs: Pick<
    Parameters<typeof defendTwoThreads>[0],
    | "canonicalDecode"
    | "contexts"
    | "builtins"
    | "phases"
    | "scriptSourcesRedeemers"
    | "failingRedeemerData"
    | "failingReferenceInputs"
    | "failingSigners"
    | "failingScript"
    | "failingExUnits"
    | "operatorFrom"
    | "failingNativeSpend"
    | "failingNativeMint"
    | "releaseDepth"
  >;
  /** The ledger's per-byte fee while the resolution plays, when raised so
   * its largest moves are priced over the per-move cap. */
  readonly minFeeA?: number;
  readonly planAt: (
    dispute: TwoThreadCase,
    holder: Holder,
    utxo: UTxO,
  ) => Promise<Plan>;
}) => {
  let dispute: TwoThreadCase;
  /** The successor thread's moves past the game. */
  let moves: PreparedMove[];
  /** Each move's transaction, by its fit label. */
  const reported = new Map<string, string>();

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

  /**
   * The hop of the successor's thread at `at` with its successor datum,
   * the holder it pays the successor to, its redeemer or what it reads
   * rewritten by `rewrite`, and a check that the thread has not moved.
   */
  const rewrittenHop = async (
    rewrite: (
      honest: ValidationTraceHop,
      plan: Plan,
    ) => Partial<
      Pick<ValidationTraceHop, "datum" | "next" | "redeemer" | "reads">
    >,
  ) => {
    const { scene, deployment, b } = dispute;
    const before = await placeOfB();
    expect(before.holder).toBe(at);
    const plan = await planAt(dispute, at, before.utxo);
    const lucid = scene.h.proverLucid;
    const prover = scene.h.proverSigner.paymentKeyHash;
    const reference = validationTraceOneStepHolder(deployment, at).reference!;
    const honest = validationTraceHop({
      plan,
      holder: at,
      utxo: before.utxo,
      prover,
      ...(plan.kind === "phase" && plan.phase.semantic === at
        ? {
            phaseYield: phaseYieldReads(deployment, plan.phase),
            phaseYieldList: phaseYieldListReads(deployment, plan.phase),
          }
        : {}),
      ...(plan.kind === "phase"
        ? { redeemerChain: scriptSourcesRedeemerReads(deployment, undefined) }
        : {}),
    });
    const rewritten = { ...honest, ...rewrite(honest, plan) };
    const window = validationTraceMoveWindow(lucid);
    return {
      build: async () =>
        await completeValidationTraceMove({
          lucid,
          stage: "resolve",
          referenced: [reference, ...(rewritten.reads?.references ?? [])],
          build: (fee) =>
            buildValidationTraceHop({
              lucid,
              thread: before.utxo,
              unit: b.unit,
              reference,
              hop: rewritten,
              address: validationTraceOneStepHolder(deployment, rewritten.next)
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
    /** Our plan of the step, read from the thread where it stands. */
    plan: async () => await planAt(dispute, at, (await placeOfB()).utxo),
    rewrittenHop,

    /** Plays both threads until the successor's stands at `at`. */
    setUp: async () => {
      dispute = await defendTwoThreads({
        withRemoval: false,
        successorFrom: from,
        ...installs,
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

    /** Stops the runner's process and starts another over its journal,
     * holding nothing the first held in memory, reading where each signed
     * transaction stands from `signedRecovery` and its adapter rewritten by
     * `wrap` when given. */
    restart: ({
      signedRecovery,
      wrap,
    }: {
      readonly signedRecovery?: FraudProofSignedTransactionRecovery;
      readonly wrap?: (
        adapter: FraudProofFamilyWorkflowAdapter,
      ) => FraudProofFamilyWorkflowAdapter;
    } = {}) => {
      const { scene, deployment, prepared, runner } = dispute;
      const recovery =
        signedRecovery ??
        (installs.releaseDepth === undefined
          ? undefined
          : emulatorSignedTransactionRecovery(
              scene.h.emulator,
              installs.releaseDepth,
            ));
      dispute = Object.freeze({
        ...dispute,
        runner: makeRunner(scene, {
          deployment,
          journal: runner.journal,
          wrap: (adapter) =>
            capturePrepared(prepared)(wrap?.(adapter) ?? adapter),
          ...(recovery === undefined ? {} : { signedRecovery: recovery }),
        }),
      });
    },

    /** The hop at `at` with a transition other than the one the resolver
     * committed: its claimed successor's program counter one higher. The
     * transition is the redeemer's field `field`. */
    otherTransition: async (field = 2) =>
      await rewrittenHop((honest) => ({
        redeemer: (inputIndex, outputIndex, context) => {
          const redeemer = honest.redeemer(inputIndex, outputIndex, context);
          const counter = [0, field, 1, 8];
          return replacePlutusConstrFieldCbor(
            redeemer,
            counter,
            integer(
              Data.from<bigint>(plutusConstrFieldCbor(redeemer, counter)) + 1n,
            ),
          );
        },
      })),

    /** Plays the runner under a transaction size limit of `maxTxSize`
     * bytes until the thread's next move does not build under it: the
     * operator's alarm, with the thread unmoved. Its reason. */
    unbuilt: async (maxTxSize: number) => {
      const { scene, runner, a, b } = dispute;
      const before = await placeOfB();
      const { result } = await withLedgerParameters(
        dispute.scene,
        { maxTxSize },
        async () =>
          await playThreads({
            scene,
            runner,
            sides: [a, b],
            until: async (run) =>
              isPendingFor(run, VALIDATION_TRACE_ONE_STEP_REQUIRED) &&
              "reason" in run &&
              run.reason.includes("does not build"),
            limit: 4,
          }),
      );
      expect(outRefOf((await placeOfB()).utxo)).toBe(outRefOf(before.utxo));
      if (result.kind !== "pending") throw new Error("the run is not pending");
      return result.reason;
    },

    /** Plays the thread through the resolution to the award and its close. */
    awards: async () => {
      const { scene, deployment, runner, prepared, a, b } = dispute;
      await withLedgerParameters(
        dispute.scene,
        minFeeA === undefined ? {} : { minFeeA },
        async () =>
          await playThreads({
            scene,
            runner,
            sides: [a, b],
            // Over a failing transaction of its own, our other thread's trace
            // parts from ours at a step of another phase, and it waits there.
            until: async () =>
              (installs.failingRedeemerData !== undefined ||
                installs.failingReferenceInputs !== undefined ||
                installs.failingSigners !== undefined ||
                installs.failingScript !== undefined ||
                installs.failingNativeSpend !== undefined ||
                installs.failingNativeMint !== undefined ||
                (await threadClosed(dispute, a.unit))) &&
              (await threadClosed(dispute, b.unit)),
            limit: 40 + 2 * path.length,
          }),
      );
      moves = prepared.filter(
        ({ stage: moved, cbor }) =>
          (moved === "enter_resolution" || moved === "resolve") &&
          threadOutput(cbor) !== undefined,
      );
      expect(moves.map(({ stage: moved }) => moved)).toStrictEqual([
        "enter_resolution",
        ...Array<string>(path.length + 1).fill("resolve"),
      ]);
      for (const [index, next] of path.entries())
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
      ].entries()) {
        reportFit(label, [moves[index]!.cbor]);
        reported.set(label, moves[index]!.cbor);
      }
      reportFit("CloseAwardedThread", [close!.cbor]);
    },

    /** The signed move reported under `label`. */
    cborOf: (label: string) => reported.get(label)!,

    /** The size in bytes of the move reported under `label`. */
    bytesOf: (label: string) => reported.get(label)!.length / 2,

    /** The fee of the move reported under `label`. */
    feeOf: (label: string) =>
      CML.Transaction.from_cbor_hex(reported.get(label)!).body().fee(),

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
