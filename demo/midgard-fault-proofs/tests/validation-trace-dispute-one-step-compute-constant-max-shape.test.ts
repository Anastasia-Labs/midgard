/** The validation-trace dispute at a CEK step that computes a constant past
 * the direct payload bound, on the emulator. The failing transaction's
 * script, `(program 1.1.0 (lam ctx [(lam pad (error)) (con bytestring P)]))`,
 * is a field-6 item that fills the field bound, and the step computes P.
 *
 * A second case on the failing block commits a trace whose successor of that
 * step returns another root of P, one memory unit off. Both operators
 * defend it with our trace up to that successor, so each holds the correct
 * post state. On each thread the one-step resolution carries the committed
 * successor through the boundary, the CEK resolver and the core step binder
 * to the compute arm, and the arm refuses it, whether the step names P's
 * root or the other one; the threads stay where they stand.
 *
 * Our case is defended by both operators with a trace that agrees with ours
 * up to that step and is pending after it. The runner bisects both threads,
 * resolves each through the compute arm, closes them into our case and, past
 * the case's defense deadline, closes it Convicted with our bond back; every
 * move of ours lands within the transaction limits. */
import { hashMidgardValidationMachineState } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES } from "@al-ft/midgard-validation";
import { plutusV3ScriptWitness } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { credentialToAddress, Data } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { confirmedValidationTraceStages } from "../src/validation-trace-dispute/actions.js";
import {
  VALIDATION_TRACE_CASE_CONVICTED,
  VALIDATION_TRACE_CASE_OPEN,
} from "../src/validation-trace-dispute/adapter.js";
import { VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE } from "../src/validation-trace-dispute/move-fee.js";
import {
  type ValidationTraceOneStepHolder,
  validationTraceOneStepHolder,
} from "../src/validation-trace-dispute/one-step-holders.js";
import { planValidationTraceThread } from "../src/validation-trace-dispute/one-step-plan.js";
import { validationTraceDefenseDeadlineMs } from "../src/validation-trace-dispute/waiting.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  MAXIMUM_ITEM_BYTES,
  plutusItem,
} from "./support/script-decode-plutus-max-shape.js";
import { network } from "./support/submit-init-emulator-shared.js";
import {
  type ComputeConstantStep,
  computeConstantStep,
  computeConstantStepOf,
  firstOutputAddress,
  hopFrom,
  maximumConstantScript,
  otherWindow,
  outRefOf,
  place,
  secondCaseAtTheStep,
} from "./support/validation-trace-dispute-compute-constant-step.js";
import { landWith } from "./support/validation-trace-dispute-defender-moves.js";
import { TIMEOUT } from "./support/validation-trace-dispute-max-shape-case.js";
import {
  expectRealMoveFee,
  landedMoveFee,
} from "./support/validation-trace-dispute-move-fees.js";
import {
  awaitLastSlotAtOrBefore,
  defendTwoThreads,
  liveCase,
  threadClosed,
  type TwoThreadCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import {
  confirmedOneStepMoves,
  isPendingFor,
  playThreads,
} from "./support/validation-trace-dispute-one-step-play.js";
import { BOND_LOVELACE } from "./support/validation-trace-dispute-open-scene.js";
import {
  liveThread,
  reportFit,
  threadOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

const { script: SCRIPT, bytes: P_BYTES } = maximumConstantScript();

/** Each one-step cut, by the holder its move pays the thread to. */
const CUT_OF: Partial<Record<ValidationTraceOneStepHolder, string>> = {
  prepare: "PrepareResolution",
  core: "PrepareSelected",
  compute: "VerifyCoreStep",
  settle: "VerifyArm compute",
  award: "Settle",
};

let dispute: TwoThreadCase;
/** The step that computes P, with P's root and the other root of P. */
let step: ComputeConstantStep;
/** The second case's successor of the step, and its two threads, each
 * ReadyForOneStep before the resolution. */
let committedSuccessorHash: string;
let ready: { unit: string; thread: SDK.CaseThread }[] = [];
/** The round each thread of our case stood at once it was ReadyForOneStep. */
const rounds = new Map<string, bigint>();

/** Carries the second case's thread `index`, with the step's witness naming
 * `named`, to the compute arm, has the arm refuse the step, and checks the
 * thread still stands at the arm. */
const resolveToTheArm = async (index: number, named: Buffer) => {
  const { unit, thread } = ready[index]!;
  const material = dispute.material;
  const planning = await planValidationTraceThread(
    {
      ...material,
      steps: async (stateIndex) =>
        stateIndex === step.agreedUpTo
          ? otherWindow(step, named)
          : await material.steps(stateIndex),
    },
    thread,
  );
  if (planning.kind !== "plan") throw new Error(planning.reason);
  const { plan } = planning;
  expect(plan.kind).toBe("core");
  const lucid = dispute.scene.h.proverLucid;
  for (const holder of ["boundary", "prepare", "core"] as const) {
    expect((await place(dispute, unit)).holder).toBe(holder);
    await landWith(lucid, await hopFrom(dispute, unit, plan, holder)());
  }
  const before = await place(dispute, unit);
  expect(before.holder).toBe("compute");
  await expectOnchainRefusal(hopFrom(dispute, unit, plan, "compute"), {
    refusedBy: "fraud_proofs/validation_trace/cek_core_arm_compute",
    check: /verify_compute_step/u,
  });
  const after = await place(dispute, unit);
  expect(after.holder).toBe("compute");
  expect(outRefOf(after.utxo)).toBe(outRefOf(before.utxo));
};

describe("validation-trace dispute: one-step resolution of a CEK step computing a constant past the direct payload bound, at the field bound", () => {
  beforeAll(async () => {
    dispute = await defendTwoThreads({
      withRemoval: true,
      failingScript: plutusV3ScriptWitness(SCRIPT),
      operatorFrom: computeConstantStep,
      successorFrom: computeConstantStep,
    });
    step = await computeConstantStepOf(dispute.material, SCRIPT);
    ({ committedSuccessorHash, ready } = await secondCaseAtTheStep(
      dispute,
      step,
    ));
  }, TIMEOUT);

  afterAll(() => dispute?.recorder?.restore());

  it("disputes a step that computes a constant past the direct payload bound, carried in a field-6 item that fills the field bound", () => {
    expect(plutusItem(SCRIPT).length).toBe(MAXIMUM_ITEM_BYTES);
    expect(P_BYTES).toBeGreaterThan(
      MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
    );
    expect(step.otherRoot.equals(step.valueRoot)).toBe(false);
  });

  it("brings each thread of the second case to ReadyForOneStep on the step, with the committed successor the challenger's", () => {
    expect(ready).toHaveLength(2);
    for (const { thread } of ready) {
      expect(thread.low_index).toBe(BigInt(step.agreedUpTo));
      expect(thread.challenger_high_hash).toBe(committedSuccessorHash);
      expect(thread.defender_high_hash).toBe(
        hashMidgardValidationMachineState(
          step.ours.states[step.agreedUpTo + 1]!,
        ).toString("hex"),
      );
    }
  });

  it(
    "a correct post state is not convicted: the compute arm refuses a step that names P's root and returns another root of P",
    async () => {
      await resolveToTheArm(0, step.valueRoot);
    },
    TIMEOUT,
  );

  it(
    "a correct post state is not convicted: the compute arm refuses a step that names another root of P and returns it",
    async () => {
      await resolveToTheArm(1, step.otherRoot);
    },
    TIMEOUT,
  );

  it(
    "bisects both threads of our case, resolves each through the compute arm and closes the case Convicted with our bond back",
    async () => {
      const { scene, runner, a, b, deployment, removal } = dispute;
      let pastDeadline = false;
      const { headerHash } = scene.failing.evidence;
      removal!.holdView();
      await removal!.l1For(headerHash).observe({ headerHash });
      const { result, history } = await playThreads({
        scene,
        runner,
        sides: [a, b],
        until: async (run) => {
          for (const { unit } of [a, b]) {
            const utxo = await liveThread(scene, unit);
            if (
              utxo?.address === scene.chain.game.spendingScriptAddress &&
              threadOf(utxo).turn === "ReadyForOneStep"
            )
              rounds.set(unit, threadOf(utxo).round);
          }
          if (
            isPendingFor(run, VALIDATION_TRACE_CASE_OPEN) &&
            (await threadClosed(dispute, a.unit)) &&
            (await threadClosed(dispute, b.unit))
          ) {
            if (pastDeadline)
              throw new Error("the case stayed open past its defense deadline");
            awaitLastSlotAtOrBefore(
              scene,
              validationTraceDefenseDeadlineMs(
                Data.from((await liveCase(dispute)).datum!, SDK.Case),
              ) + 1_000,
            );
            pastDeadline = true;
          }
          return isPendingFor(run, VALIDATION_TRACE_CASE_CONVICTED);
        },
        limit: 400,
      });
      if (!("entries" in result))
        throw new Error(`no workflow journal: ${history.join("\n")}`);
      for (const { unit } of [a, b]) expect(rounds.has(unit)).toBe(true);

      // Each thread resolved through the compute arm and was awarded.
      const moves = confirmedOneStepMoves(result.entries);
      expect(
        moves.filter(({ stage }) => stage === "close_awarded"),
      ).toHaveLength(2);
      const computeAddress = validationTraceOneStepHolder(
        deployment,
        "compute",
      ).address;
      expect(
        moves.filter(
          ({ txHash }) =>
            firstOutputAddress(scene.l1.signedCbors.get(txHash)!) ===
            computeAddress,
        ),
      ).toHaveLength(2);

      // The case closed Convicted, our bond back.
      const close = confirmedValidationTraceStages(result.entries).at(-1)!;
      expect(close.action.stage).toBe("close");
      const lucid = scene.h.proverLucid;
      expect(
        await lucid.utxosByOutRef([parseOwnOutRef(dispute.bondOutRef)]),
      ).toStrictEqual([]);
      const [refund] = await lucid.utxosByOutRef([
        { txHash: close.txHash, outputIndex: 0 },
      ]);
      expect(refund!.address).toBe(
        credentialToAddress(network, {
          type: "Key",
          hash: scene.h.proverSigner.paymentKeyHash,
        }),
      );
      expect(refund!.assets).toEqual({ lovelace: BOND_LOVELACE });
    },
    TIMEOUT,
  );

  it(
    "lands every move of ours within the transaction limits at its real fee",
    async () => {
      const { scene, prepared, deployment } = dispute;
      const cutOf = new Map(
        Object.entries(CUT_OF).map(([holder, cut]) => [
          validationTraceOneStepHolder(
            deployment,
            holder as ValidationTraceOneStepHolder,
          ).address,
          cut,
        ]),
      );
      const byCut = new Map<string, string[]>();
      for (const { stage, hash, cbor } of prepared) {
        if (!scene.l1.signedCbors.has(hash)) continue;
        const fee = await landedMoveFee(scene, hash);
        // Under the cap the thread pays the move's real fee; over it our
        // wallet pays.
        if (fee.paid < VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE)
          expectRealMoveFee(fee);
        else expect(fee.paid).toBeGreaterThanOrEqual(fee.ledger);
        const cut =
          stage === "resolve" ? cutOf.get(firstOutputAddress(cbor)) : stage;
        if (cut === undefined) throw new Error("a resolve move of no cut");
        byCut.set(cut, [...(byCut.get(cut) ?? []), cbor]);
      }
      expect([...byCut.keys()]).toEqual(
        expect.arrayContaining([
          "reveal",
          "enter_resolution",
          ...Object.values(CUT_OF),
          "close_awarded",
          "close",
        ]),
      );
      for (const [cut, cbors] of byCut) reportFit(cut, cbors);
    },
    TIMEOUT,
  );
});
