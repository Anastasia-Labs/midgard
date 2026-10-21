/** The operator's side of the game on real cases: a challenger opens cases
 * on the scene's passing block with a trace ending Terminal Rejected, and
 * the block's operator defends each with its own trace. On one thread both
 * sides reveal with their own builders down to the one-step resolution; on
 * one the challenger stays silent, on another the operator does; the last
 * is never played. Each thread's standing for its operator is read at every
 * turn, and every thread still open past its case's duration expires to its
 * operator's key, from whichever validator holds it. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { recordedValidationTraceDefenders } from "../src/validation-trace-dispute/case-history.js";
import {
  buildValidationTraceDefenseMaterial,
  type ValidationTraceDefenseMaterial,
} from "../src/validation-trace-dispute/defense-material.js";
import {
  validationTraceThreadExpiresAfterMs,
  validationTraceThreadHeader,
} from "../src/validation-trace-dispute/expiry.js";
import {
  buildValidationTraceGameMove,
  buildValidationTraceThreadClose,
  validationTraceMoveWindow,
} from "../src/validation-trace-dispute/move-builders.js";
import {
  completeValidationTraceMove,
  VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE,
} from "../src/validation-trace-dispute/move-fee.js";
import {
  classifyValidationTraceThread,
  validationTraceChallengerDeadlineMs,
  validationTraceDefenderDeadlineMs,
  type ValidationTraceThreadHolder,
  type ValidationTraceThreadStanding,
} from "../src/validation-trace-dispute/threads.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { network } from "./support/submit-init-emulator-shared.js";
import { l1SlotConfig } from "./support/validation-trace-claim-material.js";
import { sceneCaseHistory } from "./support/validation-trace-dispute-case-history.js";
import {
  type Defender,
  defendWith,
  sceneOperator,
} from "./support/validation-trace-dispute-defend.js";
import {
  challengerEntersResolution,
  challengerReveal,
  type ExpiryReferences,
  type FalseClaim,
  falseClaimMaterial,
  type Landed,
  landWith,
  movedThread,
  openFalseClaim,
  operatorReveal,
  publishExpiryReferences,
  threadExpiry,
} from "./support/validation-trace-dispute-defender-moves.js";
import {
  awaitLastSlotAtOrBefore,
  defenderEntersOurTimeout,
} from "./support/validation-trace-dispute-one-step-case.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import { runnerDeployment } from "./support/validation-trace-dispute-runner.js";
import {
  reportFit,
  threadOf,
  threadUnitOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

type Defense = Extract<ValidationTraceDefenseMaterial, { kind: "defend" }>;

/** A case the operator defended, and its thread. */
type Defended = FalseClaim &
  Readonly<{ material: Defense; unit: string; thread: UTxO }>;

describe("validation-trace dispute defender moves", () => {
  let scene: Scene;
  let operator: Defender;
  let expiry: ExpiryReferences;
  /** Played to the one-step resolution; left with the challenger silent;
   * left with the operator silent; never played. */
  let bisected: Defended;
  let challengerSilent: Defended;
  let operatorSilent: Defended;
  let untouched: Defended;
  /** Each thread's standing for its operator, as it was played. */
  const standings = new Map<Defended, ValidationTraceThreadStanding[]>();
  const operatorReveals: string[] = [];
  const challengerReveals: string[] = [];
  let entered: Landed;
  let closed: Landed;

  const now = () => validationTraceMoveWindow(scene.h.proverLucid).validFrom;
  const record = (
    defended: Defended,
    thread: UTxO,
    at: ValidationTraceThreadHolder,
  ) => {
    const list = standings.get(defended) ?? [];
    list.push(
      classifyValidationTraceThread({
        thread: threadOf(thread),
        at,
        side: "operator",
        now: now(),
      }),
    );
    standings.set(defended, list);
  };
  /** Records `thread`'s standing at the last instant of `deadline`, and one
   * slot past it. */
  const recordAcross = (defended: Defended, thread: UTxO, deadline: number) => {
    awaitLastSlotAtOrBefore(scene, deadline);
    record(defended, thread, "game");
    scene.h.emulator.awaitSlot(1);
    record(defended, thread, "game");
  };
  /** Moves the emulator to the first slot past `time`, if not already. */
  const awaitPast = (time: number) => {
    if (now() <= time) {
      awaitLastSlotAtOrBefore(scene, time);
      scene.h.emulator.awaitSlot(1);
    }
    expect(now()).toBeGreaterThan(time);
  };
  const live = async (unit: string, address: string) => {
    const [utxo] = await scene.h.proverLucid.utxosAtWithUnit(address, unit);
    if (utxo === undefined) throw new Error("the thread is not there");
    return utxo;
  };

  beforeAll(async () => {
    scene = await setUpScene();
    operator = sceneOperator(scene);
    expiry = await publishExpiryReferences(scene);
    const claim = await falseClaimMaterial(scene);
    const honestStepCount = Number(claim.honest.claim.stepCount);
    const defend = async (stepCount: number): Promise<Defended> => {
      const opened = await openFalseClaim(scene, claim, stepCount);
      const material = await buildValidationTraceDefenseMaterial({
        evidence: scene.passing.evidence,
        predecessor: completeCanonicalReplayPredecessorLedger(scene.passing),
        eventKey: scene.passing.eventKey,
        stepCount,
        l1SlotConfig,
      });
      if (material.kind !== "defend") throw new Error(material.kind);
      const txHash = await defendWith(scene, opened.caseUtxo, operator, {
        root: material.root,
        lastState: material.stateAtS,
        lastStateProof: material.proofAtS,
      });
      const [thread] = await scene.h.proverLucid.utxosByOutRef([
        { txHash, outputIndex: 1 },
      ]);
      return {
        ...opened,
        material,
        unit: threadUnitOf(scene, thread!),
        thread: thread!,
      };
    };
    bisected = await defend(honestStepCount);
    challengerSilent = await defend(honestStepCount + 1);
    operatorSilent = await defend(honestStepCount + 2);

    // Both sides reveal until the thread is ReadyForOneStep, then the
    // challenger enters the one-step resolution.
    let thread = bisected.thread;
    record(bisected, thread, "game");
    while (threadOf(thread).turn !== "ReadyForOneStep") {
      const operatorTurn =
        classifyValidationTraceThread({
          thread: threadOf(thread),
          at: "game",
          side: "challenger",
          now: now(),
        }).kind === "their_move";
      const landed = operatorTurn
        ? await landWith(
            operator.lucid,
            await operatorReveal(
              scene,
              operator,
              thread,
              bisected.material.proofAt,
            ),
          )
        : await landWith(
            scene.h.proverLucid,
            await challengerReveal(scene, thread, bisected.trace),
          );
      (operatorTurn ? operatorReveals : challengerReveals).push(landed.cbor);
      thread = await movedThread(scene.h.proverLucid, landed);
      record(bisected, thread, "game");
    }
    entered = await landWith(
      scene.h.proverLucid,
      await challengerEntersResolution(scene, thread),
    );
    record(
      bisected,
      await movedThread(scene.h.proverLucid, entered),
      "boundary",
    );

    // The operator reveals on one thread, and the challenger never answers.
    const revealed = await movedThread(
      operator.lucid,
      await landWith(
        operator.lucid,
        await operatorReveal(
          scene,
          operator,
          challengerSilent.thread,
          challengerSilent.material.proofAt,
        ),
      ),
    );
    // The operator never reveals on the other: its clock runs out first.
    recordAcross(
      operatorSilent,
      operatorSilent.thread,
      validationTraceDefenderDeadlineMs(threadOf(operatorSilent.thread)),
    );
    recordAcross(
      challengerSilent,
      revealed,
      validationTraceChallengerDeadlineMs(threadOf(revealed)),
    );

    // The operator times the challenger's thread out as its win.
    const deployment = runnerDeployment(scene);
    record(
      challengerSilent,
      await defenderEntersOurTimeout({
        deployment,
        defender: operator,
        thread: revealed,
      }),
      "timeout",
    );

    // The challenger times the operator out and closes its thread with the
    // operator's lost-defense receipt.
    const prover = scene.h.proverLucid;
    let window = validationTraceMoveWindow(prover);
    const timedOut = await movedThread(
      prover,
      await landWith(
        prover,
        await completeValidationTraceMove({
          lucid: prover,
          stage: "timeout",
          referenced: [scene.references.game],
          build: (fee) =>
            buildValidationTraceGameMove({
              lucid: prover,
              deployment: scene,
              thread: operatorSilent.thread,
              action: { kind: "EnterChallengerTimeout" },
              successor: {
                address: scene.chain.timeout.spendingScriptAddress,
                datum: operatorSilent.thread.datum!,
              },
              fee,
              ...window,
            }),
        }),
      ),
    );
    record(operatorSilent, timedOut, "timeout");
    window = validationTraceMoveWindow(prover);
    const [caseUtxo] = await prover.utxosAtWithUnit(
      scene.chain.case.spendingScriptAddress,
      operatorSilent.caseUnit,
    );
    const close = (fee: bigint) =>
      buildValidationTraceThreadClose({
        lucid: prover,
        deployment,
        via: "timeout",
        thread: { utxo: timedOut, unit: operatorSilent.unit },
        caseId: threadOf(timedOut).case_id,
        defenderVkh: operator.vkey,
        caseUtxo: caseUtxo!,
        caseConfig: scene.caseConfig,
        fee,
        ...window,
      }).tx;
    closed = await landWith(
      prover,
      await completeValidationTraceMove({
        lucid: prover,
        stage: "close_thread",
        referenced: [
          scene.caseConfig,
          scene.references.case,
          scene.references.timeout,
          scene.references.award,
        ],
        build: close,
      }),
    );

    // A case opened after the others, never played.
    untouched = await defend(honestStepCount + 3);
  }, 1_800_000);

  it("bisects a false claim to the one-step resolution with the operator's and the challenger's own builders, each move within the limits", async () => {
    expect(operatorReveals.length).toBeGreaterThanOrEqual(3);
    expect(challengerReveals).toHaveLength(operatorReveals.length);
    reportFit("RevealOperator", operatorReveals);
    reportFit("RevealChallenger", challengerReveals);
    reportFit("EnterResolution", [entered.cbor]);
    const atBoundary = await live(
      bisected.unit,
      scene.chain.boundary.spendingScriptAddress,
    );
    expect(atBoundary.txHash).toBe(entered.hash);
    const resolved = threadOf(atBoundary);
    expect(resolved.high_index - resolved.low_index).toBe(1n);
    // The operator's turn, then the challenger's, until the one step, which
    // only the challenger may enter and run.
    const kinds = standings.get(bisected)!.map(({ kind }) => kind);
    expect(kinds).toStrictEqual([
      ...operatorReveals.flatMap(() => ["our_move", "their_move"]),
      "one_step",
      "one_step",
    ]);
  });

  it("reports the challenger's silence as its clock running out, and the timeout anyone enters as the operator's win", async () => {
    expect(standings.get(challengerSilent)).toMatchObject([
      { kind: "their_move" },
      { kind: "their_clock_out" },
      { kind: "won" },
    ]);
    const atTimeout = await live(
      challengerSilent.unit,
      scene.chain.timeout.spendingScriptAddress,
    );
    expect(threadOf(atTimeout).turn).toHaveProperty("AwaitingChallenger");
  });

  it("reports the operator's own silence as its thread lost, and the challenger's close mints its lost-defense receipt", async () => {
    expect(standings.get(operatorSilent)).toMatchObject([
      { kind: "our_move", move: "reveal" },
      { kind: "lost" },
      { kind: "lost" },
    ]);
    const { chain, h } = scene;
    const [receipt] = await h.proverLucid.utxosAtWithUnit(
      chain.award.spendingScriptAddress,
      chain.award.policyId + operatorSilent.unit.slice(56),
    );
    expect(receipt?.txHash).toBe(closed.hash);
    expect(Data.from(receipt!.datum!, SDK.LostDefenseReceipt)).toMatchObject({
      defender_vkh: operator.vkey,
      outcome: "ChallengerWins",
    });
    const history = await sceneCaseHistory(scene).transactions({
      headerHash: scene.passing.evidence.headerHash,
      caseUnit: operatorSilent.caseUnit,
    });
    expect(
      recordedValidationTraceDefenders({
        transactions: history,
        caseUnit: operatorSilent.caseUnit,
      }),
    ).toStrictEqual([operator.vkey]);
  });

  it("refuses an operator reveal whose proof does not verify under the operator's root", async () => {
    const thread = await live(
      untouched.unit,
      scene.chain.game.spendingScriptAddress,
    );
    await expectOnchainRefusal(
      () => operatorReveal(scene, operator, thread, untouched.trace.proofAt),
      {
        refusedBy: "fraud_proofs/validation_trace/game_v1",
        check: /verify_trace_proof_against_root/u,
      },
    );
  });

  it("refuses an expiry in the last slot of its case's duration", async () => {
    const thread = await live(
      untouched.unit,
      scene.chain.game.spendingScriptAddress,
    );
    const expiresAfter = validationTraceThreadExpiresAfterMs(
      validationTraceThreadHeader(thread.datum!),
    );
    awaitLastSlotAtOrBefore(scene, expiresAfter);
    // The latest lower bound a slot admits at or before the case's end; the
    // next slot's is past it.
    const lucid = scene.h.proverLucid;
    expect(now()).toBeLessThanOrEqual(expiresAfter);
    expect(lucid.slotToUnixTime(lucid.currentSlot() + 1)).toBeGreaterThan(
      expiresAfter,
    );
    await expectOnchainRefusal(
      () =>
        threadExpiry({
          scene,
          references: expiry,
          lucid: operator.lucid,
          holder: "game",
          thread,
          unit: untouched.unit,
        }),
      {
        refusedBy: "fraud_proofs/validation_trace/thread_expiry_v1",
        check: /header\.started_at \+ case_duration_milliseconds/u,
      },
    );
  });

  it("expires every open thread past its case's duration from the game, the timeout and the boundary, to the operator's key", async () => {
    const pocket = credentialToAddress(network, {
      type: "Key",
      hash: operator.vkey,
    });
    expect(pocket).not.toBe(await operator.lucid.wallet().address());
    const { chain } = scene;
    for (const [defended, holder, address] of [
      [untouched, "game", chain.game.spendingScriptAddress],
      [challengerSilent, "timeout", chain.timeout.spendingScriptAddress],
      [bisected, "boundary", chain.boundary.spendingScriptAddress],
    ] as const) {
      const thread = await live(defended.unit, address);
      awaitPast(
        validationTraceThreadExpiresAfterMs(
          validationTraceThreadHeader(thread.datum!),
        ),
      );
      const landed = await landWith(
        operator.lucid,
        await threadExpiry({
          scene,
          references: expiry,
          lucid: operator.lucid,
          holder,
          thread,
          unit: defended.unit,
        }),
      );
      reportFit(`expiry from ${holder}`, [landed.cbor]);
      const outputs = await operator.lucid.utxosByOutRef([
        { txHash: landed.hash, outputIndex: 0 },
        { txHash: landed.hash, outputIndex: 1 },
      ]);
      expect(outputs.some((output) => defended.unit in output.assets)).toBe(
        false,
      );
      const paid = outputs.find((output) => output.address === pocket);
      expect(paid?.datum).toBe(
        Data.to(new Constr(0, [thread.txHash, BigInt(thread.outputIndex)])),
      );
      expect(paid!.assets.lovelace).toBeGreaterThanOrEqual(
        thread.assets.lovelace! - VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE,
      );
      expect(
        await scene.h.proverLucid.utxosAtWithUnit(address, defended.unit),
      ).toStrictEqual([]);
    }
  });
});
