/** The defender's Defend on a real case: a challenger opens cases on the
 * scene's passing block with a committed trace that ends Terminal Rejected,
 * and the block's operator defends each with its own trace of the block's
 * event at the case's step count, built by the defender's builders. */
import { MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { recordedValidationTraceDefenders } from "../src/validation-trace-dispute/case-history.js";
import type { ValidationTraceDefendersProof } from "../src/validation-trace-dispute/defend.js";
import {
  buildValidationTraceDefenseMaterial,
  type ValidationTraceDefenseMaterial,
} from "../src/validation-trace-dispute/defense-material.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { sparseTrace } from "./support/validation-game-deep-traces.js";
import { l1SlotConfig } from "./support/validation-trace-claim-material.js";
import { sceneCaseHistory } from "./support/validation-trace-dispute-case-history.js";
import {
  type Defender,
  defendersProofFor,
  defendTx,
  type DefenseTrace,
  sceneOperator,
  withDecoyThreads,
} from "./support/validation-trace-dispute-defend.js";
import {
  failingMaterial,
  initAndOpen,
  measureWithinTransactionLimits,
  type Scene,
  setUpScene,
  verifySource,
} from "./support/validation-trace-dispute-open-scene.js";
import { passingTrace } from "./support/validation-trace-dispute-passing-trace.js";

const CASE_V1 = "fraud_proofs/validation_trace/case_v1";
const DECOY = "d0".repeat(28);

type Defense = Extract<ValidationTraceDefenseMaterial, { kind: "defend" }>;

/** A case the challenger opened, and its trace's Terminal Rejected state. */
type OpenedCase = Readonly<{
  caseUnit: string;
  caseUtxo: UTxO;
  stepCount: number;
  challengerTrace: DefenseTrace;
}>;

describe("validation-trace dispute Defend", () => {
  let scene: Scene;
  let operator: Defender;
  /** Defended in setup: past the honest trace's end, and inside it. */
  let extended: OpenedCase;
  let inside: OpenedCase;
  /** Never defended. */
  let undefended: OpenedCase;
  /** `extended` and `inside` after their Defends. */
  const landed = new Map<
    OpenedCase,
    Readonly<{
      txHash: string;
      cbor: string;
      material: Defense;
      defenders: ValidationTraceDefendersProof;
    }>
  >();

  const defenseOf = (material: Defense): DefenseTrace => ({
    root: material.root,
    lastState: material.stateAtS,
    lastStateProof: material.proofAtS,
  });

  const materialAt = async (stepCount: number): Promise<Defense> => {
    const material = await buildValidationTraceDefenseMaterial({
      evidence: scene.passing.evidence,
      predecessor: completeCanonicalReplayPredecessorLedger(scene.passing),
      eventKey: scene.passing.eventKey,
      stepCount,
      l1SlotConfig,
    });
    if (material.kind !== "defend")
      throw new Error(`expected defense material, got ${material.kind}`);
    return material;
  };

  const liveCase = async (opened: OpenedCase): Promise<UTxO> => {
    const [utxo] = await scene.h.proverLucid.utxosAtWithUnit(
      scene.chain.case.spendingScriptAddress,
      opened.caseUnit,
    );
    if (utxo === undefined) throw new Error("the case is gone");
    return utxo;
  };

  beforeAll(async () => {
    scene = await setUpScene();
    operator = sceneOperator(scene);
    const honest = await passingTrace(scene);
    const rejected = (await failingMaterial(scene)).terminal;
    const honestStepCount = Number(honest.claim.stepCount);
    expect(honestStepCount).toBeGreaterThan(4);
    // A Terminal Rejected state the honest run never reaches.
    const claimedTerminal = {
      ...honest.terminal,
      verdict: "rejected" as const,
      rejectionCodeHash: rejected.rejectionCodeHash,
    };
    const open = async (stepCount: number): Promise<OpenedCase> => {
      const trace = sparseTrace(
        [
          { from: 0, state: honest.initialState },
          { from: stepCount, state: claimedTerminal },
        ],
        stepCount,
      );
      const { opener } = await initAndOpen(scene, scene.passing, {
        claimWitness: honest.claim.claimWitness,
        challengerRoot: trace.descriptor.traceRoot.toString("hex"),
        stepCount: BigInt(stepCount),
      });
      const verification = verifySource(scene, opener, {
        terminal: claimedTerminal,
        terminalProof: trace.proofAt(stepCount),
        initialState: honest.initialState,
      });
      const signed = await (await verification.build()).sign
        .withWallet()
        .complete();
      const txHash = await signed.submit();
      await scene.h.proverLucid.awaitTx(txHash);
      const [caseUtxo] = await scene.h.proverLucid.utxosByOutRef([
        { txHash, outputIndex: 0 },
      ]);
      expect(caseUtxo?.assets[verification.opening.caseUnit]).toBe(1n);
      return {
        caseUnit: verification.opening.caseUnit,
        caseUtxo: caseUtxo!,
        stepCount,
        challengerTrace: {
          root: trace.descriptor.traceRoot.toString("hex"),
          lastState: claimedTerminal,
          lastStateProof: trace.proofAt(stepCount),
        },
      };
    };
    extended = await open(honestStepCount + 3);
    inside = await open(Math.floor(honestStepCount / 2));
    undefended = await open(honestStepCount);
    for (const opened of [extended, inside]) {
      const material = await materialAt(opened.stepCount);
      const defenders = await defendersProofFor(opened.caseUtxo, operator);
      const built = await defendTx(
        scene,
        opened.caseUtxo,
        operator,
        defenseOf(material),
        defenders,
      );
      // One Defend also leaves a thread datum for another key, no token.
      const tx = withDecoyThreads(
        scene,
        opened.caseUtxo,
        built,
        opened === inside ? [DECOY] : [],
      );
      const signed = await (await tx.complete({ localUPLCEval: true })).sign
        .withWallet()
        .complete();
      const txHash = await signed.submit();
      await operator.lucid.awaitTx(txHash);
      landed.set(opened, {
        txHash,
        cbor: signed.toCBOR(),
        material,
        defenders,
      });
    }
  }, 900_000);

  it("lands a Defend whose state at the case's step count the honest replay reaches, opening the operator's thread", async () => {
    for (const opened of [extended, inside]) {
      const { txHash, cbor, material, defenders } = landed.get(opened)!;
      const measured = measureWithinTransactionLimits(cbor);
      console.info(
        `fit Defend S=${opened.stepCount.toString()}: bytes=${measured.completeSignedBytes.toString()} mem=${measured.executionMemory.toString()} steps=${measured.executionSteps.toString()}`,
      );
      if (opened === extended)
        expect(material.stateAtS).toMatchObject({
          phase: "terminal",
          verdict: "accepted",
        });
      else expect(material.stateAtS.phase).not.toBe("terminal");
      const [caseUtxo, thread] = await operator.lucid.utxosByOutRef([
        { txHash, outputIndex: 0 },
        { txHash, outputIndex: 1 },
      ]);
      const caseDatum = Data.from(caseUtxo!.datum!, SDK.Case);
      expect(caseDatum.defenders_root).toBe(defenders.defendersRoot);
      expect(caseDatum.open_threads).toBe(1n);
      const threadDatum = Data.from(thread!.datum!, SDK.CaseThreadDatum).data!;
      expect(threadDatum).toMatchObject({
        defender_root: material.root,
        defender_high_hash: material.proofAtS.stateHash.toString("hex"),
        step_count: BigInt(opened.stepCount),
        header: { defender_vkh: operator.vkey },
        turn: { AwaitingOperator: { midpoint: BigInt(opened.stepCount) / 2n } },
      });
    }
  });

  it("names every defender the case's history recorded, never a key only a look-alike thread datum names, and refuses a defenders proof that history does not back", async () => {
    for (const opened of [extended, inside]) {
      const transactions = await sceneCaseHistory(scene).transactions({
        headerHash: scene.passing.evidence.headerHash,
        caseUnit: opened.caseUnit,
      });
      expect(
        recordedValidationTraceDefenders({
          transactions,
          caseUnit: opened.caseUnit,
        }),
      ).toStrictEqual([operator.vkey]);
    }
    const defended = await liveCase(extended);
    await expect(
      defendersProofFor(defended, operator, [operator.vkey]),
    ).rejects.toThrow(/already defended/u);
    await expect(defendersProofFor(defended, operator, [])).rejects.toThrow(
      /not the recorded ones/u,
    );
  });

  it("refuses a Defend that carries the challenger's own Terminal Rejected state", async () => {
    const caseUtxo = await liveCase(undefended);
    const defenders = await defendersProofFor(caseUtxo, operator);
    const { tx } = await defendTx(
      scene,
      caseUtxo,
      operator,
      undefended.challengerTrace,
      defenders,
    );
    await expectOnchainRefusal(() => tx.complete({ localUPLCEval: true }), {
      refusedBy: CASE_V1,
      check: /defense_is_valid/u,
    });
  });

  it("refuses a second Defend by an operator that already defended the case", async () => {
    const { material, defenders } = landed.get(extended)!;
    const { tx } = await defendTx(
      scene,
      await liveCase(extended),
      operator,
      defenseOf(material),
      defenders,
    );
    await expectOnchainRefusal(() => tx.complete({ localUPLCEval: true }), {
      refusedBy: CASE_V1,
      check: /validation_case_v1\.defend\(/u,
    });
  });

  it("refuses a Defend whose lower bound is past the defender's clock, and admits one at the clock's last instant", async () => {
    const caseUtxo = await liveCase(undefended);
    const deadline =
      Number(Data.from(caseUtxo.datum!, SDK.Case).started_at) +
      MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS;
    const lucid = scene.h.proverLucid;
    let slot = lucid.unixTimeToSlot(deadline);
    while (lucid.slotToUnixTime(slot) > deadline) slot -= 1;
    while (lucid.slotToUnixTime(slot + 1) <= deadline) slot += 1;
    scene.h.emulator.awaitSlot(slot - lucid.currentSlot());
    const material = await materialAt(undefended.stepCount);
    const defenders = await defendersProofFor(caseUtxo, operator);
    const defend = async () =>
      (
        await defendTx(
          scene,
          caseUtxo,
          operator,
          defenseOf(material),
          defenders,
        )
      ).tx.complete({ localUPLCEval: true });
    await defend();
    scene.h.emulator.awaitSlot(1);
    await expectOnchainRefusal(defend, {
      refusedBy: CASE_V1,
      check: /expect elapsed <= clock/u,
    });
  });
});
