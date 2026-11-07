/** The Defend at the deepest case: a challenger opens cases on the scene's
 * passing block at the largest step count a case admits (a 32-level trace),
 * and the block's operator defends with its own trace at that step count,
 * and with a state whose every field is at its largest encoding. Each Defend
 * lands within the transaction limits. */
import { type MidgardValidationMachineState } from "@al-ft/midgard-core";
import { beforeAll, describe, expect, it } from "vitest";

import { buildValidationTraceDefenseMaterial } from "../src/validation-trace-dispute/defense-material.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.js";
import { sparseTrace } from "./support/validation-game-deep-traces.js";
import { l1SlotConfig } from "./support/validation-trace-claim-material.js";
import {
  type Defender,
  defendersProofFor,
  defendTx,
  type DefenseTrace,
  sceneOperator,
} from "./support/validation-trace-dispute-defend.js";
import {
  failingMaterial,
  initAndOpen,
  type Scene,
  setUpScene,
  verifySource,
} from "./support/validation-trace-dispute-open-scene.js";
import { passingTrace } from "./support/validation-trace-dispute-passing-trace.js";
import { measureWithinTransactionLimits } from "./support/validation-trace-dispute-transaction-limits.js";

/** 2^32 - 1 steps: a 32-level trace tree, the deepest the case admits. */
const MAX_STEP_COUNT = 2 ** 32 - 1;
/** The largest unsigned integer a Data integer encodes without a big-number tag. */
const MAX_UINT64 = 2n ** 64n - 1n;

describe("validation-trace dispute Defend at the deepest case", () => {
  let scene: Scene;
  let operator: Defender;
  let honest: Awaited<ReturnType<typeof passingTrace>>;
  let claimedTerminal: MidgardValidationMachineState;

  /** A case at the deepest step count, opened with a Terminal Rejected claim. */
  const openDeepest = async () => {
    const trace = sparseTrace(
      [
        { from: 0, state: honest.initialState },
        { from: MAX_STEP_COUNT, state: claimedTerminal },
      ],
      MAX_STEP_COUNT,
    );
    const { opener } = await initAndOpen(scene, scene.passing, {
      claimWitness: honest.claim.claimWitness,
      challengerRoot: trace.descriptor.traceRoot.toString("hex"),
      stepCount: BigInt(MAX_STEP_COUNT),
    });
    const signed = await (
      await verifySource(scene, opener, {
        terminal: claimedTerminal,
        terminalProof: trace.proofAt(MAX_STEP_COUNT),
        initialState: honest.initialState,
      }).build()
    ).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    await scene.h.proverLucid.awaitTx(txHash);
    const [caseUtxo] = await scene.h.proverLucid.utxosByOutRef([
      { txHash, outputIndex: 0 },
    ]);
    return caseUtxo!;
  };

  /** Lands the Defend of a new deepest case with `defense`; its measurement. */
  const defendDeepest = async (defense: DefenseTrace, label: string) => {
    expect(defense.lastStateProof.siblings).toHaveLength(32);
    const caseUtxo = await openDeepest();
    const { tx } = await defendTx(
      scene,
      caseUtxo,
      operator,
      defense,
      await defendersProofFor(caseUtxo, operator),
    );
    const signed = await (await tx.complete({ localUPLCEval: true })).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    await operator.lucid.awaitTx(txHash);
    const measured = measureWithinTransactionLimits(signed.toCBOR());
    console.info(
      `fit Defend ${label} S=${MAX_STEP_COUNT.toString()}: bytes=${measured.completeSignedBytes.toString()} mem=${measured.executionMemory.toString()} steps=${measured.executionSteps.toString()}`,
    );
    return measured;
  };

  beforeAll(async () => {
    scene = await setUpScene();
    operator = sceneOperator(scene);
    honest = await passingTrace(scene);
    claimedTerminal = {
      ...honest.terminal,
      verdict: "rejected",
      rejectionCodeHash: (await failingMaterial(scene)).terminal
        .rejectionCodeHash,
    };
  }, 900_000);

  it("lands the Defend of its own honest trace at the deepest step count", async () => {
    const material = await buildValidationTraceDefenseMaterial({
      evidence: scene.passing.evidence,
      predecessor: completeCanonicalReplayPredecessorLedger(scene.passing),
      eventKey: scene.passing.eventKey,
      stepCount: MAX_STEP_COUNT,
      l1SlotConfig,
    });
    if (material.kind !== "defend") throw new Error(material.kind);
    await defendDeepest(
      {
        root: material.root,
        lastState: material.stateAtS,
        lastStateProof: material.proofAtS,
      },
      "honest",
    );
  }, 300_000);

  it("lands the Defend of a state whose every field is at its largest encoding at the deepest step count", async () => {
    const largest: MidgardValidationMachineState = {
      ...honest.initialState,
      // The non-terminal phase with the largest constructor index.
      phase: "valueAndMint",
      programCounter: MAX_STEP_COUNT,
      executionCpu: MAX_UINT64,
      executionMemory: MAX_UINT64,
      verdict: "pending",
    };
    const trace = sparseTrace(
      [
        { from: 0, state: honest.initialState },
        { from: MAX_STEP_COUNT, state: largest },
      ],
      MAX_STEP_COUNT,
    );
    await defendDeepest(
      {
        root: trace.descriptor.traceRoot.toString("hex"),
        lastState: largest,
        lastStateProof: trace.proofAt(MAX_STEP_COUNT),
      },
      "largest-state",
    );
  }, 300_000);
});
