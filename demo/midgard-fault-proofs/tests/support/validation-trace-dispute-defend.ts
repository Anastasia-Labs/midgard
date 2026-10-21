/** A Defend on the Open scene's real deployment, built by the defender's
 * own builder: an active operator defends the case with a trace whose state
 * at the case's step count is not the challenger's, starting one thread. By
 * default the operator that committed the challenged block defends with a
 * trace pending throughout. */
import {
  type MidgardValidationMachineState,
  type MidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  buildValidationTraceDefend,
  VALIDATION_TRACE_THREAD_FLOOR_LOVELACE,
  type ValidationTraceDefendersProof,
  validationTraceDefendersProof,
} from "../../src/validation-trace-dispute/defend.js";
import { validationTraceMoveWindow } from "../../src/validation-trace-dispute/move-builders.js";
import { network } from "./submit-init-emulator-shared.js";
import { sparseTrace } from "./validation-game-deep-traces.js";
import { machineState } from "./validation-game-traces.js";
import { type Scene } from "./validation-trace-dispute-open-scene.js";

const hex = (bytes: Buffer) => bytes.toString("hex");

/** An active operator that defends: its key, its node's unit and its wallet. */
export type Defender = Readonly<{
  vkey: string;
  nodeUnit: string;
  lucid: LucidEvolution;
}>;

/** The trace a defense commits: its root, and its last state with that state's proof. */
export type DefenseTrace = Readonly<{
  root: string;
  lastState: MidgardValidationMachineState;
  lastStateProof: MidgardValidationTraceProof;
}>;

/** The scene's operator, which committed the challenged block. */
export const sceneOperator = (scene: Scene): Defender => ({
  ...scene.operator,
  lucid: scene.h.funderLucid,
});

/**
 * Defend `caseUtxo` by the scene's operator, now, signed by its wallet, with
 * a trace still pending at the case's last state: never the challenger's
 * Terminal Rejected one. Returns the Defend's hash once it lands.
 */
export const defendAsOperator = async (
  scene: Scene,
  caseUtxo: UTxO,
  decoys: readonly string[] = [],
): Promise<string> => {
  const stepCount = Number(Data.from(caseUtxo.datum!, SDK.Case).step_count);
  const claim = sparseTrace(
    [{ from: 0, state: machineState(0, "cek") }],
    stepCount,
  );
  return await defendWith(
    scene,
    caseUtxo,
    sceneOperator(scene),
    {
      root: hex(claim.descriptor.traceRoot),
      lastState: claim.stateAt(stepCount),
      lastStateProof: claim.proofAt(stepCount),
    },
    [],
    decoys,
  );
};

/** The defender's active-operator node, by its unit. */
export const activeNodeOf = async (
  scene: Scene,
  defender: Defender,
): Promise<UTxO> => {
  const [node] = await defender.lucid.utxosAtWithUnit(
    scene.h.contracts.activeOperators.spendingScriptAddress,
    defender.nodeUnit,
  );
  if (node === undefined) throw new Error("the operator has no active node");
  return node;
};

/**
 * The Defend of `caseUtxo` by `defender` committing `trace`, made now
 * (`validationTraceMoveWindow`), before completion; `defenders` joins it to
 * the case's defenders.
 */
export const defendTx = async (
  scene: Scene,
  caseUtxo: UTxO,
  defender: Defender,
  trace: DefenseTrace,
  defenders: ValidationTraceDefendersProof,
): Promise<ReturnType<typeof buildValidationTraceDefend>> =>
  buildValidationTraceDefend({
    lucid: defender.lucid,
    deployment: { network, chain: scene.chain, references: scene.references },
    caseUtxo,
    caseConfig: scene.caseConfig,
    hubOracle: scene.hubOracle,
    activeNode: await activeNodeOf(scene, defender),
    activeOperatorsReference: scene.references.activeOperators,
    defenderVkh: defender.vkey,
    defenders,
    defense: {
      root: trace.root,
      state: trace.lastState,
      stateProof: trace.lastStateProof,
    },
    ...validationTraceMoveWindow(defender.lucid),
  });

/** The proof that `defender` joins `caseUtxo`'s `priorDefenders`. */
export const defendersProofFor = async (
  caseUtxo: UTxO,
  defender: Defender,
  priorDefenders: readonly string[] = [],
): Promise<ValidationTraceDefendersProof> =>
  await validationTraceDefendersProof({
    caseDatum: Data.from(caseUtxo.datum!, SDK.Case),
    priorDefenders,
    defenderVkh: defender.vkey,
  });

/**
 * `built` with one extra output after the Defend's own for each of
 * `decoys`: the Defend's thread datum renamed to that defender, at the game
 * validator, holding the thread floor and no thread token.
 */
export const withDecoyThreads = (
  scene: Scene,
  caseUtxo: UTxO,
  built: ReturnType<typeof buildValidationTraceDefend>,
  decoys: readonly string[],
): TxBuilder => {
  const fraudProver = Data.from(caseUtxo.datum!, SDK.Case).bounty_claimant;
  const config = Data.from(scene.caseConfig.datum!, SDK.CaseConfig);
  const game = credentialToAddress(network, {
    type: "Script",
    hash: config.game_v1,
  });
  let tx = built.tx;
  for (const decoy of decoys)
    tx = tx.pay.ToContract(
      game,
      {
        kind: "inline",
        value: Data.to(
          {
            fraud_prover: fraudProver,
            data: {
              ...built.thread,
              header: { ...built.thread.header, defender_vkh: decoy },
            },
          },
          SDK.CaseThreadDatum,
        ),
      },
      { lovelace: VALIDATION_TRACE_THREAD_FLOOR_LOVELACE },
    );
  return tx;
};

/**
 * Defend `caseUtxo` by `defender`, now, signed by its wallet, committing
 * `trace`. `priorDefenders` are the keys that already defended the case.
 * The thread is the Defend's output 1. Each of `decoys` names a defender of
 * an extra output after the Defend's own: a thread datum of the case for
 * that defender, at the game validator, with no thread token. Returns the
 * Defend's hash once it lands.
 */
export const defendWith = async (
  scene: Scene,
  caseUtxo: UTxO,
  defender: Defender,
  trace: DefenseTrace,
  priorDefenders: readonly string[] = [],
  decoys: readonly string[] = [],
): Promise<string> => {
  const { lucid } = defender;
  const built = await defendTx(
    scene,
    caseUtxo,
    defender,
    trace,
    await defendersProofFor(caseUtxo, defender, priorDefenders),
  );
  const tx = withDecoyThreads(scene, caseUtxo, built, decoys);
  const signed = await (await tx.complete({ localUPLCEval: true })).sign
    .withWallet()
    .complete();
  const hash = await signed.submit();
  await lucid.awaitTx(hash);
  return hash;
};
