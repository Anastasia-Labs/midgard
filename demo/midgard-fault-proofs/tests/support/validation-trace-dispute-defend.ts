/** A Defend on the Open scene's real deployment: an active operator defends
 * the case with a trace whose state at the case's step count is not the
 * challenger's, starting one thread. By default the operator that committed
 * the challenged block defends with a trace pending throughout. */
import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS,
  MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS,
  MIDGARD_VALIDATION_DISPUTE_THREAD_FEE_RESERVE_LOVELACE,
  type MidgardValidationMachineState,
  type MidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import { network } from "./submit-init-emulator-shared.js";
import { threadTokenName } from "./validation-case-names.js";
import { sparseTrace } from "./validation-game-deep-traces.js";
import { machineState } from "./validation-game-traces.js";
import { type Scene } from "./validation-trace-dispute-open-scene.js";

const hex = (bytes: Buffer) => bytes.toString("hex");

/** `validation_case_v1.thread_floor_lovelace`. */
const THREAD_FLOOR_LOVELACE =
  BigInt(SELECTED_DEPLOYMENT_PROFILE.limits.fraud_proof_output_lovelace) +
  MIDGARD_VALIDATION_DISPUTE_THREAD_FEE_RESERVE_LOVELACE;

/** `validation_case_v1.defense_bond_unlock_time`: D_case + P = M - M/2. */
const defenseBondUnlockTime = (startedAt: bigint): bigint => {
  const maturity = BigInt(SELECTED_DEPLOYMENT_PROFILE.timing.block_maturity_ms);
  return startedAt + maturity - maturity / 2n;
};

/** `validation_case_v1.open_thread`'s first turn. */
const firstTurn = (stepCount: bigint): SDK.CaseThread["turn"] =>
  stepCount === 1n
    ? "ReadyForOneStep"
    : { AwaitingOperator: { midpoint: stepCount / 2n } };

/** The case token's name (the case id) among the case output's assets. */
const caseIdOf = (scene: Scene, caseUtxo: UTxO): string => {
  const names = Object.keys(caseUtxo.assets)
    .filter((unit) => unit.startsWith(scene.chain.case.policyId))
    .map((unit) => unit.slice(56));
  if (names.length !== 1) throw new Error("the case output has no one case");
  return names[0]!;
};

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
): Promise<string> => {
  const stepCount = Number(Data.from(caseUtxo.datum!, SDK.Case).step_count);
  const claim = sparseTrace(
    [{ from: 0, state: machineState(0, "cek") }],
    stepCount,
  );
  return await defendWith(scene, caseUtxo, sceneOperator(scene), {
    root: hex(claim.descriptor.traceRoot),
    lastState: claim.stateAt(stepCount),
    lastStateProof: claim.proofAt(stepCount),
  });
};

/**
 * Defend `caseUtxo` by `defender`, now, signed by its wallet, committing
 * `trace`; the thread's datum, the case's and the node's are the ones case-v1
 * requires. `priorDefenders` are the keys that already defended the case.
 * The thread is the Defend's output 1. Returns the Defend's hash once it
 * lands.
 */
export const defendWith = async (
  scene: Scene,
  caseUtxo: UTxO,
  defender: Defender,
  trace: DefenseTrace,
  priorDefenders: readonly string[] = [],
): Promise<string> => {
  const { h, chain } = scene;
  const { lucid } = defender;
  const defenderVkh = defender.vkey;
  const caseDatum = Data.from(caseUtxo.datum!, SDK.Case);
  const caseId = caseIdOf(scene, caseUtxo);
  const defenderState = trace.lastState;
  const defenderStateProof = trace.lastStateProof;

  const defenders = new Trie();
  for (const prior of priorDefenders) {
    const priorKey = Buffer.from(prior, "hex");
    await defenders.insert(priorKey, priorKey);
  }
  const key = Buffer.from(defenderVkh, "hex");
  const defendersProof = Data.from(
    (await defenders.prove(key, true)).toCBOR().toString("hex"),
    SDK.Proof,
  );
  await defenders.insert(key, key);
  const defended: SDK.Case = {
    ...caseDatum,
    open_threads: caseDatum.open_threads + 1n,
    defenders_root: hex(defenders.hash!),
  };

  const config = Data.from(scene.caseConfig.datum!, SDK.CaseConfig);
  const validFrom = h.emulator.now();
  const validTo = validFrom + MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS;
  const lower = BigInt(validFrom);
  const upper = BigInt(validTo - 1);
  const elapsed =
    lower > caseDatum.started_at ? lower - caseDatum.started_at : 0n;
  const clock = BigInt(MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS);
  const thread: SDK.CaseThread = {
    header: { started_at: caseDatum.started_at, defender_vkh: defenderVkh },
    version: 1n,
    case_id: caseId,
    challenger_root: caseDatum.challenger_root,
    defender_root: trace.root,
    step_count: caseDatum.step_count,
    low_index: 0n,
    high_index: caseDatum.step_count,
    agreed_low_hash: caseDatum.agreed_initial_hash,
    defender_high_hash: hex(defenderStateProof.stateHash),
    challenger_high_hash: caseDatum.challenger_terminal_hash,
    round: 0n,
    turn_started_at: upper,
    defender_clock_ms: clock - elapsed,
    challenger_clock_ms: clock,
    turn: firstTurn(caseDatum.step_count),
  };
  const threadUnit = chain.case.policyId + threadTokenName(caseId, defenderVkh);

  const [node] = await lucid.utxosAtWithUnit(
    h.contracts.activeOperators.spendingScriptAddress,
    defender.nodeUnit,
  );
  if (node === undefined) throw new Error("the operator has no active node");
  const nodeDatum = Data.from(node.datum!, SDK.LinkedListDatum);
  if (!("Node" in nodeDatum.data)) throw new Error("not an operator node");
  const operatorData = Data.castFrom(
    nodeDatum.data.Node.data as Data,
    SDK.ActiveOperatorDatum,
  );
  const needed = defenseBondUnlockTime(caseDatum.started_at);
  const old = operatorData.bond_unlock_time;
  const continuedNode = Data.to(
    {
      ...nodeDatum,
      data: {
        Node: {
          data: SDK.castActiveOperatorDatumToData({
            ...operatorData,
            bond_unlock_time: old !== null && old > needed ? old : needed,
          }) as Data,
        },
      },
    },
    SDK.LinkedListDatum,
  );

  const caseRedeemer = (ctx: Parameters<typeof SDK.requireInputIndex>[0]) =>
    Data.to(
      {
        Defend: {
          case_output_index: 0n,
          thread_output_index: 1n,
          active_node_input_index: SDK.requireInputIndex(ctx, node, "node"),
          defender_vkh: defenderVkh,
          defenders_proof: defendersProof,
          defender_root: trace.root,
          defender_state: SDK.validationMachineStateDataFromCore(defenderState),
          defender_state_proof:
            SDK.validationTraceProofDataFromCore(defenderStateProof),
        },
      },
      SDK.CaseRedeemer,
    );
  const nodeRedeemer = (ctx: Parameters<typeof SDK.requireInputIndex>[0]) =>
    Data.to(
      {
        UpdateBondHoldNewDefense: {
          active_operator: defenderVkh,
          active_node_input_index: SDK.requireInputIndex(ctx, node, "node"),
          active_node_output_index: 2n,
        },
      },
      SDK.ActiveOperatorSpendRedeemer,
    );
  const built = await lucid
    .newTx()
    .collectFrom([caseUtxo], caseRedeemer)
    .collectFrom([node], nodeRedeemer)
    .attach.SpendingValidator(h.contracts.activeOperators.spendingScript)
    .mintAssets({ [threadUnit]: 1n }, caseRedeemer)
    .readFrom([scene.caseConfig, scene.hubOracle, scene.references.case])
    .pay.ToContract(
      caseUtxo.address,
      { kind: "inline", value: Data.to(defended, SDK.Case) },
      caseUtxo.assets,
    )
    .pay.ToContract(
      credentialToAddress(network, { type: "Script", hash: config.game_v1 }),
      {
        kind: "inline",
        value: Data.to(
          { fraud_prover: caseDatum.bounty_claimant, data: thread },
          SDK.CaseThreadDatum,
        ),
      },
      { lovelace: THREAD_FLOOR_LOVELACE, [threadUnit]: 1n },
    )
    .pay.ToContract(
      node.address,
      { kind: "inline", value: continuedNode },
      node.assets,
    )
    .addSignerKey(defenderVkh)
    .validFrom(validFrom)
    .validTo(validTo)
    .complete({ localUPLCEval: true });
  const signed = await built.sign.withWallet().complete();
  const hash = await signed.submit();
  await lucid.awaitTx(hash);
  return hash;
};
