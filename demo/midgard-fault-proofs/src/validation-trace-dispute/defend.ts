/** The Defend of a validation case, before completion: an active operator
 * commits its own trace root and its state at the case's step count, joins
 * the case's defenders, holds its bond on its active-operator node and
 * opens its thread at game-v1 with the thread's reserve. Every script runs
 * from its published reference. */
import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
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
  type RedeemerContext,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import { validationTraceThreadTokenName } from "./case-history.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

/** `validation_case_v1.thread_floor_lovelace`: what a new thread holds. */
export const VALIDATION_TRACE_THREAD_FLOOR_LOVELACE =
  BigInt(SELECTED_DEPLOYMENT_PROFILE.limits.fraud_proof_output_lovelace) +
  MIDGARD_VALIDATION_DISPUTE_THREAD_FEE_RESERVE_LOVELACE;

/** The Defend's output layout. */
const CASE_OUTPUT_INDEX = 0n;
const THREAD_OUTPUT_INDEX = 1n;
const NODE_OUTPUT_INDEX = 2n;

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

const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");

/** The case's defenders root, and the proof that joins one more defender. */
export type ValidationTraceDefendersProof = Readonly<{
  proof: SDK.Proof;
  /** The root once the defender has joined. */
  defendersRoot: string;
}>;

/**
 * The proof that `defenderVkh` joins `caseDatum`'s defenders, given every
 * defender that case ever had (`recordedValidationTraceDefenders`). Throws
 * when `defenderVkh` already defended the case, or when the defenders given
 * are not the ones the case's root holds (an incomplete history).
 */
export const validationTraceDefendersProof = async ({
  caseDatum,
  priorDefenders,
  defenderVkh,
}: {
  readonly caseDatum: SDK.Case;
  readonly priorDefenders: readonly string[];
  readonly defenderVkh: string;
}): Promise<ValidationTraceDefendersProof> => {
  if (priorDefenders.includes(defenderVkh))
    throw new Error("validation defense: this operator already defended");
  const defenders = new Trie();
  for (const prior of new Set(priorDefenders)) {
    const key = Buffer.from(prior, "hex");
    await defenders.insert(key, key);
  }
  const root =
    defenders.hash === null ? SDK.EMPTY_MERKLE_TREE_ROOT : hex(defenders.hash);
  if (root !== caseDatum.defenders_root)
    throw new Error(
      "validation defense: the case's defenders are not the recorded ones",
    );
  const key = Buffer.from(defenderVkh, "hex");
  const proof = Data.from(
    (await defenders.prove(key, true)).toCBOR().toString("hex"),
    SDK.Proof,
  );
  await defenders.insert(key, key);
  return { proof, defendersRoot: hex(defenders.hash!) };
};

/** Our trace at the case's step count S: its root, our state at S and that
 * state's proof under the root. */
export type ValidationTraceDefense = Readonly<{
  root: string;
  state: MidgardValidationMachineState;
  stateProof: MidgardValidationTraceProof;
}>;

/**
 * The Defend of `caseUtxo` by `defenderVkh`, before completion; it needs
 * the defender's signature. The case continues at output 0, the thread opens
 * at output 1 holding the thread floor, and the defender's active node
 * `activeNode` continues at output 2 with its bond held through the case.
 * Outputs a caller adds follow these.
 * The window is the caller's: the Defend's lower bound is charged to the
 * defender's clock, and on chain it must be at most `started_at + C`.
 */
export const buildValidationTraceDefend = ({
  lucid,
  deployment,
  caseUtxo,
  caseConfig,
  hubOracle,
  activeNode,
  activeOperatorsReference,
  defenderVkh,
  defenders,
  defense,
  validFrom,
  validTo,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: Pick<
    ValidationTraceDisputeDeployment,
    "network" | "chain"
  > &
    Readonly<{
      references: Pick<ValidationTraceDisputeDeployment["references"], "case">;
    }>;
  readonly caseUtxo: UTxO;
  readonly caseConfig: UTxO;
  readonly hubOracle: UTxO;
  /** The defender's active-operator node. */
  readonly activeNode: UTxO;
  /** The published reference of the active-operators spending script. */
  readonly activeOperatorsReference: UTxO;
  readonly defenderVkh: string;
  readonly defenders: ValidationTraceDefendersProof;
  readonly defense: ValidationTraceDefense;
  readonly validFrom: number;
  readonly validTo: number;
}): Readonly<{
  tx: TxBuilder;
  threadUnit: string;
  /** The thread the Defend opens at output 1. */
  thread: SDK.CaseThread;
}> => {
  const { chain } = deployment;
  const caseDatum = Data.from(caseUtxo.datum!, SDK.Case);
  const caseIds = Object.keys(caseUtxo.assets)
    .filter((unit) => unit.startsWith(chain.case.policyId))
    .map((unit) => unit.slice(56));
  if (caseIds.length !== 1)
    throw new Error("validation defense: the case output holds no one case");
  const caseId = caseIds[0]!;
  const threadUnit =
    chain.case.policyId + validationTraceThreadTokenName(caseId, defenderVkh);
  const config = Data.from(caseConfig.datum!, SDK.CaseConfig);

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
    defender_root: defense.root,
    step_count: caseDatum.step_count,
    low_index: 0n,
    high_index: caseDatum.step_count,
    agreed_low_hash: caseDatum.agreed_initial_hash,
    defender_high_hash: hex(defense.stateProof.stateHash),
    challenger_high_hash: caseDatum.challenger_terminal_hash,
    round: 0n,
    turn_started_at: upper,
    defender_clock_ms: clock - elapsed,
    challenger_clock_ms: clock,
    turn: firstTurn(caseDatum.step_count),
  };

  const nodeDatum = Data.from(activeNode.datum!, SDK.LinkedListDatum);
  if (!("Node" in nodeDatum.data))
    throw new Error("validation defense: not an active-operator node");
  const operator = Data.castFrom(
    nodeDatum.data.Node.data as Data,
    SDK.ActiveOperatorDatum,
  );
  const needed = defenseBondUnlockTime(caseDatum.started_at);
  const held = operator.bond_unlock_time;
  const continuedNode = Data.to(
    {
      ...nodeDatum,
      data: {
        Node: {
          data: SDK.castActiveOperatorDatumToData({
            ...operator,
            bond_unlock_time: held !== null && held > needed ? held : needed,
          }) as Data,
        },
      },
    },
    SDK.LinkedListDatum,
  );

  const nodeIndex = (ctx: RedeemerContext) =>
    SDK.requireInputIndex(ctx, activeNode, "active node");
  const caseRedeemer = (ctx: RedeemerContext) =>
    Data.to(
      {
        Defend: {
          case_output_index: CASE_OUTPUT_INDEX,
          thread_output_index: THREAD_OUTPUT_INDEX,
          active_node_input_index: nodeIndex(ctx),
          defender_vkh: defenderVkh,
          defenders_proof: defenders.proof,
          defender_root: defense.root,
          defender_state: SDK.validationMachineStateDataFromCore(defense.state),
          defender_state_proof: SDK.validationTraceProofDataFromCore(
            defense.stateProof,
          ),
        },
      },
      SDK.CaseRedeemer,
    );
  const nodeRedeemer = (ctx: RedeemerContext) =>
    Data.to(
      {
        UpdateBondHoldNewDefense: {
          active_operator: defenderVkh,
          active_node_input_index: nodeIndex(ctx),
          active_node_output_index: NODE_OUTPUT_INDEX,
        },
      },
      SDK.ActiveOperatorSpendRedeemer,
    );
  const tx = lucid
    .newTx()
    .collectFrom([caseUtxo], caseRedeemer)
    .collectFrom([activeNode], nodeRedeemer)
    .mintAssets({ [threadUnit]: 1n }, caseRedeemer)
    .readFrom([
      caseConfig,
      hubOracle,
      deployment.references.case,
      activeOperatorsReference,
    ])
    .pay.ToContract(
      caseUtxo.address,
      {
        kind: "inline",
        value: Data.to(
          {
            ...caseDatum,
            open_threads: caseDatum.open_threads + 1n,
            defenders_root: defenders.defendersRoot,
          },
          SDK.Case,
        ),
      },
      caseUtxo.assets,
    )
    .pay.ToContract(
      credentialToAddress(deployment.network, {
        type: "Script",
        hash: config.game_v1,
      }),
      {
        kind: "inline",
        value: Data.to(
          { fraud_prover: caseDatum.bounty_claimant, data: thread },
          SDK.CaseThreadDatum,
        ),
      },
      { lovelace: VALIDATION_TRACE_THREAD_FLOOR_LOVELACE, [threadUnit]: 1n },
    )
    .pay.ToContract(
      activeNode.address,
      { kind: "inline", value: continuedNode },
      activeNode.assets,
    )
    .addSignerKey(defenderVkh)
    .validFrom(validFrom)
    .validTo(validTo);
  return { tx, threadUnit, thread };
};
