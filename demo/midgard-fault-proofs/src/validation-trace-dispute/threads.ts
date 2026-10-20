/** The threads defenders opened on a case of ours, and what the challenger
 * does next on each.
 *
 * A thread carries one token of the case policy, which mints it only in a
 * Defend on that case, named hash(case id ‖ defender key hash). With no
 * spent-by query to follow our case output into the Defend that spent it,
 * the threads are found by that policy-unique token (the fallback path): the
 * candidate defenders are every operator in the hub's active and retired
 * operator lists, and each candidate's token is looked up at the game and
 * timeout validators. The threads found are checked against the case's own
 * count of open threads; a shortfall holds and reports, never decides.
 *
 * Each thread is played on its own: on our turn we reveal our midpoint; a
 * defender whose clock ran out is timed out and its thread closed into the
 * case; ReadyForOneStep stops at a resumable "one-step required" state. A
 * thread on our turn is never timed out. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofWorkflowObservation } from "../workflow/orchestrator.js";
import { validationTraceAction } from "./actions.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import {
  hubOracleUtxo,
  type ValidationTraceDisputeDeployment,
} from "./transactions.js";

/** A defended case whose threads are all closed; its close is not installed. */
export const VALIDATION_TRACE_CASE_CONTESTED =
  "validation-trace-case-contested";

/** A thread of our case waits on its defender's move until a deadline. */
export const VALIDATION_TRACE_THREAD_AWAITING =
  "validation-trace-thread-awaiting-defender";

/** A thread reached ReadyForOneStep: the one-step resolution must run. */
export const VALIDATION_TRACE_ONE_STEP_REQUIRED =
  "validation-trace-one-step-required";

/** A thread went the defender's way; the challenger has no move on it. */
export const VALIDATION_TRACE_THREAD_LOST = "validation-trace-thread-lost";

/** The threads found differ from the case's count of open threads. */
export const VALIDATION_TRACE_THREAD_COUNT_HOLD =
  "validation-trace-thread-count-hold";

/** Our turn on a thread, but no trace of ours matches its roots. */
export const VALIDATION_TRACE_MATERIAL_HOLD = "validation-trace-material-hold";

/** How the threads were found: by token, with no spent-by query. */
export const VALIDATION_TRACE_THREAD_DISCOVERY = "fallback" as const;

const blake2b256 = (hex: string): string =>
  Effect.runSync(SDK.hashHexWithBlake2b(hex, 32));

/** hash(case id ‖ defender key hash): a thread token's and receipt's name. */
export const validationTraceThreadTokenName = (
  caseId: string,
  defenderVkh: string,
): string => blake2b256(caseId + defenderVkh);

/** A thread of our case, as found and checked. */
export type ValidationTraceThread = Readonly<{
  utxo: UTxO;
  outRef: string;
  unit: string;
  at: "game" | "timeout";
  thread: SDK.CaseThread;
}>;

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const addressOf = (network: Network, address: SDK.AddressData): string => {
  const credential = (value: SDK.CredentialD) =>
    "PublicKeyCredential" in value
      ? { type: "Key" as const, hash: value.PublicKeyCredential[0] }
      : { type: "Script" as const, hash: value.ScriptCredential[0] };
  if (address.stakeCredential === null)
    return credentialToAddress(network, credential(address.paymentCredential));
  if ("Inline" in address.stakeCredential)
    return credentialToAddress(
      network,
      credential(address.paymentCredential),
      credential(address.stakeCredential.Inline[0]),
    );
  throw new Error("validation-trace operator list has a pointer address");
};

/**
 * `utxo` as a thread of case `caseId` for `defenderVkh` under `unit`, or
 * undefined when it is not one.
 */
export const readValidationTraceThread = ({
  utxo,
  unit,
  caseId,
  defenderVkh,
  prover,
  at,
}: {
  readonly utxo: UTxO;
  readonly unit: string;
  readonly caseId: string;
  readonly defenderVkh: string;
  readonly prover: string;
  readonly at: "game" | "timeout";
}): ValidationTraceThread | undefined => {
  if (utxo.assets[unit] !== 1n || typeof utxo.datum !== "string") return;
  let datum: SDK.CaseThreadDatum;
  try {
    datum = Data.from(utxo.datum, SDK.CaseThreadDatum);
  } catch {
    return;
  }
  const thread = datum.data;
  if (
    thread === null ||
    datum.fraud_prover !== prover ||
    thread.case_id !== caseId ||
    thread.header.defender_vkh !== defenderVkh
  )
    return;
  return Object.freeze({ utxo, outRef: outRefOf(utxo), unit, at, thread });
};

/**
 * The live threads of case `caseId`, found by their tokens over every
 * operator the hub lists as active or retired, sorted by token.
 */
export const discoverValidationTraceThreads = async ({
  lucid,
  deployment,
  hubOracle,
  caseId,
  prover,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly hubOracle: UTxO;
  readonly caseId: string;
  readonly prover: string;
}): Promise<readonly ValidationTraceThread[]> => {
  const hub = Data.from(hubOracle.datum!, SDK.HubOracleDatum);
  const lists = [
    {
      address: addressOf(deployment.network, hub.active_operators_addr),
      prefix: hub.active_operators + SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
    },
    {
      address: addressOf(deployment.network, hub.retired_operators_addr),
      prefix:
        hub.retired_operators + SDK.RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
    },
  ];
  const defenders = new Set<string>();
  for (const { address, prefix } of lists)
    for (const node of await lucid.utxosAt(address))
      for (const [unit, quantity] of Object.entries(node.assets))
        if (
          quantity === 1n &&
          unit.startsWith(prefix) &&
          unit.length === prefix.length + 56
        )
          defenders.add(unit.slice(prefix.length));
  const { chain } = deployment;
  const found: ValidationTraceThread[] = [];
  for (const defenderVkh of [...defenders].sort()) {
    const unit =
      chain.case.policyId + validationTraceThreadTokenName(caseId, defenderVkh);
    for (const [at, address] of [
      ["game", chain.game.spendingScriptAddress],
      ["timeout", chain.timeout.spendingScriptAddress],
    ] as const)
      for (const utxo of await lucid.utxosAtWithUnit(address, unit)) {
        const read = readValidationTraceThread({
          utxo,
          unit,
          caseId,
          defenderVkh,
          prover,
          at,
        });
        if (read !== undefined) found.push(read);
      }
  }
  return Object.freeze(
    found.sort((a, b) => (a.unit < b.unit ? -1 : a.unit > b.unit ? 1 : 0)),
  );
};

export type ValidationTraceThreadDecision =
  | Readonly<{
      kind: "move";
      stage: "reveal" | "timeout" | "close_thread";
      thread: ValidationTraceThread;
    }>
  | Readonly<{ kind: "pending"; reason: string }>;

/** The end of a defender's turn: a timeout lands with its lower bound past it. */
export const validationTraceDefenderDeadlineMs = (
  thread: SDK.CaseThread,
): number => Number(thread.turn_started_at + thread.defender_clock_ms);

/** The end of our turn: a reveal's lower bound may not pass it. */
const challengerDeadlineMs = (thread: SDK.CaseThread): number =>
  Number(thread.turn_started_at + thread.challenger_clock_ms);

/** Whether `material` is the trace the thread's challenger root commits. */
export const isThreadMaterial = (
  thread: SDK.CaseThread,
  material: ValidationTraceClaimMaterial | undefined,
): material is ValidationTraceClaimMaterial =>
  material !== undefined &&
  material.challengerRoot === thread.challenger_root &&
  material.stepCount === thread.step_count;

/**
 * The next action over the threads of our case at `caseOutRef`. Our reveals
 * come first, then timeouts, then closes; otherwise the most pressing wait.
 */
export const decideValidationTraceThreads = ({
  threads,
  caseUnit,
  caseOutRef,
  caseDatum,
  now,
  material,
}: {
  readonly threads: readonly ValidationTraceThread[];
  readonly caseUnit: string;
  readonly caseOutRef: string;
  readonly caseDatum: SDK.Case;
  readonly now: number;
  readonly material: ValidationTraceClaimMaterial | undefined;
}): ValidationTraceThreadDecision => {
  const moves: Record<"reveal" | "timeout" | "close_thread", typeof threads> = {
    reveal: [],
    timeout: [],
    close_thread: [],
  };
  const oneStep: string[] = [];
  const lost: string[] = [];
  const held: string[] = [];
  const awaiting: { reason: string; deadline: number }[] = [];
  for (const found of threads) {
    const { thread, unit, outRef } = found;
    const turn = thread.turn;
    const label = `thread ${unit} at ${outRef}`;
    if (found.at === "timeout") {
      if (typeof turn !== "string" && "AwaitingOperator" in turn)
        moves.close_thread = [...moves.close_thread, found];
      else lost.push(`${label} timed out on the challenger's side`);
    } else if (turn === "ReadyForOneStep")
      oneStep.push(
        `${label} is ReadyForOneStep between states ${thread.low_index.toString()} and ${thread.high_index.toString()}`,
      );
    else if ("AwaitingChallenger" in turn) {
      if (now > challengerDeadlineMs(thread))
        lost.push(`${label} ran out of the challenger's clock`);
      else if (!isThreadMaterial(thread, material))
        held.push(`${label} commits a trace this workflow does not hold`);
      else moves.reveal = [...moves.reveal, found];
    } else if (now > validationTraceDefenderDeadlineMs(thread))
      moves.timeout = [...moves.timeout, found];
    else {
      const deadline = validationTraceDefenderDeadlineMs(thread);
      awaiting.push({
        deadline,
        reason: `${VALIDATION_TRACE_THREAD_AWAITING}: ${label} awaits its defender until ${deadline.toString()}`,
      });
    }
  }
  for (const stage of ["reveal", "timeout", "close_thread"] as const)
    if (moves[stage].length > 0)
      return { kind: "move", stage, thread: moves[stage][0]! };
  const pending = (reason: string) => ({ kind: "pending" as const, reason });
  if (oneStep.length > 0)
    return pending(
      `${VALIDATION_TRACE_ONE_STEP_REQUIRED}: ${oneStep.join("; ")}; the one-step resolution is not installed, so it waits`,
    );
  if (lost.length > 0)
    return pending(`${VALIDATION_TRACE_THREAD_LOST}: ${lost.join("; ")}`);
  if (held.length > 0)
    return pending(`${VALIDATION_TRACE_MATERIAL_HOLD}: ${held.join("; ")}`);
  if (BigInt(threads.length) !== caseDatum.open_threads)
    return pending(
      `${VALIDATION_TRACE_THREAD_COUNT_HOLD}: case ${caseUnit} at ${caseOutRef} counts ${caseDatum.open_threads.toString()} open threads; ${VALIDATION_TRACE_THREAD_DISCOVERY} discovery found ${threads.length.toString()}, so it holds until they agree`,
    );
  const [first] = awaiting.sort((a, b) => a.deadline - b.deadline);
  if (first !== undefined) return pending(first.reason);
  return pending(
    `${VALIDATION_TRACE_CASE_CONTESTED}: case ${caseUnit} at ${caseOutRef} was defended and has no open thread; its close is not installed, so it waits`,
  );
};

/**
 * The next step on our defended case `caseUnit` at `caseOutRef`: a move on
 * one of its threads, or why it waits. `caseOriginOutRef` is the case output
 * our VerifySource left, which every move's funding authority binds; a case
 * this workflow did not verify holds.
 */
export const observeValidationTraceThreads = async ({
  lucid,
  signer,
  deployment,
  caseUnit,
  caseOutRef,
  caseDatum,
  caseOriginOutRef,
  now,
  material,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly caseUnit: string;
  readonly caseOutRef: string;
  readonly caseDatum: SDK.Case;
  readonly caseOriginOutRef: string | undefined;
  readonly now: number;
  readonly material: ValidationTraceClaimMaterial | undefined;
}): Promise<FraudProofWorkflowObservation> => {
  if (caseOriginOutRef === undefined)
    return {
      kind: "pending",
      reason: `${VALIDATION_TRACE_CASE_CONTESTED}: case ${caseUnit} at ${caseOutRef} has a defender but this workflow did not verify it, so it waits`,
    };
  const decision = decideValidationTraceThreads({
    threads: await discoverValidationTraceThreads({
      lucid,
      deployment,
      hubOracle: await hubOracleUtxo(lucid, deployment),
      caseId: caseUnit.slice(56),
      prover: signer.paymentKeyHash,
    }),
    caseUnit,
    caseOutRef,
    caseDatum,
    now,
    material,
  });
  return decision.kind === "pending"
    ? decision
    : {
        kind: "action_required",
        action: validationTraceAction({
          stage: decision.stage,
          consumes: decision.thread.outRef,
        }),
      };
};
