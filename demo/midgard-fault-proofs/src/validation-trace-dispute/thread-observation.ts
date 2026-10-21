/** The threads of our defended case as the chain holds them, and the next
 * step on the case they decide (threads.ts). Each thread its history holds
 * is re-read by its token at the game, timeout and boundary validators and,
 * once the deployment installs the one-step resolution, at each holder past
 * the boundary, where the holder's datum must name us its prover. A thread
 * ReadyForOneStep on a trace of ours is planned from that trace. With every
 * thread closed and none the defender's, the case closes once its defense
 * deadline has passed, as an undefended case does. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofWorkflowObservation } from "../workflow/orchestrator.js";
import { validationTraceAction } from "./actions.js";
import {
  recordedValidationTraceThreads,
  type ValidationTraceCaseHistory,
  type ValidationTraceRecordedThread,
} from "./case-history.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import {
  validationTraceOneStepContracts,
  validationTraceOneStepDrives,
} from "./one-step-holders.js";
import {
  planValidationTraceThread,
  type ValidationTraceOneStepPlanning,
} from "./one-step-plan.js";
import {
  decideValidationTraceThreads,
  isThreadMaterial,
  readValidationTraceThread,
  type ValidationTraceResolvingThread,
  type ValidationTraceThread,
} from "./threads.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";
import {
  validationTraceCaseOpenReason,
  validationTraceDefenseDeadlineMs,
} from "./waiting.js";

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The threads case `caseUnit`'s history holds open, with their defenders. */
export const validationTraceRecordedThreads = async ({
  caseHistory,
  headerHash,
  caseUnit,
}: {
  readonly caseHistory: ValidationTraceCaseHistory;
  readonly headerHash: string;
  readonly caseUnit: string;
}): Promise<readonly ValidationTraceRecordedThread[]> =>
  recordedValidationTraceThreads({
    transactions: await caseHistory.transactions({ headerHash, caseUnit }),
    caseUnit,
  });

/** The prover a one-step holder's step datum names, if it names one. */
export const validationTraceOneStepProver = (
  utxo: UTxO,
): string | undefined => {
  if (typeof utxo.datum !== "string") return;
  try {
    const datum = Data.from(utxo.datum);
    const [prover] = datum instanceof Constr ? datum.fields : [];
    return typeof prover === "string" ? prover : undefined;
  } catch {
    return;
  }
};

/**
 * The live threads of case `caseUnit` its history `recorded` holds, sorted
 * by token: at the game, timeout and boundary, and past the boundary at the
 * one-step holders the deployment installs.
 */
export const discoverValidationTraceThreads = async ({
  lucid,
  deployment,
  recorded,
  caseUnit,
  prover,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly recorded: readonly ValidationTraceRecordedThread[];
  readonly caseUnit: string;
  readonly prover: string;
}): Promise<
  Readonly<{
    threads: readonly ValidationTraceThread[];
    resolving: readonly ValidationTraceResolvingThread[];
  }>
> => {
  const caseId = caseUnit.slice(56);
  const { chain } = deployment;
  const threads: ValidationTraceThread[] = [];
  const resolving: ValidationTraceResolvingThread[] = [];
  const pastBoundary = validationTraceOneStepContracts(deployment).flatMap(
    (contract) =>
      contract.holder === "boundary"
        ? []
        : [{ ...contract, holder: contract.holder }],
  );
  for (const { unit, defenderVkh } of recorded) {
    for (const at of ["game", "timeout", "boundary"] as const)
      for (const utxo of await lucid.utxosAtWithUnit(
        chain[at].spendingScriptAddress,
        unit,
      )) {
        const read = readValidationTraceThread({
          utxo,
          unit,
          caseId,
          defenderVkh,
          prover,
          at,
        });
        if (read !== undefined) threads.push(read);
      }
    for (const { holder, address } of pastBoundary)
      for (const utxo of await lucid.utxosAtWithUnit(address, unit))
        if (
          utxo.assets[unit] === 1n &&
          validationTraceOneStepProver(utxo) === prover
        )
          resolving.push(
            Object.freeze({
              utxo,
              outRef: outRefOf(utxo),
              unit,
              defenderVkh,
              holder,
            }),
          );
  }
  return Object.freeze({
    threads: Object.freeze(threads),
    resolving: Object.freeze(resolving),
  });
};

/** The plan of each thread ReadyForOneStep on our trace, by its out-ref. */
const planReadyThreads = async (
  deployment: ValidationTraceDisputeDeployment,
  threads: readonly ValidationTraceThread[],
  material: ValidationTraceClaimMaterial | undefined,
): Promise<ReadonlyMap<string, ValidationTraceOneStepPlanning>> => {
  const plannings = new Map<string, ValidationTraceOneStepPlanning>();
  if (deployment.references.oneStep === undefined) return plannings;
  for (const { at, thread, outRef } of threads)
    if (
      at === "game" &&
      thread.turn === "ReadyForOneStep" &&
      isThreadMaterial(thread, material)
    )
      plannings.set(
        outRef,
        await planValidationTraceThread(
          material,
          thread,
          validationTraceOneStepDrives(deployment),
        ),
      );
  return plannings;
};

/**
 * The next step on our defended case `caseUnit` at `caseOutRef`: a move on
 * one of its threads, its close once every thread is closed, or why it
 * waits. The case is ours whoever verified it: its id derives from our
 * opener.
 */
export const observeValidationTraceThreads = async ({
  lucid,
  signer,
  deployment,
  caseHistory,
  headerHash,
  caseUnit,
  caseOutRef,
  caseDatum,
  now,
  material,
  skipped,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly caseHistory: ValidationTraceCaseHistory;
  readonly headerHash: string;
  readonly caseUnit: string;
  readonly caseOutRef: string;
  readonly caseDatum: SDK.Case;
  readonly now: number;
  readonly material: ValidationTraceClaimMaterial | undefined;
  /** The action ids this run skipped because they did not build. */
  readonly skipped?: ReadonlySet<string> | undefined;
}): Promise<FraudProofWorkflowObservation> => {
  const { threads, resolving } = await discoverValidationTraceThreads({
    lucid,
    deployment,
    recorded: await validationTraceRecordedThreads({
      caseHistory,
      headerHash,
      caseUnit,
    }),
    caseUnit,
    prover: signer.paymentKeyHash,
  });
  const decision = decideValidationTraceThreads({
    threads,
    resolving,
    plannings: await planReadyThreads(deployment, threads, material),
    caseUnit,
    caseOutRef,
    caseDatum,
    now,
    material,
    ...(skipped === undefined ? {} : { skipped }),
  });
  if (decision.kind === "pending") return decision;
  if (decision.kind === "move")
    return {
      kind: "action_required",
      action: validationTraceAction({
        stage: decision.stage,
        consumes: decision.thread.outRef,
      }),
    };
  // Every thread is closed: the case closes once its defense deadline has
  // passed, as an undefended case does.
  return now > validationTraceDefenseDeadlineMs(caseDatum)
    ? {
        kind: "action_required",
        action: validationTraceAction({ stage: "close", consumes: caseOutRef }),
      }
    : {
        kind: "pending",
        reason: validationTraceCaseOpenReason({
          caseUnit,
          outRef: caseOutRef,
          caseDatum,
        }),
      };
};
