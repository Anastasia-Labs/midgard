/** The challenger's moves on a thread a defender opened on our case, made
 * now from the thread's live state: reveal our midpoint on our turn, move a
 * thread whose defender ran out of clock to the timeout validator, and close
 * it there into the case with the defender's lost-defense receipt (builders
 * in move-builders.ts). Each signed move carries the funding permit's move
 * authority and names the outputs it leaves. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type TxSignBuilder, type UTxO } from "@lucid-evolution/lucid";

import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import { grantValidationDisputeMoveAuthority } from "../workflow/funding-reservation-permit.assert-validation-dispute-move.js";
import { validationTraceRecovery } from "./actions.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import {
  buildValidationTraceGameMove,
  buildValidationTraceThreadClose,
  challengerRevealedThread,
  validationTraceMoveWindow,
} from "./move-builders.js";
import { completeValidationTraceMove } from "./move-fee.js";
import { validationTraceOneStepContracts } from "./one-step-holders.js";
import {
  isThreadMaterial,
  readValidationTraceThread,
  validationTraceDefenderDeadlineMs,
  type ValidationTraceThread,
  type ValidationTraceThreadHolder,
  type ValidationTraceThreadMove,
  validationTraceThreadTokenName,
} from "./threads.js";
import {
  captured,
  type CapturedValidationTraceStage,
  caseConfigUtxo,
  ours,
  type StageInput,
} from "./transactions.js";

export type ValidationTraceMoveInput = StageInput &
  Readonly<{
    /** The thread the move spends. */
    threadOutRef: string;
    /** Our case's unit, which names the thread's case. */
    caseUnit: string;
    /** The pending claim our Open left, which the authority binds. */
    caseOpenerOutRef: string;
  }>;

/** The thread at `threadOutRef`, a thread of our case at `at`, or changed. */
export const liveThread = async (
  {
    lucid,
    signer,
    deployment,
    threadOutRef,
    caseUnit,
  }: ValidationTraceMoveInput,
  at: ValidationTraceThreadHolder,
): Promise<ValidationTraceThread> => {
  const utxo = await ours(lucid, threadOutRef, "thread");
  const caseId = caseUnit.slice(56);
  const datum =
    typeof utxo.datum === "string"
      ? Data.from(utxo.datum, SDK.CaseThreadDatum)
      : undefined;
  const defenderVkh = datum?.data?.header.defender_vkh ?? "";
  const address = deployment.chain[at].spendingScriptAddress;
  const thread =
    utxo.address === address
      ? readValidationTraceThread({
          utxo,
          unit:
            deployment.chain.case.policyId +
            validationTraceThreadTokenName(caseId, defenderVkh),
          caseId,
          defenderVkh,
          prover: signer.paymentKeyHash,
          at,
        })
      : undefined;
  if (thread === undefined)
    throw new WorkflowActionChangedError(
      `validation-trace ${threadOutRef} is not a thread of case ${caseUnit}`,
    );
  return thread;
};

const awaiting = (thread: SDK.CaseThread) =>
  typeof thread.turn !== "string" && "AwaitingOperator" in thread.turn;

/**
 * Signs, grants the move authority over the thread `thread` of `defenderVkh`
 * and names the move's own outputs.
 */
export const captureMove = async ({
  input,
  stage,
  unsigned,
  thread,
  defenderVkh,
  caseInput,
  slots,
}: {
  readonly input: ValidationTraceMoveInput;
  readonly stage: ValidationTraceThreadMove;
  readonly unsigned: TxSignBuilder;
  readonly thread: Readonly<{ utxo: UTxO }>;
  readonly defenderVkh: string;
  readonly caseInput?: UTxO;
  readonly slots: readonly Readonly<{ address: string; unit: string }>[];
}): Promise<CapturedValidationTraceStage> => {
  const signed = await unsigned.sign.withWallet().complete();
  grantValidationDisputeMoveAuthority({
    signed,
    kind: stage,
    deploymentFingerprint: input.deployment.deploymentFingerprint,
    headerHash: input.artifact.headerHash,
    caseOpenerOutRef: input.caseOpenerOutRef,
    thread: thread.utxo,
    defenderVkh,
    ...(caseInput === undefined ? {} : { caseInput }),
  });
  const { references } = input.deployment;
  const transaction = captured(signed, [
    { role: "validation-trace-game", utxo: references.game },
    { role: "validation-case", utxo: references.case },
    { role: "validation-trace-timeout", utxo: references.timeout },
    { role: "validation-trace-award", utxo: references.award },
    ...validationTraceOneStepContracts(input.deployment).map(
      ({ holder, reference }) => ({
        role: `validation-trace-${holder}`,
        utxo: reference,
      }),
    ),
  ]);
  return {
    transaction,
    durableRecovery: validationTraceRecovery({ stage, transaction, slots }),
  };
};

/** RevealChallenger: our midpoint on a thread on our turn, now. */
export const captureValidationTraceReveal = async (
  input: ValidationTraceMoveInput & {
    readonly material: ValidationTraceClaimMaterial | undefined;
  },
): Promise<CapturedValidationTraceStage> => {
  const { lucid, signer, deployment, material } = input;
  const thread = await liveThread(input, "game");
  const { turn } = thread.thread;
  if (typeof turn === "string" || !("AwaitingChallenger" in turn))
    throw new WorkflowActionChangedError(
      `validation-trace thread ${thread.outRef} is not on our turn`,
    );
  if (!isThreadMaterial(thread.thread, material))
    throw new Error(
      "validation-trace reveal has no trace matching its thread's challenger root",
    );
  const proof = SDK.validationTraceProofDataFromCore(
    material.leafProof(Number(turn.AwaitingChallenger.midpoint)),
  );
  signer.selectWallet(lucid);
  const { validFrom, validTo } = validationTraceMoveWindow(lucid);
  const next = challengerRevealedThread({
    thread: thread.thread,
    proof,
    lower: BigInt(validFrom),
    upper: BigInt(validTo - 1),
  });
  const address = deployment.chain.game.spendingScriptAddress;
  const unsigned = await completeValidationTraceMove({
    lucid,
    stage: "reveal",
    referenced: [deployment.references.game],
    build: (fee) =>
      buildValidationTraceGameMove({
        lucid,
        deployment,
        thread: thread.utxo,
        action: { kind: "RevealChallenger", proof },
        successor: {
          address,
          datum: Data.to(
            { fraud_prover: signer.paymentKeyHash, data: next },
            SDK.CaseThreadDatum,
          ),
        },
        fee,
        validFrom,
        validTo,
      }),
  });
  return await captureMove({
    input,
    stage: "reveal",
    unsigned,
    thread,
    defenderVkh: thread.thread.header.defender_vkh,
    slots: [{ address, unit: thread.unit }],
  });
};

/** EnterChallengerTimeout: a thread whose defender's clock ran out, now. */
export const captureValidationTraceTimeout = async (
  input: ValidationTraceMoveInput,
): Promise<CapturedValidationTraceStage> => {
  const { lucid, signer, deployment } = input;
  const thread = await liveThread(input, "game");
  if (!awaiting(thread.thread))
    throw new WorkflowActionChangedError(
      `validation-trace thread ${thread.outRef} is not on its defender's turn`,
    );
  signer.selectWallet(lucid);
  const { validFrom, validTo } = validationTraceMoveWindow(lucid);
  const deadline = validationTraceDefenderDeadlineMs(thread.thread);
  if (validFrom <= deadline)
    throw new Error(
      `validation-trace thread cannot time out before its defender's deadline ${deadline.toString()}`,
    );
  const address = deployment.chain.timeout.spendingScriptAddress;
  const unsigned = await completeValidationTraceMove({
    lucid,
    stage: "timeout",
    referenced: [deployment.references.game],
    build: (fee) =>
      buildValidationTraceGameMove({
        lucid,
        deployment,
        thread: thread.utxo,
        action: { kind: "EnterChallengerTimeout" },
        successor: { address, datum: thread.utxo.datum! },
        fee,
        validFrom,
        validTo,
      }),
  });
  return await captureMove({
    input,
    stage: "timeout",
    unsigned,
    thread,
    defenderVkh: thread.thread.header.defender_vkh,
    slots: [{ address, unit: thread.unit }],
  });
};

/** ThreadClose: a thread timed out on its defender's side, into our case. */
export const captureValidationTraceThreadClose = async (
  input: ValidationTraceMoveInput & { readonly caseOutRef: string },
): Promise<CapturedValidationTraceStage> => {
  const { lucid, signer, deployment, caseUnit } = input;
  const thread = await liveThread(input, "timeout");
  if (!awaiting(thread.thread))
    throw new WorkflowActionChangedError(
      `validation-trace thread ${thread.outRef} did not time out its defender`,
    );
  const caseUtxo = await ours(lucid, input.caseOutRef, "case");
  if (
    caseUtxo.address !== deployment.chain.case.spendingScriptAddress ||
    caseUtxo.assets[caseUnit] !== 1n ||
    typeof caseUtxo.datum !== "string"
  )
    throw new WorkflowActionChangedError(
      `validation-trace ${input.caseOutRef} is not case ${caseUnit}`,
    );
  const caseConfig = await caseConfigUtxo(lucid, deployment);
  signer.selectWallet(lucid);
  const window = validationTraceMoveWindow(lucid);
  const close = (fee: bigint) =>
    buildValidationTraceThreadClose({
      lucid,
      deployment,
      via: "timeout",
      thread,
      caseId: thread.thread.case_id,
      defenderVkh: thread.thread.header.defender_vkh,
      caseUtxo,
      caseConfig,
      fee,
      ...window,
    });
  const { references } = deployment;
  const unsigned = await completeValidationTraceMove({
    lucid,
    stage: "close_thread",
    referenced: [
      caseConfig,
      references.case,
      references.timeout,
      references.award,
    ],
    build: (fee) => close(fee).tx,
  });
  const { receiptUnit } = close(0n);
  return await captureMove({
    input,
    stage: "close_thread",
    unsigned,
    thread,
    defenderVkh: thread.thread.header.defender_vkh,
    caseInput: caseUtxo,
    slots: [
      { address: deployment.chain.case.spendingScriptAddress, unit: caseUnit },
      {
        address: deployment.chain.award.spendingScriptAddress,
        unit: receiptUnit,
      },
    ],
  });
};
