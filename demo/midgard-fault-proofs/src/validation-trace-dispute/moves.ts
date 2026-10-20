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
  completeValidationTraceMove,
  validationTraceMoveWindow,
} from "./move-builders.js";
import {
  isThreadMaterial,
  readValidationTraceThread,
  validationTraceDefenderDeadlineMs,
  type ValidationTraceThread,
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
    /** The case output our VerifySource left, which the authority binds. */
    caseOriginOutRef: string;
  }>;

/** The thread at `threadOutRef`, a thread of our case at `at`, or changed. */
const liveThread = async (
  {
    lucid,
    signer,
    deployment,
    threadOutRef,
    caseUnit,
  }: ValidationTraceMoveInput,
  at: "game" | "timeout",
): Promise<ValidationTraceThread> => {
  const utxo = await ours(lucid, threadOutRef, "thread");
  const caseId = caseUnit.slice(56);
  const datum =
    typeof utxo.datum === "string"
      ? Data.from(utxo.datum, SDK.CaseThreadDatum)
      : undefined;
  const defenderVkh = datum?.data?.header.defender_vkh ?? "";
  const address =
    at === "game"
      ? deployment.chain.game.spendingScriptAddress
      : deployment.chain.timeout.spendingScriptAddress;
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

/** Signs, grants the move authority and names the move's own outputs. */
const captureMove = async ({
  input,
  stage,
  unsigned,
  thread,
  caseInput,
  slots,
}: {
  readonly input: ValidationTraceMoveInput;
  readonly stage: "reveal" | "timeout" | "close_thread";
  readonly unsigned: TxSignBuilder;
  readonly thread: ValidationTraceThread;
  readonly caseInput?: UTxO;
  readonly slots: readonly Readonly<{ address: string; unit: string }>[];
}): Promise<CapturedValidationTraceStage> => {
  const signed = await unsigned.sign.withWallet().complete();
  grantValidationDisputeMoveAuthority({
    signed,
    kind: stage,
    deploymentFingerprint: input.deployment.deploymentFingerprint,
    headerHash: input.artifact.headerHash,
    caseOriginOutRef: input.caseOriginOutRef,
    thread: thread.utxo,
    ...(caseInput === undefined ? {} : { caseInput }),
  });
  const { references } = input.deployment;
  const transaction = captured(signed, [
    { role: "validation-trace-game", utxo: references.game },
    { role: "validation-case", utxo: references.case },
    { role: "validation-trace-timeout", utxo: references.timeout },
    { role: "validation-trace-award", utxo: references.award },
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
  const unsigned = await completeValidationTraceMove(
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
      validFrom,
      validTo,
    }),
  );
  return await captureMove({
    input,
    stage: "reveal",
    unsigned,
    thread,
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
  const unsigned = await completeValidationTraceMove(
    buildValidationTraceGameMove({
      lucid,
      deployment,
      thread: thread.utxo,
      action: { kind: "EnterChallengerTimeout" },
      successor: { address, datum: thread.utxo.datum! },
      validFrom,
      validTo,
    }),
  );
  return await captureMove({
    input,
    stage: "timeout",
    unsigned,
    thread,
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
  const { tx, receiptUnit } = buildValidationTraceThreadClose({
    lucid,
    deployment,
    thread,
    caseUtxo,
    caseConfig,
    ...validationTraceMoveWindow(lucid),
  });
  return await captureMove({
    input,
    stage: "close_thread",
    unsigned: await completeValidationTraceMove(tx),
    thread,
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
