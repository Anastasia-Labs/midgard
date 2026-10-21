/** The transactions of the challenger's thread moves, before completion: a
 * game-v1 move of a thread (RevealChallenger, EnterResolution,
 * EnterChallengerTimeout), the thread after our reveal or our entry into the
 * resolution, and the ThreadClose of a thread timed out on its defender's
 * side or awarded to us. The thread pays the fee the caller names
 * (move-fee.ts settles it at the move's minimum, up to the cap); a move the
 * caller names no fee for keeps the thread's lovelace whole, for our
 * wallet's coin selection to pay. */
import { MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS } from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type RedeemerContext,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import { validationTraceThreadTokenName } from "./threads.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

/** `env.fraud_proof_output_lovelace`: what a lost-defense receipt holds. */
const RECEIPT_LOVELACE = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.limits.fraud_proof_output_lovelace,
);

const lovelaceOf = (utxo: UTxO): bigint => utxo.assets.lovelace ?? 0n;

/** The validity range of a move made now: L_d from the current slot. */
export const validationTraceMoveWindow = (lucid: LucidEvolution) => {
  const validFrom = lucid.slotToUnixTime(lucid.currentSlot());
  return {
    validFrom,
    validTo: validFrom + MIDGARD_VALIDATION_DISPUTE_MAX_VALIDITY_RANGE_MS,
  };
};

export type ValidationTraceGameAction =
  | Readonly<{ kind: "RevealChallenger"; proof: SDK.ValidationTraceProof }>
  | Readonly<{ kind: "EnterResolution" }>
  | Readonly<{ kind: "EnterChallengerTimeout" }>;

/**
 * A game move of `thread` to `successor`, which keeps the thread's tokens
 * and its lovelace less `fee`, before completion; signed by `signer` when
 * given (EnterResolution is the prover's alone).
 */
export const buildValidationTraceGameMove = ({
  lucid,
  deployment,
  thread,
  action,
  successor,
  fee,
  validFrom,
  validTo,
  signer,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly thread: UTxO;
  readonly action: ValidationTraceGameAction;
  readonly successor: Readonly<{ address: string; datum: string }>;
  /** What the thread pays for the move. */
  readonly fee: bigint;
  readonly validFrom: number;
  readonly validTo: number;
  readonly signer?: string;
}): TxBuilder => {
  const redeemer = (ctx: RedeemerContext) => {
    const indices = {
      input_index: SDK.requireInputIndex(ctx, thread, "thread"),
      output_index: 0n,
    };
    return Data.to(
      {
        Continue: [
          action.kind === "RevealChallenger"
            ? { RevealChallenger: { ...indices, proof: action.proof } }
            : action.kind === "EnterResolution"
              ? { EnterResolution: indices }
              : { EnterChallengerTimeout: indices },
        ],
      },
      SDK.ValidationGameSpendRedeemer,
    );
  };
  const tx = lucid
    .newTx()
    .collectFrom([thread], redeemer)
    .readFrom([deployment.references.game])
    .pay.ToContract(
      successor.address,
      { kind: "inline", value: successor.datum },
      {
        ...thread.assets,
        lovelace: lovelaceOf(thread) - fee,
      },
    )
    .validFrom(validFrom)
    .validTo(validTo);
  return signer === undefined ? tx : tx.addSignerKey(signer);
};

const charge = (clock: bigint, startedAt: bigint, lower: bigint): bigint => {
  const elapsed = lower > startedAt ? lower - startedAt : 0n;
  if (elapsed > clock)
    throw new Error("validation-trace challenger's clock has run out");
  return clock - elapsed;
};

/**
 * The thread after our EnterResolution (case-thread-v1 `enter_resolution`)
 * by a move whose inclusive bounds are `lower` and `upper`.
 */
export const challengerEnteredResolution = ({
  thread,
  lower,
  upper,
}: {
  readonly thread: SDK.CaseThread;
  readonly lower: bigint;
  readonly upper: bigint;
}): SDK.CaseThread => {
  if (thread.turn !== "ReadyForOneStep")
    throw new Error("validation-trace thread is not ReadyForOneStep");
  return {
    ...thread,
    turn_started_at: upper,
    challenger_clock_ms: charge(
      thread.challenger_clock_ms,
      thread.turn_started_at,
      lower,
    ),
  };
};

/**
 * The thread after our reveal of `proof` (case-thread-v1
 * `reveal_challenger_midpoint`) by a move whose inclusive bounds are
 * `lower` and `upper`.
 */
export const challengerRevealedThread = ({
  thread,
  proof,
  lower,
  upper,
}: {
  readonly thread: SDK.CaseThread;
  readonly proof: SDK.ValidationTraceProof;
  readonly lower: bigint;
  readonly upper: bigint;
}): SDK.CaseThread => {
  const { turn } = thread;
  if (typeof turn === "string" || !("AwaitingChallenger" in turn))
    throw new Error("validation-trace thread is not on the challenger's turn");
  const { midpoint, operator_midpoint_hash } = turn.AwaitingChallenger;
  const agrees = operator_midpoint_hash === proof.state_hash;
  const low = agrees ? midpoint : thread.low_index;
  const high = agrees ? thread.high_index : midpoint;
  return {
    ...thread,
    low_index: low,
    high_index: high,
    agreed_low_hash: agrees ? proof.state_hash : thread.agreed_low_hash,
    defender_high_hash: agrees
      ? thread.defender_high_hash
      : operator_midpoint_hash,
    challenger_high_hash: agrees
      ? thread.challenger_high_hash
      : proof.state_hash,
    round: thread.round + 1n,
    turn_started_at: upper,
    challenger_clock_ms: charge(
      thread.challenger_clock_ms,
      thread.turn_started_at,
      lower,
    ),
    turn:
      high === low + 1n
        ? "ReadyForOneStep"
        : { AwaitingOperator: { midpoint: low + (high - low) / 2n } },
  };
};

/**
 * The ThreadClose of `thread` of defender `defenderVkh` on case `caseId`,
 * won by us at `via` (timed out on its defender's side, or awarded by its
 * one-step resolution), into the case `caseUtxo`: the thread token burns,
 * the defender's lost-defense receipt is minted to the award validator, and
 * the rest of the thread less `fee` goes to the case's bounty claimant.
 * Before completion.
 */
export const buildValidationTraceThreadClose = ({
  lucid,
  deployment,
  via,
  thread,
  caseId,
  defenderVkh,
  caseUtxo,
  caseConfig,
  fee,
  validFrom,
  validTo,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly via: "timeout" | "award";
  readonly thread: Readonly<{ utxo: UTxO; unit: string }>;
  readonly caseId: string;
  readonly defenderVkh: string;
  readonly caseUtxo: UTxO;
  readonly caseConfig: UTxO;
  /** What the thread pays for the close. */
  readonly fee: bigint;
  readonly validFrom: number;
  readonly validTo: number;
}): Readonly<{ tx: TxBuilder; receiptUnit: string }> => {
  const { chain, references } = deployment;
  const caseDatum = Data.from(caseUtxo.datum!, SDK.Case);
  const receiptUnit =
    chain.award.policyId + validationTraceThreadTokenName(caseId, defenderVkh);
  const threadReference =
    via === "timeout" ? references.timeout : references.award;
  const outcome = "ChallengerWins" as const;
  const caseRedeemer = (ctx: RedeemerContext) =>
    Data.to(
      {
        ThreadClose: {
          case_output_index: 0n,
          thread_input_index: SDK.requireInputIndex(ctx, thread.utxo, "thread"),
          outcome,
          defender_vkh: defenderVkh,
        },
      },
      SDK.CaseRedeemer,
    );
  const caseIndex = (ctx: RedeemerContext) =>
    SDK.requireInputIndex(ctx, caseUtxo, "case");
  const tx = lucid
    .newTx()
    .collectFrom([thread.utxo], (ctx) => {
      const indices = {
        input_index: SDK.requireInputIndex(ctx, thread.utxo, "thread"),
        case_input_index: caseIndex(ctx),
      };
      return via === "timeout"
        ? Data.to(indices, SDK.ValidationTimeoutSpendRedeemer)
        : Data.to(
            { CloseAwardedThread: indices },
            SDK.ValidationAwardSpendRedeemer,
          );
    })
    .collectFrom([caseUtxo], caseRedeemer)
    .mintAssets({ [thread.unit]: -1n }, caseRedeemer)
    .mintAssets({ [receiptUnit]: 1n }, (ctx) =>
      Data.to(
        { MintReceipt: { case_input_index: caseIndex(ctx) } },
        SDK.ReceiptRedeemer,
      ),
    )
    .readFrom([
      caseConfig,
      references.case,
      ...new Set([threadReference, references.award]),
    ])
    .pay.ToContract(
      caseUtxo.address,
      {
        kind: "inline",
        value: Data.to(
          { ...caseDatum, open_threads: caseDatum.open_threads - 1n },
          SDK.Case,
        ),
      },
      caseUtxo.assets,
    )
    .pay.ToContract(
      chain.award.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to(
          {
            case_id: caseId,
            defender_vkh: defenderVkh,
            outcome,
            bounty_claimant: caseDatum.bounty_claimant,
          },
          SDK.LostDefenseReceipt,
        ),
      },
      { lovelace: RECEIPT_LOVELACE, [receiptUnit]: 1n },
    )
    .pay.ToAddressWithData(
      credentialToAddress(deployment.network, {
        type: "Key",
        hash: caseDatum.bounty_claimant,
      }),
      {
        kind: "inline",
        value: Data.to(
          new Constr(0, [thread.utxo.txHash, BigInt(thread.utxo.outputIndex)]),
        ),
      },
      {
        lovelace: lovelaceOf(thread.utxo) - RECEIPT_LOVELACE - fee,
      },
    )
    .validFrom(validFrom)
    .validTo(validTo);
  return { tx, receiptUnit };
};
