import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type IntentJournal,
  journaledIntent,
} from "../../services/intent-journal.js";
import { alignedUnixTimeStrictlyAfter } from "../../workers/utils/commit-end-time.js";
import { awaitLedgerTipAtLowerBound } from "../ledger-tip-lower-bound.js";
import { resolveL1NowMs } from "../register-active-operator/clock.js";
import { handleSignSubmit } from "../utils.js";
import {
  deriveWitnesses,
  exitValidityWindow,
  type OperatorEconomics,
  type OperatorExitError,
  OperatorExitRefusal,
  type PlannedSnapshot,
  plannedSnapshotProgram,
  resolveOperatorScriptRefsProgram,
  type RetirementSubmission,
  schedulerRouteOf,
} from "./exit.resolve-operator-script-refs-program.js";
import {
  collateralForExactFee,
  formatAda,
  operatorWalletInputsProgram,
  requireOperatorFundingProgram,
} from "./funding-preflight.js";

/**
 * Retires an active operator.
 *
 * `voluntary` needs the selected wallet to be the operator and a strike count
 * below the maximum; the wallet pays an ordinary fee and the retired node
 * keeps the full bond. `forced-inactivity` may be submitted by anyone once
 * the strike count has reached the maximum; the inactivity penalty is paid
 * from the bond as the transaction fee and the retired node keeps the rest.
 */
export const retireOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  referenceScriptsAddress: string,
  input: {
    readonly operatorKeyHash: string;
    readonly mode: SDK.RetirementMode;
    readonly economics: OperatorEconomics;
  } & PlannedSnapshot,
  options: { readonly label?: string } = {},
): Effect.Effect<RetirementSubmission, OperatorExitError, IntentJournal> =>
  Effect.gen(function* () {
    const label =
      options.label ??
      (input.mode === "voluntary"
        ? "retire-operator"
        : "force-retire-operator");
    const { snapshot, intentPlan } = yield* plannedSnapshotProgram(
      lucid,
      contracts,
      input,
    );
    const status = SDK.deriveOperatorStatus(
      snapshot,
      input.operatorKeyHash,
      yield* resolveL1NowMs(lucid),
      { maxInactivityStrikes: SDK.MAX_INACTIVITY_STRIKES },
    );
    if (status.state !== "active") {
      return yield* Effect.fail(
        new OperatorExitRefusal(
          `Operator ${input.operatorKeyHash} is ${status.state === "none" ? "not in the operator directory" : `${status.state}, not active`}; only an active operator can retire`,
          input.operatorKeyHash,
          { state: status.state },
        ),
      );
    }
    // Reaching the strike limit flips the operator from "may retire" to
    // "must be force-retired"; each mode is refused on the other side.
    const forced = input.mode === "forced-inactivity";
    const strikes = status.inactivityStrikes ?? 0n;
    const maxStrikes = SDK.MAX_INACTIVITY_STRIKES;
    if (status.forcedRetirementEligible !== forced) {
      return yield* Effect.fail(
        new OperatorExitRefusal(
          forced
            ? `Operator ${input.operatorKeyHash} has ${strikes.toString()} inactivity strikes; forced retirement needs ${maxStrikes.toString()}`
            : `Operator ${input.operatorKeyHash} has ${strikes.toString()} inactivity strikes (maximum ${maxStrikes.toString()}); it can only be force-retired, which costs ${formatAda(input.economics.inactivitySlashingPenaltyLovelace)} of the bond`,
          input.operatorKeyHash,
          {
            inactivityStrikes: Number(strikes),
            maxInactivityStrikes: Number(maxStrikes),
          },
        ),
      );
    }

    // The bond moves node to node; the wallet only needs fee headroom. A
    // forced retirement pays the penalty as its fee, and the ledger wants the
    // collateral percentage of that fee present in the submitter's wallet.
    yield* requireOperatorFundingProgram(lucid, {
      label,
      lockedLovelace: 0n,
      collateralLovelace: forced
        ? collateralForExactFee(
            lucid,
            input.economics.inactivitySlashingPenaltyLovelace,
          )
        : 0n,
    });

    const scriptRefs = yield* resolveOperatorScriptRefsProgram(
      lucid,
      contracts,
      referenceScriptsAddress,
      ["scheduler", "active-operators", "retired-operators"],
    );
    const { validFrom, validTo } = yield* exitValidityWindow(lucid);
    const witnesses = yield* deriveWitnesses(() =>
      SDK.deriveRetireOperatorWitnesses({
        snapshot,
        contracts,
        operatorKeyHash: input.operatorKeyHash,
        validTo,
        schedulerSpendingScriptRef: scriptRefs.spending.scheduler,
      }),
    );
    const retiredNodeLovelace = SDK.retiredOperatorBondTranche(
      input.mode,
      input.economics,
    );
    const { tx } = yield* SDK.buildUnsignedRetireOperatorTxProgram({
      lucid,
      contracts,
      operatorKeyHash: input.operatorKeyHash,
      activeOperatorScriptRefs: scriptRefs.family("active-operators"),
      retiredOperatorScriptRefs: scriptRefs.family("retired-operators"),
      hubOracleRefInput: snapshot.hubOracle.utxo,
      activeNode: witnesses.activeNode,
      activeAnchor: witnesses.activeAnchor,
      retiredInsertionAnchor: witnesses.retiredInsertionAnchor,
      activeNodeUnit: witnesses.activeNodeUnit,
      retiredNodeUnit: witnesses.retiredNodeUnit,
      bondUnlockTime: witnesses.bondUnlockTime,
      retiredNodeLovelace,
      mode: input.mode,
      inactivitySlashingPenaltyLovelace:
        input.economics.inactivitySlashingPenaltyLovelace,
      schedulerSync: witnesses.schedulerSync,
      validFrom,
      validTo,
      walletInputs: yield* operatorWalletInputsProgram(lucid, label),
    });
    const txHash = yield* handleSignSubmit(
      lucid,
      tx,
      journaledIntent(
        "retire",
        `retire:${input.mode}:${input.operatorKeyHash}`,
        intentPlan,
      ),
      { label },
    );
    return {
      txHash,
      operatorKeyHash: input.operatorKeyHash,
      mode: input.mode,
      retiredBondLovelace: retiredNodeLovelace,
      inactivityStrikes: witnesses.inactivityStrikes,
      bondUnlockTime: witnesses.bondUnlockTime,
      schedulerRoute: schedulerRouteOf(witnesses.schedulerSync),
    };
  });

// ---------------------------------------------------------------------------
// Bond recovery
// ---------------------------------------------------------------------------

export type BondRecoverySubmission = {
  readonly txHash: string;
  readonly operatorKeyHash: string;
  readonly bondLovelace: bigint;
};

/**
 * Burns the operator's retired node and returns its bond to the selected
 * wallet. Refused before `bond_unlock_time`; the refusal names the unlock time.
 */
export const recoverOperatorBondProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  referenceScriptsAddress: string,
  input: {
    readonly operatorKeyHash: string;
  } & PlannedSnapshot,
  options: { readonly label?: string } = {},
): Effect.Effect<BondRecoverySubmission, OperatorExitError, IntentJournal> =>
  Effect.gen(function* () {
    const label = options.label ?? "recover-operator-bond";
    const { snapshot, intentPlan } = yield* plannedSnapshotProgram(
      lucid,
      contracts,
      input,
    );
    const nowMs = yield* resolveL1NowMs(lucid);
    const status = SDK.deriveOperatorStatus(
      snapshot,
      input.operatorKeyHash,
      nowMs,
      { maxInactivityStrikes: SDK.MAX_INACTIVITY_STRIKES },
    );
    if (status.state !== "retired" && !status.occupancies.includes("retired")) {
      return yield* Effect.fail(
        new OperatorExitRefusal(
          `Operator ${input.operatorKeyHash} has no retired node to recover from (state: ${status.state})${status.state === "active" ? "; retire first" : ""}`,
          input.operatorKeyHash,
          { state: status.state },
        ),
      );
    }
    if (!status.bondRecoveryAllowedNow) {
      const unlock = status.bondUnlockTime;
      return yield* Effect.fail(
        new OperatorExitRefusal(
          `Bond for operator ${input.operatorKeyHash} is locked until ${unlock === null ? "an unknown time" : `${new Date(Number(unlock)).toISOString()} (${unlock.toString()})`}; it can be recovered once the chain time passes it`,
          input.operatorKeyHash,
          {
            bondUnlockTime: unlock?.toString() ?? null,
            recoverableFrom: status.bondRecoveryAllowedFrom?.toString() ?? null,
            nowMs: nowMs.toString(),
          },
        ),
      );
    }

    yield* requireOperatorFundingProgram(lucid, {
      label,
      lockedLovelace: 0n,
    });
    const scriptRefs = yield* resolveOperatorScriptRefsProgram(
      lucid,
      contracts,
      referenceScriptsAddress,
      ["retired-operators"],
    );
    const witnesses = yield* deriveWitnesses(() =>
      SDK.deriveRecoverOperatorBondWitnesses({
        snapshot,
        contracts,
        operatorKeyHash: input.operatorKeyHash,
      }),
    );
    // The validators read the bond unlock against the lower bound, so the
    // range starts after it. A one-shot verb waits for the observed ledger
    // tip to reach that bound before it builds; submit refuses a transaction
    // the tip has not reached.
    const unlockLowerBound =
      witnesses.bondUnlockTime === null
        ? null
        : BigInt(
            alignedUnixTimeStrictlyAfter(
              lucid,
              Number(witnesses.bondUnlockTime),
            ),
          );
    if (unlockLowerBound !== null) {
      yield* Effect.mapError(
        awaitLedgerTipAtLowerBound(lucid, unlockLowerBound, label),
        (cause) =>
          new Error(
            `${label} needs the observed L1 ledger tip to reach the bond unlock: ${formatUnknownError(cause)}`,
            { cause },
          ),
      );
    }
    // The upper bound runs from the L1 clock read after the wait, which may
    // have outlasted a whole validity window.
    const { validFrom, validTo } = yield* exitValidityWindow(
      lucid,
      yield* resolveL1NowMs(lucid),
    );
    const lowerBound =
      unlockLowerBound === null || validFrom > unlockLowerBound
        ? validFrom
        : unlockLowerBound;
    const { tx } = yield* SDK.buildUnsignedRecoverOperatorBondTxProgram({
      lucid,
      contracts,
      operatorKeyHash: input.operatorKeyHash,
      retiredOperatorScriptRefs: scriptRefs.family("retired-operators"),
      retiredNode: witnesses.retiredNode,
      retiredAnchor: witnesses.retiredAnchor,
      retiredNodeUnit: witnesses.retiredNodeUnit,
      validFrom: lowerBound,
      validTo,
      walletInputs: yield* operatorWalletInputsProgram(lucid, label),
    });
    const txHash = yield* handleSignSubmit(
      lucid,
      tx,
      journaledIntent(
        "recover_bond",
        `recover_bond:${input.operatorKeyHash}`,
        intentPlan,
      ),
      { label },
    );
    return {
      txHash,
      operatorKeyHash: input.operatorKeyHash,
      bondLovelace: witnesses.bondLovelace,
    };
  });

// ---------------------------------------------------------------------------
// Duplicate-registration slashing
// ---------------------------------------------------------------------------

export type DuplicateSlashSubmission = {
  readonly txHash: string;
  readonly operatorKeyHash: string;
  /** Key (activation time hex) of the registered node that was removed. */
  readonly removedRegisteredNodeKey: string;
  readonly proofKind: SDK.DuplicateProof["kind"];
  readonly feeLovelace: bigint;
  /** The key the bounty was paid to: this node's wallet key. */
  readonly bountyClaimantKeyHash: string;
  /** The bounty paid to the claimant's enterprise address. */
  readonly bountyLovelace: bigint;
};
