/**
 * Node-side programs behind the operator exit verbs: voluntary and forced
 * retirement, bond recovery, and duplicate-registration slashing. Each program
 * reads the live directory, refuses locally with a plain reason when the
 * ledger would refuse anyway, runs the funding preflight, then builds, signs,
 * and submits through the SDK builder.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  configuredContractDeploymentInfoPath,
  readFinalizedDeploymentIdentity,
  verifyConfiguredDeploymentManifestProgram,
} from "../../commands/contract-deployment-info.js";
import { slotToUnixTimeForLucidOrEmulatorFallback } from "../../lucid-time.js";
import { type NodeConfig } from "../../services/index.js";
import { alignedUnixTimeStrictlyAfter } from "../../workers/utils/commit-end-time.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptTargetsByCommand,
} from "../reference-scripts.js";
import { currentTimeMsForLucidOrEmulatorFallback } from "../register-active-operator/clock.js";
import {
  handleSignSubmit,
  type TxConfirmError,
  type TxSignError,
  type TxSubmitError,
} from "../utils.js";
import {
  collateralForExactFee,
  formatAda,
  OperatorFundingShortfall,
  requireOperatorFundingProgram,
} from "./funding-preflight.js";

// ---------------------------------------------------------------------------
// Economics
// ---------------------------------------------------------------------------

/**
 * The deployment's operator economics, read from the finalized deployment
 * manifest so every verb uses the same bond and penalties the validators were
 * parameterized with.
 */
export type OperatorEconomics = {
  readonly requiredBondLovelace: bigint;
  readonly slashingPenaltyLovelace: bigint;
  readonly inactivitySlashingPenaltyLovelace: bigint;
};

export const configuredOperatorEconomicsProgram: Effect.Effect<
  OperatorEconomics,
  Error,
  NodeConfig
> = Effect.gen(function* () {
  const verification = yield* verifyConfiguredDeploymentManifestProgram;
  if (!verification.ok) {
    return yield* Effect.fail(
      new Error(
        `Operator lifecycle refused deployment manifest drift: ${verification.mismatches.join("; ")}`,
      ),
    );
  }
  const identity = yield* Effect.try({
    try: () =>
      readFinalizedDeploymentIdentity(configuredContractDeploymentInfoPath()),
    catch: (cause) =>
      new Error(
        `Failed to load finalized deployment economics: ${String(cause)}`,
      ),
  });
  const economics = identity.manifest.economics;
  return {
    requiredBondLovelace: BigInt(economics.requiredBondLovelace),
    slashingPenaltyLovelace: BigInt(economics.slashingPenaltyLovelace),
    inactivitySlashingPenaltyLovelace: BigInt(
      economics.inactivitySlashingPenaltyLovelace,
    ),
  };
});

// ---------------------------------------------------------------------------
// Shared plumbing
// ---------------------------------------------------------------------------

export type OperatorExitError =
  | SDK.OperatorDirectorySnapshotError
  | SDK.StateQueueError
  | SDK.OperatorExitError
  | OperatorFundingShortfall
  | OperatorExitRefusal
  | TxSignError
  | TxSubmitError
  | TxConfirmError
  | Error;

/**
 * A local refusal: the directory shows the transaction could not be accepted,
 * so nothing is built. The message is meant to be printed as-is.
 */
export class OperatorExitRefusal extends Error {
  override readonly name = "OperatorExitRefusal";
  readonly operatorKeyHash: string;
  readonly detail: Readonly<Record<string, string | number | boolean | null>>;

  constructor(
    message: string,
    operatorKeyHash: string,
    detail: Readonly<Record<string, string | number | boolean | null>> = {},
  ) {
    super(message);
    this.operatorKeyHash = operatorKeyHash;
    this.detail = detail;
  }
}

export type OperatorScriptFamily =
  | "scheduler"
  | "registered-operators"
  | "active-operators"
  | "retired-operators";

/**
 * The published reference scripts an operator transaction reads. `family`
 * lists every publication of a validator family (spending, minting, ...);
 * `spending` holds each requested family's spending script, resolved up front
 * so a missing publication fails the program instead of the build.
 */
export type OperatorScriptRefs<F extends OperatorScriptFamily> = {
  readonly family: (family: F) => readonly SDK.ReferenceScriptPublication[];
  readonly spending: Readonly<Record<F, UTxO>>;
};

export const resolveOperatorScriptRefsProgram = <
  F extends OperatorScriptFamily,
>(
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  referenceScriptsAddress: string,
  families: readonly F[],
): Effect.Effect<OperatorScriptRefs<F>, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const targetsByCommand = referenceScriptTargetsByCommand(contracts);
    const resolved = yield* fetchReferenceScriptUtxosProgram(
      lucid,
      referenceScriptsAddress,
      families.flatMap((family) => targetsByCommand[family]),
      contracts.referenceScriptAuth,
    );
    const spending: Partial<Record<F, UTxO>> = {};
    for (const family of families) {
      const published = resolved.find(
        ({ name }) => name === `${family} spending`,
      );
      if (published === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: `Missing the published ${family} spending script`,
            cause: undefined,
          }),
        );
      }
      spending[family] = published.utxo;
    }
    return {
      family: (family) =>
        resolved
          .filter(({ name }) => name.startsWith(`${family} `))
          .map(({ name, utxo }) => ({ name, utxo })),
      spending: spending as Readonly<Record<F, UTxO>>,
    };
  });

export type ExitValidityWindow = {
  readonly nowMs: bigint;
  readonly validFrom: bigint;
  readonly validTo: bigint;
};

/** Operator transactions look sixty slots back and two minutes ahead. */
export const EXIT_VALIDITY_LOOKBACK_SLOTS = 60;
export const OPERATOR_TX_VALIDITY_WINDOW_MS = 120_000n;

/**
 * A closed validity range short enough for the on-chain range-length check
 * and aligned to slot boundaries so Lucid does not widen it. The lower bound
 * is expressed in slots and clamped at the slot configuration's zero slot,
 * so a young emulator (or any chain younger than the lookback) never produces
 * a bound the evaluator rejects as too far in the past.
 */
export const exitValidityWindow = (
  lucid: LucidEvolution,
  nowMs: bigint = currentTimeMsForLucidOrEmulatorFallback(lucid),
): ExitValidityWindow => {
  const firstSlot = lucid.config().slotConfig?.zeroSlot ?? 0;
  const fromSlot = Math.max(
    firstSlot,
    lucid.currentSlot() - EXIT_VALIDITY_LOOKBACK_SLOTS,
  );
  return {
    nowMs,
    validFrom: BigInt(
      slotToUnixTimeForLucidOrEmulatorFallback(lucid, fromSlot),
    ),
    validTo: BigInt(
      alignedUnixTimeStrictlyAfter(
        lucid,
        Number(nowMs + OPERATOR_TX_VALIDITY_WINDOW_MS),
      ),
    ),
  };
};

/** Lifts a throwing SDK witness derivation into the error channel. */
const deriveWitnesses = <A>(derive: () => A): Effect.Effect<A, Error> =>
  Effect.try({
    try: derive,
    catch: (cause) =>
      cause instanceof Error ? cause : new Error(String(cause)),
  });

// ---------------------------------------------------------------------------
// Retirement
// ---------------------------------------------------------------------------

export type RetirementSubmission = {
  readonly txHash: string;
  readonly operatorKeyHash: string;
  readonly mode: SDK.RetirementMode;
  /** Lovelace now held by the retired node. */
  readonly retiredBondLovelace: bigint;
  readonly inactivityStrikes: bigint;
  readonly bondUnlockTime: bigint | null;
  /** How the scheduler was kept consistent. */
  readonly schedulerRoute: "OperatorIsInactive" | "GoToNext" | "Rewind";
};

const schedulerRouteOf = (
  sync: SDK.RetireSchedulerSync,
): RetirementSubmission["schedulerRoute"] =>
  sync.kind === "OperatorIsInactive"
    ? "OperatorIsInactive"
    : sync.removingOperatorsAnchorElementKey === null
      ? "Rewind"
      : "GoToNext";

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
    readonly snapshot?: SDK.OperatorDirectorySnapshot;
  },
  options: { readonly label?: string } = {},
): Effect.Effect<RetirementSubmission, OperatorExitError> =>
  Effect.gen(function* () {
    const label =
      options.label ??
      (input.mode === "voluntary"
        ? "retire-operator"
        : "force-retire-operator");
    const snapshot =
      input.snapshot ??
      (yield* SDK.fetchOperatorDirectorySnapshotProgram(lucid, contracts));
    const status = SDK.deriveOperatorStatus(
      snapshot,
      input.operatorKeyHash,
      currentTimeMsForLucidOrEmulatorFallback(lucid),
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
    const { validFrom, validTo } = exitValidityWindow(lucid);
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
    });
    const txHash = yield* handleSignSubmit(lucid, tx, { label });
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
    readonly snapshot?: SDK.OperatorDirectorySnapshot;
  },
  options: { readonly label?: string } = {},
): Effect.Effect<BondRecoverySubmission, OperatorExitError> =>
  Effect.gen(function* () {
    const label = options.label ?? "recover-operator-bond";
    const snapshot =
      input.snapshot ??
      (yield* SDK.fetchOperatorDirectorySnapshotProgram(lucid, contracts));
    const nowMs = currentTimeMsForLucidOrEmulatorFallback(lucid);
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
    const { validFrom, validTo } = exitValidityWindow(lucid, nowMs);
    const lowerBound =
      witnesses.bondUnlockTime === null
        ? validFrom
        : validFrom > witnesses.bondUnlockTime
          ? validFrom
          : BigInt(
              alignedUnixTimeStrictlyAfter(
                lucid,
                Number(witnesses.bondUnlockTime),
              ),
            );
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
    });
    const txHash = yield* handleSignSubmit(lucid, tx, { label });
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
  /** What the submitter kept after the fee. */
  readonly submitterRewardLovelace: bigint;
};

/**
 * Picks the registered node to remove and the membership that proves it a
 * duplicate. An active or retired membership is the proof when one exists;
 * otherwise two registered nodes prove each other and the later registration
 * is the one removed. A single registration yields `null`: a node cannot prove
 * its own duplication, because the ledger requires a transaction's reference
 * inputs to be disjoint from its inputs, so there is nothing to slash.
 */
export const selectDuplicateRegistration = (
  view: SDK.OperatorDirectoryView &
    Pick<SDK.OperatorDirectorySnapshot, "hubOracle">,
  operatorKeyHash: string,
): {
  readonly removed: SDK.NodeWithDatum;
  readonly proof: SDK.DuplicateProof;
} | null => {
  const occupancies = SDK.findOperatorDirectoryOccupancies(
    view,
    operatorKeyHash,
  );
  const registered = occupancies.filter(({ kind }) => kind === "registered");
  if (registered.length === 0) {
    return null;
  }
  const active = occupancies.find(({ kind }) => kind === "active");
  const retired = occupancies.find(({ kind }) => kind === "retired");
  const byKeyDescending = [...registered].sort((left, right) => {
    const l = SDK.nodeKeyHex(left.node.datum.key) ?? "";
    const r = SDK.nodeKeyHex(right.node.datum.key) ?? "";
    return l < r ? 1 : l > r ? -1 : 0;
  });
  const removed = byKeyDescending[0]!.node;
  if (active !== undefined) {
    return {
      removed,
      proof: {
        kind: "active",
        node: active.node,
        hubOracleRefInput: view.hubOracle.utxo,
      },
    };
  }
  if (retired !== undefined) {
    return { removed, proof: { kind: "retired", node: retired.node } };
  }
  if (byKeyDescending.length >= 2) {
    return {
      removed,
      proof: { kind: "registered", node: byKeyDescending[1]!.node },
    };
  }
  return null;
};

/**
 * Removes a duplicate registered node. Anyone may submit; the slashing
 * penalty is paid from the removed bond as the transaction fee and the rest of
 * that bond is left to the submitter as change.
 */
export const slashDuplicateOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  referenceScriptsAddress: string,
  input: {
    readonly operatorKeyHash: string;
    readonly economics: OperatorEconomics;
    readonly snapshot?: SDK.OperatorDirectorySnapshot;
  },
  options: { readonly label?: string } = {},
): Effect.Effect<DuplicateSlashSubmission, OperatorExitError> =>
  Effect.gen(function* () {
    const label = options.label ?? "slash-duplicate-operator";
    const snapshot =
      input.snapshot ??
      (yield* SDK.fetchOperatorDirectorySnapshotProgram(lucid, contracts));
    const selection = selectDuplicateRegistration(
      snapshot,
      input.operatorKeyHash,
    );
    if (selection === null) {
      const occupancies = SDK.findOperatorDirectoryOccupancies(
        snapshot,
        input.operatorKeyHash,
      ).map(({ kind }) => kind);
      return yield* Effect.fail(
        new OperatorExitRefusal(
          `Operator ${input.operatorKeyHash} has no duplicate registration to slash (memberships: ${occupancies.length === 0 ? "none" : occupancies.join(", ")})`,
          input.operatorKeyHash,
          { memberships: occupancies.join(",") || null },
        ),
      );
    }
    const removedKey = SDK.nodeKeyHex(selection.removed.datum.key);
    if (removedKey === null) {
      return yield* Effect.fail(
        new OperatorExitRefusal(
          "Selected a root node as the duplicate registration; refusing",
          input.operatorKeyHash,
        ),
      );
    }
    const registeredAnchor = SDK.findAnchorNodeForKey(
      snapshot.registered,
      removedKey,
    );
    if (registeredAnchor === undefined) {
      return yield* Effect.fail(
        new OperatorExitRefusal(
          `Found no registered-operators element linking to node ${removedKey}`,
          input.operatorKeyHash,
        ),
      );
    }

    // The slashing penalty leaves as the fee, so the ledger wants the
    // collateral percentage of the whole penalty in the submitter's wallet.
    yield* requireOperatorFundingProgram(lucid, {
      label,
      lockedLovelace: 0n,
      collateralLovelace: collateralForExactFee(
        lucid,
        input.economics.slashingPenaltyLovelace,
      ),
    });
    const scriptRefs = yield* resolveOperatorScriptRefsProgram(
      lucid,
      contracts,
      referenceScriptsAddress,
      ["registered-operators"],
    );
    const { validFrom, validTo } = exitValidityWindow(lucid);
    const { tx } = yield* SDK.buildUnsignedSlashDuplicateOperatorTxProgram({
      lucid,
      contracts,
      operatorKeyHash: input.operatorKeyHash,
      registeredOperatorScriptRefs: scriptRefs.family("registered-operators"),
      duplicateRegisteredNode: selection.removed,
      registeredAnchor,
      duplicateRegisteredNodeUnit: toUnit(
        contracts.registeredOperators.policyId,
        SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX + removedKey,
      ),
      duplicateProof: selection.proof,
      slashingPenaltyLovelace: input.economics.slashingPenaltyLovelace,
      validFrom,
      validTo,
    });
    const feeLovelace = tx.toTransaction().body().fee();
    const txHash = yield* handleSignSubmit(lucid, tx, { label });
    return {
      txHash,
      operatorKeyHash: input.operatorKeyHash,
      removedRegisteredNodeKey: removedKey,
      proofKind: selection.proof.kind,
      feeLovelace,
      submitterRewardLovelace:
        (selection.removed.utxo.assets["lovelace"] ?? 0n) - feeLovelace,
    };
  });
