import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
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
  type TxConfirmError,
  type TxSignError,
  type TxSubmitError,
} from "../utils.js";
import { OperatorFundingShortfall } from "./funding-preflight.js";

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
export const deriveWitnesses = <A>(derive: () => A): Effect.Effect<A, Error> =>
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

export const schedulerRouteOf = (
  sync: SDK.RetireSchedulerSync,
): RetirementSubmission["schedulerRoute"] =>
  sync.kind === "OperatorIsInactive"
    ? "OperatorIsInactive"
    : sync.removingOperatorsAnchorElementKey === null
      ? "Rewind"
      : "GoToNext";
