import {
  type Assets,
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type RedeemerContext,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as Availability from "./availability-challenge.js";
import { type MidgardValidators } from "./common.js";
import { DaBondPoolDatum } from "./da-bond-pool.js";
import { type StateQueueUTxO, utxoToStateQueueUTxO } from "./state-queue.js";
import {
  requireMintRedeemerIndex,
  requireOwnSpendPurpose,
} from "./tx-context-redeemer.js";
import { outputDatumCborMatches } from "./tx-output-utils.js";

export type DaAvailabilityTransactionAction =
  | "open"
  | "publish"
  | "settle"
  | "close"
  | "timeout"
  | "prune"
  | "remove";

/**
 * Everything a challenge builder authenticates against. The pooled DA bond is
 * part of the deployment: `contracts.daBondPool` is its spending validator and
 * NFT policy, and `referenceScripts["da-bond-pool spending"]` is the
 * authenticated reference script `TimeoutChallenge` spends it with.
 */
export type DaAvailabilityDeployment = {
  readonly contracts: Pick<
    MidgardValidators,
    "availabilityChallenge" | "stateQueue" | "correctionLock" | "daBondPool"
  >;
  readonly hubOraclePolicyId: string;
  readonly referenceScriptAuthPolicyId: string;
  readonly parameters: Availability.DaAvailabilityParameters;
  readonly referenceScripts: Readonly<Record<string, UTxO>>;
  readonly hubOracleRefInput: UTxO;
};

export type DaAvailabilityTransactionResources = {
  /**
   * Plain-ADA wallet coins offered as collateral. The builder picks at most
   * three of them, largest first, covering the ledger's collateral percentage
   * of the exact fee.
   */
  readonly collateralInputs: readonly UTxO[];
  readonly feeLovelace: bigint;
  readonly validFrom: bigint;
  readonly validTo: bigint;
};

export type DaAvailabilityExpectedOutput = {
  readonly address: string;
  readonly assets: Assets;
  readonly datum?: string;
};

export type BuiltDaAvailabilityTransaction = {
  readonly tx: TxSignBuilder;
  readonly unsignedCbor: string;
  readonly txId: string;
  readonly action: DaAvailabilityTransactionAction;
  readonly headerHash: string;
  readonly challengeAssetName: string;
  readonly validityRange: {
    readonly validFrom: bigint;
    readonly validTo: bigint;
  };
  readonly spentOutRefs: readonly UTxO[];
  readonly referenceOutRefs: readonly UTxO[];
  readonly collateralOutRefs: readonly UTxO[];
  readonly expectedOutputs: readonly DaAvailabilityExpectedOutput[];
  readonly feeLovelace: bigint;
  /**
   * Timeout only: the slashed penalty share of the fee, `min(penalty, taken)`.
   * The challenger's own contribution is `feeLovelace - timeoutFeePartLovelace`.
   */
  readonly timeoutFeePartLovelace?: bigint;
};

export type DaAvailabilityChallengeSnapshot = {
  readonly headerHash: string;
  readonly record?: UTxO;
  readonly recordDatum?: Availability.DaAvailabilityChallengeRecord;
  readonly pool?: UTxO;
  readonly poolDatum?: DaBondPoolDatum;
  readonly queue?: StateQueueUTxO;
  readonly confirmedState: StateQueueUTxO;
  readonly descendant?: StateQueueUTxO;
  readonly correctionLock: UTxO;
  readonly terminal?: UTxO;
  readonly terminalDatum?: Availability.DaAvailabilityTerminalAccumulatorDatum;
  readonly tranches: readonly {
    readonly utxo: UTxO;
    readonly datum: Availability.DaAvailabilityTrancheDatum;
    readonly carrier?: UTxO;
  }[];
};

export type OpenDaAvailabilityChallengeParams =
  DaAvailabilityTransactionResources & {
    /**
     * The full signed commitment. Its hash must equal the queue node's
     * `Attested{commitment_hash}`; recover it from the Apply transaction with
     * `recoverDaAvailabilityCommitmentFromApplyTx` when it is not at hand.
     */
    readonly commitment: Availability.DaAvailabilityCommitment;
    readonly queue: UTxO;
    /** Exactly `challenger_bond_lovelace + challenge_record_lovelace + fee`. */
    readonly challengerFunding: UTxO;
    readonly challenger: string;
    /**
     * The deployment profile's `timing.da_challenge_window_ms` (compiled into
     * the validator as `da_challenge_window_ms_v1`).
     */
    readonly daChallengeWindowMs: bigint;
  };

export type PublishDaAvailabilityChunkParams =
  DaAvailabilityTransactionResources & {
    readonly thread: UTxO;
    readonly previousCarrier?: UTxO;
    readonly publication: Availability.DaAvailabilityPublicationDatum;
  };

export type SettleDaAvailabilityTrancheParams =
  DaAvailabilityTransactionResources & {
    /** The challenge record, read as a reference input. */
    readonly record: UTxO;
    readonly terminal: UTxO;
    readonly thread: UTxO;
    readonly carrier?: UTxO;
  };

export type CloseDaAvailabilityChallengeParams =
  DaAvailabilityTransactionResources & {
    readonly record: UTxO;
    readonly terminal: UTxO;
    readonly queue: UTxO;
  };

export type DaAvailabilityRemovalParams = DaAvailabilityTransactionResources & {
  readonly queue: UTxO;
  readonly confirmedState: UTxO;
  readonly descendant?: UTxO;
  readonly correctionLock: UTxO;
  readonly challengeAssetName: string;
  readonly headerHash: string;
  readonly rentRefundAddress: string;
  readonly feeFunding?: UTxO;
  readonly fundingQueueTailRefInput?: UTxO;
};

/**
 * The timeout derives its own exact fee, `min(penalty, taken) + c`, so it takes
 * no `feeLovelace` and no fee funding: the removed record, terminal and pool
 * pay it.
 */
export type TimeoutDaAvailabilityChallengeParams = Omit<
  DaAvailabilityRemovalParams,
  "feeLovelace" | "feeFunding"
> & {
  readonly record: UTxO;
  readonly terminal: UTxO;
  /** The one pooled DA bond UTxO, slashed whatever its backing. */
  readonly pool: UTxO;
  /**
   * Pins the challenger's fee contribution `c`. When absent the builder
   * measures the smallest `c` the ledger accepts (see
   * `buildTimeoutDaAvailabilityChallengeTxProgram`).
   */
  readonly challengerFeeLovelace?: bigint;
};

export type DaAvailabilityTransactionErrorReason =
  /** The commitment does not hash to the node's `Attested{commitment_hash}`. */
  | "commitment-hash-mismatch"
  /** The record's live min-UTxO exceeds `challenge_record_lovelace`. */
  | "record-min-ada"
  /** The open's upper bound is at or past `end_time + da_challenge_window_ms`. */
  | "challenge-window-closed"
  /** The timeout needs a challenger fee contribution above `max_timeout_fee`. */
  | "timeout-challenger-fee-cap"
  /** No three wallet coins cover the ledger collateral for the exact fee. */
  | "collateral-insufficient"
  /** Lucid could not complete the transaction at the pinned exact fee. */
  | "completion-failed"
  /** The Apply transaction does not yield the attested commitment. */
  | "apply-commitment-unrecoverable";

export class DaAvailabilityTransactionError extends Error {
  readonly name = "DaAvailabilityTransactionError";
  constructor(
    message: string,
    readonly reason?: DaAvailabilityTransactionErrorReason,
  ) {
    super(message);
  }
}

export const fail = (
  message: string,
  reason?: DaAvailabilityTransactionErrorReason,
): never => {
  throw new DaAvailabilityTransactionError(message, reason);
};

export const effect = <A>(
  body: () => Promise<A>,
): Effect.Effect<A, DaAvailabilityTransactionError> =>
  Effect.tryPromise({
    try: body,
    catch: (cause) =>
      cause instanceof DaAvailabilityTransactionError
        ? cause
        : new DaAvailabilityTransactionError(
            cause instanceof Error ? cause.message : String(cause),
          ),
  });

export const refKey = (u: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${u.txHash}#${u.outputIndex}`;

export const inline = (value: string) => ({ kind: "inline" as const, value });

export const datum = (u: UTxO): string =>
  u.datum ?? fail(`Missing inline datum on ${refKey(u)}`);

// Providers may return a ledger-normalized CBOR representation. Validate the
// typed Plutus Data value; wire canonicality belongs to signed payload codecs.
export const recordDatum = (u: UTxO, d: DaAvailabilityDeployment) => {
  const value = Data.from(datum(u), Availability.DaAvailabilityChallengeRecord);
  Availability.assertCanonicalDaAvailabilityChallengeRecord(
    value,
    d.parameters,
  );
  return value;
};

export const trancheDatum = (u: UTxO) => {
  const value = Data.from(datum(u), Availability.DaAvailabilityTrancheDatum);
  Availability.assertCanonicalDaAvailabilityTrancheDatum(value);
  return value;
};

export const terminalDatum = (u: UTxO) => {
  const value = Data.from(
    datum(u),
    Availability.DaAvailabilityTerminalAccumulatorDatum,
  );
  Availability.assertCanonicalDaAvailabilityTerminalAccumulatorDatum(value);
  return value;
};

export const state = (u: UTxO, d: DaAvailabilityDeployment) => {
  if (
    u.address !== d.contracts.stateQueue.spendingScriptAddress ||
    u.scriptRef != null
  )
    fail("Unauthentic state queue address");
  return Effect.runPromise(
    utxoToStateQueueUTxO(u, d.contracts.stateQueue.policyId),
  );
};

export const keyAddress = (lucid: LucidEvolution, hash: string) =>
  credentialToAddress(lucid.config().network ?? fail("Missing network"), {
    type: "Key",
    hash,
  });

export const sameAssets = (a: Assets, b: Assets) =>
  Object.keys(a).length === Object.keys(b).length &&
  Object.entries(a).every(([u, v]) => b[u] === v);

export const auth = (u: UTxO, address: string, units: readonly string[]) => {
  if (
    u.address !== address ||
    u.scriptRef != null ||
    units.some((unit) => u.assets[unit] !== 1n) ||
    Object.entries(u.assets).some(
      ([unit, value]) =>
        unit !== "lovelace" && (!units.includes(unit) || value !== 1n),
    )
  )
    fail(`Unauthentic protocol input ${refKey(u)}`);
  datum(u);
};

/**
 * The redeemer index of an output the builder placed itself. Lucid keeps the
 * pay order and these builders add no change output, so each protected output
 * sits at the position the builder chose; this proves it before encoding it.
 */
export const at = (
  ctx: RedeemerContext,
  index: number,
  o: DaAvailabilityExpectedOutput,
  label: string,
): bigint => {
  const actual = ctx.outputs[index];
  if (
    actual === undefined ||
    actual.address !== o.address ||
    !sameAssets(actual.assets, o.assets) ||
    (o.datum === undefined
      ? actual.datum != null
      : !outputDatumCborMatches(actual, o.datum))
  )
    fail(`${label} output is not at its reserved position ${index}`);
  return BigInt(index);
};

export const spend =
  (
    u: UTxO,
    build: (ctx: RedeemerContext) => Availability.DaAvailabilitySpendRedeemer,
  ): BuildTxWithRedeemer =>
  (ctx) => {
    requireOwnSpendPurpose(ctx, u, "availability");
    return Data.to(build(ctx), Availability.DaAvailabilitySpendRedeemer);
  };

export const coordinate = (u: UTxO, policy: string): BuildTxWithRedeemer =>
  spend(u, (ctx) => ({
    Coordinate: {
      mint_redeemer_index: requireMintRedeemerIndex(
        ctx,
        policy,
        "availability",
      ),
    },
  }));
