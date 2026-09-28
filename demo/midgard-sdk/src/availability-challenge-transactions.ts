import {
  type Assets,
  type BuildTxWithRedeemer,
  calculateMinLovelaceFromUTxO,
  CML,
  coreToTxOutput,
  credentialToAddress,
  Data,
  getAddressDetails,
  type LucidEvolution,
  type ProtocolParameters,
  type RedeemerContext,
  type Script,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as Availability from "./availability-challenge.js";
import { scriptRewardAddress } from "./cardano-addresses.js";
import { type MidgardValidators, outputReferenceFromUTxO } from "./common.js";
import {
  CorrectionLockDatum,
  CorrectionLockRedeemer,
  correctionLockUnit,
} from "./correction-lock.js";
import {
  DA_ATTESTATION_ASSET_NAME_PREFIX,
  DaAttestationDatum,
} from "./da-attestation.js";
import {
  assertCanonicalDaBondPoolDatum,
  DaBondPoolDatum,
  DaBondPoolSpendRedeemer,
  daBondPoolUnit,
  planDaBondPoolSlash,
} from "./da-bond-pool.js";
import { HUB_ORACLE_ASSET_NAME } from "./hub-oracle.js";
import { castStateQueueNodeToData, StateQueueNode } from "./ledger-state.js";
import {
  encodeLinkedListNodeView,
  type LinkedListNodeView,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "./linked-list.js";
import { referenceScriptAuthUnit } from "./reference-scripts.js";
import {
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
  StateQueueSpendRedeemer,
  type StateQueueUTxO,
  utxoToStateQueueUTxO,
} from "./state-queue.js";
import { completeOptionsWithLocalEval } from "./tx-completion.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
} from "./tx-context-redeemer.js";
import {
  isPlainPositiveAdaOnlyUtxo,
  outputDatumCborMatches,
} from "./tx-output-utils.js";

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
const fail = (
  message: string,
  reason?: DaAvailabilityTransactionErrorReason,
): never => {
  throw new DaAvailabilityTransactionError(message, reason);
};
const effect = <A>(
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
const refKey = (u: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${u.txHash}#${u.outputIndex}`;
const inline = (value: string) => ({ kind: "inline" as const, value });
const datum = (u: UTxO): string =>
  u.datum ?? fail(`Missing inline datum on ${refKey(u)}`);
// Providers may return a ledger-normalized CBOR representation. Validate the
// typed Plutus Data value; wire canonicality belongs to signed payload codecs.
const recordDatum = (u: UTxO, d: DaAvailabilityDeployment) => {
  const value = Data.from(datum(u), Availability.DaAvailabilityChallengeRecord);
  Availability.assertCanonicalDaAvailabilityChallengeRecord(
    value,
    d.parameters,
  );
  return value;
};
const trancheDatum = (u: UTxO) => {
  const value = Data.from(datum(u), Availability.DaAvailabilityTrancheDatum);
  Availability.assertCanonicalDaAvailabilityTrancheDatum(value);
  return value;
};
const terminalDatum = (u: UTxO) => {
  const value = Data.from(
    datum(u),
    Availability.DaAvailabilityTerminalAccumulatorDatum,
  );
  Availability.assertCanonicalDaAvailabilityTerminalAccumulatorDatum(value);
  return value;
};
const state = (u: UTxO, d: DaAvailabilityDeployment) => {
  if (
    u.address !== d.contracts.stateQueue.spendingScriptAddress ||
    u.scriptRef != null
  )
    fail("Unauthentic state queue address");
  return Effect.runPromise(
    utxoToStateQueueUTxO(u, d.contracts.stateQueue.policyId),
  );
};
const keyAddress = (lucid: LucidEvolution, hash: string) =>
  credentialToAddress(lucid.config().network ?? fail("Missing network"), {
    type: "Key",
    hash,
  });
const sameAssets = (a: Assets, b: Assets) =>
  Object.keys(a).length === Object.keys(b).length &&
  Object.entries(a).every(([u, v]) => b[u] === v);
const auth = (u: UTxO, address: string, units: readonly string[]) => {
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
const at = (
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
const spend =
  (
    u: UTxO,
    build: (ctx: RedeemerContext) => Availability.DaAvailabilitySpendRedeemer,
  ): BuildTxWithRedeemer =>
  (ctx) => {
    requireOwnSpendPurpose(ctx, u, "availability");
    return Data.to(build(ctx), Availability.DaAvailabilitySpendRedeemer);
  };
const coordinate = (u: UTxO, policy: string): BuildTxWithRedeemer =>
  spend(u, (ctx) => ({
    Coordinate: {
      mint_redeemer_index: requireMintRedeemerIndex(
        ctx,
        policy,
        "availability",
      ),
    },
  }));
const mint =
  (
    policy: string,
    build: (ctx: RedeemerContext) => Availability.DaAvailabilityMintRedeemer,
  ): BuildTxWithRedeemer =>
  (ctx) => {
    requireOwnMintPurpose(ctx, policy, "availability");
    return Data.to(build(ctx), Availability.DaAvailabilityMintRedeemer);
  };
const queueUpdate =
  (
    u: UTxO,
    policy: string,
    output: DaAvailabilityExpectedOutput,
    outputIndex: number,
  ): BuildTxWithRedeemer =>
  (ctx) => {
    requireOwnSpendPurpose(ctx, u, "availability queue");
    return Data.to(
      {
        AvailabilityStatusUpdate: {
          state_queue_input_index: requireInputIndex(ctx, u, "queue"),
          state_queue_output_index: at(ctx, outputIndex, output, "queue"),
          availability_mint_redeemer_index: requireMintRedeemerIndex(
            ctx,
            policy,
            "availability",
          ),
        },
      },
      StateQueueSpendRedeemer,
    );
  };
const role = (
  d: DaAvailabilityDeployment,
  name: string,
  script: Script,
): UTxO => {
  const u =
    d.referenceScripts[name] ??
    fail(`Missing authenticated reference script ${name}`);
  if (
    u.assets[referenceScriptAuthUnit(d.referenceScriptAuthPolicyId, name)] !==
      1n ||
    u.scriptRef == null ||
    validatorToScriptHash(u.scriptRef) !== validatorToScriptHash(script)
  )
    fail(`Unauthentic reference script ${name}`);
  return u;
};
const baseRefs = (d: DaAvailabilityDeployment) => [
  role(
    d,
    "availability-challenge spending",
    d.contracts.availabilityChallenge.spendingScript,
  ),
];
const mintRefs = (
  d: DaAvailabilityDeployment,
  arm: "open" | "settle" | "close" | "timeout",
) => [
  ...baseRefs(d),
  role(
    d,
    "availability-challenge minting",
    d.contracts.availabilityChallenge.mintingScript,
  ),
  role(
    d,
    `availability-challenge ${arm} withdrawal`,
    d.contracts.availabilityChallenge.yields[arm].withdrawalScript,
  ),
];
const hub = (d: DaAvailabilityDeployment) => {
  if (
    d.hubOracleRefInput.assets[d.hubOraclePolicyId + HUB_ORACLE_ASSET_NAME] !==
    1n
  )
    fail("Unauthentic hub oracle");
  return d.hubOracleRefInput;
};
const withYield = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  tx: TxBuilder,
  arm: "open" | "settle" | "close" | "timeout",
) =>
  tx.withdraw(
    scriptRewardAddress(
      lucid.config().network ?? fail("Missing network"),
      d.contracts.availabilityChallenge.yields[arm].withdrawalScript,
    ),
    0n,
    Data.void(),
  );
const pay = (tx: TxBuilder, outputs: readonly DaAvailabilityExpectedOutput[]) =>
  outputs.reduce(
    (next, o) =>
      o.datum === undefined
        ? next.pay.ToAddress(o.address, o.assets)
        : next.pay.ToContract(o.address, inline(o.datum), o.assets),
    tx,
  );
const protocolParameters = (lucid: LucidEvolution): ProtocolParameters =>
  lucid.config().protocolParameters ?? fail("Missing live protocol parameters");
const minAda = (lucid: LucidEvolution, o: DaAvailabilityExpectedOutput) =>
  calculateMinLovelaceFromUTxO(protocolParameters(lucid).coinsPerUtxoByte, {
    ...o,
    txHash: "00".repeat(32),
    outputIndex: 0,
  });

/**
 * Refuses a record output whose live min-UTxO exceeds the exact
 * `challenge_record_lovelace` it must hold. `OpenChallenge` fixes the record's
 * value, so Lucid cannot top it up: a larger floor would only surface as a
 * ledger rejection. Same floor computation Lucid applies (the
 * `withSettledLovelace` pattern), asserted instead of raised.
 */
export const assertDaAvailabilityChallengeRecordMinAda = (input: {
  readonly coinsPerUtxoByte: bigint;
  readonly record: DaAvailabilityExpectedOutput;
  readonly challengeRecordLovelace: bigint;
}): void => {
  if (input.record.assets.lovelace !== input.challengeRecordLovelace)
    fail("Challenge record must hold exactly challenge_record_lovelace");
  const floor = calculateMinLovelaceFromUTxO(input.coinsPerUtxoByte, {
    ...input.record,
    txHash: "00".repeat(32),
    outputIndex: 0,
  });
  if (floor > input.challengeRecordLovelace)
    fail(
      `Challenge record needs ${floor} lovelace at live coinsPerUtxoByte, above challenge_record_lovelace ${input.challengeRecordLovelace}`,
      "record-min-ada",
    );
};

/**
 * The Open's commitment binding: the commitment names this deployment and the
 * queue node's block, and hashes to the node's `Attested{commitment_hash}`.
 * Returns that hash.
 */
export const assertDaAvailabilityOpenCommitment = (input: {
  readonly commitment: Availability.DaAvailabilityCommitment;
  readonly deploymentIdentity: string;
  readonly queueAssetName: string;
  readonly status: StateQueueNode["da_attestation"];
  readonly parameters: Availability.DaAvailabilityParameters;
}): string => {
  Availability.assertCanonicalDaAvailabilityCommitment(
    input.commitment,
    input.parameters.response_geometry,
  );
  if (input.commitment.deployment_identity !== input.deploymentIdentity)
    fail("Commitment names another deployment");
  if (
    input.queueAssetName !==
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + input.commitment.header_hash
  )
    fail("Queue node is not the commitment's block");
  if (typeof input.status !== "object" || !("Attested" in input.status))
    fail("Challenge opening requires an Attested queue node");
  const hash = Availability.daAvailabilityCommitmentHash(input.commitment);
  const attested = (
    input.status as Extract<
      StateQueueNode["da_attestation"],
      { Attested: unknown }
    >
  ).Attested.commitment_hash;
  if (hash !== attested)
    fail(
      `Commitment hash ${hash} does not match the node's Attested commitment_hash ${attested}`,
      "commitment-hash-mismatch",
    );
  return hash;
};

/**
 * `OpenChallenge` requires `inclusive_upper < end_time + da_challenge_window`.
 * The inclusive upper bound is `validTo - 1`.
 */
export const assertDaAvailabilityOpenWithinChallengeWindow = (input: {
  readonly validTo: bigint;
  readonly nodeEndTime: bigint;
  readonly daChallengeWindowMs: bigint;
}): void => {
  if (input.daChallengeWindowMs <= 0n)
    fail("da_challenge_window_ms must be positive");
  if (input.validTo - 1n >= input.nodeEndTime + input.daChallengeWindowMs)
    fail(
      `Challenge window closed: inclusive upper ${input.validTo - 1n} is not before ${input.nodeEndTime + input.daChallengeWindowMs}`,
      "challenge-window-closed",
    );
};

export type DaAvailabilityTimeoutPlan = Readonly<{
  /** `min(da_bond, backing)`: what leaves the pool. */
  taken: bigint;
  /** `min(penalty, taken)`: burned as fee. */
  feePart: bigint;
  /** `taken - feePart`: merged into the challenger output. */
  payout: bigint;
  /** `pool_in - taken`, beside the pool NFT. */
  poolOutputLovelace: bigint;
  /** `c`, the challenger's own fee contribution. */
  challengerFeeLovelace: bigint;
  /** `feePart + c`: the transaction's exact fee. */
  feeLovelace: bigint;
  /** `remaining - c + challenge_record_lovelace + payout`. */
  challengerOutputLovelace: bigint;
}>;

/**
 * Timeout value arithmetic (twin of `validate_timeout_challenge`): the pool
 * gives up `taken`, the penalty share of it is fee, and the one challenger
 * output merges the remaining reserve (less `c`), the record's lovelace and
 * the payout.
 */
export const planDaAvailabilityTimeout = (input: {
  readonly poolLovelace: bigint;
  readonly remainingChallengerLovelace: bigint;
  readonly challengerFeeLovelace: bigint;
  readonly parameters: Availability.DaAvailabilityParameters;
}): DaAvailabilityTimeoutPlan => {
  const slash = planDaBondPoolSlash({
    poolLovelace: input.poolLovelace,
    parameters: input.parameters,
  });
  const c = input.challengerFeeLovelace;
  if (c < 0n || c > input.parameters.max_timeout_fee_lovelace)
    fail(
      `Timeout challenger fee ${c} is outside [0, max_timeout_fee_lovelace ${input.parameters.max_timeout_fee_lovelace}]`,
      "timeout-challenger-fee-cap",
    );
  if (c > input.remainingChallengerLovelace)
    fail("Timeout challenger fee exceeds the remaining challenger reserve");
  const feeLovelace = slash.feePart + c;
  if (feeLovelace <= 0n) fail("Timeout fee must be positive");
  return {
    taken: slash.taken,
    feePart: slash.feePart,
    payout: slash.payout,
    poolOutputLovelace: slash.poolOutputLovelace,
    challengerFeeLovelace: c,
    feeLovelace,
    challengerOutputLovelace:
      input.remainingChallengerLovelace -
      c +
      input.parameters.challenge_record_lovelace +
      slash.payout,
  };
};

/**
 * The smallest challenger contribution `c` that lifts the fee to the ledger
 * minimum: `max(0, requiredFee - feePart)`, refused above the cap.
 */
export const daAvailabilityTimeoutChallengerFee = (input: {
  readonly feePartLovelace: bigint;
  readonly requiredFeeLovelace: bigint;
  readonly parameters: Availability.DaAvailabilityParameters;
}): bigint => {
  const c =
    input.requiredFeeLovelace > input.feePartLovelace
      ? input.requiredFeeLovelace - input.feePartLovelace
      : 0n;
  if (c > input.parameters.max_timeout_fee_lovelace)
    fail(
      `Timeout needs a challenger fee of ${c}, above max_timeout_fee_lovelace ${input.parameters.max_timeout_fee_lovelace}`,
      "timeout-challenger-fee-cap",
    );
  return c;
};

/** Ledger limit on collateral inputs. */
const MAX_COLLATERAL_INPUTS = 3;

/**
 * Picks at most three plain-ADA coins, largest first, whose total covers
 * `requiredLovelace` and leaves either nothing or at least
 * `minimumReturnLovelace` as collateral return.
 */
export const selectDaAvailabilityCollateral = (input: {
  readonly candidates: readonly UTxO[];
  readonly requiredLovelace: bigint;
  readonly minimumReturnLovelace: bigint;
}): readonly UTxO[] => {
  const sorted = [...input.candidates].sort((a, b) => {
    const x = a.assets.lovelace ?? 0n,
      y = b.assets.lovelace ?? 0n;
    if (x !== y) return x > y ? -1 : 1;
    // Ties break in code-unit order of the out-ref, independent of locale.
    const ka = refKey(a),
      kb = refKey(b);
    return ka < kb ? -1 : ka > kb ? 1 : 0;
  });
  const selected: UTxO[] = [];
  let total = 0n;
  for (const u of sorted) {
    if (selected.length === MAX_COLLATERAL_INPUTS) break;
    selected.push(u);
    total += u.assets.lovelace ?? 0n;
    const change = total - input.requiredLovelace;
    if (change === 0n || change >= input.minimumReturnLovelace) return selected;
  }
  return fail(
    `No ${MAX_COLLATERAL_INPUTS} collateral coins cover ${input.requiredLovelace} lovelace with a valid collateral return`,
    "collateral-insufficient",
  );
};

/**
 * The ledger's minimum fee for a completed, still unsigned transaction: CML's
 * `min_fee` over the body, redeemer budgets and reference scripts, plus one
 * vkey witness per distinct signing key and a small allowance for the
 * witness-set header and for coin-width changes when `c` is lowered. The
 * reference-script size is the provider's script CBOR, which is at least the
 * ledger's count, so the estimate errs high.
 */
export const daAvailabilityLedgerMinFee = (input: {
  readonly unsignedCbor: string;
  readonly protocolParameters: ProtocolParameters;
  readonly referenceScriptBytes: bigint;
  readonly vkeyWitnessCount: number;
}): bigint => {
  const p = input.protocolParameters;
  const tx = CML.Transaction.from_cbor_hex(input.unsignedCbor);
  const linear = CML.LinearFee.new(
    BigInt(p.minFeeA),
    BigInt(p.minFeeB),
    BigInt(p.minFeeRefScriptCostPerByte),
  );
  const mem = CML.SubCoin.new(BigInt(Math.round(p.priceMem * 1e8)), 100000000n);
  const step = CML.SubCoin.new(
    BigInt(Math.round(p.priceStep * 1e8)),
    100000000n,
  );
  const prices = CML.ExUnitPrices.new(mem, step);
  try {
    const base = CML.min_fee(tx, linear, prices, input.referenceScriptBytes);
    // A vkey witness is [bytes32, bytes64]: 101 bytes. The allowance covers the
    // witness-set key, the (tagged) set header and fee/coin width changes.
    const extraBytes = 101n * BigInt(input.vkeyWitnessCount) + 24n;
    return base + extraBytes * BigInt(p.minFeeA);
  } finally {
    prices.free();
    step.free();
    mem.free();
    linear.free();
    tx.free();
  }
};

const alignResources = <P extends DaAvailabilityTransactionResources>(
  lucid: LucidEvolution,
  p: P,
): P => {
  const lowerSlot = lucid.unixTimeToSlot(Number(p.validFrom));
  const lower = BigInt(lucid.slotToUnixTime(lowerSlot));
  const validFrom =
    lower < p.validFrom ? BigInt(lucid.slotToUnixTime(lowerSlot + 1)) : lower;
  const validTo = BigInt(
    lucid.slotToUnixTime(lucid.unixTimeToSlot(Number(p.validTo))),
  );
  if (validTo <= validFrom)
    fail("Validity interval contains no complete ledger slot");
  return { ...p, validFrom, validTo };
};

const complete = async (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: DaAvailabilityTransactionResources,
  tx: TxBuilder,
  meta: {
    action: DaAvailabilityTransactionAction;
    headerHash: string;
    challengeAssetName: string;
    inputs: readonly UTxO[];
    refs: readonly UTxO[];
    outputs: readonly DaAvailabilityExpectedOutput[];
    /** Timeout only: the slashed share of the fee, exempt from the cap. */
    timeoutFeePart?: bigint;
  },
): Promise<BuiltDaAvailabilityTransaction> => {
  Availability.assertCanonicalDaAvailabilityParameters(d.parameters);
  const cap =
    meta.action === "publish"
      ? d.parameters.max_publication_fee_lovelace
      : meta.action === "settle"
        ? d.parameters.max_settlement_fee_lovelace
        : meta.action === "open"
          ? d.parameters.max_open_fee_lovelace
          : meta.action === "close"
            ? d.parameters.max_close_fee_lovelace
            : d.parameters.max_timeout_fee_lovelace;
  if ((meta.action === "timeout") !== (meta.timeoutFeePart !== undefined))
    fail("Only a timeout carries a slashed fee part");
  // The timeout caps only the challenger's contribution c = fee - feePart; the
  // slashed penalty share is fee by protocol and outside every cap.
  const capped = p.feeLovelace - (meta.timeoutFeePart ?? 0n);
  if (
    p.feeLovelace <= 0n ||
    capped < 0n ||
    capped > cap ||
    p.validFrom < 0n ||
    p.validTo <= p.validFrom ||
    p.validTo - p.validFrom > 120_000n ||
    p.validTo > BigInt(Number.MAX_SAFE_INTEGER)
  )
    fail("Invalid fee or bounded validity interval");
  const walletAddress = await lucid.wallet().address();
  if (
    p.collateralInputs.length === 0 ||
    p.collateralInputs.some(
      (u) => !isPlainPositiveAdaOnlyUtxo(u) || u.address !== walletAddress,
    )
  )
    fail("Explicit plain-ADA wallet collateral is required");
  const spent = new Set(meta.inputs.map(refKey));
  const refs = new Set(meta.refs.map(refKey));
  if (
    spent.size !== meta.inputs.length ||
    p.collateralInputs.some(
      (u) => spent.has(refKey(u)) || refs.has(refKey(u)),
    ) ||
    meta.inputs.some((u) => refs.has(refKey(u)))
  )
    fail("Transaction resources overlap");
  const protocol = protocolParameters(lucid);
  const collateral =
    (p.feeLovelace * BigInt(protocol.collateralPercentage) + 99n) / 100n;
  const collateralInputs = selectDaAvailabilityCollateral({
    candidates: p.collateralInputs,
    requiredLovelace: collateral,
    minimumReturnLovelace: minAda(lucid, {
      address: walletAddress,
      assets: { lovelace: collateral },
    }),
  });
  const available = await lucid.utxosByOutRef([
    ...meta.inputs,
    ...meta.refs,
    ...collateralInputs,
  ]);
  const observed = new Map(available.map((u) => [refKey(u), u]));
  for (const u of [...meta.inputs, ...meta.refs, ...collateralInputs]) {
    const live = observed.get(refKey(u));
    if (
      !live ||
      live.address !== u.address ||
      !sameAssets(live.assets, u.assets) ||
      (u.datum == null
        ? live.datum != null
        : !outputDatumCborMatches(live, u.datum)) ||
      (live.scriptRef ? validatorToScriptHash(live.scriptRef) : null) !==
        (u.scriptRef ? validatorToScriptHash(u.scriptRef) : null)
    )
      fail(`Stale transaction resource ${refKey(u)}`);
  }
  for (const o of meta.outputs)
    if ((o.assets.lovelace ?? 0n) < minAda(lucid, o))
      fail("Protected output is below live ledger minimum ADA");
  let completed: TxSignBuilder;
  try {
    completed = await tx
      .setMinFee(p.feeLovelace)
      .validFrom(Number(p.validFrom))
      .validTo(Number(p.validTo))
      .complete({
        ...completeOptionsWithLocalEval({
          coinSelection: false,
          presetWalletInputs: collateralInputs,
        }),
        setCollateral: collateral,
      });
  } catch (cause) {
    if (cause instanceof DaAvailabilityTransactionError) throw cause;
    return fail(
      `Transaction does not complete at the exact fee ${p.feeLovelace}: ${cause instanceof Error ? cause.message : String(cause)}`,
      "completion-failed",
    );
  }
  const body = completed.toTransaction().body();
  if (
    body.fee() !== p.feeLovelace ||
    body.inputs().len() !== meta.inputs.length ||
    body.outputs().len() !== meta.outputs.length
  )
    fail(
      "Completed transaction changed protected fee, inputs, or outputs",
      "completion-failed",
    );
  for (let i = 0; i < body.inputs().len(); i++) {
    const u = body.inputs().get(i);
    if (!spent.has(`${u.transaction_id().to_hex()}#${u.index()}`))
      fail("Completed transaction selected an unreserved input");
  }
  const actualCollateral: UTxO[] = [];
  const collateralBody = body.collateral_inputs();
  if (!collateralBody || collateralBody.len() === 0)
    fail("Completed transaction lacks collateral");
  if (collateralBody!.len() > MAX_COLLATERAL_INPUTS)
    fail("Completed transaction exceeds the collateral input limit");
  for (let i = 0; i < collateralBody!.len(); i++) {
    const c = collateralBody!.get(i);
    const u = collateralInputs.find(
      (u) => refKey(u) === `${c.transaction_id().to_hex()}#${c.index()}`,
    );
    if (!u) fail("Completed transaction selected unreserved collateral");
    actualCollateral.push(u!);
  }
  const totalCollateral = body.total_collateral();
  if (totalCollateral !== undefined && totalCollateral < collateral)
    fail("Completed transaction collateral is below the ledger percentage");
  return {
    tx: completed,
    unsignedCbor: completed.toCBOR(),
    txId: completed.toHash(),
    action: meta.action,
    headerHash: meta.headerHash,
    challengeAssetName: meta.challengeAssetName,
    validityRange: { validFrom: p.validFrom, validTo: p.validTo },
    spentOutRefs: meta.inputs,
    referenceOutRefs: meta.refs,
    collateralOutRefs: actualCollateral,
    expectedOutputs: meta.outputs,
    feeLovelace: p.feeLovelace,
    ...(meta.timeoutFeePart === undefined
      ? {}
      : { timeoutFeePartLovelace: meta.timeoutFeePart }),
  };
};

/** Conservatively reserves a full-ledger-size carrier and thread at live rent. */
export const assertDaAvailabilityOpeningWorkingCapital = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  plan: Availability.DaAvailabilityChallengeDatumPlan,
): void => {
  const protocol = protocolParameters(lucid);
  // No accepted carrier can be larger than its entire transaction. Encoding this
  // bound as a bytes datum also covers the datum-envelope overhead conservatively.
  const carrierFloor = minAda(lucid, {
    address: d.contracts.availabilityChallenge.spendingScriptAddress,
    assets: { lovelace: 100_000_000n },
    datum: Data.to("00".repeat(protocol.maxTxSize)),
  });
  for (let i = 0; i < plan.trancheThreads.length; i++) {
    const thread = plan.trancheThreads[i]!;
    if (!("Active" in thread)) fail("Opening must create active tranches");
    const active = (
      thread as Extract<
        Availability.DaAvailabilityTrancheDatum,
        { Active: unknown }
      >
    ).Active;
    const funded = plan.trancheFunding[i]!;
    const worstThread = {
      Active: {
        ...active,
        next_offset:
          active.descriptor.start_offset + active.descriptor.byte_length,
        latest_carrier_output_index: 1n,
      },
    };
    const threadFloor = minAda(lucid, {
      address: d.contracts.availabilityChallenge.spendingScriptAddress,
      assets: {
        lovelace: funded.initialLovelace,
        [d.contracts.availabilityChallenge.policyId +
        Availability.daAvailabilityTrancheAssetName({
          challengeAssetName: plan.challengeAssetName,
          trancheIndex: i,
        })]: 1n,
      },
      datum: Data.to(worstThread, Availability.DaAvailabilityTrancheDatum),
    });
    if (
      funded.initialLovelace -
        funded.maximumPublicationFeeReserveLovelace -
        funded.maximumSettlementFeeReserveLovelace <
      carrierFloor + threadFloor
    )
      fail(
        "challenger_bond_lovelace cannot fund all publication fees and live carrier/thread working capital",
      );
  }
};

/**
 * Opens a challenge (spec #685 E1). Inputs: the challenger's exact funding
 * coin and the Attested queue node. Outputs, in this order: the challenge
 * record (0), the queue node now `Challenged` (1), one thread per tranche
 * (2..), the terminal accumulator (last). The DACH identity derives from the
 * funding coin's out-reference and `opened_at` is `validTo - 1`.
 */
export const buildOpenDaAvailabilityChallengeTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: OpenDaAvailabilityChallengeParams,
) =>
  effect(async () => {
    p = alignResources(lucid, p);
    const policy = d.contracts.availabilityChallenge.policyId,
      address = d.contracts.availabilityChallenge.spendingScriptAddress;
    const q = await state(p.queue, d);
    const node = Data.castFrom(q.datum.data, StateQueueNode);
    const commitmentHash = assertDaAvailabilityOpenCommitment({
      commitment: p.commitment,
      deploymentIdentity: d.hubOraclePolicyId,
      queueAssetName: q.assetName,
      status: node.da_attestation,
      parameters: d.parameters,
    });
    assertDaAvailabilityOpenWithinChallengeWindow({
      validTo: p.validTo,
      nodeEndTime: node.header.endTime,
      daChallengeWindowMs: p.daChallengeWindowMs,
    });
    if (
      !isPlainPositiveAdaOnlyUtxo(p.challengerFunding) ||
      p.challengerFunding.address !== keyAddress(lucid, p.challenger) ||
      p.challengerFunding.assets.lovelace !==
        d.parameters.challenger_bond_lovelace +
          d.parameters.challenge_record_lovelace +
          p.feeLovelace
    )
      fail(
        "Opening requires an exact isolated challenger_bond_lovelace + challenge_record_lovelace + fee input",
      );
    const plan = Availability.buildDaAvailabilityChallengeDatumPlan({
      commitment: p.commitment,
      challengerFundingOutRef: outputReferenceFromUTxO(p.challengerFunding),
      challenger: p.challenger,
      // The validator anchors the response window at the inclusive upper
      // validity bound; the ledger's upper end is exclusive.
      openedAt: p.validTo - 1n,
      parameters: d.parameters,
    });
    assertDaAvailabilityOpeningWorkingCapital(lucid, d, plan);
    const record: DaAvailabilityExpectedOutput = {
      address,
      assets: {
        lovelace: plan.recordLovelace,
        [policy + plan.challengeAssetName]: 1n,
      },
      datum: Availability.encodeDaAvailabilityChallengeRecord(
        plan.record,
        d.parameters,
      ),
    };
    assertDaAvailabilityChallengeRecordMinAda({
      coinsPerUtxoByte: protocolParameters(lucid).coinsPerUtxoByte,
      record,
      challengeRecordLovelace: d.parameters.challenge_record_lovelace,
    });
    const outputs: DaAvailabilityExpectedOutput[] = [
      record,
      {
        address: p.queue.address,
        assets: p.queue.assets,
        datum: encodeLinkedListNodeView({
          ...q.datum,
          data: castStateQueueNodeToData({
            ...node,
            da_attestation: {
              Challenged: {
                commitment_hash: commitmentHash,
                challenge_asset_name: plan.challengeAssetName,
              },
            },
          }) as LinkedListNodeView["data"],
        }),
      },
    ];
    const minted: Assets = { [policy + plan.challengeAssetName]: 1n };
    for (let i = 0; i < plan.trancheThreads.length; i++) {
      const unit =
        policy +
        Availability.daAvailabilityTrancheAssetName({
          challengeAssetName: plan.challengeAssetName,
          trancheIndex: i,
        });
      minted[unit] = 1n;
      outputs.push({
        address,
        assets: {
          lovelace: plan.trancheFunding[i]!.initialLovelace,
          [unit]: 1n,
        },
        datum: Availability.encodeDaAvailabilityTrancheDatum(
          plan.trancheThreads[i]!,
        ),
      });
    }
    const terminalUnit =
      policy +
      Availability.daAvailabilityTerminalAccumulatorAssetName(
        plan.challengeAssetName,
      );
    minted[terminalUnit] = 1n;
    outputs.push({
      address,
      assets: {
        lovelace: plan.terminalAccumulatorFundingLovelace,
        [terminalUnit]: 1n,
      },
      datum: Availability.encodeDaAvailabilityTerminalAccumulatorDatum(
        plan.terminalAccumulator,
      ),
    });
    const terminalIndex = outputs.length - 1;
    const refs = [
      ...mintRefs(d, "open"),
      hub(d),
      role(d, "state-queue spending", d.contracts.stateQueue.spendingScript),
    ];
    const yieldRef = refs[2]!;
    let tx = lucid
      .newTx()
      .collectFrom([p.challengerFunding])
      .collectFrom([p.queue], queueUpdate(p.queue, policy, outputs[1]!, 1))
      .readFrom(refs)
      .mintAssets(
        minted,
        mint(policy, (ctx) => {
          for (let i = 0; i < plan.trancheThreads.length; i++)
            at(ctx, 2 + i, outputs[2 + i]!, "tranche");
          return {
            OpenChallenge: {
              yield_to_ref_input_index: requireReferenceInputIndex(
                ctx,
                yieldRef,
                "open yield",
              ),
              hub_oracle_ref_input_index: requireReferenceInputIndex(
                ctx,
                d.hubOracleRefInput,
                "hub",
              ),
              record_output_index: at(ctx, 0, record, "record"),
              challenger_input_index: requireInputIndex(
                ctx,
                p.challengerFunding,
                "challenger",
              ),
              state_queue_input_index: requireInputIndex(ctx, p.queue, "queue"),
              state_queue_output_index: at(ctx, 1, outputs[1]!, "queue"),
              first_tranche_output_index: 2n,
              terminal_accumulator_output_index: at(
                ctx,
                terminalIndex,
                outputs[terminalIndex]!,
                "terminal",
              ),
              challenger: p.challenger,
            },
          };
        }),
      )
      .addSignerKey(p.challenger);
    tx = withYield(lucid, d, pay(tx, outputs), "open");
    return complete(lucid, d, p, tx, {
      action: "open",
      headerHash: p.commitment.header_hash,
      challengeAssetName: plan.challengeAssetName,
      inputs: [p.challengerFunding, p.queue],
      refs,
      outputs,
    });
  });

const authenticateCarrier = (
  d: DaAvailabilityDeployment,
  thread: UTxO,
  t: Availability.DaAvailabilityTrancheDatum,
  carrier?: UTxO,
) => {
  const v = "Active" in t ? t.Active : t.Receipt;
  const index =
    "Active" in t
      ? t.Active.latest_carrier_output_index
      : t.Receipt.terminal_carrier_output_index;
  auth(thread, d.contracts.availabilityChallenge.spendingScriptAddress, [
    d.contracts.availabilityChallenge.policyId +
      Availability.daAvailabilityTrancheAssetName({
        challengeAssetName: v.challenge_asset_name,
        trancheIndex: Number(v.descriptor.tranche_index),
      }),
  ]);
  if (index === null) {
    if (carrier) fail("Unexpected carrier for a fresh tranche");
    return;
  }
  if (
    !carrier ||
    carrier.txHash !== thread.txHash ||
    BigInt(carrier.outputIndex) !== index
  )
    fail("Missing exact latest carrier out-reference");
  const c = carrier!;
  auth(c, thread.address, []);
  const publication = Availability.parseDaAvailabilityPublicationDatumCbor(
    Data.to(
      Data.from(datum(c), Availability.DaAvailabilityPublicationDatum),
      Availability.DaAvailabilityPublicationDatum,
    ),
    d.parameters.response_geometry,
    v.descriptor,
  );
  const accumulator =
    "Active" in t ? t.Active.accumulator : t.Receipt.terminal_accumulator;
  if (
    publication.challenge_asset_name !== v.challenge_asset_name ||
    publication.header_hash !== v.header_hash ||
    publication.deployment_identity !== v.deployment_identity ||
    publication.tranche_index !== v.descriptor.tranche_index ||
    publication.next_accumulator !== accumulator ||
    publication.chunk_offset + publication.chunk_byte_length !==
      ("Active" in t
        ? t.Active.next_offset
        : t.Receipt.descriptor.start_offset + t.Receipt.descriptor.byte_length)
  )
    fail("Carrier does not authenticate tranche continuation");
};
export const buildPublishDaAvailabilityChunkTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: PublishDaAvailabilityChunkParams,
) =>
  effect(async () => {
    p = alignResources(lucid, p);
    const t = trancheDatum(p.thread);
    authenticateCarrier(d, p.thread, t, p.previousCarrier);
    if (!("Active" in t)) fail("Only an active tranche accepts publication");
    const active = (
      t as Extract<Availability.DaAvailabilityTrancheDatum, { Active: unknown }>
    ).Active;
    const carrier: DaAvailabilityExpectedOutput = {
      address: p.thread.address,
      assets: { lovelace: 0n },
      datum: Availability.encodeDaAvailabilityPublicationDatum(
        p.publication,
        d.parameters.response_geometry,
        active.descriptor,
      ),
    };
    carrier.assets.lovelace = minAda(lucid, carrier);
    const next = Availability.advanceDaAvailabilityTranche({
      active: t,
      publication: p.publication,
      responseGeometry: Availability.availabilityResponseGeometry({
        chunkByteLength: Number(
          d.parameters.response_geometry.chunk_byte_length,
        ),
        trancheByteLength: Number(
          d.parameters.response_geometry.tranche_byte_length,
        ),
        maxTrancheCount: Number(
          d.parameters.response_geometry.max_tranche_count,
        ),
      }),
      inclusiveValidityUpper: p.validTo - 1n,
      carrierOutputIndex: 1n,
    });
    const output: DaAvailabilityExpectedOutput = {
      address: p.thread.address,
      assets: { ...p.thread.assets },
      datum: Availability.encodeDaAvailabilityTrancheDatum(next),
    };
    const transition =
      Availability.planDaAvailabilityPublicationValueTransition({
        threadInputLovelace: p.thread.assets.lovelace,
        previousCarrierInputLovelace: p.previousCarrier?.assets.lovelace ?? 0n,
        nextCarrierOutputLovelace: carrier.assets.lovelace,
        transactionFeeLovelace: p.feeLovelace,
        minimumThreadOutputLovelace: minAda(lucid, output),
        isFirstPublication: active.latest_carrier_output_index === null,
        parameters: d.parameters,
      });
    output.assets.lovelace = transition;
    const refs = baseRefs(d);
    let tx = lucid
      .newTx()
      .readFrom(refs)
      .collectFrom(
        [p.thread],
        spend(p.thread, (ctx) => ({
          AdvanceTranche: {
            thread_output_index: at(ctx, 0, output, "thread"),
            carrier_output_index: at(ctx, 1, carrier, "carrier"),
            m_previous_carrier_input_index: p.previousCarrier
              ? requireInputIndex(ctx, p.previousCarrier, "previous carrier")
              : null,
          },
        })),
      );
    if (p.previousCarrier)
      tx = tx.collectFrom(
        [p.previousCarrier],
        spend(p.previousCarrier, (ctx) => ({
          ConsumeCarrier: {
            thread_input_index: requireInputIndex(ctx, p.thread, "thread"),
            thread_spend_redeemer_index: requireSpendRedeemerIndex(
              ctx,
              p.thread,
              "thread",
            ),
          },
        })),
      );
    const outputs = [output, carrier];
    return complete(lucid, d, p, pay(tx, outputs), {
      action: "publish",
      headerHash: active.header_hash,
      challengeAssetName: active.challenge_asset_name,
      inputs: [p.thread, ...(p.previousCarrier ? [p.previousCarrier] : [])],
      refs,
      outputs,
    });
  });

/**
 * Authenticates a challenge record (the DACH token and exactly
 * `challenge_record_lovelace` at the availability address, with a canonical
 * record datum for this deployment) and, when given, the terminal accumulator
 * it binds.
 */
const challenged = (
  d: DaAvailabilityDeployment,
  record: UTxO,
  terminal?: UTxO,
) => {
  const r = recordDatum(record, d);
  if (r.commitment.deployment_identity !== d.hubOraclePolicyId)
    fail("Challenge record deployment identity mismatch");
  auth(record, d.contracts.availabilityChallenge.spendingScriptAddress, [
    d.contracts.availabilityChallenge.policyId + r.challenge_asset_name,
  ]);
  if (record.assets.lovelace !== d.parameters.challenge_record_lovelace)
    fail("Challenge record must hold exactly challenge_record_lovelace");
  if (terminal) {
    const t = terminalDatum(terminal);
    auth(terminal, record.address, [
      d.contracts.availabilityChallenge.policyId +
        Availability.daAvailabilityTerminalAccumulatorAssetName(
          r.challenge_asset_name,
        ),
    ]);
    if (
      t.challenge_asset_name !== r.challenge_asset_name ||
      t.header_hash !== r.commitment.header_hash ||
      t.deployment_identity !== r.commitment.deployment_identity ||
      t.challenger !== r.challenger ||
      t.response_deadline !== r.response_deadline ||
      t.remaining_challenger_lovelace !== terminal.assets.lovelace
    )
      fail("Terminal accumulator does not authenticate the challenge record");
  }
  return r;
};
/** The queue node's status is exactly the `Challenged` the record implies. */
const assertChallengedNode = (
  q: StateQueueUTxO,
  r: Availability.DaAvailabilityChallengeRecord,
) => {
  const node = Data.castFrom(q.datum.data, StateQueueNode);
  const status = node.da_attestation;
  if (
    q.assetName !==
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + r.commitment.header_hash ||
    typeof status !== "object" ||
    !("Challenged" in status) ||
    status.Challenged.challenge_asset_name !== r.challenge_asset_name ||
    status.Challenged.commitment_hash !==
      Availability.daAvailabilityCommitmentHash(r.commitment)
  )
    fail("Queue does not authenticate the challenge");
  return node;
};
export const buildSettleDaAvailabilityTrancheTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: SettleDaAvailabilityTrancheParams,
) =>
  effect(async () => {
    p = alignResources(lucid, p);
    const r = challenged(d, p.record, p.terminal),
      t = trancheDatum(p.thread);
    authenticateCarrier(d, p.thread, t, p.carrier);
    const term = terminalDatum(p.terminal);
    const plan = Availability.planDaAvailabilitySettlement({
      commitment: r.commitment,
      terminalAccumulator: term,
      tranche: t,
      threadLovelace: p.thread.assets.lovelace,
      carrierLovelace: p.carrier?.assets.lovelace ?? 0n,
      transactionFeeLovelace: p.feeLovelace,
      inclusiveValidityLower: p.validFrom,
      parameters: d.parameters,
    });
    const policy = d.contracts.availabilityChallenge.policyId;
    const output = {
      address: p.terminal.address,
      assets: { ...p.terminal.assets, lovelace: plan.nextTerminalLovelace },
      datum: Availability.encodeDaAvailabilityTerminalAccumulatorDatum(
        plan.nextTerminalAccumulator,
      ),
    };
    const refs = [...mintRefs(d, "settle"), p.record];
    const inputs = [p.terminal, p.thread, ...(p.carrier ? [p.carrier] : [])];
    let tx = lucid.newTx().readFrom(refs);
    for (const u of inputs) tx = tx.collectFrom([u], coordinate(u, policy));
    tx = tx.mintAssets(
      {
        [policy +
        Availability.daAvailabilityTrancheAssetName({
          challengeAssetName: r.challenge_asset_name,
          trancheIndex: Number(term.next_tranche_index),
        })]: -1n,
      },
      mint(policy, (ctx) => ({
        SettleTranche: {
          yield_to_ref_input_index: requireReferenceInputIndex(
            ctx,
            refs[2]!,
            "settle yield",
          ),
          record_ref_input_index: requireReferenceInputIndex(
            ctx,
            p.record,
            "record",
          ),
          terminal_accumulator_input_index: requireInputIndex(
            ctx,
            p.terminal,
            "terminal",
          ),
          terminal_accumulator_output_index: at(ctx, 0, output, "terminal"),
          tranche_input_index: requireInputIndex(ctx, p.thread, "thread"),
          carrier_input_index: p.carrier
            ? requireInputIndex(ctx, p.carrier, "carrier")
            : null,
        },
      })),
    );
    return complete(
      lucid,
      d,
      p,
      withYield(lucid, d, pay(tx, [output]), "settle"),
      {
        action: "settle",
        headerHash: r.commitment.header_hash,
        challengeAssetName: r.challenge_asset_name,
        inputs,
        refs,
        outputs: [output],
      },
    );
  });
/**
 * Closes a fully published challenge. Inputs: record, terminal, queue node.
 * Outputs: the queue node now `Published` (0) and the challenger's refund of
 * `remaining - fee + challenge_record_lovelace` (1). The committee's pooled
 * bond is not touched.
 */
export const buildCloseDaAvailabilityChallengeTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: CloseDaAvailabilityChallengeParams,
) =>
  effect(async () => {
    p = alignResources(lucid, p);
    const r = challenged(d, p.record, p.terminal),
      term = terminalDatum(p.terminal);
    if (
      term.has_timed_out_tranche ||
      term.next_tranche_index !==
        BigInt(r.commitment.tranche_descriptors.length)
    )
      fail("Challenge is not completely published and settled");
    if (term.remaining_challenger_lovelace <= p.feeLovelace)
      fail("Close fee must leave challenger reserve");
    const q = await state(p.queue, d);
    const node = assertChallengedNode(q, r);
    const outputs: DaAvailabilityExpectedOutput[] = [
      {
        address: p.queue.address,
        assets: p.queue.assets,
        datum: encodeLinkedListNodeView({
          ...q.datum,
          data: castStateQueueNodeToData({
            ...node,
            da_attestation: {
              Published: {
                terminal_commitment:
                  Availability.daAvailabilityPublishedTerminalCommitment(
                    r.commitment,
                  ),
              },
            },
          }) as LinkedListNodeView["data"],
        }),
      },
      {
        address: keyAddress(lucid, r.challenger),
        assets: {
          lovelace:
            term.remaining_challenger_lovelace -
            p.feeLovelace +
            d.parameters.challenge_record_lovelace,
        },
      },
    ];
    const policy = d.contracts.availabilityChallenge.policyId,
      refs = [
        ...mintRefs(d, "close"),
        hub(d),
        role(d, "state-queue spending", d.contracts.stateQueue.spendingScript),
      ];
    const tx = lucid
      .newTx()
      .collectFrom([p.record], coordinate(p.record, policy))
      .collectFrom([p.terminal], coordinate(p.terminal, policy))
      .collectFrom([p.queue], queueUpdate(p.queue, policy, outputs[0]!, 0))
      .readFrom(refs)
      .mintAssets(
        {
          [policy + r.challenge_asset_name]: -1n,
          [policy +
          Availability.daAvailabilityTerminalAccumulatorAssetName(
            r.challenge_asset_name,
          )]: -1n,
        },
        mint(policy, (ctx) => ({
          CloseChallenge: {
            yield_to_ref_input_index: requireReferenceInputIndex(
              ctx,
              refs[2]!,
              "close yield",
            ),
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.hubOracleRefInput,
              "hub",
            ),
            record_input_index: requireInputIndex(ctx, p.record, "record"),
            terminal_accumulator_input_index: requireInputIndex(
              ctx,
              p.terminal,
              "terminal",
            ),
            state_queue_input_index: requireInputIndex(ctx, p.queue, "queue"),
            state_queue_output_index: at(ctx, 0, outputs[0]!, "queue"),
            challenger_refund_output_index: at(
              ctx,
              1,
              outputs[1]!,
              "challenger refund",
            ),
          },
        })),
      );
    return complete(
      lucid,
      d,
      p,
      withYield(lucid, d, pay(tx, outputs), "close"),
      {
        action: "close",
        headerHash: r.commitment.header_hash,
        challengeAssetName: r.challenge_asset_name,
        inputs: [p.record, p.terminal, p.queue],
        refs,
        outputs,
      },
    );
  });

/**
 * The pooled DA bond input: the pool NFT exactly once beside lovelace, at a
 * script address whose payment credential is the pool policy (as
 * `get_authentic_pool_input` requires), with a canonical inline pool datum.
 */
const authenticPool = (d: DaAvailabilityDeployment, u: UTxO) => {
  const policy = d.contracts.daBondPool.policyId;
  const unit = daBondPoolUnit(policy);
  const credential = getAddressDetails(u.address).paymentCredential;
  if (
    u.address !== d.contracts.daBondPool.spendingScriptAddress ||
    credential?.type !== "Script" ||
    credential.hash !== policy ||
    u.scriptRef != null ||
    u.datumHash != null ||
    u.assets[unit] !== 1n ||
    Object.keys(u.assets).some((k) => k !== "lovelace" && k !== unit)
  )
    fail(`Unauthentic DA bond pool input ${refKey(u)}`);
  const value = Data.from(datum(u), DaBondPoolDatum);
  assertCanonicalDaBondPoolDatum(value);
  return value;
};

type TimeoutLeg = {
  readonly record: UTxO;
  readonly terminal: UTxO;
  readonly pool: UTxO;
  readonly feePart: bigint;
};

const removal = async (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: DaAvailabilityRemovalParams,
  initial?: TimeoutLeg,
): Promise<BuiltDaAvailabilityTransaction> => {
  p = alignResources(lucid, p);
  const q = await state(p.queue, d),
    root = await state(p.confirmedState, d),
    desc = p.descendant ? await state(p.descendant, d) : undefined;
  if (
    q.assetName !== STATE_QUEUE_NODE_ASSET_NAME_PREFIX + p.headerHash ||
    root.datum.key !== "Empty" ||
    root.datum.next === "Empty" ||
    root.datum.next.Key.key !== p.headerHash
  )
    fail("Unavailable removal requires the current queue head");
  const node = Data.castFrom(q.datum.data, StateQueueNode);
  if (
    typeof node.da_attestation !== "object" ||
    !("Challenged" in node.da_attestation) ||
    node.da_attestation.Challenged.challenge_asset_name !== p.challengeAssetName
  )
    fail("Unavailable queue identity mismatch");
  if (desc) {
    if (
      q.datum.next === "Empty" ||
      desc.datum.key === "Empty" ||
      q.datum.next.Key.key !== desc.datum.key.Key.key
    )
      fail("Removal requires the immediate descendant");
  } else if (q.datum.next !== "Empty")
    fail("Remove head only after its descendants are pruned");
  auth(p.correctionLock, d.contracts.correctionLock.spendingScriptAddress, [
    correctionLockUnit(d.hubOraclePolicyId),
  ]);
  const lock = Data.from(datum(p.correctionLock), CorrectionLockDatum),
    locked: CorrectionLockDatum = {
      Locked: {
        target_header_hash: p.headerHash,
        correction_identity: {
          AvailabilityChallenge: { challenge_asset_name: p.challengeAssetName },
        },
      },
    };
  // The timeout (and so the pool's Slash) runs only on an Idle lock; the
  // resume steps run on the Locked lock the timeout left behind.
  if (
    initial
      ? lock !== "Idle"
      : Data.to(lock, CorrectionLockDatum) !==
        Data.to(locked, CorrectionLockDatum)
  )
    fail("Correction lock does not authorize this challenge transition");
  const continued = desc ? q : root,
    removed = desc ?? q;
  const outputs: DaAvailabilityExpectedOutput[] = [
    {
      address: continued.utxo.address,
      assets: continued.utxo.assets,
      datum: encodeLinkedListNodeView({
        ...continued.datum,
        next: removed.datum.next,
      }),
    },
    {
      address: p.correctionLock.address,
      assets: p.correctionLock.assets,
      datum: Data.to(desc ? locked : "Idle", CorrectionLockDatum),
    },
  ];
  const inputs = [
    q.utxo,
    ...(desc ? [desc.utxo] : [root.utxo]),
    p.correctionLock,
  ];
  const refs = [
    hub(d),
    role(d, "state-queue spending", d.contracts.stateQueue.spendingScript),
    role(d, "state-queue minting", d.contracts.stateQueue.mintingScript),
    role(
      d,
      "state-queue unavailable-timeout withdrawal",
      d.contracts.stateQueue.yields.unavailableTimeout.withdrawalScript,
    ),
    role(
      d,
      "correction-lock spending",
      d.contracts.correctionLock.spendingScript,
    ),
    ...(desc ? [root.utxo] : []),
  ];
  if (initial && p.fundingQueueTailRefInput) {
    const tail = await state(p.fundingQueueTailRefInput, d);
    if (tail.datum.next !== "Empty")
      fail("Timeout funding witness must be the current queue tail");
    if (![...inputs, ...refs].some((u) => refKey(u) === refKey(tail.utxo)))
      refs.push(tail.utxo);
  }
  const qp = d.contracts.stateQueue.policyId,
    ap = d.contracts.availabilityChallenge.policyId;
  let tx = lucid
    .newTx()
    .collectFrom(
      [q.utxo, ...(desc ? [desc.utxo] : [root.utxo])],
      Data.to("LinkedListMutation", StateQueueSpendRedeemer),
    )
    .collectFrom([p.correctionLock], ((ctx) => {
      requireOwnSpendPurpose(ctx, p.correctionLock, "correction lock");
      return Data.to(
        {
          Correct: {
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.hubOracleRefInput,
              "hub",
            ),
          },
        },
        CorrectionLockRedeemer,
      );
    }) satisfies BuildTxWithRedeemer);
  if (initial) {
    const r = challenged(d, initial.record, initial.terminal),
      terminal = terminalDatum(initial.terminal);
    if (
      r.challenge_asset_name !== p.challengeAssetName ||
      r.commitment.header_hash !== p.headerHash ||
      !terminal.has_timed_out_tranche ||
      terminal.next_tranche_index !==
        BigInt(r.commitment.tranche_descriptors.length) ||
      p.validFrom < r.response_deadline
    )
      fail(
        "Timeout requires all tranches settled and at least one expired active tranche",
      );
    assertChallengedNode(q, r);
    authenticPool(d, initial.pool);
    const plan = planDaAvailabilityTimeout({
      poolLovelace: initial.pool.assets.lovelace ?? 0n,
      remainingChallengerLovelace: terminal.remaining_challenger_lovelace,
      challengerFeeLovelace: p.feeLovelace - initial.feePart,
      parameters: d.parameters,
    });
    if (plan.feePart !== initial.feePart)
      fail("Timeout fee part does not match the pool's slash");
    const challenger = keyAddress(lucid, r.challenger);
    if (p.rentRefundAddress === challenger)
      fail(
        "Queue rent output must be distinct from the one protected challenger output",
      );
    const challengerIndex = outputs.length;
    outputs.push({
      address: challenger,
      assets: { lovelace: plan.challengerOutputLovelace },
    });
    const poolIndex = outputs.length;
    const poolOutput: DaAvailabilityExpectedOutput = {
      address: initial.pool.address,
      assets: {
        lovelace: plan.poolOutputLovelace,
        [daBondPoolUnit(d.contracts.daBondPool.policyId)]: 1n,
      },
      datum: datum(initial.pool),
    };
    outputs.push(poolOutput);
    refs.push(
      ...mintRefs(d, "timeout"),
      role(d, "da-bond-pool spending", d.contracts.daBondPool.spendingScript),
    );
    inputs.push(initial.record, initial.terminal, initial.pool);
    tx = tx
      .collectFrom([initial.record], coordinate(initial.record, ap))
      .collectFrom([initial.terminal], coordinate(initial.terminal, ap))
      .collectFrom([initial.pool], ((ctx) => {
        requireOwnSpendPurpose(ctx, initial.pool, "DA bond pool");
        return Data.to(
          {
            Slash: {
              hub_oracle_ref_input_index: requireReferenceInputIndex(
                ctx,
                d.hubOracleRefInput,
                "hub",
              ),
              state_queue_mint_redeemer_index: requireMintRedeemerIndex(
                ctx,
                qp,
                "queue",
              ),
              correction_lock_input_index: requireInputIndex(
                ctx,
                p.correctionLock,
                "correction lock",
              ),
              output_index: at(ctx, poolIndex, poolOutput, "DA bond pool"),
            },
          },
          DaBondPoolSpendRedeemer,
        );
      }) satisfies BuildTxWithRedeemer)
      .mintAssets(
        {
          [ap + r.challenge_asset_name]: -1n,
          [ap +
          Availability.daAvailabilityTerminalAccumulatorAssetName(
            r.challenge_asset_name,
          )]: -1n,
        },
        mint(ap, (ctx) => ({
          TimeoutChallenge: {
            yield_to_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.referenceScripts["availability-challenge timeout withdrawal"]!,
              "timeout yield",
            ),
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.hubOracleRefInput,
              "hub",
            ),
            record_input_index: requireInputIndex(
              ctx,
              initial.record,
              "record",
            ),
            terminal_accumulator_input_index: requireInputIndex(
              ctx,
              initial.terminal,
              "terminal",
            ),
            state_queue_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              qp,
              "queue",
            ),
            pool_input_index: requireInputIndex(
              ctx,
              initial.pool,
              "DA bond pool",
            ),
            pool_output_index: at(ctx, poolIndex, poolOutput, "DA bond pool"),
            challenger_refund_output_index: at(
              ctx,
              challengerIndex,
              outputs[challengerIndex]!,
              "challenger refund",
            ),
          },
        })),
      );
    tx = withYield(lucid, d, tx, "timeout");
  } else {
    const funding =
      p.feeFunding ??
      fail("Continuation requires an isolated fee funding input");
    if (
      !isPlainPositiveAdaOnlyUtxo(funding) ||
      funding.assets.lovelace < p.feeLovelace ||
      funding.address !== (await lucid.wallet().address())
    )
      fail("Continuation fee input must cover the explicit fee");
    inputs.push(funding);
    tx = tx.collectFrom([funding]);
    if (funding.assets.lovelace > p.feeLovelace)
      outputs.push({
        address: funding.address,
        assets: { lovelace: funding.assets.lovelace - p.feeLovelace },
      });
  }
  const continuedIndex = 0;
  outputs.push({
    address: p.rentRefundAddress,
    assets: { lovelace: removed.utxo.assets.lovelace },
  });
  tx = tx
    .readFrom(refs)
    .mintAssets({ [qp + removed.assetName]: -1n }, ((ctx) => {
      requireOwnMintPurpose(ctx, qp, "unavailable queue removal");
      return Data.to(
        {
          RemoveUnavailableBlockAfterTimeout: {
            yield_to_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.referenceScripts["state-queue unavailable-timeout withdrawal"]!,
              "queue yield",
            ),
            unavailable_header_hash: p.headerHash,
            challenge_asset_name: p.challengeAssetName,
            removal_approach: desc
              ? {
                  PruneTimedOutBlockDescendant: {
                    confirmed_state_ref_input_index: requireReferenceInputIndex(
                      ctx,
                      root.utxo,
                      "root",
                    ),
                    timed_out_node_input_outref: outputReferenceFromUTxO(
                      q.utxo,
                    ),
                    timed_out_node_output_index: at(
                      ctx,
                      continuedIndex,
                      outputs[continuedIndex]!,
                      "continued unavailable head",
                    ),
                  },
                }
              : {
                  RemoveTimedOutHead: {
                    confirmed_state_input_outref: outputReferenceFromUTxO(
                      root.utxo,
                    ),
                    confirmed_state_output_index: at(
                      ctx,
                      continuedIndex,
                      outputs[continuedIndex]!,
                      "continued root",
                    ),
                  },
                },
          },
        },
        StateQueueRedeemer,
      );
    }) satisfies BuildTxWithRedeemer)
    .withdraw(
      scriptRewardAddress(
        lucid.config().network ?? fail("Missing network"),
        d.contracts.stateQueue.yields.unavailableTimeout.withdrawalScript,
      ),
      0n,
      Data.void(),
    );
  return complete(lucid, d, p, pay(tx, outputs), {
    action: initial ? "timeout" : desc ? "prune" : "remove",
    headerHash: p.headerHash,
    challengeAssetName: p.challengeAssetName,
    inputs,
    refs,
    outputs,
    ...(initial ? { timeoutFeePart: initial.feePart } : {}),
  });
};

const isCompletionFailure = (cause: unknown) =>
  cause instanceof DaAvailabilityTransactionError &&
  cause.reason === "completion-failed";

/** Distinct key hashes that must sign: key-address inputs, collateral, required signers. */
const vkeyWitnessCount = (built: BuiltDaAvailabilityTransaction): number => {
  const keys = new Set<string>();
  for (const u of [...built.spentOutRefs, ...built.collateralOutRefs]) {
    const credential = getAddressDetails(u.address).paymentCredential;
    if (credential?.type === "Key") keys.add(credential.hash);
  }
  const signers = built.tx.toTransaction().body().required_signers();
  for (let i = 0; i < (signers?.len() ?? 0); i++)
    keys.add(signers!.get(i).to_hex());
  return keys.size;
};

/**
 * Times out an expired challenge (spec #685 E2): burns the record and terminal
 * accumulator, removes the challenged block (the head, or its immediate
 * descendant first) under an Idle correction lock, and slashes the pooled DA
 * bond in the same transaction.
 *
 * Outputs, in order: the continued queue node (0), the correction lock (1),
 * the ONE challenger output `remaining - c + challenge_record_lovelace +
 * payout` (2), the pool continuing with `pool_in - taken` beside its NFT and
 * its datum unchanged (3), the removed node's rent (4). The inputs pay the
 * outputs and the fee exactly, so no wallet coin is spent; the wallet only
 * backs collateral.
 *
 * The fee is exactly `feePart + c`, `feePart = min(penalty, taken)`. `c` is
 * the least the ledger needs:
 * 1. with a slashed penalty (`feePart > 0`), `c = 0` is tried first; a full
 *    pool's penalty covers any timeout's fee;
 * 2. otherwise the transaction is built at `c = max_timeout_fee`, which also
 *    proves the cap suffices, and its ledger minimum fee is measured
 *    (`daAvailabilityLedgerMinFee`);
 * 3. `c = max(0, measured - feePart)` is rebuilt; should Lucid still refuse
 *    that fee, the capped build stands.
 * A `c` above the cap is refused (`timeout-challenger-fee-cap`).
 */
export const buildTimeoutDaAvailabilityChallengeTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: TimeoutDaAvailabilityChallengeParams,
) =>
  effect(async () => {
    Availability.assertCanonicalDaAvailabilityParameters(d.parameters);
    authenticPool(d, p.pool);
    const { feePart } = planDaBondPoolSlash({
      poolLovelace: p.pool.assets.lovelace ?? 0n,
      parameters: d.parameters,
    });
    const { record, terminal, pool, challengerFeeLovelace, ...rest } = p;
    const attempt = (c: bigint) =>
      removal(
        lucid,
        d,
        { ...rest, feeLovelace: feePart + c },
        { record, terminal, pool, feePart },
      );
    if (challengerFeeLovelace !== undefined)
      return attempt(challengerFeeLovelace);
    if (feePart > 0n) {
      try {
        return await attempt(0n);
      } catch (cause) {
        if (!isCompletionFailure(cause)) throw cause;
      }
    }
    const cap = d.parameters.max_timeout_fee_lovelace;
    let capped: BuiltDaAvailabilityTransaction;
    try {
      capped = await attempt(cap);
    } catch (cause) {
      if (!isCompletionFailure(cause)) throw cause;
      return fail(
        `Timeout does not complete even at c = max_timeout_fee_lovelace ${cap}: ${(cause as Error).message}`,
        "timeout-challenger-fee-cap",
      );
    }
    const referenceScriptBytes = [
      ...capped.referenceOutRefs,
      ...capped.spentOutRefs,
    ].reduce(
      (total, u) =>
        total + (u.scriptRef ? BigInt(u.scriptRef.script.length / 2) : 0n),
      0n,
    );
    const measured = daAvailabilityLedgerMinFee({
      unsignedCbor: capped.unsignedCbor,
      protocolParameters: protocolParameters(lucid),
      referenceScriptBytes,
      vkeyWitnessCount: vkeyWitnessCount(capped),
    });
    const c = daAvailabilityTimeoutChallengerFee({
      feePartLovelace: feePart,
      requiredFeeLovelace: measured,
      parameters: d.parameters,
    });
    if (c >= cap) return capped;
    try {
      return await attempt(c);
    } catch (cause) {
      if (!isCompletionFailure(cause)) throw cause;
      return capped;
    }
  });
export const buildPruneDaUnavailableBlockDescendantTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: DaAvailabilityRemovalParams,
) =>
  effect(() => {
    if (!p.descendant) fail("Pruning requires an immediate descendant");
    return removal(lucid, d, p);
  });
export const buildRemoveDaUnavailableHeadTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: DaAvailabilityRemovalParams,
) =>
  effect(() => {
    if (p.descendant) fail("Head removal cannot include a descendant");
    return removal(lucid, d, p);
  });
export const assertDaAvailabilityReferenceScript = role;

export type RecoverDaAvailabilityCommitmentParams = {
  readonly applyTxHash: string;
  /** The DA attestation policy whose DAAT token the Apply transaction burns. */
  readonly daAttestationPolicyId: string;
  /**
   * A transaction's CBOR by id, or `undefined` when unknown. Called for the
   * Apply transaction and for the transactions that produced its inputs. Lucid
   * providers expose no transaction fetch, so the caller wires one (Blockfrost
   * `/txs/{hash}/cbor`, an Ogmios/chain-sync archive, or the CBOR it submitted
   * itself).
   */
  readonly fetchTransactionCbor: (
    txHash: string,
  ) => Promise<string | undefined>;
  /** When given, the recovered commitment must hash to it. */
  readonly expectedCommitmentHash?: string;
};
export type RecoveredDaAvailabilityCommitment = {
  readonly commitment: Availability.DaAvailabilityCommitment;
  readonly commitmentHash: string;
  readonly headerHash: string;
  /** The spent DAAT UTxO the commitment was read from. */
  readonly attestationOutRef: {
    readonly txHash: string;
    readonly outputIndex: number;
  };
  /**
   * `inline-datum`: the DAAT output's inline datum in its producing
   * transaction (the normal case: attestations carry inline datums).
   * `witness-datum`: a hashed DAAT datum resolved from the Apply transaction's
   * witness datums. `provider-datum`: a hashed DAAT datum the provider's datum
   * table resolves.
   */
  readonly source: "inline-datum" | "witness-datum" | "provider-datum";
};

const plutusDataHash = (cbor: string) => {
  const data = CML.PlutusData.from_cbor_hex(cbor);
  try {
    return CML.hash_plutus_data(data).to_hex();
  } finally {
    data.free();
  }
};

const cborTransaction = async (
  fetch: RecoverDaAvailabilityCommitmentParams["fetchTransactionCbor"],
  txHash: string,
) => {
  const cbor = await fetch(txHash);
  if (cbor === undefined) return undefined;
  const tx = CML.Transaction.from_cbor_hex(cbor);
  if (CML.hash_transaction(tx.body()).to_hex() !== txHash) {
    tx.free();
    return fail(
      `Fetched transaction does not hash to ${txHash}`,
      "apply-commitment-unrecoverable",
    );
  }
  return tx;
};

/**
 * Recovers the full attested commitment an Open needs from the Apply
 * transaction that set the node's `Attested{commitment_hash}`: finds the
 * input holding the DAAT token that Apply burns, reads that output from its
 * producing transaction, and parses `DaAttestationDatum.availability_commitment`.
 * A hashed datum falls back to the Apply transaction's witness datums, then
 * to the provider's datum lookup.
 *
 * The spent DAAT UTxO is gone from every UTxO view once Apply lands, so the
 * producing transaction is the primary source. In the Lucid emulator (which
 * deletes spent UTxOs at each block and keeps no transaction bodies) only this
 * path works, and only when `fetchTransactionCbor` serves the CBOR the harness
 * submitted; its datum table holds hashed datums alone, so the provider
 * fallback never sees an inline DAAT datum.
 */
export const recoverDaAvailabilityCommitmentFromApplyTx = async (
  lucid: Pick<LucidEvolution, "config">,
  params: RecoverDaAvailabilityCommitmentParams,
): Promise<RecoveredDaAvailabilityCommitment> => {
  const apply =
    (await cborTransaction(params.fetchTransactionCbor, params.applyTxHash)) ??
    fail(
      `Apply transaction ${params.applyTxHash} is unknown`,
      "apply-commitment-unrecoverable",
    );
  try {
    const body = apply.body();
    const burned: string[] = [];
    const policyTokens = body
      .mint()
      ?.get_assets(CML.ScriptHash.from_hex(params.daAttestationPolicyId));
    const names = policyTokens?.keys();
    for (let i = 0; i < (names?.len() ?? 0); i++) {
      const name = names!.get(i);
      const hex = Buffer.from(name.to_raw_bytes()).toString("hex");
      if (
        hex.startsWith(DA_ATTESTATION_ASSET_NAME_PREFIX) &&
        policyTokens!.get(name) === -1n
      )
        burned.push(hex);
    }
    if (burned.length !== 1)
      fail(
        "Apply transaction must burn exactly one DAAT token",
        "apply-commitment-unrecoverable",
      );
    const unit = params.daAttestationPolicyId + burned[0]!;
    const headerHash = burned[0]!.slice(
      DA_ATTESTATION_ASSET_NAME_PREFIX.length,
    );
    const inputs = body.inputs();
    for (let i = 0; i < inputs.len(); i++) {
      const input = inputs.get(i);
      const outRef = {
        txHash: input.transaction_id().to_hex(),
        outputIndex: Number(input.index()),
      };
      const producer = await cborTransaction(
        params.fetchTransactionCbor,
        outRef.txHash,
      );
      if (producer === undefined) continue;
      let output: ReturnType<typeof coreToTxOutput> | undefined;
      try {
        const outputs = producer.body().outputs();
        if (outRef.outputIndex < outputs.len())
          output = coreToTxOutput(outputs.get(outRef.outputIndex));
      } finally {
        producer.free();
      }
      if (output === undefined || output.assets[unit] !== 1n) continue;
      let datumCbor = output.datum ?? undefined;
      let source: RecoveredDaAvailabilityCommitment["source"] = "inline-datum";
      if (datumCbor == null && output.datumHash != null) {
        const witnessDatums = apply.witness_set().plutus_datums();
        for (let j = 0; j < (witnessDatums?.len() ?? 0); j++) {
          const candidate = witnessDatums!.get(j);
          if (CML.hash_plutus_data(candidate).to_hex() === output.datumHash) {
            datumCbor = candidate.to_cbor_hex();
            source = "witness-datum";
          }
        }
      }
      if (datumCbor == null && output.datumHash != null) {
        const provider = lucid.config().provider;
        const resolved = provider
          ? await provider.getDatum(output.datumHash).catch(() => undefined)
          : undefined;
        if (
          resolved !== undefined &&
          plutusDataHash(resolved) === output.datumHash
        ) {
          datumCbor = resolved;
          source = "provider-datum";
        }
      }
      if (datumCbor == null)
        return fail(
          "Spent DAAT output carries no resolvable datum",
          "apply-commitment-unrecoverable",
        );
      const attestation = Data.from(datumCbor, DaAttestationDatum);
      const commitment = attestation.availability_commitment;
      Availability.assertCanonicalDaAvailabilityCommitment(commitment);
      if (
        attestation.header_hash !== headerHash ||
        commitment.header_hash !== headerHash
      )
        fail(
          "DAAT datum does not name the burned token's block",
          "apply-commitment-unrecoverable",
        );
      const commitmentHash =
        Availability.daAvailabilityCommitmentHash(commitment);
      if (
        params.expectedCommitmentHash !== undefined &&
        commitmentHash !== params.expectedCommitmentHash
      )
        fail(
          `Recovered commitment hash ${commitmentHash} does not match ${params.expectedCommitmentHash}`,
          "commitment-hash-mismatch",
        );
      return {
        commitment,
        commitmentHash,
        headerHash,
        attestationOutRef: outRef,
        source,
      };
    }
    return fail(
      "No producing transaction of the Apply inputs yields the DAAT output",
      "apply-commitment-unrecoverable",
    );
  } finally {
    apply.free();
  }
};

export type DaAvailabilitySnapshotUtxos = {
  readonly availabilityUtxos: readonly UTxO[];
  readonly stateQueueUtxos: readonly UTxO[];
  readonly correctionLockUtxos: readonly UTxO[];
  /** UTxOs at the DA bond pool address; the pool is omitted when absent. */
  readonly poolUtxos?: readonly UTxO[];
  readonly carrierUtxos?: readonly UTxO[];
};
export const daAvailabilityChallengeSnapshotFromUtxos = async (
  d: DaAvailabilityDeployment,
  headerHash: string,
  utxos: DaAvailabilitySnapshotUtxos,
): Promise<DaAvailabilityChallengeSnapshot> => {
  const policy = d.contracts.availabilityChallenge.policyId;
  const byUnit = (
    list: readonly UTxO[],
    unit: string,
    required = false,
  ): UTxO | undefined => {
    const matches = list.filter((u) => (u.assets[unit] ?? 0n) !== 0n);
    if (
      matches.length > 1 ||
      (required && matches.length !== 1) ||
      matches.some((u) => u.assets[unit] !== 1n)
    )
      fail(`Nonunique authenticated unit ${unit}`);
    return matches[0];
  };
  const queueU = byUnit(
    utxos.stateQueueUtxos,
    d.contracts.stateQueue.policyId +
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
      headerHash,
  );
  const root = await state(
    byUnit(
      utxos.stateQueueUtxos,
      d.contracts.stateQueue.policyId + STATE_QUEUE_ROOT_ASSET_NAME,
      true,
    )!,
    d,
  );
  const queue = queueU ? await state(queueU, d) : undefined;
  const correctionLock = byUnit(
    utxos.correctionLockUtxos,
    correctionLockUnit(d.hubOraclePolicyId),
    true,
  )!;
  auth(correctionLock, d.contracts.correctionLock.spendingScriptAddress, [
    correctionLockUnit(d.hubOraclePolicyId),
  ]);
  Data.from(datum(correctionLock), CorrectionLockDatum);
  const pool =
    utxos.poolUtxos === undefined
      ? undefined
      : byUnit(
          utxos.poolUtxos,
          daBondPoolUnit(d.contracts.daBondPool.policyId),
        );
  const poolDatum = pool ? authenticPool(d, pool) : undefined;
  let descendant: StateQueueUTxO | undefined;
  if (queue && queue.datum.next !== "Empty") {
    const u = byUnit(
      utxos.stateQueueUtxos,
      d.contracts.stateQueue.policyId +
        STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
        queue.datum.next.Key.key,
      true,
    )!;
    descendant = await state(u, d);
  }
  let record: UTxO | undefined,
    r: Availability.DaAvailabilityChallengeRecord | undefined,
    terminal: UTxO | undefined,
    td: Availability.DaAvailabilityTerminalAccumulatorDatum | undefined;
  const tranches: DaAvailabilityChallengeSnapshot["tranches"][number][] = [];
  if (queue) {
    const node = Data.castFrom(queue.datum.data, StateQueueNode),
      status = node.da_attestation;
    // Only a Challenged node has a record; an Attested one has nothing to read.
    if (typeof status === "object" && "Challenged" in status) {
      const name = status.Challenged.challenge_asset_name;
      record = byUnit(utxos.availabilityUtxos, policy + name);
      // A locked removal legitimately outlives the burned record.
      if (!record) {
        const lock = Data.from(datum(correctionLock), CorrectionLockDatum);
        if (
          typeof lock !== "object" ||
          lock.Locked.target_header_hash !== headerHash ||
          typeof lock.Locked.correction_identity !== "object" ||
          !("AvailabilityChallenge" in lock.Locked.correction_identity) ||
          lock.Locked.correction_identity.AvailabilityChallenge
            .challenge_asset_name !== name
        )
          fail("Authenticated challenge record is missing");
      } else {
        terminal = byUnit(
          utxos.availabilityUtxos,
          policy +
            Availability.daAvailabilityTerminalAccumulatorAssetName(name),
          true,
        )!;
        r = challenged(d, record, terminal);
        if (r.commitment.header_hash !== headerHash)
          fail("Challenge record deployment/header mismatch");
        assertChallengedNode(queue, r);
        td = terminalDatum(terminal);
        if (
          td.next_tranche_index >
          BigInt(r.commitment.tranche_descriptors.length)
        )
          fail("Terminal tranche cursor exceeds commitment");
        for (
          let i = Number(td.next_tranche_index);
          i < r.commitment.tranche_descriptors.length;
          i++
        ) {
          const u = byUnit(
            utxos.availabilityUtxos,
            policy +
              Availability.daAvailabilityTrancheAssetName({
                challengeAssetName: name,
                trancheIndex: i,
              }),
            true,
          )!;
          const t = trancheDatum(u),
            tv = "Active" in t ? t.Active : t.Receipt;
          if (
            tv.header_hash !== headerHash ||
            tv.deployment_identity !== d.hubOraclePolicyId ||
            tv.descriptor.tranche_index !== BigInt(i) ||
            tv.challenger !== r.challenger ||
            ("Active" in t &&
              t.Active.response_deadline !== r.response_deadline) ||
            Data.to(
              tv.descriptor,
              Availability.DaAvailabilityTrancheDescriptor,
            ) !==
              Data.to(
                r.commitment.tranche_descriptors[i]!,
                Availability.DaAvailabilityTrancheDescriptor,
              )
          )
            fail("Tranche commitment mismatch");
          const index =
            "Active" in t
              ? t.Active.latest_carrier_output_index
              : t.Receipt.terminal_carrier_output_index;
          const carrier =
            index === null
              ? undefined
              : [
                  ...utxos.availabilityUtxos,
                  ...(utxos.carrierUtxos ?? []),
                ].find(
                  (c) =>
                    c.txHash === u.txHash && BigInt(c.outputIndex) === index,
                );
          authenticateCarrier(d, u, t, carrier);
          tranches.push({
            utxo: u,
            datum: t,
            ...(carrier ? { carrier } : {}),
          });
        }
      }
    }
  }
  return {
    headerHash,
    confirmedState: root,
    correctionLock,
    tranches,
    ...(queue ? { queue } : {}),
    ...(descendant ? { descendant } : {}),
    ...(record ? { record } : {}),
    ...(r ? { recordDatum: r } : {}),
    ...(pool ? { pool } : {}),
    ...(poolDatum ? { poolDatum } : {}),
    ...(terminal ? { terminal } : {}),
    ...(td ? { terminalDatum: td } : {}),
  };
};
export const fetchDaAvailabilityChallengeSnapshot = async (
  lucid: Pick<LucidEvolution, "utxosAt">,
  d: DaAvailabilityDeployment,
  headerHash: string,
): Promise<DaAvailabilityChallengeSnapshot> => {
  const [availabilityUtxos, stateQueueUtxos, correctionLockUtxos, poolUtxos] =
    await Promise.all([
      lucid.utxosAt(d.contracts.availabilityChallenge.spendingScriptAddress),
      lucid.utxosAt(d.contracts.stateQueue.spendingScriptAddress),
      lucid.utxosAt(d.contracts.correctionLock.spendingScriptAddress),
      lucid.utxosAt(d.contracts.daBondPool.spendingScriptAddress),
    ]);
  return daAvailabilityChallengeSnapshotFromUtxos(d, headerHash, {
    availabilityUtxos,
    stateQueueUtxos,
    correctionLockUtxos,
    poolUtxos,
  });
};
export const fetchDaAvailabilityChallengeSnapshotProgram = (
  lucid: Pick<LucidEvolution, "utxosAt">,
  d: DaAvailabilityDeployment,
  headerHash: string,
) => effect(() => fetchDaAvailabilityChallengeSnapshot(lucid, d, headerHash));
