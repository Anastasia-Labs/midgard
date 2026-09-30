import {
  type BuildTxWithRedeemer,
  calculateMinLovelaceFromUTxO,
  Data,
  type LucidEvolution,
  type ProtocolParameters,
  type RedeemerContext,
  type Script,
  type TxBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import * as Availability from "./availability-challenge.js";
import {
  at,
  type DaAvailabilityDeployment,
  type DaAvailabilityExpectedOutput,
  fail,
  inline,
} from "./availability-challenge-transactions.at.js";
import { scriptRewardAddress } from "./cardano-addresses.js";
import { planDaBondPoolSlash } from "./da-bond-pool.js";
import { HUB_ORACLE_ASSET_NAME } from "./hub-oracle.js";
import { StateQueueNode } from "./ledger-state.js";
import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "./linked-list.js";
import { referenceScriptAuthUnit } from "./reference-scripts.js";
import { StateQueueSpendRedeemer } from "./state-queue.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
} from "./tx-context-redeemer.js";

export const mint =
  (
    policy: string,
    build: (ctx: RedeemerContext) => Availability.DaAvailabilityMintRedeemer,
  ): BuildTxWithRedeemer =>
  (ctx) => {
    requireOwnMintPurpose(ctx, policy, "availability");
    return Data.to(build(ctx), Availability.DaAvailabilityMintRedeemer);
  };

export const queueUpdate =
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

export const role = (
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

export const baseRefs = (d: DaAvailabilityDeployment) => [
  role(
    d,
    "availability-challenge spending",
    d.contracts.availabilityChallenge.spendingScript,
  ),
];

export const mintRefs = (
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

export const hub = (d: DaAvailabilityDeployment) => {
  if (
    d.hubOracleRefInput.assets[d.hubOraclePolicyId + HUB_ORACLE_ASSET_NAME] !==
    1n
  )
    fail("Unauthentic hub oracle");
  return d.hubOracleRefInput;
};

export const withYield = (
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

export const pay = (
  tx: TxBuilder,
  outputs: readonly DaAvailabilityExpectedOutput[],
) =>
  outputs.reduce(
    (next, o) =>
      o.datum === undefined
        ? next.pay.ToAddress(o.address, o.assets)
        : next.pay.ToContract(o.address, inline(o.datum), o.assets),
    tx,
  );

export const protocolParameters = (lucid: LucidEvolution): ProtocolParameters =>
  lucid.config().protocolParameters ?? fail("Missing live protocol parameters");

export const minAda = (
  lucid: LucidEvolution,
  o: DaAvailabilityExpectedOutput,
) =>
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
export const MAX_COLLATERAL_INPUTS = 3;
