import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  Data,
  fromText,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  assertCanonicalDaAvailabilityParameters,
  DaAvailabilityCommitmentError,
  type DaAvailabilityParameters,
} from "./availability-challenge.js";

/**
 * Codecs and arithmetic for the pooled DA committee bond: one UTxO at
 * `Script(pool policy)` holding the committee's bond, identified by a one-shot
 * NFT that is never burned (Aiken `lib/midgard/da-bond-pool.ak`). Timing is
 * never a constant here: callers pass the deployment profile's values
 * (`@al-ft/midgard-core/deployment-profile`), which the validator compiles in.
 */

/** Twin of Aiken `da_bond_pool_asset_name`. */
export const DA_BOND_POOL_ASSET_NAME = fromText("MIDGARD_DA_BOND_POOL");

/**
 * `Bonded` backs attestations. `Withdrawing` refuses new attestations until the
 * quorum cancels the withdrawal or completes it at or after `unlock_at`.
 */
export const DaBondPoolDatumSchema = Data.Enum([
  Data.Literal("Bonded"),
  Data.Object({
    Withdrawing: Data.Object({ unlock_at: Data.Integer() }),
  }),
]);
export type DaBondPoolDatum = Data.Static<typeof DaBondPoolDatumSchema>;
export const DaBondPoolDatum = asDataType<DaBondPoolDatum>(
  DaBondPoolDatumSchema,
);

/**
 * Minting-policy ABI (Aiken `MintRedeemer`, the one constructor
 * `InitPool { output_index }`): the one-shot pool NFT mint. A one-constructor
 * Aiken type encodes as `Constr 0 [output_index]`, which is exactly a plain
 * object here. It is not written as a one-arm `Data.Enum`: Lucid collapses a
 * single-variant enum to its fields, so an `{ InitPool: ... }` value would not
 * encode at all.
 */
export const DaBondPoolMintRedeemerSchema = Data.Object({
  output_index: Data.Integer(),
});
export type DaBondPoolMintRedeemer = Data.Static<
  typeof DaBondPoolMintRedeemerSchema
>;
export const DaBondPoolMintRedeemer = asDataType<DaBondPoolMintRedeemer>(
  DaBondPoolMintRedeemerSchema,
);

/**
 * Spending-validator ABI (Aiken `SpendRedeemer`), in constructor order. Every
 * arm continues the pool at `output_index`.
 */
export const DaBondPoolSpendRedeemerSchema = Data.Enum([
  Data.Object({
    TopUp: Data.Object({ output_index: Data.Integer() }),
  }),
  Data.Object({
    Slash: Data.Object({
      hub_oracle_ref_input_index: Data.Integer(),
      state_queue_mint_redeemer_index: Data.Integer(),
      correction_lock_input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    BeginWithdraw: Data.Object({
      da_params_ref_input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    CancelWithdraw: Data.Object({
      da_params_ref_input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    CompleteWithdraw: Data.Object({
      amount: Data.Integer(),
      da_params_ref_input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
]);
export type DaBondPoolSpendRedeemer = Data.Static<
  typeof DaBondPoolSpendRedeemerSchema
>;
export const DaBondPoolSpendRedeemer = asDataType<DaBondPoolSpendRedeemer>(
  DaBondPoolSpendRedeemerSchema,
);

const CANONICAL_CBOR_HEX = /^(?:[0-9a-f]{2})+$/u;

export const assertCanonicalDaBondPoolDatum = (
  datum: DaBondPoolDatum,
): void => {
  if (datum === "Bonded") return;
  if (
    typeof datum !== "object" ||
    datum === null ||
    !("Withdrawing" in datum) ||
    typeof datum.Withdrawing.unlock_at !== "bigint" ||
    datum.Withdrawing.unlock_at < 0n
  ) {
    throw new DaAvailabilityCommitmentError(
      "DA bond pool datum must be Bonded or Withdrawing with a non-negative unlock_at",
    );
  }
};

export const encodeDaBondPoolDatum = (datum: DaBondPoolDatum): string => {
  assertCanonicalDaBondPoolDatum(datum);
  return Data.to(datum as never, DaBondPoolDatumSchema as never);
};

/** Strict pool-datum codec: canonical CBOR and a canonical datum. */
export const parseDaBondPoolDatumCbor = (cborHex: string): DaBondPoolDatum => {
  if (!CANONICAL_CBOR_HEX.test(cborHex)) {
    throw new DaAvailabilityCommitmentError(
      "DA bond pool datum must be non-empty lowercase CBOR hex",
    );
  }
  let datum: DaBondPoolDatum;
  try {
    datum = Data.from(
      cborHex,
      DaBondPoolDatumSchema as never,
    ) as DaBondPoolDatum;
  } catch (error) {
    throw new DaAvailabilityCommitmentError(
      `DA bond pool datum is not valid Plutus Data: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  if (Data.to(datum as never, DaBondPoolDatumSchema as never) !== cborHex) {
    throw new DaAvailabilityCommitmentError(
      "DA bond pool datum must use the canonical Plutus Data encoding",
    );
  }
  assertCanonicalDaBondPoolDatum(datum);
  return datum;
};

/** The pool NFT's unit under the pool policy. */
export const daBondPoolUnit = (policyId: string): string =>
  toUnit(policyId, DA_BOND_POOL_ASSET_NAME);

/**
 * Lovelace above the pool floor (twin of Aiken `lovelace_backing`). The floor
 * keeps the pool UTxO above its min-UTxO and never counts as backing, so a
 * pool at or below it backs 0.
 */
export const daBondPoolBacking = (input: {
  readonly lovelace: bigint;
  readonly parameters: DaAvailabilityParameters;
}): bigint => {
  const backing = input.lovelace - input.parameters.da_bond_pool_floor_lovelace;
  return backing > 0n ? backing : 0n;
};

/**
 * The `unlock_at` a `BeginWithdraw` transaction must write. The validator
 * anchors it at the transaction's inclusive upper validity bound, as
 * `OpenChallenge` anchors `opened_at`. The ledger's `validTo` is exclusive and
 * Aiken's short-range normaliser resolves an exclusive upper bound to
 * `upper - 1`, so the inclusive upper is `validToMs - 1`. `withdrawDelayMs` is
 * the deployment profile's `timing.da_bond_withdraw_delay_ms`.
 */
export const daBondPoolUnlockAt = (input: {
  readonly validToMs: bigint;
  readonly withdrawDelayMs: bigint;
}): bigint => input.validToMs - 1n + input.withdrawDelayMs;

type DaBondPoolSlashPlan = Readonly<{
  /** `max(0, poolLovelace - floor)`. */
  backing: bigint;
  /** `min(da_bond, backing)`: exactly what leaves the pool. */
  taken: bigint;
  /** `min(penalty, taken)`: burned as transaction fee. */
  feePart: bigint;
  /** `taken - feePart`: paid to the challenger. */
  payout: bigint;
  /** The continuing pool output's lovelace, `poolLovelace - taken`. */
  poolOutputLovelace: bigint;
}>;

/**
 * Timeout slash arithmetic (twin of the `TimeoutChallenge` pool leg): the pool
 * gives up `min(da_bond, backing)`, the penalty is taken from that first as
 * fee, and the rest is the challenger's payout, merged into the one
 * challenger refund output.
 */
export const planDaBondPoolSlash = (input: {
  readonly poolLovelace: bigint;
  readonly parameters: DaAvailabilityParameters;
}): DaBondPoolSlashPlan => {
  assertCanonicalDaAvailabilityParameters(input.parameters);
  if (input.poolLovelace < 0n) {
    throw new DaAvailabilityCommitmentError(
      "DA bond pool lovelace must be non-negative",
    );
  }
  const backing = daBondPoolBacking({
    lovelace: input.poolLovelace,
    parameters: input.parameters,
  });
  const daBond = input.parameters.da_bond_lovelace;
  const taken = daBond < backing ? daBond : backing;
  const penalty = input.parameters.da_slash_penalty_lovelace;
  const feePart = penalty < taken ? penalty : taken;
  return {
    backing,
    taken,
    feePart,
    payout: taken - feePart,
    poolOutputLovelace: input.poolLovelace - taken,
  };
};

type DaBondPoolView = Readonly<{
  utxo: UTxO;
  datum: DaBondPoolDatum;
  /** Present when `parameters` was given. */
  backing?: bigint;
}>;

/**
 * Fetches the one pool UTxO: the output at `address` holding the pool NFT
 * exactly once with an inline, canonical pool datum. Anything else (no pool,
 * several candidates, a datum hash) fails closed.
 */
export const fetchDaBondPool = async (
  lucid: LucidEvolution,
  input: {
    readonly policyId: string;
    readonly address: string;
    readonly parameters?: DaAvailabilityParameters;
  },
): Promise<DaBondPoolView> => {
  const unit = daBondPoolUnit(input.policyId);
  const candidates = await lucid.utxosAtWithUnit(input.address, unit);
  if (candidates.length !== 1) {
    throw new DaAvailabilityCommitmentError(
      `expected exactly one DA bond pool UTxO at the pool address, found ${candidates.length.toString()}`,
    );
  }
  const utxo = candidates[0]!;
  if (utxo.address !== input.address || utxo.assets[unit] !== 1n) {
    throw new DaAvailabilityCommitmentError(
      "DA bond pool UTxO must sit at the pool address and hold the pool NFT exactly once",
    );
  }
  if (typeof utxo.datum !== "string" || utxo.datumHash != null) {
    throw new DaAvailabilityCommitmentError(
      "DA bond pool UTxO must carry an inline datum",
    );
  }
  const datum = parseDaBondPoolDatumCbor(utxo.datum);
  if (input.parameters === undefined) {
    return { utxo, datum };
  }
  return {
    utxo,
    datum,
    backing: daBondPoolBacking({
      lovelace: utxo.assets.lovelace ?? 0n,
      parameters: input.parameters,
    }),
  };
};
