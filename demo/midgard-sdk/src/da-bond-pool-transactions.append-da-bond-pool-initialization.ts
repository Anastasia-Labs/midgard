import {
  type BuildTxWithRedeemer,
  Data,
  getAddressDetails,
  type LucidEvolution,
  type RedeemerContext,
  type TxBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import type { DaAvailabilityParameters } from "./availability-challenge.js";
import type { AuthenticatedValidator } from "./common.js";
import {
  type DaBondPoolDatum,
  DaBondPoolMintRedeemer,
  daBondPoolUnit,
  encodeDaBondPoolDatum,
} from "./da-bond-pool.js";
import type { GenericErrorFields } from "./errors.js";
import { requireUniqueOutputIndex } from "./tx-context-redeemer.js";

/**
 * Transaction builders for the pooled DA committee bond (Aiken
 * `validators/da-bond-pool.ak`): the one-shot `InitPool` mint, the open
 * `TopUp`, and the owner-quorum `BeginWithdraw` / `CancelWithdraw` /
 * `CompleteWithdraw` steps. Every builder returns an incomplete `TxBuilder`;
 * the caller funds, completes, signs and submits it. Without reference-script
 * UTxOs a builder attaches the pool script inline.
 *
 * Timing is never a constant here: the withdraw delay a `BeginWithdraw`
 * writes is the deployment profile's `timing.da_bond_withdraw_delay_ms`, which
 * the validator compiles in, and callers pass it.
 *
 * Redeemer indices are positional and read off the final transaction:
 * `da_params_ref_input_index` counts the sorted reference inputs and
 * `output_index` is the continuing pool output's position.
 */

/** Why a pool build refused, so a caller (and a test) can tell which. */
export type DaBondPoolBuildFailureReason =
  | "amount_exceeds_backing"
  | "before_unlock"
  | "below_floor"
  | "below_min_top_up"
  | "duplicate_signer"
  | "insufficient_signers"
  | "invalid_amount"
  | "invalid_da_params"
  | "invalid_pool"
  | "invalid_pool_address"
  | "invalid_validity_range"
  | "invalid_witness"
  | "missing_witness"
  | "pool_not_bonded"
  | "pool_not_withdrawing"
  | "reference_script_mismatch"
  | "signer_not_owner";

export class DaBondPoolBuildError extends EffectData.TaggedError(
  "DaBondPoolBuildError",
)<GenericErrorFields & { readonly reason: DaBondPoolBuildFailureReason }> {}

/**
 * Published pool scripts. The pool is one multivalidator, so both roles carry
 * the same script.
 */
export type DaBondPoolReferenceScripts = {
  readonly daBondPoolMinting?: UTxO;
  readonly daBondPoolSpending?: UTxO;
};

/** The pool UTxO to spend, as `fetchDaBondPool` returns it. */
export type DaBondPoolSpendInput = {
  readonly utxo: UTxO;
};

/** Validity bounds, in POSIX milliseconds, as passed to Lucid. */
export type DaBondPoolValidity = {
  readonly validFrom: bigint;
  readonly validTo: bigint;
};

export const refusal = (
  reason: DaBondPoolBuildFailureReason,
  message: string,
  cause: unknown = undefined,
): DaBondPoolBuildError => new DaBondPoolBuildError({ reason, message, cause });

export const fail = (
  reason: DaBondPoolBuildFailureReason,
  message: string,
  cause: unknown = undefined,
): Effect.Effect<never, DaBondPoolBuildError> =>
  Effect.fail(refusal(reason, message, cause));

/** Runs a synchronous check that throws `DaBondPoolBuildError`. */
export const check = <A>(
  run: () => A,
  otherwise: DaBondPoolBuildFailureReason,
): Effect.Effect<A, DaBondPoolBuildError> =>
  Effect.try({
    try: run,
    catch: (error) =>
      error instanceof DaBondPoolBuildError
        ? error
        : refusal(
            otherwise,
            error instanceof Error ? error.message : String(error),
            error,
          ),
  });

export const outRefLabel = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/**
 * The pool lives at `Address { Script(own policy), None }` (InitPool pins the
 * stake part to None), so a validator object whose spending address has any
 * other shape does not describe the pool.
 */
const requirePoolAddress = (poolValidator: AuthenticatedValidator): string => {
  const address = poolValidator.spendingScriptAddress;
  let details: ReturnType<typeof getAddressDetails>;
  try {
    details = getAddressDetails(address);
  } catch (error) {
    throw refusal(
      "invalid_pool_address",
      "DA bond pool spending address does not decode",
      error,
    );
  }
  if (
    details.paymentCredential?.type !== "Script" ||
    details.paymentCredential.hash !== poolValidator.policyId ||
    poolValidator.spendingScriptHash !== poolValidator.policyId ||
    details.stakeCredential !== undefined
  ) {
    throw refusal(
      "invalid_pool_address",
      "DA bond pool address must be Script(pool policy) with no stake credential",
      address,
    );
  }
  return address;
};

export const requireReferenceScript = (
  reference: UTxO | undefined,
  scriptHash: string,
  label: string,
): UTxO | undefined => {
  if (reference === undefined) {
    return undefined;
  }
  if (
    reference.scriptRef == null ||
    validatorToScriptHash(reference.scriptRef) !== scriptHash
  ) {
    throw refusal(
      "reference_script_mismatch",
      `${label} reference script does not carry the DA bond pool script`,
      outRefLabel(reference),
    );
  }
  return reference;
};

/** The one output at the pool address holding the pool NFT. */
export const poolOutputIndex = (
  ctx: RedeemerContext,
  poolAddress: string,
  unit: string,
  label: string,
): bigint =>
  requireUniqueOutputIndex(
    ctx.outputs,
    (output) =>
      output.address === poolAddress && (output.assets[unit] ?? 0n) === 1n,
    label,
  );

/**
 * Appends the pool's one-shot `InitPool` to `tx`: mints the one pool NFT and
 * pays it, with `lovelace` and an inline `Bonded` datum, to
 * `Script(pool policy)`. The caller spends the validator's `init_ref` in the
 * same transaction (the atomic protocol init already spends the hub one-shot,
 * which is the pool's `init_ref`).
 *
 * The redeemer's `output_index` is the pool output's position in the final
 * transaction. When `outputIndex` is given, the build also refuses unless the
 * pool lands exactly there.
 *
 * Throws `DaBondPoolBuildError`: this fragment composes into other builders,
 * like `appendEventHistoryInitialization`.
 */
export const appendDaBondPoolInitialization = (
  tx: TxBuilder,
  params: {
    readonly poolValidator: AuthenticatedValidator;
    /** `ParametersV1.da_bond_pool_floor_lovelace`. */
    readonly floorLovelace: bigint;
    readonly lovelace: bigint;
    readonly outputIndex?: bigint;
    readonly referenceScript?: UTxO;
  },
): TxBuilder => {
  const { poolValidator } = params;
  const poolAddress = requirePoolAddress(poolValidator);
  if (params.lovelace < params.floorLovelace) {
    throw refusal(
      "below_floor",
      "DA bond pool initial lovelace is below the pool floor",
      `lovelace=${params.lovelace.toString()},floor=${params.floorLovelace.toString()}`,
    );
  }
  if (params.outputIndex !== undefined && params.outputIndex < 0n) {
    throw refusal(
      "invalid_pool",
      "DA bond pool output index must be non-negative",
      params.outputIndex.toString(),
    );
  }
  const reference = requireReferenceScript(
    params.referenceScript,
    poolValidator.policyId,
    "DA bond pool minting",
  );
  const unit = daBondPoolUnit(poolValidator.policyId);
  const initRedeemer = ((ctx) => {
    const outputIndex = poolOutputIndex(
      ctx,
      poolAddress,
      unit,
      "DA bond pool init",
    );
    if (
      params.outputIndex !== undefined &&
      outputIndex !== params.outputIndex
    ) {
      throw new Error(
        `DA bond pool init output landed at ${outputIndex.toString()}, expected ${params.outputIndex.toString()}`,
      );
    }
    return Data.to(
      { output_index: outputIndex } satisfies DaBondPoolMintRedeemer as never,
      DaBondPoolMintRedeemer as never,
    );
  }) satisfies BuildTxWithRedeemer;

  tx.mintAssets({ [unit]: 1n }, initRedeemer).pay.ToContract(
    poolAddress,
    { kind: "inline", value: encodeDaBondPoolDatum("Bonded") },
    { lovelace: params.lovelace, [unit]: 1n },
  );
  if (reference === undefined) {
    tx.attach.Script(poolValidator.mintingScript);
  } else {
    tx.readFrom([reference]);
  }
  return tx;
};

/**
 * A standalone `InitPool`: spends `initUtxo` (the validator's `init_ref`) and
 * mints the pool with `lovelace >= da_bond_pool_floor_lovelace`.
 */
export const buildInitDaBondPoolTxProgram = (
  lucid: LucidEvolution,
  config: {
    readonly poolValidator: AuthenticatedValidator;
    readonly parameters: DaAvailabilityParameters;
    readonly initUtxo: UTxO;
    readonly lovelace: bigint;
    readonly outputIndex?: bigint;
    readonly referenceScripts?: Pick<
      DaBondPoolReferenceScripts,
      "daBondPoolMinting"
    >;
  },
): Effect.Effect<TxBuilder, DaBondPoolBuildError> =>
  check(
    () =>
      appendDaBondPoolInitialization(
        lucid.newTx().collectFrom([config.initUtxo]),
        {
          poolValidator: config.poolValidator,
          floorLovelace: config.parameters.da_bond_pool_floor_lovelace,
          lovelace: config.lovelace,
          outputIndex: config.outputIndex,
          referenceScript: config.referenceScripts?.daBondPoolMinting,
        },
      ),
    "invalid_pool",
  );

export type ResolvedPool = {
  readonly utxo: UTxO;
  readonly datum: DaBondPoolDatum;
  readonly datumCbor: string;
  readonly lovelace: bigint;
  readonly address: string;
  readonly unit: string;
};
