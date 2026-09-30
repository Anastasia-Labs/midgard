import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  getAddressDetails,
  type LucidEvolution,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";

import { awaitExactTransactionConfirmation } from "../transactions/utils.js";

/** Everything the chain-facing `da-bond` commands read or call. */
export type DaBondContext = Readonly<{
  lucid: LucidEvolution;
  /** The deployment's network, recorded in and checked against every file. */
  network: string;
  manifestId: string;
  poolValidator: SDK.AuthenticatedValidator;
  /** The authenticated `da-bond-pool spending` reference-script UTxO. */
  poolSpendingReference: UTxO;
  parameters: SDK.DaAvailabilityParameters;
  /** Where the DA params UTxO lives and the governor NFT that marks it. */
  daParamsGovernor: Readonly<{ address: string; unit: string }>;
  /** The deployment profile's `timing.da_bond_withdraw_delay_ms`. */
  withdrawDelayMs: bigint;
  /**
   * The ledger's current POSIX time in milliseconds: the time of the local
   * node's tip slot when the command started (the emulator's clock in tests).
   * The node checks a transaction's validity bounds against its tip, which
   * trails the wall clock, so a lower bound taken from the wall clock can be
   * refused as not yet valid.
   */
  now: () => number;
  /**
   * Submits a signed transaction, waits for it to land, returns its id. A
   * failure after the node accepted it names the transaction
   * (`daBondAfterSubmitError`).
   */
  submit: (txCbor: string) => Promise<string>;
}>;

const KEY_HASH = /^[0-9a-f]{56}$/u;

export const LOVELACE = /^[1-9][0-9]*$/u;

export const outRefLabel = (
  utxo: Pick<UTxO, "txHash" | "outputIndex">,
): string => `${utxo.txHash}#${utxo.outputIndex.toString()}`;

export const parseOutRef = (
  value: string,
): { readonly txHash: string; readonly outputIndex: number } => {
  const [txHash, outputIndex] = value.split("#");
  return { txHash: txHash!, outputIndex: Number(outputIndex) };
};

export const isoOf = (ms: bigint): string => new Date(Number(ms)).toISOString();

export const parseLovelace = (
  value: string | undefined,
  flag: string,
): bigint => {
  if (value === undefined || !LOVELACE.test(value.trim())) {
    throw new Error(`${flag} must be a positive whole number of lovelace`);
  }
  return BigInt(value.trim());
};

/** `--valid-for-ms`: a positive whole number of milliseconds. */
export const parsePositiveMs = (value: string, flag: string): bigint => {
  if (!LOVELACE.test(value.trim())) {
    throw new Error(`${flag} must be a positive whole number of milliseconds`);
  }
  return BigInt(value.trim());
};

export const requireAddress = (value: string, flag: string): string => {
  try {
    getAddressDetails(value);
  } catch {
    throw new Error(`${flag} must be a bech32 Cardano address`);
  }
  if (!value.startsWith("addr")) {
    throw new Error(`${flag} must be a bech32 Cardano address`);
  }
  return value;
};

export const paymentKeyHashOf = (address: string, flag: string): string => {
  const payment = getAddressDetails(
    requireAddress(address, flag),
  ).paymentCredential;
  if (payment?.type !== "Key") {
    throw new Error(`${flag} must have a payment key credential`);
  }
  return payment.hash;
};

export const parseSigners = (value: string): readonly string[] => {
  const signers = value
    .split(",")
    .map((signer) => signer.trim().toLowerCase())
    .filter((signer) => signer.length > 0);
  if (signers.length === 0 || signers.some((s) => !KEY_HASH.test(s))) {
    throw new Error(
      "--signers must list the signing owners' 28-byte payment key hashes, comma separated",
    );
  }
  return signers;
};

type PoolView = Readonly<{ utxo: UTxO; datum: SDK.DaBondPoolDatum }>;

export const fetchPool = async (ctx: DaBondContext): Promise<PoolView> => {
  const { utxo, datum } = await SDK.fetchDaBondPool(ctx.lucid, {
    policyId: ctx.poolValidator.policyId,
    address: ctx.poolValidator.spendingScriptAddress,
  });
  return { utxo, datum };
};

/** The status readout, with bigints as decimal strings. */
const statusJson = (ctx: DaBondContext, pool: PoolView) => {
  const status = SDK.daBondPoolStatus({
    lovelace: pool.utxo.assets.lovelace ?? 0n,
    datum: pool.datum,
    parameters: ctx.parameters,
  });
  return {
    poolOutRef: outRefLabel(pool.utxo),
    state: status.state,
    lovelace: status.lovelace.toString(),
    backing: status.backing.toString(),
    requiredBacking: status.requiredBacking.toString(),
    belowBond: status.belowBond,
    ...(status.unlockAt === undefined
      ? {}
      : {
          unlockAt: status.unlockAt.toString(),
          unlockAtIso: isoOf(status.unlockAt),
          unlockable: BigInt(ctx.now()) >= status.unlockAt,
        }),
  };
};

export type DaBondStatus = ReturnType<typeof statusJson>;

/** `da-bond status`. */
export const daBondStatusCommand = async (
  ctx: DaBondContext,
): Promise<DaBondStatus> => statusJson(ctx, await fetchPool(ctx));

/**
 * A failure after the node accepted a transaction, fatal like any other. It
 * names the transaction and what is known of it (`submitted`: its
 * confirmation was not seen; `confirmed`: it landed and only the status read
 * failed), so the operator does not run the command again: a second `top-up`
 * would pay the pool twice, and the funder cannot take lovelace back out.
 */
export const daBondAfterSubmitError = (
  txHash: string,
  state: "submitted" | "confirmed",
  cause: unknown,
): Error => {
  const reason = cause instanceof Error ? cause.message : String(cause);
  return new Error(
    state === "submitted"
      ? `Transaction ${txHash} was submitted, but waiting for its confirmation failed: ${reason}. Check whether ${txHash} landed before retrying; do not submit it again`
      : `Transaction ${txHash} is confirmed, but reading the pool status failed: ${reason}; do not submit it again`,
    { cause },
  );
};

/**
 * The production `submit`: send, then wait for exact confirmation. A failed
 * wait (a Kupo poll error aborts it) still names the submitted transaction.
 */
export const daBondSubmitAndConfirm =
  (
    send: (txCbor: string) => Promise<string>,
    confirm: (txHash: string) => Promise<unknown>,
  ) =>
  async (txCbor: string): Promise<string> => {
    const txHash = await send(txCbor);
    try {
      await confirm(txHash);
    } catch (error) {
      throw daBondAfterSubmitError(txHash, "submitted", error);
    }
    return txHash;
  };

/**
 * The `submit` `loadDaBondContext` installs: send through the chain provider,
 * then wait for `lucid`'s exact confirmation, a failed wait naming the hash.
 */
export const daBondChainSubmit = (
  provider: Readonly<{ submitTx: (txCbor: string) => Promise<string> }>,
  lucid: LucidEvolution,
): ((txCbor: string) => Promise<string>) =>
  daBondSubmitAndConfirm(
    (txCbor) => provider.submitTx(txCbor),
    (txHash) => awaitExactTransactionConfirmation(lucid, txHash),
  );

/** The status readout after `txHash` is confirmed; a failed read names it. */
export const statusAfterSubmit = (
  ctx: DaBondContext,
  txHash: string,
): Promise<DaBondStatus> =>
  daBondStatusCommand(ctx).catch((error: unknown) => {
    throw daBondAfterSubmitError(txHash, "confirmed", error);
  });

export type DaParamsView = Readonly<{ utxo: UTxO; datum: SDK.DaParamsDatum }>;

export const isDaParamsUtxo = (ctx: DaBondContext, utxo: UTxO): boolean =>
  utxo.address === ctx.daParamsGovernor.address &&
  utxo.assets[ctx.daParamsGovernor.unit] === 1n;

export const daParamsDatumOf = (utxo: UTxO): SDK.DaParamsDatum => {
  if (typeof utxo.datum !== "string" || utxo.datumHash != null) {
    throw new Error(
      `DA params UTxO ${outRefLabel(utxo)} carries no inline datum`,
    );
  }
  return Data.from(utxo.datum, SDK.DaParamsDatum);
};

/** The one live DA params UTxO: the governor NFT at the governor address. */
export const fetchDaParams = async (
  ctx: DaBondContext,
): Promise<DaParamsView> => {
  const candidates = (
    await ctx.lucid.utxosAtWithUnit(
      ctx.daParamsGovernor.address,
      ctx.daParamsGovernor.unit,
    )
  ).filter((utxo) => isDaParamsUtxo(ctx, utxo));
  if (candidates.length !== 1) {
    throw new Error(
      `Expected exactly one DA params UTxO at the governor address, found ${candidates.length.toString()}`,
    );
  }
  const utxo = candidates[0]!;
  return { utxo, datum: daParamsDatumOf(utxo) };
};

/** Runs an SDK pool builder, mapping its refusal to a one-line message. */
export const runPoolBuilder = async (
  program: Effect.Effect<TxBuilder, SDK.DaBondPoolBuildError>,
  refusal: (error: SDK.DaBondPoolBuildError) => string,
): Promise<TxBuilder> => {
  const result = await Effect.runPromise(Effect.either(program));
  if (Either.isLeft(result)) {
    throw new Error(refusal(result.left), { cause: result.left });
  }
  return result.right;
};

export type RefusalFacts = Readonly<{
  action: string;
  pool: PoolView;
  parameters: SDK.DaAvailabilityParameters;
  daParams?: SDK.DaParamsDatum;
  signers?: readonly string[];
  amount?: bigint;
  validFrom?: bigint;
}>;
