import * as SDK from "@al-ft/midgard-sdk";
import { CML, type LucidEvolution } from "@lucid-evolution/lucid";

import {
  type DaBondContext,
  fetchPool,
  isoOf,
  outRefLabel,
  parseLovelace,
  type RefusalFacts,
  runPoolBuilder,
  statusAfterSubmit,
} from "./da-bond.da-bond-context.js";
import {
  daBondSigningKeyFromSecret,
  type DaBondWithdrawAction,
} from "./da-bond-files.js";

/**
 * One line per validator refusal the SDK prechecks mirror; any other reason
 * keeps the SDK's own message.
 */
export const refusalMessage = (
  error: SDK.DaBondPoolBuildError,
  facts: RefusalFacts,
): string => {
  const { action, pool, parameters } = facts;
  switch (error.reason) {
    case "below_min_top_up":
      return `Refusing ${action}: ${(facts.amount ?? 0n).toString()} lovelace is below da_bond_min_top_up_lovelace ${parameters.da_bond_min_top_up_lovelace.toString()}; nothing was submitted`;
    case "before_unlock": {
      // The SDK compares the slot-aligned lower bound; report the values it
      // compared.
      const compared = /validFrom=(\d+),unlock_at=(\d+)/u.exec(
        String(error.cause),
      );
      const lower = BigInt(compared?.[1] ?? facts.validFrom ?? 0n);
      const unlockAt = BigInt(
        compared?.[2] ??
          (pool.datum === "Bonded" ? 0n : pool.datum.Withdrawing.unlock_at),
      );
      return `Refusing ${action}: the pool unlocks at unlock_at ${unlockAt.toString()} (${isoOf(unlockAt)}), after this transaction's lower bound ${lower.toString()} (${isoOf(lower)}); retry at or after unlock_at`;
    }
    case "insufficient_signers":
      return SDK.daBondPoolQuorumShortfallMessage(
        new Set(facts.signers ?? []).size,
        facts.daParams?.update_threshold ?? 0n,
      );
    case "pool_not_bonded":
      return `Refusing ${action}: the pool is already Withdrawing (unlock_at ${pool.datum === "Bonded" ? "" : pool.datum.Withdrawing.unlock_at.toString()}); cancel or complete that withdrawal first`;
    case "pool_not_withdrawing":
      return `Refusing ${action}: the pool is Bonded; begin a withdrawal first`;
    case "amount_exceeds_backing":
      return `Refusing ${action}: amount ${(facts.amount ?? 0n).toString()} lovelace exceeds the pool backing ${SDK.daBondPoolBacking(
        {
          lovelace: pool.utxo.assets.lovelace ?? 0n,
          parameters,
        },
      ).toString()} (lovelace above the ${parameters.da_bond_pool_floor_lovelace.toString()} floor)`;
    case "signer_not_owner":
      return `Refusing ${action}: signer ${String(error.cause)} is not a DA params owner (owners: ${(facts.daParams?.owners ?? []).join(", ")})`;
    case "duplicate_signer":
      return `Refusing ${action}: signer ${String(error.cause)} is listed twice in --signers`;
    case "below_floor":
    case "invalid_amount":
    case "invalid_da_params":
    case "invalid_pool":
    case "invalid_pool_address":
    case "invalid_validity_range":
    case "invalid_witness":
    case "missing_witness":
    case "reference_script_mismatch":
      return `Refusing ${action}: ${error.message}`;
  }
};

export type DaBondTopUpOptions = Readonly<{
  amount: string;
  /**
   * The funding wallet: a bech32 payment key (its enterprise address), or a
   * mnemonic selected as the node selects `L1_OPERATOR_SEED_PHRASE` (its base
   * address, account 0).
   */
  walletSecret: string;
}>;

/** `da-bond top-up`: the wallet adds `amount` lovelace and submits. */
export const daBondTopUpCommand = async (
  ctx: DaBondContext,
  options: DaBondTopUpOptions,
) => {
  const amount = parseLovelace(options.amount, "--amount");
  const pool = await fetchPool(ctx);
  const secret = options.walletSecret.trim();
  if (secret.startsWith("ed25519_sk1") || secret.startsWith("ed25519e_sk1")) {
    ctx.lucid.selectWallet.fromPrivateKey(secret);
  } else {
    // Validates the secret without echoing it.
    daBondSigningKeyFromSecret(secret);
    ctx.lucid.selectWallet.fromSeed(secret);
  }
  const tx = await runPoolBuilder(
    SDK.buildTopUpDaBondPoolTxProgram(ctx.lucid, {
      poolValidator: ctx.poolValidator,
      parameters: ctx.parameters,
      pool: { utxo: pool.utxo },
      amount,
      referenceScripts: { daBondPoolSpending: ctx.poolSpendingReference },
    }),
    (error) =>
      refusalMessage(error, {
        action: "top-up",
        pool,
        parameters: ctx.parameters,
        amount,
      }),
  );
  const signed = await (await tx.complete({ localUPLCEval: true })).sign
    .withWallet()
    .complete();
  const txHash = await ctx.submit(signed.toCBOR());
  return {
    action: "top-up",
    txHash,
    amount: amount.toString(),
    previousPoolOutRef: outRefLabel(pool.utxo),
    status: await statusAfterSubmit(ctx, txHash),
  };
};

export type DaBondWithdrawStep = "begin" | "cancel" | "complete";

export const WITHDRAW_ACTION: Readonly<
  Record<DaBondWithdrawStep, DaBondWithdrawAction>
> = {
  begin: "BeginWithdraw",
  cancel: "CancelWithdraw",
  complete: "CompleteWithdraw",
};

export type DaBondWithdrawBuildOptions = Readonly<{
  feeAddress: string;
  signers: string;
  buildUnsigned: string;
  validForMs?: string;
  amount?: string;
  to?: string;
}>;

/** The body's validity bounds, in POSIX milliseconds. */
export const txValidity = (
  lucid: LucidEvolution,
  txCbor: string,
): { readonly validFrom?: bigint; readonly validTo?: bigint } => {
  const tx = CML.Transaction.from_cbor_hex(txCbor);
  try {
    const body = tx.body();
    try {
      const start = body.validity_interval_start();
      const ttl = body.ttl();
      return {
        ...(start === undefined
          ? {}
          : { validFrom: BigInt(lucid.slotToUnixTime(Number(start))) }),
        ...(ttl === undefined
          ? {}
          : { validTo: BigInt(lucid.slotToUnixTime(Number(ttl))) }),
      };
    } finally {
      body.free();
    }
  } finally {
    tx.free();
  }
};

/** The body's spent inputs and reference inputs, as `txHash#index`. */
export const txInputs = (
  txCbor: string,
): { readonly inputs: string[]; readonly referenceInputs: string[] } => {
  const tx = CML.Transaction.from_cbor_hex(txCbor);
  const labels = (list: CML.TransactionInputList | undefined): string[] => {
    if (list === undefined) return [];
    const out: string[] = [];
    for (let i = 0; i < list.len(); i += 1) {
      const input = list.get(i);
      out.push(
        `${input.transaction_id().to_hex()}#${input.index().toString()}`,
      );
      input.free();
    }
    list.free();
    return out;
  };
  try {
    const body = tx.body();
    try {
      return {
        inputs: labels(body.inputs()),
        referenceInputs: labels(body.reference_inputs()),
      };
    } finally {
      body.free();
    }
  } finally {
    tx.free();
  }
};
