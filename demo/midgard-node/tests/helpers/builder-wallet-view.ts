/**
 * Shared steps for the tests of builders funded from the node's wallet view
 * (plan §8.5, I2b), over `openIntentEmulator`.
 */
import {
  CML,
  Lucid,
  type LucidEvolution,
  type UTxO,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { journaledIntent } from "../../src/services/intent-journal.js";
import {
  readSelectedWalletView,
  signOverWalletView,
} from "../../src/transactions/utils.wallet-view.js";
import type { IntentEmulator } from "./intent-journal-emulator.js";

/** An own payment's journaled intent (a payout funding step's; content: a settled event's id). */
export const ownPaymentIntent = (label: string) =>
  journaledIntent(
    "reserve_payout",
    `reserve_payout:${label}:add_funds`,
    Buffer.alloc(36, 1),
  );

export const refOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

export const viewOf = (env: IntentEmulator, lucid: LucidEvolution) =>
  Effect.runPromise(
    readSelectedWalletView(lucid).pipe(Effect.provide(env.journalLayer)),
  );

/** A transaction's spent inputs, as `txHash#index`. */
export const inputsOf = (cbor: string): string[] => {
  const inputs = CML.Transaction.from_cbor_hex(cbor).body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

/** A transaction's collateral inputs, as `txHash#index`. */
export const collateralsOf = (cbor: string): string[] => {
  const inputs = CML.Transaction.from_cbor_hex(cbor).body().collateral_inputs();
  if (inputs === undefined) return [];
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

/**
 * Every string `cause` along an error's chain (and an Either's `left`), so a
 * test can name the check that refused a build however its callers wrapped
 * it.
 */
export const causeTrail = (error: unknown): string[] => {
  const trail: string[] = [];
  const seen = new Set<unknown>();
  const walk = (value: unknown): void => {
    if (typeof value === "string") {
      trail.push(value);
      return;
    }
    if (typeof value !== "object" || value === null || seen.has(value)) return;
    seen.add(value);
    const record = value as Record<string, unknown>;
    walk(record.left);
    walk(record.cause);
  };
  walk(error);
  return trail;
};

/**
 * `payer` (by default the payee) pays the own wallet `lovelace`, and the
 * follower sees it land.
 */
export const fundOwn = async (
  env: IntentEmulator,
  lovelace: bigint,
  payer?: LucidEvolution,
): Promise<UTxO> => {
  let from = payer;
  if (from === undefined) {
    from = await Lucid(env.emulator, "Custom");
    from.selectWallet.fromSeed(env.payee.seedPhrase);
  }
  const signed = await (
    await from.newTx().pay.ToAddress(env.own.address, { lovelace }).complete()
  ).sign
    .withWallet()
    .complete();
  await env.emulator.submitTx(signed.toCBOR());
  env.emulator.awaitBlock(1);
  await env.follow();
  if ((await env.stage.run()).length !== 0)
    throw new Error("the follower stage reported trouble after funding");
  return {
    txHash: signed.toHash(),
    outputIndex: 0,
    address: env.own.address,
    assets: { lovelace },
  };
};

/**
 * Records, without sending, a signed own intent that leaves the wallet view
 * nothing: with `"inputs"`, it spends every own output; with
 * `"collateral"`, the wallet is first funded with a second output, and the
 * intent spends the first and reserves the second as its collateral. Its
 * change goes to the payee. Returns the own outputs it holds.
 */
export const holdWholeWallet = async (
  env: IntentEmulator,
  lucid: LucidEvolution,
  mode: "inputs" | "collateral",
): Promise<UTxO[]> => {
  const extra = mode === "collateral" ? await fundOwn(env, 3_000_000n) : null;
  const own = (await viewOf(env, lucid)).utxos;
  const spent =
    extra === null ? [...own] : own.filter((u) => refOf(u) !== refOf(extra));
  const unsigned = await lucid
    .newTx()
    .collectFrom(spent)
    .pay.ToAddress(env.payee.address, { lovelace: 2_000_000n })
    .complete({
      presetWalletInputs: spent,
      coinSelection: false,
      changeAddress: env.payee.address,
    });
  let cbor: string;
  if (extra === null) {
    cbor = (
      await (
        await Effect.runPromise(
          signOverWalletView(lucid, unsigned).pipe(
            Effect.provide(env.journalLayer),
          ),
        )
      ).complete()
    ).toCBOR();
  } else {
    const tx = CML.Transaction.from_cbor_hex(unsigned.toCBOR());
    const body = tx.body();
    const collateral = CML.TransactionInputList.new();
    collateral.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(extra.txHash),
        BigInt(extra.outputIndex),
      ),
    );
    body.set_collateral_inputs(collateral);
    const withCollateral = CML.Transaction.new(
      body,
      tx.witness_set(),
      true,
      tx.auxiliary_data(),
    ).to_cbor_hex();
    const { paymentKey } = walletFromSeed(env.own.seedPhrase, {
      network: "Custom",
      addressType: "Base",
      accountIndex: 0,
    });
    cbor = (
      await lucid
        .fromTx(withCollateral)
        .sign.withPrivateKey(paymentKey)
        .complete()
    ).toCBOR();
  }
  const hash = CML.hash_transaction(
    CML.Transaction.from_cbor_hex(cbor).body(),
  ).to_hex();
  await env.record(ownPaymentIntent(`holds-wallet-${mode}`), cbor, hash);
  const view = await viewOf(env, lucid);
  if (view.utxos.length !== 0)
    throw new Error("the holding intent left the wallet view an output");
  return extra === null ? spent : [...spent, extra];
};

/**
 * Records, without sending, a signed own intent that spends exactly `coin`
 * and pays all of it to the payee: `coin` is held, the rest of the view is
 * untouched. Returns the intent's hash.
 */
export const holdCoin = async (
  env: IntentEmulator,
  lucid: LucidEvolution,
  coin: UTxO,
): Promise<string> => {
  const unsigned = await lucid
    .newTx()
    .collectFrom([coin])
    .pay.ToAddress(env.payee.address, { lovelace: 2_000_000n })
    .complete({
      presetWalletInputs: [coin],
      coinSelection: false,
      changeAddress: env.payee.address,
    });
  const signed = await (
    await Effect.runPromise(
      signOverWalletView(lucid, unsigned).pipe(
        Effect.provide(env.journalLayer),
      ),
    )
  ).complete();
  await env.record(
    ownPaymentIntent(`holds-${coin.txHash.slice(0, 8)}`),
    signed.toCBOR(),
    signed.toHash(),
  );
  const view = await viewOf(env, lucid);
  if (!view.held.has(refOf(coin)))
    throw new Error("the holding intent does not hold its coin");
  return signed.toHash();
};

/** The view's plain-ADA coin with the most lovelace. */
export const largestCoin = (utxos: readonly UTxO[]): UTxO =>
  [...utxos]
    .filter(
      (utxo) =>
        Object.keys(utxo.assets).every((unit) => unit === "lovelace") &&
        utxo.scriptRef == null,
    )
    .sort((a, b) =>
      (b.assets.lovelace ?? 0n) > (a.assets.lovelace ?? 0n) ? 1 : -1,
    )[0]!;
