/**
 * The node's wallet view (plan §8.5, I2) on a Lucid emulator, over the
 * node's follower store and intent journal:
 *
 * - a dead intent's inputs are offered again on the next head (§15 I2 a);
 * - after a fork drops an own transaction, the next build spends its
 *   predicted change and is signed over the view, so it submits with no
 *   "Missing vkey witness" and no "Could not spend UTxO" (§15 I2 b); the
 *   wallet's own signer, which looks inputs up at the provider, misses the
 *   witness on the same build;
 * - a build made while an own intent is live chains on its predicted
 *   change, and the view follows both through landing and rollback;
 * - a live intent's collateral is never offered to another build, though
 *   the provider's wallet still offers it;
 * - a wallet whose every output a live intent holds is refused by name
 *   before coin selection, which would otherwise read the provider (a
 *   DA attestation step included);
 * - initialization finds its one-shot nonce in the view, and not while a
 *   live own intent spends it.
 */
import {
  type LucidEvolution,
  type UTxO,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { CML, Lucid } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  type IntentPlan,
  journaledIntent,
} from "../src/services/intent-journal.js";
import { completeWithLocalUplc } from "../src/transactions/da-attestation.fetch-da-attestation-reference-scripts.js";
import { fetchConfiguredNonceUtxo } from "../src/transactions/initialization.fetch-configured-nonce-utxo.js";
import {
  handleSignSubmit,
  handleSignSubmitNoConfirmation,
} from "../src/transactions/utils.js";
import {
  readSelectedWalletView,
  requireWalletViewInputs,
  signOverWalletView,
} from "../src/transactions/utils.wallet-view.js";
import { RECORD_ONLY } from "./helpers/intent-journal.js";
import {
  type IntentEmulator,
  openIntentEmulator,
} from "./helpers/intent-journal-emulator.js";
import { testDatabases } from "./helpers/l1-events-store.js";

const databases = testDatabases();
const opened: IntentEmulator[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((env) => env.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

const open = async () => {
  const env = await openIntentEmulator(databases);
  opened.push(env);
  // The first pass seeds the own wallet at the origin.
  expect(await env.stage.run()).toEqual([]);
  return env;
};

/** A payout funding step's intent (content: the settled event's id), under
 * `plan`, opened before the wallet-view reads its build rests on (S5). */
const intent = (label: string, plan: IntentPlan) =>
  journaledIntent(
    "reserve_payout",
    `reserve_payout:${label}:add_funds`,
    plan,
    Buffer.alloc(36, 1),
  );

const refOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const viewOf = (env: IntentEmulator, lucid: LucidEvolution) =>
  Effect.runPromise(
    readSelectedWalletView(lucid).pipe(Effect.provide(env.journalLayer)),
  );

const inputsOf = (cbor: string): string[] => {
  const inputs = CML.Transaction.from_cbor_hex(cbor).body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

/** A payment to the payee whose wallet inputs are the view's, as node builders take them. */
const buildFromView = async (
  env: IntentEmulator,
  lucid: LucidEvolution,
  lovelace: bigint,
) => {
  const inputs = await Effect.runPromise(
    requireWalletViewInputs(await viewOf(env, lucid), "a payment"),
  );
  return lucid
    .newTx()
    .pay.ToAddress(env.payee.address, { lovelace })
    .complete({ presetWalletInputs: inputs });
};

/** Signs and sends through the node's submit seam, without waiting. */
const submitThroughSeam = (
  env: IntentEmulator,
  lucid: LucidEvolution,
  unsigned: Awaited<ReturnType<typeof buildFromView>>,
  label: string,
  plan: IntentPlan,
) =>
  Effect.runPromise(
    handleSignSubmitNoConfirmation(lucid, unsigned, intent(label, plan)).pipe(
      Effect.provide(env.journalLayer),
    ),
  );

/** The payee pays the own wallet `lovelace`: a second own output. */
const fundOwn = async (env: IntentEmulator, lovelace: bigint) => {
  const payee = await Lucid(env.emulator, "Custom");
  payee.selectWallet.fromSeed(env.payee.seedPhrase);
  const signed = await (
    await payee.newTx().pay.ToAddress(env.own.address, { lovelace }).complete()
  ).sign
    .withWallet()
    .complete();
  await env.emulator.submitTx(signed.toCBOR());
  env.emulator.awaitBlock(1);
  await env.follow();
  expect(await env.stage.run()).toEqual([]);
  return { txHash: signed.toHash(), outputIndex: 0 };
};

const statusOf = (env: IntentEmulator, hash: string) =>
  env.stage.lastReport()!.entry(Buffer.from(hash, "hex"))?.status;

describe("the node wallet view on the emulator", () => {
  it("offers a dead intent's inputs again on the next head (§15 I2 a)", async () => {
    const env = await open();
    const lucid = await env.wallet();
    const plan = await env.plan();
    const funded = await fundOwn(env, 3_000_000n);
    const [seed, extra] = [...(await viewOf(env, lucid)).utxos].sort((a, b) =>
      refOf(a) === refOf(funded) ? 1 : refOf(b) === refOf(funded) ? -1 : 0,
    );
    expect(refOf(extra!)).toBe(refOf(funded));

    // A journaled intent spends both own outputs; it is live (never sent).
    const both = await lucid
      .newTx()
      .collectFrom([seed!, extra!])
      .pay.ToAddress(env.payee.address, { lovelace: 2_000_000n })
      .complete({ presetWalletInputs: [seed!, extra!] });
    const signed = await (
      await Effect.runPromise(
        signOverWalletView(lucid, both).pipe(Effect.provide(env.journalLayer)),
      )
    ).complete();
    await env.record(
      intent("dead", plan),
      signed.toCBOR(),
      signed.toHash(),
      RECORD_ONLY,
    );
    const live = await viewOf(env, lucid);
    expect([...live.held].sort()).toEqual([refOf(seed!), refOf(extra!)].sort());
    expect(live.utxos.map(refOf)).toEqual([`${signed.toHash()}#1`]);

    // The same wallet, used outside the node, spends one of them first.
    const outside = await Lucid(env.emulator, "Custom");
    outside.selectWallet.fromSeed(env.own.seedPhrase);
    const foreign = await (
      await outside
        .newTx()
        .collectFrom([extra!])
        .pay.ToAddress(env.payee.address, { lovelace: 1_000_000n })
        .complete({ presetWalletInputs: [extra!], coinSelection: false })
    ).sign
      .withWallet()
      .complete();
    await env.emulator.submitTx(foreign.toCBOR());
    env.emulator.awaitBlock(1);
    await env.follow();
    expect(await env.stage.run()).toEqual([]);
    expect(statusOf(env, signed.toHash())).toMatchObject({
      kind: "conflicted",
    });

    // Next head: the dead intent holds nothing; its other input is back,
    // its change is gone.
    const next = await viewOf(env, lucid);
    expect(next.held.size).toBe(0);
    expect(next.predictedBy.size).toBe(0);
    expect(next.utxos.map(refOf)).toContain(refOf(seed!));
    expect(next.utxos.map(refOf)).not.toContain(refOf(extra!));
    expect(next.utxos.map(refOf)).not.toContain(`${signed.toHash()}#1`);

    // And the next build spends it and lands.
    const again = await buildFromView(env, lucid, 2_000_000n);
    expect(inputsOf(again.toCBOR())).toContain(refOf(seed!));
    const hash = await Effect.runPromise(
      handleSignSubmit(lucid, again, intent("after-dead", plan)).pipe(
        Effect.provide(env.journalLayer),
      ),
    );
    await env.follow();
    expect(await env.stage.run()).toEqual([]);
    expect(statusOf(env, hash)).toMatchObject({ kind: "landed" });
  });

  it("after a fork drops an own transaction, the next build spends its predicted change and submits with no missing witness (§15 I2 b)", async () => {
    const env = await open();
    const lucid = await env.wallet();
    const plan = await env.plan();
    const [seed] = (await viewOf(env, lucid)).utxos;

    // tx1 is journaled and lands on a branch the emulator never sees.
    const first = await (
      await Effect.runPromise(
        signOverWalletView(
          lucid,
          await buildFromView(env, lucid, 2_000_000n),
        ).pipe(Effect.provide(env.journalLayer)),
      )
    ).complete();
    const firstHash = first.toHash();
    await env.record(
      intent("fork-first", plan),
      first.toCBOR(),
      firstHash,
      RECORD_ONLY,
    );
    await env.applyForkBlock([Buffer.from(first.toCBOR(), "hex")]);
    expect(await env.stage.run()).toEqual([]);
    expect(statusOf(env, firstHash)).toMatchObject({ kind: "landed" });
    const landed = await viewOf(env, lucid);
    expect(landed.utxos.map(refOf)).toEqual([`${firstHash}#1`]);
    expect(landed.predictedBy.size).toBe(0);

    // The fork drops it: tx1 is live again, its change predicted, and S6
    // sends its exact bytes to the mempool.
    await env.rewindTo(0);
    // S5: the rewind moved the generation; the next build's plan opens
    // after it, before the reads that build rests on.
    const replanned = await env.plan();
    expect(await env.stage.run()).toEqual([]);
    expect(statusOf(env, firstHash)).toMatchObject({ kind: "live" });
    expect(env.sent.map((bytes) => bytes.toString("hex"))).toEqual([
      first.toCBOR(),
    ]);
    const dropped = await viewOf(env, lucid);
    expect(dropped.utxos.map(refOf)).toEqual([`${firstHash}#1`]);
    expect(dropped.predictedBy.get(`${firstHash}#1`)).toBe(firstHash);
    expect([...dropped.held]).toEqual([refOf(seed!)]);
    // The provider sees only its ledger: the change is not there yet.
    expect((await lucid.utxosAt(env.own.address)).map(refOf)).toEqual([]);

    // Control (the wallet's own signer, as the node signed before I2): it
    // looks the input up at the provider, finds no own key, and the
    // emulator refuses the transaction.
    const control = await (await buildFromView(env, lucid, 1_000_000n)).sign
      .withWallet()
      .complete();
    await expect(env.emulator.submitTx(control.toCBOR())).rejects.toThrow(
      /Missing vkey witness/,
    );

    // The node's build: the view's predicted change, signed over the view.
    const second = await buildFromView(env, lucid, 1_000_000n);
    expect(inputsOf(second.toCBOR())).toEqual([`${firstHash}#1`]);
    const secondHash = await submitThroughSeam(
      env,
      lucid,
      second,
      "fork-second",
      replanned,
    );
    env.emulator.awaitBlock(1);
    await env.follow();
    expect(await env.stage.run()).toEqual([]);
    expect(statusOf(env, firstHash)).toMatchObject({ kind: "landed" });
    expect(statusOf(env, secondHash)).toMatchObject({ kind: "landed" });
    expect((await viewOf(env, lucid)).utxos.map(refOf)).toEqual([
      `${secondHash}#1`,
    ]);
  });

  it("chains a second build on a live intent's predicted change, and follows both through landing and rollback", async () => {
    const env = await open();
    const lucid = await env.wallet();
    const plan = await env.plan();
    const [seed] = (await viewOf(env, lucid)).utxos;

    const firstHash = await submitThroughSeam(
      env,
      lucid,
      await buildFromView(env, lucid, 2_000_000n),
      "chain-first",
      plan,
    );
    const afterFirst = await viewOf(env, lucid);
    expect(afterFirst.utxos.map(refOf)).toEqual([`${firstHash}#1`]);
    expect([...afterFirst.held]).toEqual([refOf(seed!)]);

    // Built while the first is live: it can only spend the first's change.
    const second = await buildFromView(env, lucid, 1_000_000n);
    expect(inputsOf(second.toCBOR())).toEqual([`${firstHash}#1`]);
    const secondHash = await submitThroughSeam(
      env,
      lucid,
      second,
      "chain-second",
      plan,
    );
    const bothLive = await viewOf(env, lucid);
    expect(bothLive.utxos.map(refOf)).toEqual([`${secondHash}#1`]);
    expect(bothLive.predictedBy.get(`${secondHash}#1`)).toBe(secondHash);
    expect([...bothLive.held].sort()).toEqual(
      [refOf(seed!), `${firstHash}#1`].sort(),
    );

    // Both land: the second's change is a fact, nothing is held.
    env.emulator.awaitBlock(1);
    await env.follow();
    const landed = await viewOf(env, lucid);
    expect(landed.utxos.map(refOf)).toEqual([`${secondHash}#1`]);
    expect(landed.predictedBy.size).toBe(0);
    expect(landed.held.size).toBe(0);

    // Rolled back: both live again, the view is the chained one again.
    await env.rewindTo(0);
    const rolledBack = await viewOf(env, lucid);
    expect(rolledBack.utxos.map(refOf)).toEqual([`${secondHash}#1`]);
    expect(rolledBack.predictedBy.get(`${secondHash}#1`)).toBe(secondHash);
    expect([...rolledBack.held].sort()).toEqual(
      [refOf(seed!), `${firstHash}#1`].sort(),
    );

    // Followed again: landed again.
    await env.follow();
    expect((await viewOf(env, lucid)).held.size).toBe(0);
  });

  it("never offers a live intent's collateral to another build", async () => {
    const env = await open();
    const lucid = await env.wallet();
    const plan = await env.plan();
    const funded = await fundOwn(env, 3_000_000n);
    const seed = (await viewOf(env, lucid)).utxos.find(
      (utxo) => refOf(utxo) !== refOf(funded),
    )!;

    // A live intent spends the seed with the other own output as collateral.
    const unsigned = await lucid
      .newTx()
      .collectFrom([seed])
      .pay.ToAddress(env.payee.address, { lovelace: 2_000_000n })
      .complete({ presetWalletInputs: [seed], coinSelection: false });
    const tx = CML.Transaction.from_cbor_hex(unsigned.toCBOR());
    const body = tx.body();
    const collateral = CML.TransactionInputList.new();
    collateral.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(funded.txHash),
        BigInt(funded.outputIndex),
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
    const signed = await lucid
      .fromTx(withCollateral)
      .sign.withPrivateKey(paymentKey)
      .complete();
    await env.record(
      intent("collateral", plan),
      signed.toCBOR(),
      signed.toHash(),
      RECORD_ONLY,
    );

    const view = await viewOf(env, lucid);
    expect([...view.held].sort()).toEqual([refOf(seed), refOf(funded)].sort());
    expect(view.utxos.map(refOf)).toEqual([`${signed.toHash()}#1`]);
    // The provider's wallet, which builders read before I2, still offers it.
    expect((await lucid.wallet().getUtxos()).map(refOf)).toContain(
      refOf(funded),
    );

    const next = await buildFromView(env, lucid, 1_000_000n);
    expect(inputsOf(next.toCBOR())).toEqual([`${signed.toHash()}#1`]);
  });

  it("refuses by name a build whose every own output a live intent holds, before coin selection reads the provider", async () => {
    const env = await open();
    const lucid = await env.wallet();
    const plan = await env.plan();
    const [seed] = (await viewOf(env, lucid)).utxos;
    // A live intent that spends the whole wallet and returns nothing to it.
    const unsigned = await lucid
      .newTx()
      .pay.ToAddress(env.payee.address, { lovelace: 2_000_000n })
      .complete({
        presetWalletInputs: [seed!],
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
      intent("whole", plan),
      signed.toCBOR(),
      signed.toHash(),
      RECORD_ONLY,
    );

    const view = await viewOf(env, lucid);
    expect(view.utxos).toEqual([]);
    const refused = await Effect.runPromise(
      Effect.flip(requireWalletViewInputs(view, "a payment")),
    );
    expect(refused).toMatchObject({
      _tag: "WalletViewUnavailable",
      cause: "wallet_view_empty",
    });
    // A DA attestation step completes from the same view, and is refused
    // by the same check.
    const attestation = await Effect.runPromise(
      Effect.flip(
        completeWithLocalUplc(
          lucid,
          lucid.newTx().pay.ToAddress(env.payee.address, { lovelace: 1n }),
          "DA attestation init",
        ).pipe(Effect.provide(env.journalLayer)),
      ),
    );
    expect(attestation.message).toContain(
      "has no spendable output to fund DA attestation init transaction",
    );
    // The provider would have offered the held output.
    expect((await lucid.wallet().getUtxos()).map(refOf)).toEqual([
      refOf(seed!),
    ]);
  });

  it("finds the initialization nonce in the view, and refuses it while a live intent holds it", async () => {
    const env = await open();
    const lucid = await env.wallet();
    const plan = await env.plan();
    const [nonce] = (await viewOf(env, lucid)).utxos;
    const fetchNonce = () =>
      Effect.runPromise(
        Effect.either(
          fetchConfiguredNonceUtxo(lucid, {
            HUB_ORACLE_ONE_SHOT_TX_HASH: nonce!.txHash,
            HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonce!.outputIndex,
            L1_OPERATOR_SEED_PHRASE: env.own.seedPhrase,
            NETWORK: "Custom",
          }).pipe(Effect.provide(env.journalLayer)),
        ),
      );
    const found = await fetchNonce();
    expect(found._tag === "Right" && refOf(found.right)).toBe(refOf(nonce!));

    // An own transaction that spends it is live (sent, not landed).
    await submitThroughSeam(
      env,
      lucid,
      await buildFromView(env, lucid, 2_000_000n),
      "spends-nonce",
      plan,
    );
    const held = await fetchNonce();
    expect(held._tag === "Left" && held.left.message).toBe(
      "Configured one-shot hub oracle UTxO is not available in the operator wallet",
    );
  });
});
