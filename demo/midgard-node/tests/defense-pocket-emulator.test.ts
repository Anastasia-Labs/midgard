/**
 * The defense pocket and the journal's live intents on a Lucid emulator,
 * over the node's follower store and intent journal:
 *
 * - the operator key's Enterprise address is one of the node's own
 *   wallets: an output paid there is in its wallet view, and not in the
 *   Base wallet's;
 * - with the operator wallet selected (its Base address), a transaction
 *   spending the pocket alone is signed over the view and lands;
 * - where no follower runs, a seed selected without the pocket signs over
 *   its Base address's provider read alone, and the operator's over the
 *   pocket's too;
 * - the live intents of a workflow key: a journaled defense is live until
 *   its validity passes, and then dead; another family's intent under the
 *   same key, and another key, are not among them;
 * - the intent stage sends a recorded defense whose inputs are live, and
 *   follows it to landed.
 */
import {
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  IntentJournalWithoutFollower,
  type IntentPlan,
  journaledIntent,
} from "../src/services/intent-journal.js";
import {
  defensePocketAddress,
  nodeOwnWallets,
} from "../src/services/intent-journal.tracked-set.js";
import { handleSignSubmit } from "../src/transactions/utils.js";
import {
  readSelectedWalletView,
  readWalletView,
  requireWalletViewInputs,
  selectNodeWallet,
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

/** The node's wallet configuration with `seed` as its operator seed. */
const operatorConfig = (seed: string) => ({
  NETWORK: "Custom" as const,
  L1_OPERATOR_SEED_PHRASE: seed,
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: "",
  L1_SETTLEMENT_SEED_PHRASE: "",
  L1_REFERENCE_SCRIPT_SEED_PHRASE: "",
});

/** The emulator with the node's own wallets for the own account's seed. */
const open = async () => {
  const env = await openIntentEmulator(databases, {
    ownWallets: (own) => nodeOwnWallets(operatorConfig(own.seedPhrase)),
  });
  opened.push(env);
  // The first pass seeds the own wallets at the origin.
  expect(await env.stage.run()).toEqual([]);
  return {
    env,
    pocket: defensePocketAddress(operatorConfig(env.own.seedPhrase)),
  };
};

const refOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const viewAt = (env: IntentEmulator, lucid: LucidEvolution, address: string) =>
  Effect.runPromise(
    readWalletView(lucid, address).pipe(Effect.provide(env.journalLayer)),
  );

/** The wallet of `seed`, outside the node, pays `to` `lovelace`, and the
 * follower applies it. */
const payFrom = async (
  env: IntentEmulator,
  seed: string,
  to: string,
  lovelace: bigint,
) => {
  const payer = await Lucid(env.emulator, "Custom");
  payer.selectWallet.fromSeed(seed);
  const signed = await (
    await payer.newTx().pay.ToAddress(to, { lovelace }).complete()
  ).sign
    .withWallet()
    .complete();
  await env.emulator.submitTx(signed.toCBOR());
  env.emulator.awaitBlock(1);
  await env.follow();
  return { txHash: signed.toHash(), outputIndex: 0 };
};

/** A payment from the Base wallet's view, valid until `validTo`, signed. */
const signedFromBase = async (
  env: IntentEmulator,
  lucid: LucidEvolution,
  validTo: number,
) => {
  const inputs = await Effect.runPromise(
    readSelectedWalletView(lucid).pipe(
      Effect.flatMap((view) => requireWalletViewInputs(view, "a defense")),
      Effect.provide(env.journalLayer),
    ),
  );
  const unsigned = await lucid
    .newTx()
    .pay.ToAddress(env.payee.address, { lovelace: 2_000_000n })
    .validTo(validTo)
    .complete({ presetWalletInputs: inputs });
  const signBuilder = await Effect.runPromise(
    signOverWalletView(lucid, unsigned).pipe(Effect.provide(env.journalLayer)),
  );
  return signBuilder.complete();
};

/** A defense intent under `workflowKey`, under `plan`. */
const defense = (workflowKey: string, plan: IntentPlan) =>
  journaledIntent("dispute_defense", workflowKey, plan);

describe("the defense pocket on the emulator", () => {
  it("is an own wallet of the operator's payment key: an output paid to it is in its view and not in the Base wallet's", async () => {
    const { env, pocket } = await open();
    const lucid = await env.wallet();
    const base = getAddressDetails(env.own.address);
    const details = getAddressDetails(pocket);
    expect(details.type).toBe("Enterprise");
    expect(details.paymentCredential).toEqual(base.paymentCredential);
    expect(base.type).toBe("Base");

    const paid = await payFrom(env, env.own.seedPhrase, pocket, 7_000_000n);
    expect(await env.stage.run()).toEqual([]);

    const view = await viewAt(env, lucid, pocket);
    expect(view.source).toBe("follower");
    expect(view.utxos.map(refOf)).toEqual([refOf(paid)]);
    expect(view.utxos[0]!.assets).toEqual({ lovelace: 7_000_000n });
    const wallet = await Effect.runPromise(
      readSelectedWalletView(lucid).pipe(Effect.provide(env.journalLayer)),
    );
    expect(wallet.address).toBe(env.own.address);
    expect(wallet.utxos.map(refOf)).not.toContain(refOf(paid));
  });

  it("signs a transaction spending the pocket alone with the operator wallet selected, and it lands", async () => {
    const { env, pocket } = await open();
    const lucid = await env.wallet({ defensePocket: true });
    const plan = await env.plan();
    await payFrom(env, env.own.seedPhrase, pocket, 7_000_000n);
    expect(await env.stage.run()).toEqual([]);

    const inputs = await Effect.runPromise(
      requireWalletViewInputs(await viewAt(env, lucid, pocket), "a defense"),
    );
    const unsigned = await lucid
      .newTx()
      .collectFrom(inputs)
      .pay.ToAddress(env.payee.address, { lovelace: 2_000_000n })
      .complete({ presetWalletInputs: inputs, coinSelection: false });
    const hash = await Effect.runPromise(
      handleSignSubmit(lucid, unsigned, defense("pocket:spend", plan)).pipe(
        Effect.provide(env.journalLayer),
      ),
    );
    await env.follow();
    expect(await env.stage.run()).toEqual([]);
    expect(
      env.stage.lastReport()!.entry(Buffer.from(hash, "hex"))?.status,
    ).toMatchObject({ kind: "landed" });
    expect((await viewAt(env, lucid, pocket)).utxos).toEqual([]);
  });
});

describe("the views a node wallet signs over where no follower runs", () => {
  it("reads only the Base address at the provider for a seed selected without the pocket, and the pocket too for the operator wallet", async () => {
    const account = generateEmulatorAccount({ lovelace: 100_000_000n });
    const emulator = new Emulator([account]);
    const reads: string[] = [];
    const getUtxos = emulator.getUtxos.bind(emulator);
    emulator.getUtxos = async (address) => {
      reads.push(typeof address === "string" ? address : address.hash);
      return getUtxos(address);
    };
    const readsOfSign = async (defensePocket: boolean) => {
      const lucid = await Lucid(emulator, "Custom");
      selectNodeWallet(lucid, account.seedPhrase, { defensePocket });
      const unsigned = await lucid
        .newTx()
        .pay.ToAddress(account.address, { lovelace: 2_000_000n })
        .complete();
      reads.length = 0;
      const signed = await Effect.runPromise(
        signOverWalletView(lucid, unsigned).pipe(
          Effect.provide(IntentJournalWithoutFollower),
        ),
      );
      const signedReads = [...reads];
      await emulator.submitTx((await signed.complete()).toCBOR());
      emulator.awaitBlock(1);
      return signedReads;
    };

    expect(await readsOfSign(false)).toEqual([account.address]);
    expect(await readsOfSign(true)).toEqual([
      account.address,
      defensePocketAddress(operatorConfig(account.seedPhrase)),
    ]);
  });
});

describe("the journal's live intents of a workflow key", () => {
  it("names a journaled defense while it is live and not once its validity has passed, and never another family's or another key's", async () => {
    const { env } = await open();
    const lucid = await env.wallet();
    const plan = await env.plan();
    const validTo = env.emulator.now() + 20_000;
    const build = () => signedFromBase(env, lucid, validTo);
    const key = "defend:case-unit";
    const defended = await build();
    await env.record(
      defense(key, plan),
      defended.toCBOR(),
      defended.toHash(),
      RECORD_ONLY,
    );
    const funding = await build();
    await env.record(
      journaledIntent("reference_funding", key, plan),
      funding.toCBOR(),
      funding.toHash(),
      RECORD_ONLY,
    );
    const live = (workflowKey: string) =>
      Effect.runPromise(
        env.journal.liveIntents("dispute_defense", workflowKey),
      );

    expect(await live(key)).toEqual([
      { txHash: defended.toHash(), inputsAvailable: true },
    ]);
    expect(await live("defend:another-unit")).toEqual([]);

    // Past its validity, a block lands and the follower applies it.
    env.emulator.awaitSlot(30);
    await payFrom(env, env.payee.seedPhrase, env.payee.address, 1_000_000n);
    expect(await live(key)).toEqual([]);
  });
});

describe("the intent stage and a journaled defense", () => {
  it("sends a recorded defense whose inputs are live, and follows it to landed", async () => {
    const { env } = await open();
    const lucid = await env.wallet();
    const defended = await signedFromBase(
      env,
      lucid,
      env.emulator.now() + 60_000,
    );
    await env.record(
      defense("defend:case-unit", await env.plan()),
      defended.toCBOR(),
      defended.toHash(),
      RECORD_ONLY,
    );
    expect(env.sent).toEqual([]);
    const statusOf = () =>
      env.stage.lastReport()!.entry(Buffer.from(defended.toHash(), "hex"));

    expect(await env.stage.run()).toEqual([]);
    expect(statusOf()).toMatchObject({
      action: "resubmit",
      status: { kind: "live" },
    });
    expect(env.sent.map((bytes) => bytes.toString("hex"))).toEqual([
      defended.toCBOR(),
    ]);
    env.emulator.awaitBlock(1);
    await env.follow();
    expect(await env.stage.run()).toEqual([]);
    expect(statusOf()).toMatchObject({
      action: "follow",
      status: { kind: "landed" },
    });
  });
});
