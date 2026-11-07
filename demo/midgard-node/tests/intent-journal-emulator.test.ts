/**
 * The node intent journal (plan §8.2, §8.3, I1) on a Lucid emulator, in
 * both polarities, for a payout funding step (its §8.4 predicate holds
 * while every input is live):
 *
 * - honest: the signed transaction is journaled and its first send is lost
 *   (the process stopped between the journal write and the send); S6 sends
 *   the exact journaled bytes, they land, and the derived status is landed
 *   with no status write;
 * - adversarial: a transaction the node did not journal (the same wallet
 *   used elsewhere) spends the journaled transaction's input first; the
 *   journaled one is derived dead (a foreign conflict) and S6 never sends
 *   it, at that tip or the next.
 *
 * Each family's real flow replays its journaled intents onto a follower
 * (`replayJournaledOnFollower`): the deposit-to-payout journey, the operator
 * commands, operator registration and the reference-script sweep.
 *
 * The submit seam itself (`handleSignSubmit`) journals the exact bytes
 * before the provider sees them, and those bytes are what lands.
 *
 * The seam sends only on S6's decision, taken in the record's transaction
 * (S5/S6, §8.1, I5), in both polarities: a plan a rewind passed before the
 * record is recorded `stale_at_write` and never sent from that write; a
 * rewind between the record and a retry's send removes its view and holds
 * the retry; a follower view behind wall-clock time past the bound holds
 * the send, which goes out once the view is within it.
 */
import {
  decodeTransaction,
  FOLLOWER_NODE_BEHIND,
  readIntentEventsIn,
} from "@al-ft/midgard-l1-follower";
import { Lucid, type TxSignBuilder } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit, Layer } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  IntentJournal,
  intentJournalOver,
  type IntentPlan,
  IntentSubmitHeld,
  journaledIntent,
} from "../src/services/intent-journal.js";
import {
  handleSignSubmit,
  submitSignedTxWithRecovery,
} from "../src/transactions/utils.js";
import { RECORD_ONLY } from "./helpers/intent-journal.js";
import {
  type IntentEmulator,
  openIntentEmulator,
  signedPayment,
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

const statusOf = (env: IntentEmulator, hash: string) =>
  env.stage.lastReport()!.entry(Buffer.from(hash, "hex"));

/** A payout funding step's intent (content: the settled event's id). */
const fundingIntent = (label: string, plan: IntentPlan) =>
  journaledIntent(
    "reserve_payout",
    `reserve_payout:${label}:add_funds`,
    plan,
    Buffer.alloc(36, 1),
  );

describe("the node intent journal on the emulator", () => {
  it("the journaled bytes are sent by S6 after a lost first send, and land", async () => {
    const env = await open();
    const tx = await signedPayment(
      await env.wallet(),
      env.payee.address,
      2_000_000n,
    );
    await env.record(
      fundingIntent("honest", await env.plan()),
      tx.cbor,
      tx.hash,
      RECORD_ONLY,
    );
    expect(env.sent).toEqual([]);

    expect(await env.stage.run()).toEqual([]);
    expect(statusOf(env, tx.hash)).toMatchObject({
      action: "resubmit",
      status: { kind: "live" },
    });
    expect(env.sent.map((bytes) => bytes.toString("hex"))).toEqual([tx.cbor]);

    env.emulator.awaitBlock(1);
    await env.follow();
    expect(env.emulator.transactionHistory[tx.hash]).toMatchObject({
      status: "confirmed",
    });
    expect(await env.stage.run()).toEqual([]);
    expect(statusOf(env, tx.hash)).toMatchObject({
      action: "follow",
      status: { kind: "landed", depth: 1 },
    });
    expect(env.sent).toHaveLength(1);
  });

  it("a resubmit after a foreign spend of its input is refused as dead and never sent", async () => {
    const env = await open();
    const journaled = await signedPayment(
      await env.wallet(),
      env.payee.address,
      2_000_000n,
    );
    await env.record(
      fundingIntent("adversarial", await env.plan()),
      journaled.cbor,
      journaled.hash,
      RECORD_ONLY,
    );
    // The same wallet, used outside the node, spends the same input.
    const foreign = await signedPayment(
      await env.wallet(),
      env.payee.address,
      3_000_000n,
    );
    const spent = (cbor: string) =>
      decodeTransaction(Buffer.from(cbor, "hex")).inputs.map(
        (input) => `${input.txHash.toString("hex")}#${input.index}`,
      );
    expect(spent(foreign.cbor)).toEqual(spent(journaled.cbor));
    await env.emulator.submitTx(foreign.cbor);
    env.emulator.awaitBlock(1);
    await env.follow();

    expect(await env.stage.run()).toEqual([]);
    expect(statusOf(env, journaled.hash)).toMatchObject({
      action: "dead",
      status: { kind: "conflicted", ownSpender: false },
    });
    env.emulator.awaitBlock(1);
    await env.follow();
    await env.stage.run();
    expect(statusOf(env, journaled.hash)!.action).toBe("dead");
    expect(env.sent).toEqual([]);
    expect(env.emulator.transactionHistory[journaled.hash]).toBeUndefined();
  });

  it("handleSignSubmit journals the exact bytes before the provider sees them, and those bytes land", async () => {
    const env = await open();
    const lucid = await env.wallet();
    const unsigned = await lucid
      .newTx()
      .pay.ToAddress(env.payee.address, { lovelace: 2_000_000n })
      .complete();
    const order: string[] = [];
    const provider = lucid.config().provider!;
    const submitTx = provider.submitTx.bind(provider);
    provider.submitTx = (tx) => {
      order.push(`submit ${tx}`);
      return submitTx(tx);
    };
    const hash = await Effect.runPromise(
      handleSignSubmit(
        lucid,
        unsigned,
        fundingIntent("seam", await env.plan()),
      ).pipe(
        Effect.provide(
          Layer.succeed(IntentJournal, {
            ...env.journal,
            record: (...args) =>
              Effect.sync(() => order.push(`record ${args[1]}`)).pipe(
                Effect.zipRight(env.journal.record(...args)),
              ),
          }),
        ),
      ),
    );
    await env.follow();
    expect(await env.stage.run()).toEqual([]);
    const entry = statusOf(env, hash)!;
    expect(entry.status.kind).toBe("landed");
    expect(entry.intent.family).toBe("reserve_payout");
    const landed = env.accepted.get(hash)!.toString("hex");
    expect(order).toEqual([`record ${landed}`, `submit ${landed}`]);
    expect(env.sent).toEqual([]);
  });
});

/** Lands a payment between foreign wallets: the follower's block 1. */
const landForeignBlock = async (env: IntentEmulator) => {
  const lucid = await Lucid(env.emulator, "Custom");
  lucid.selectWallet.fromSeed(env.payee.seedPhrase);
  const tx = await signedPayment(lucid, env.payee.address, 1_000_000n);
  await env.emulator.submitTx(tx.cbor);
  env.emulator.awaitBlock(1);
  await env.follow();
  expect((await env.store.cursor())?.height).toBe(1);
};

/** Rewinds the follower to its origin, removing block 1: a new generation. */
const rewindToOrigin = async (env: IntentEmulator) => {
  const origin = (await env.store.blockAtHeight(0))!;
  await env.store.rewind({ slot: origin.slot, hash: origin.hash });
};

/** A signed own payment of `lovelace`, and the provider sends it reaches. */
const signedOwnPayment = async (env: IntentEmulator, lovelace = 2_000_000n) => {
  const lucid = await env.wallet();
  const unsigned: TxSignBuilder = await lucid
    .newTx()
    .pay.ToAddress(env.payee.address, { lovelace })
    .complete();
  const signed = await unsigned.sign.withWallet().complete();
  const provider = lucid.config().provider!;
  const sent: string[] = [];
  const submitTx = provider.submitTx.bind(provider);
  provider.submitTx = (tx) => {
    sent.push(tx);
    return submitTx(tx);
  };
  return { lucid, signed, txHash: signed.toHash(), provider, sent };
};

const eventKinds = async (env: IntentEmulator, txHash: string) =>
  (
    await env.store.transaction("read", (sql) =>
      readIntentEventsIn(sql, Buffer.from(txHash, "hex")),
    )
  ).map((event) => event.kind);

/** The hold a seam run failed with, or the run's outcome when it did not. */
const heldReason = (exit: Exit.Exit<unknown, unknown>): string => {
  if (Exit.isSuccess(exit)) return "sent";
  const failure = Cause.failureOption(exit.cause);
  return failure._tag === "Some" && failure.value instanceof IntentSubmitHeld
    ? failure.value.reason
    : Cause.pretty(exit.cause);
};

describe("the submit seam sends only on S6's decision in the record's transaction", () => {
  it("records a plan a rewind passed before the record as stale_at_write and never sends it", async () => {
    const env = await open();
    await landForeignBlock(env);
    const plan = await env.plan();
    // The rewind lands between the plan and the record.
    await rewindToOrigin(env);
    const { lucid, signed, txHash, sent } = await signedOwnPayment(env);
    const exit = await Effect.runPromiseExit(
      submitSignedTxWithRecovery(
        lucid,
        signed,
        txHash,
        fundingIntent("planned-across-rewind", plan),
      ).pipe(Effect.provide(env.journalLayer)),
    );
    expect(heldReason(exit)).toBe("intent_stale_at_write");
    expect(sent).toEqual([]);
    expect(await eventKinds(env, txHash)).toEqual(["signed", "stale_at_write"]);
    // Opposite polarity: a plan opened after the rewind sends its bytes (a
    // different transaction: the held one is S6's).
    const fresh = await signedOwnPayment(env, 3_000_000n);
    const sentFresh = await Effect.runPromiseExit(
      submitSignedTxWithRecovery(
        fresh.lucid,
        fresh.signed,
        fresh.txHash,
        fundingIntent("planned-after-rewind", await env.plan()),
      ).pipe(Effect.provide(env.journalLayer)),
    );
    expect(heldReason(sentFresh)).toBe("sent");
    expect(fresh.sent).toEqual([fresh.signed.toCBOR()]);
  });

  it("holds a retry whose view a rewind removed after the record, and leaves the bytes to S6", async () => {
    const env = await open();
    await landForeignBlock(env);
    const plan = await env.plan();
    const { lucid, signed, txHash, provider, sent } =
      await signedOwnPayment(env);
    let attempts = 0;
    const send = provider.submitTx.bind(provider);
    provider.submitTx = async (tx) => {
      attempts += 1;
      if (attempts > 1) return send(tx);
      // The first send is lost, and a rewind removes the record's view
      // before the retry.
      await rewindToOrigin(env);
      throw new Error("provider connection reset");
    };
    const exit = await Effect.runPromiseExit(
      submitSignedTxWithRecovery(
        lucid,
        signed,
        txHash,
        fundingIntent("rewound-before-retry", plan),
        { sleep: () => Effect.void },
      ).pipe(Effect.provide(env.journalLayer)),
    );
    expect(heldReason(exit)).toBe("intent_view_stale");
    expect(attempts).toBe(1);
    expect(sent).toEqual([]);
    expect(await eventKinds(env, txHash)).toEqual(["signed", "submit_attempt"]);
    // S6 decides under the current view: its input survived and it is
    // wanted, so it sends the exact journaled bytes.
    expect(await env.stage.run()).toEqual([]);
    expect(statusOf(env, txHash)?.action).toBe("resubmit");
    expect(env.sent.map((bytes) => bytes.toString("hex"))).toEqual([
      signed.toCBOR(),
    ]);
  });

  it("holds a send while the follower's view is behind wall-clock time past the bound, and sends once it is within", async () => {
    const env = await open();
    const { lucid, signed, txHash, sent } = await signedOwnPayment(env);
    const intent = fundingIntent("node-behind", await env.plan());
    // A bound of 1 ms: the origin's slot time is already further behind.
    await new Promise((resolve) => setTimeout(resolve, 10));
    const behind = intentJournalOver(env.sql, () => false, {
      nodeBehindMs: 1,
    });
    const held = await Effect.runPromiseExit(
      submitSignedTxWithRecovery(lucid, signed, txHash, intent).pipe(
        Effect.provide(Layer.succeed(IntentJournal, behind)),
      ),
    );
    expect(heldReason(held)).toBe(FOLLOWER_NODE_BEHIND);
    expect(sent).toEqual([]);
    expect(await eventKinds(env, txHash)).toEqual(["signed"]);
    // Within the default bound the same bytes go out.
    const within = await Effect.runPromiseExit(
      submitSignedTxWithRecovery(lucid, signed, txHash, intent).pipe(
        Effect.provide(env.journalLayer),
      ),
    );
    expect(heldReason(within)).toBe("sent");
    expect(sent).toEqual([signed.toCBOR()]);
    expect(await eventKinds(env, txHash)).toEqual(["signed", "submit_attempt"]);
  });
});
