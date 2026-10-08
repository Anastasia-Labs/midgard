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
 */
import { decodeTransaction } from "@al-ft/midgard-l1-follower";
import { Effect, Layer } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  IntentJournal,
  journaledIntent,
} from "../src/services/intent-journal.js";
import { handleSignSubmit } from "../src/transactions/utils.js";
import {
  type IntentEmulator,
  openIntentEmulator,
  signedPayment,
} from "./helpers/intent-journal-emulator.js";
import {
  DROP_ALL_TIMEOUT_MS,
  testDatabases,
} from "./helpers/l1-events-store.js";

const databases = testDatabases();
const opened: IntentEmulator[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((env) => env.close()));
});
afterAll(async () => {
  await databases.dropAll();
}, DROP_ALL_TIMEOUT_MS);

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
const fundingIntent = (label: string) =>
  journaledIntent(
    "reserve_payout",
    `reserve_payout:${label}:add_funds`,
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
    await env.record(fundingIntent("honest"), tx.cbor, tx.hash);
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
      fundingIntent("adversarial"),
      journaled.cbor,
      journaled.hash,
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
      handleSignSubmit(lucid, unsigned, fundingIntent("seam")).pipe(
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
