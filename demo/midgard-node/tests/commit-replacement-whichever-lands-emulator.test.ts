/**
 * Whichever lands wins (plan §8.3, I3) for an own commit and its
 * replacement on the same tail, on a Lucid emulator through the production
 * intent journal and S6 (`createNodeIntentStage`).
 *
 * Stand-in: the old commit and its replacement are two signed payments that
 * spend the same own input, as the old and the new commit spend the same
 * tail node; both are journaled, the old one kept when the replacement is
 * built. The landed-block side (revival, disposal) is pinned on the node
 * database by `landed-blocks-own-journals.test.ts` and under the fork
 * simulator by `landed-blocks-fork-sim.test.ts`.
 *
 * - (a) The old commit lands: it is followed, and the replacement is
 *   superseded and never sent, at that tip or the next.
 * - (b) The replacement lands: the old commit is superseded and never sent.
 * - Neither outcome holds anything.
 * - Adversarial: a transaction the node did not journal takes the tail
 *   first: both are dead (a foreign conflict), and neither is sent.
 */
import { decodeTransaction } from "@al-ft/midgard-l1-follower";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  type IntentPlan,
  journaledIntent,
} from "../src/services/intent-journal.js";
import { RECORD_ONLY } from "./helpers/intent-journal.js";
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

const intent = (label: string, plan: IntentPlan) =>
  journaledIntent(
    "reserve_payout",
    `reserve_payout:${label}:add_funds`,
    plan,
    Buffer.alloc(36, label.length),
  );

const spent = (cbor: string) =>
  decodeTransaction(Buffer.from(cbor, "hex")).inputs.map(
    (input) => `${input.txHash.toString("hex")}#${input.index}`,
  );

/** The old commit and its replacement, both journaled, on one tail input. */
const oldAndReplacement = async (env: IntentEmulator) => {
  const oldPlan = await env.plan();
  const old = await signedPayment(
    await env.wallet(),
    env.payee.address,
    2_000_000n,
  );
  await env.record(intent("old", oldPlan), old.cbor, old.hash, RECORD_ONLY);
  const replacementPlan = await env.plan();
  const replacement = await signedPayment(
    await env.wallet(),
    env.payee.address,
    3_000_000n,
  );
  await env.record(
    intent("replacement", replacementPlan),
    replacement.cbor,
    replacement.hash,
    RECORD_ONLY,
  );
  expect(spent(replacement.cbor)).toEqual(spent(old.cbor));
  return { old, replacement };
};

const entryOf = (env: IntentEmulator, hash: string) =>
  env.stage.lastReport()!.entry(Buffer.from(hash, "hex"));

const sentHashes = (env: IntentEmulator) =>
  env.sent.map((bytes) => decodeTransaction(bytes).hash.toString("hex"));

/** Lands `winner` and checks `loser` is superseded at this tip and the next. */
const landAndExpect = async (
  env: IntentEmulator,
  winner: { cbor: string; hash: string },
  loser: { hash: string },
) => {
  await env.emulator.submitTx(winner.cbor);
  env.emulator.awaitBlock(1);
  await env.follow();
  for (let tip = 0; tip < 2; tip += 1) {
    expect(await env.stage.run()).toEqual([]);
    expect(entryOf(env, winner.hash)).toMatchObject({
      action: "follow",
      status: { kind: "landed" },
    });
    expect(entryOf(env, loser.hash)).toMatchObject({
      action: "superseded",
      status: { kind: "conflicted", ownSpender: true },
    });
    env.emulator.awaitBlock(1);
    await env.follow();
  }
  expect(sentHashes(env)).not.toContain(loser.hash);
  expect(env.emulator.transactionHistory[loser.hash]).toBeUndefined();
};

describe("an own commit and its replacement on the same tail", () => {
  it("(a) the old commit lands: it is followed and the replacement is superseded, never sent", async () => {
    const env = await open();
    const { old, replacement } = await oldAndReplacement(env);
    await landAndExpect(env, old, replacement);
  });

  it("(b) the replacement lands: the old commit is superseded, never sent", async () => {
    const env = await open();
    const { old, replacement } = await oldAndReplacement(env);
    await landAndExpect(env, replacement, old);
  });

  it("a foreign transaction that takes the tail first leaves both dead, and neither is sent", async () => {
    const env = await open();
    const { old, replacement } = await oldAndReplacement(env);
    // The same wallet, used outside the node, spends the same input.
    const foreign = await signedPayment(
      await env.wallet(),
      env.payee.address,
      4_000_000n,
    );
    expect(spent(foreign.cbor)).toEqual(spent(old.cbor));
    await env.emulator.submitTx(foreign.cbor);
    env.emulator.awaitBlock(1);
    await env.follow();
    expect(await env.stage.run()).toEqual([]);
    for (const loser of [old, replacement])
      expect(entryOf(env, loser.hash)).toMatchObject({
        action: "dead",
        status: { kind: "conflicted", ownSpender: false },
      });
    expect(env.sent).toEqual([]);
  });
});
