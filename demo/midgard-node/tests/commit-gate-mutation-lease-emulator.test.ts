/**
 * A commit's signed intent is written only under the mutation lease its
 * journal names, and only to a journal still pending submission; the gate
 * runs before broadcast, so a refusal sends nothing. A commit worker holding
 * its lease signs and sends its commit; one that outlived its lease
 * (released, or expired) signs nothing, and neither does one whose journal
 * the next lease holder abandoned as unsigned.
 */
import "./helpers/follower-emulator-installed.js";

import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { MUTATION_LEASE_NOT_HELD } from "../src/database/pendingBlockFinalizations.hold-mutation-lease.js";
import { FollowerWrite } from "../src/services/follower-write-gate.js";
import {
  followerPermit,
  journaled,
  MODELED_LEASE_TOKEN,
  pendingIntent,
  preparePendingBlock,
  submitCommit,
} from "./helpers/commit-gate-emulator.js";
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

type Ending = "held" | "released" | "expired" | "abandoned";

/**
 * Prepares a pending block for a payment, ends what the worker's signing
 * rests on as `ending` says (`held` ends nothing), then submits the payment
 * through the gate.
 */
const signAfter = async (ending: Ending) => {
  const env = await openIntentEmulator(databases, { nodeSchema: true });
  opened.push(env);
  expect(await env.stage.run()).toEqual([]);
  const permit = await followerPermit(env);
  const lucid = await env.wallet();
  const unsigned = await lucid
    .newTx()
    .pay.ToAddress(env.payee.address, { lovelace: 2_000_000n })
    .complete();
  const headerHash = await preparePendingBlock(
    env,
    permit,
    Buffer.from(unsigned.toHash(), "hex"),
  );
  if (ending === "abandoned")
    await env.runtime.runPromise(
      PendingBlockFinalizationsDB.markUnsubmittedAbandoned(headerHash).pipe(
        Effect.provideService(FollowerWrite, permit),
      ),
    );
  // The worker's lease row as it is once it ended: the gate's lease helper
  // then uses it as it stands.
  else if (ending !== "held")
    await env.runtime.runPromise(
      env.sql`INSERT INTO state_queue_mutation_leases
        (token, scope, holder, status, expires_at, released_at)
        VALUES (${MODELED_LEASE_TOKEN}, 'state_queue', 'commit-worker',
          ${ending === "released" ? "released" : "active"},
          ${
            ending === "released"
              ? env.sql`NOW() + INTERVAL '1 hour'`
              : env.sql`NOW() - INTERVAL '1 second'`
          },
          ${ending === "released" ? env.sql`NOW()` : null})`,
    );
  const submitted = await submitCommit(
    env,
    permit,
    lucid,
    unsigned,
    headerHash,
  );
  return {
    ...submitted,
    env,
    journaled: await journaled(env),
    intent: await pendingIntent(env, headerHash),
    sent: env.sent,
  };
};

describe("the commit gate after the worker's lease or journal ended", () => {
  it("records the signed intent under a held lease and sends the commit", async () => {
    const result = await signAfter("held");
    expect(result.text).toBe("success");
    expect(result.direct).toHaveLength(1);
    expect(result.journaled).toHaveLength(1);
    expect(result.intent?.signed_tx_cbor?.toString("hex")).toBe(
      result.direct[0],
    );
    // The commit lands through the follower provider.
    result.env.emulator.awaitBlock(1);
    await result.env.follow();
  });

  it.each(["released", "expired"] as const)(
    "refuses a signed intent under a %s lease and sends nothing",
    async (ending) => {
      const result = await signAfter(ending);
      expect(result.exit._tag).toBe("Failure");
      expect(result.text).toContain(MUTATION_LEASE_NOT_HELD);
      expect(result.direct).toEqual([]);
      expect(result.sent).toEqual([]);
      expect(result.journaled).toEqual([]);
      expect(result.intent).toMatchObject({
        intended_tx_hash: null,
        signed_tx_cbor: null,
      });
    },
  );

  it("refuses a signed intent for a journal abandoned as unsigned and sends nothing", async () => {
    const result = await signAfter("abandoned");
    expect(result.exit._tag).toBe("Failure");
    expect(result.text).toContain(
      "Signed intent conflicts with the pending journal",
    );
    expect(result.direct).toEqual([]);
    expect(result.sent).toEqual([]);
    expect(result.journaled).toEqual([]);
    expect(result.intent).toMatchObject({
      intended_tx_hash: null,
      signed_tx_cbor: null,
    });
  });
});
