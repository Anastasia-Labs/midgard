/**
 * Reward-account registrations funded from the node's wallet view (plan
 * §8.5, I2b) on a Lucid emulator, over the node's follower store and intent
 * journal:
 *
 * - the PHAS membership registration builds from the view, signs over it and
 *   lands;
 * - with every own output held by a live intent (reserved as its
 *   collateral), it is refused by name before submission, though the
 *   provider's wallet still offers the held outputs.
 */
import "./helpers/follower-emulator-installed.js";

import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { ensurePhasMembershipRewardAccountRegisteredProgram } from "../src/transactions/phas-membership-registration.js";
import {
  causeTrail,
  holdWholeWallet,
  inputsOf,
  refOf,
  viewOf,
} from "./helpers/builder-wallet-view.js";
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
  expect(await env.stage.run()).toEqual([]);
  return env;
};

const registerPhas = (env: IntentEmulator, lucid: LucidEvolution) =>
  Effect.runPromise(
    Effect.either(
      ensurePhasMembershipRewardAccountRegisteredProgram(lucid).pipe(
        Effect.provide(env.journalLayer),
      ),
    ),
  );

const statusOf = (env: IntentEmulator, hash: string) =>
  env.stage.lastReport()!.entry(Buffer.from(hash, "hex"))?.status;

/** The tx the emulator accepted last, as hex. */
const lastAccepted = (env: IntentEmulator): string =>
  [...env.accepted.values()].at(-1)!.toString("hex");

const expectLandsFromView = async (
  env: IntentEmulator,
  txHash: string,
  viewed: readonly UTxO[],
) => {
  const allowed = new Set(viewed.map(refOf));
  expect(inputsOf(lastAccepted(env)).every((ref) => allowed.has(ref))).toBe(
    true,
  );
  await env.follow();
  expect(await env.stage.run()).toEqual([]);
  expect(statusOf(env, txHash)).toMatchObject({ kind: "landed" });
  const after = await viewOf(env, await env.wallet());
  expect(after.held.size).toBe(0);
  expect(after.utxos.map(refOf)).toContain(`${txHash}#0`);
};

describe("registrations funded from the node wallet view", () => {
  it("registers the PHAS membership reward account from the view, signed over it, and it lands", async () => {
    const env = await open();
    const lucid = await env.wallet();
    const viewed = (await viewOf(env, lucid)).utxos;
    const registered = await registerPhas(env, lucid);
    if (registered._tag === "Left") throw registered.left;
    expect(registered.right.status).toBe("registration_submitted");
    await expectLandsFromView(env, registered.right.txHash!, viewed);
  });

  it("refuses the PHAS registration by name when a live intent reserves the other own output as collateral", async () => {
    const env = await open();
    const lucid = await env.wallet();
    const held = await holdWholeWallet(env, lucid, "collateral");
    expect(held).toHaveLength(2);
    const before = env.accepted.size;
    const refused = await registerPhas(env, lucid);
    expect(refused._tag).toBe("Left");
    expect(causeTrail(refused)).toContain("wallet_view_empty");
    expect(env.accepted.size).toBe(before);
    expect((await lucid.wallet().getUtxos()).map(refOf).sort()).toEqual(
      held.map(refOf).sort(),
    );
  });
});
