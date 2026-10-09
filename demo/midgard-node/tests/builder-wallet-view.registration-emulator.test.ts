/**
 * Reward-account registrations funded from the node's wallet view (plan
 * §8.5, I2b) on a Lucid emulator, over the node's follower store and intent
 * journal:
 *
 * - the script reward-account and PHAS membership registrations build from
 *   the view, sign over it and land;
 * - with every own output held by a live intent (spent, or reserved as its
 *   collateral), each is refused by name before submission, though the
 *   provider's wallet still offers the held outputs;
 * - a registration built while an own intent is live spends that intent's
 *   predicted change, never the output the intent spends that the provider
 *   still offers until the spend confirms, and both land.
 */
import "./helpers/follower-emulator-installed.js";

import {
  type LucidEvolution,
  scriptFromNative,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { ensurePhasMembershipRewardAccountRegisteredProgram } from "../src/transactions/phas-membership-registration.js";
import { ensureScriptRewardAccountRegisteredProgram } from "../src/transactions/script-reward-registration.js";
import { handleSignSubmitNoConfirmation } from "../src/transactions/utils.js";
import { readSelectedWalletViewInputs } from "../src/transactions/utils.wallet-view.js";
import {
  causeTrail,
  holdWholeWallet,
  inputsOf,
  ownPaymentIntent,
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

/** A native script whose reward account no test registers twice. */
const rewardScript = scriptFromNative({ type: "all", scripts: [] });

const registerScript = (env: IntentEmulator, lucid: LucidEvolution) =>
  Effect.runPromise(
    Effect.either(
      ensureScriptRewardAccountRegisteredProgram(lucid, rewardScript).pipe(
        Effect.provide(env.journalLayer),
      ),
    ),
  );

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
  it("registers a script reward account from the view, signed over it, and it lands", async () => {
    const env = await open();
    const lucid = await env.wallet();
    const viewed = (await viewOf(env, lucid)).utxos;
    const registered = await registerScript(env, lucid);
    if (registered._tag === "Left") throw registered.left;
    expect(registered.right.txHash).not.toBeNull();
    expect(
      (await lucid.rewardAccountAt(registered.right.rewardAddress)).registered,
    ).toBe(true);
    await expectLandsFromView(env, registered.right.txHash!, viewed);
  });

  it("refuses the script reward registration by name when a live intent spends every own output", async () => {
    const env = await open();
    const lucid = await env.wallet();
    const held = await holdWholeWallet(env, lucid, "inputs");
    const before = env.accepted.size;
    const refused = await registerScript(env, lucid);
    expect(refused._tag).toBe("Left");
    expect(causeTrail(refused)).toContain("wallet_view_empty");
    expect(env.accepted.size).toBe(before);
    // The provider's wallet still offers what the intent holds.
    expect((await lucid.wallet().getUtxos()).map(refOf)).toEqual(
      held.map(refOf),
    );
  });

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

  it("chains a registration on a live own intent's predicted change, and both land", async () => {
    const env = await open();
    const lucid = await env.wallet();
    const [seed] = (await viewOf(env, lucid)).utxos;

    // A prior own intent, sent and not yet landed, with change to the wallet;
    // its plan opens before the view read it is built from (S5).
    const plan = await env.plan();
    const inputs = await Effect.runPromise(
      readSelectedWalletViewInputs(lucid, "a payment").pipe(
        Effect.provide(env.journalLayer),
      ),
    );
    const firstHash = await Effect.runPromise(
      handleSignSubmitNoConfirmation(
        lucid,
        await lucid
          .newTx()
          .pay.ToAddress(env.payee.address, { lovelace: 2_000_000n })
          .complete({ presetWalletInputs: inputs }),
        ownPaymentIntent("chain-first", plan),
      ).pipe(Effect.provide(env.journalLayer)),
    );
    const live = await viewOf(env, lucid);
    expect(live.utxos.map(refOf)).toEqual([`${firstHash}#1`]);
    expect([...live.held]).toEqual([refOf(seed!)]);
    // Until the payment confirms, the provider still offers the seed it
    // spends and has not seen its change.
    expect((await lucid.utxosAt(env.own.address)).map(refOf)).toEqual([
      refOf(seed!),
    ]);

    // The registration funds from the view: the change, never the seed the
    // live intent holds (a second spend of it would conflict).
    const registered = await registerScript(env, lucid);
    if (registered._tag === "Left") throw registered.left;
    const secondHash = registered.right.txHash!;
    expect(inputsOf(lastAccepted(env))).toEqual([`${firstHash}#1`]);

    await env.follow();
    expect(await env.stage.run()).toEqual([]);
    expect(statusOf(env, firstHash)).toMatchObject({ kind: "landed" });
    expect(statusOf(env, secondHash)).toMatchObject({ kind: "landed" });
    expect((await viewOf(env, lucid)).utxos.map(refOf)).toEqual([
      `${secondHash}#0`,
    ]);
  });
});
