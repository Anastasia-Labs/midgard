/**
 * Operator exits funded from the node's wallet view (plan §8.5, I2b) on the
 * real compiled validators in the emulator, with the node's follower store
 * and intent journal attached to the submitting wallet:
 *
 * - voluntary retirement and bond recovery build from the view: a coin a
 *   live own intent spends is neither an input nor collateral, though the
 *   provider's wallet still offers it, and both land;
 * - with every own coin held by a live intent, retirement is refused by the
 *   funding preflight over the view, before anything is built;
 * - the exact-fee builders (forced retirement, duplicate slashing) take
 *   their collateral and funding from the view: the largest coin, held by a
 *   live intent, is never their collateral, and both land.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { generateEmulatorAccount } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import type { IntentJournal } from "../src/services/intent-journal.js";
import {
  type OperatorEconomics,
  recoverOperatorBondProgram,
  retireOperatorProgram,
} from "../src/transactions/operators/exit.js";
import { slashDuplicateOperatorProgram } from "../src/transactions/operators/exit.slash-duplicate-operator-program.js";
import { OperatorFundingShortfall } from "../src/transactions/operators/funding-preflight.js";
import {
  planTakeoverProgram,
  submitInactivityStrikeProgram,
} from "../src/transactions/operators/takeover.js";
import {
  collateralsOf,
  fundOwn,
  holdCoin,
  holdWholeWallet,
  inputsOf,
  largestCoin,
  refOf,
  viewOf,
} from "./helpers/builder-wallet-view.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import {
  attachIntentFollower,
  type IntentEmulator,
} from "./helpers/intent-journal-emulator.js";
import {
  DROP_ALL_TIMEOUT_MS,
  testDatabases,
} from "./helpers/l1-events-store.js";
import {
  advanceEmulatorPastUnixTime,
  appointFirstSchedulerOperator,
  initOperatorInactivityFixture,
  type OperatorInactivityFixture,
  strikeOperatorToMaxStrikes,
} from "./helpers/operator-inactivity.js";
import { initOperatorExitFixture } from "./operator-exit-emulator.build-operator-exit-snapshot.js";
import { resyncWallet } from "./operator-exit-emulator.build-retire-tx.js";
import { forceRegisterOperator } from "./operator-exit-emulator.force-activate-operator.js";

const ECONOMICS: OperatorEconomics = {
  requiredBondLovelace: 900_000_000n,
  slashingPenaltyLovelace: 500_000_000n,
  inactivitySlashingPenaltyLovelace: 100_000_000n,
};

const databases = testDatabases();
const opened: IntentEmulator[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((env) => env.close()));
});
afterAll(async () => {
  await databases.dropAll();
}, DROP_ALL_TIMEOUT_MS);

const attach = async (
  options: Parameters<typeof attachIntentFollower>[1],
): Promise<IntentEmulator> => {
  const env = await attachIntentFollower(databases, options);
  opened.push(env);
  // The first pass seeds the own wallet as the emulator holds it.
  expect(await env.stage.run()).toEqual([]);
  return env;
};

const run = <A, E>(
  env: IntentEmulator,
  effect: Effect.Effect<A, E, IntentJournal>,
) => Effect.runPromise(Effect.either(Effect.provide(effect, env.journalLayer)));

/** The transaction the emulator accepted last, as hex. */
const lastAccepted = (env: IntentEmulator): string =>
  [...env.accepted.values()].at(-1)!.toString("hex");

const statusOf = (env: IntentEmulator, hash: string) =>
  env.stage.lastReport()!.entry(Buffer.from(hash, "hex"))?.status;

/** `held` was neither spent nor collateral in the last accepted transaction. */
const expectUntouched = (env: IntentEmulator, held: string) => {
  const cbor = lastAccepted(env);
  expect(inputsOf(cbor)).not.toContain(held);
  expect(collateralsOf(cbor)).not.toContain(held);
};

const expectLanded = async (env: IntentEmulator, txHash: string) => {
  await env.follow();
  expect(await env.stage.run()).toEqual([]);
  expect(statusOf(env, txHash)).toMatchObject({ kind: "landed" });
};

const protocolOf = (fixture: OperatorInactivityFixture) => ({
  contracts: fixture.contracts,
  referenceScriptAddresses: [fixture.referenceScriptsAddress],
});

/** The two-operator deployment with the scheduler appointed. */
const appointedFixture = async () => {
  const fixture = await initOperatorInactivityFixture(2);
  const appointed = await appointFirstSchedulerOperator(fixture);
  const idle = fixture.operators.find(
    ({ keyHash }) => keyHash !== appointed.operatorKeyHash,
  )!;
  const scheduled = fixture.operators.find(
    ({ keyHash }) => keyHash === appointed.operatorKeyHash,
  )!;
  return { fixture, idle, scheduled };
};

/** Drives `struck` to the strike limit, `successor` taking the first shift. */
const exhaustStrikes = async (
  fixture: OperatorInactivityFixture,
  successorKeyHash: string,
  struckKeyHash: string,
) => {
  const successorLucid = await fixture.lucidFor(successorKeyHash);
  const early = await Effect.runPromise(
    planTakeoverProgram(successorLucid, fixture.contracts),
  );
  if (early.plan.kind !== "not-yet") throw new Error("expected not-yet");
  advanceEmulatorPastUnixTime(fixture.emulator, early.plan.thresholdMs);
  const due = await Effect.runPromise(
    planTakeoverProgram(successorLucid, fixture.contracts),
  );
  if (due.plan.kind !== "ready") throw new Error("expected ready");
  await runWithoutFollower(
    submitInactivityStrikeProgram(
      successorLucid,
      fixture.contracts,
      fixture.referenceScriptsAddress,
      { ...due, plan: due.plan },
    ),
  );
  const exhausted = await strikeOperatorToMaxStrikes(fixture, struckKeyHash);
  expect(exhausted.inactivityStrikes).toBe(SDK.MAX_INACTIVITY_STRIKES);
};

describe("operator exits funded from the node wallet view", () => {
  it("retires and recovers the bond from the view, never touching a coin a live intent holds, and both land", async () => {
    const { fixture, idle, scheduled } = await appointedFixture();
    const env = await attach({
      emulator: fixture.emulator,
      own: idle,
      payee: scheduled,
      protocol: protocolOf(fixture),
    });
    const lucid = await env.wallet();
    await fundOwn(env, 50_000_000n, await fixture.lucidFor(scheduled.keyHash));
    const held = largestCoin((await viewOf(env, lucid)).utxos);
    await holdCoin(env, lucid, held);
    // The provider still offers the held coin; the view does not.
    expect((await lucid.wallet().getUtxos()).map(refOf)).toContain(refOf(held));
    expect((await viewOf(env, lucid)).utxos.map(refOf)).not.toContain(
      refOf(held),
    );

    const retired = await run(
      env,
      retireOperatorProgram(
        lucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        {
          operatorKeyHash: idle.keyHash,
          mode: "voluntary",
          economics: ECONOMICS,
        },
      ),
    );
    if (retired._tag === "Left") throw retired.left;
    expect(retired.right.retiredBondLovelace).toBe(900_000_000n);
    expectUntouched(env, refOf(held));
    await expectLanded(env, retired.right.txHash);

    const recovered = await run(
      env,
      recoverOperatorBondProgram(
        lucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        { operatorKeyHash: idle.keyHash },
      ),
    );
    if (recovered._tag === "Left") throw recovered.left;
    expect(recovered.right.bondLovelace).toBe(900_000_000n);
    expectUntouched(env, refOf(held));
    await expectLanded(env, recovered.right.txHash);
  }, 600_000);

  it("refuses retirement at the funding preflight over the view when a live intent holds every own coin", async () => {
    const { fixture, idle, scheduled } = await appointedFixture();
    const env = await attach({
      emulator: fixture.emulator,
      own: idle,
      payee: scheduled,
      protocol: protocolOf(fixture),
    });
    const lucid = await env.wallet();
    const held = await holdWholeWallet(env, lucid, "inputs");
    const before = env.accepted.size;
    const refused = await run(
      env,
      retireOperatorProgram(
        lucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        {
          operatorKeyHash: idle.keyHash,
          mode: "voluntary",
          economics: ECONOMICS,
        },
      ),
    );
    expect(refused._tag).toBe("Left");
    if (refused._tag === "Left") {
      expect(refused.left).toBeInstanceOf(OperatorFundingShortfall);
      expect(
        (refused.left as OperatorFundingShortfall).preflight.availableLovelace,
      ).toBe(0n);
    }
    expect(env.accepted.size).toBe(before);
    expect((await lucid.wallet().getUtxos()).map(refOf).sort()).toEqual(
      held.map(refOf).sort(),
    );
  }, 600_000);

  it("force-retires with collateral and funding from the view, never the larger coin a live intent holds, and it lands", async () => {
    const { fixture, idle, scheduled } = await appointedFixture();
    // The idle operator takes the first shift and strikes the scheduled one
    // up to the limit; then it force-retires it from its own node.
    await exhaustStrikes(fixture, idle.keyHash, scheduled.keyHash);
    const env = await attach({
      emulator: fixture.emulator,
      own: idle,
      payee: scheduled,
      protocol: protocolOf(fixture),
    });
    const lucid = await env.wallet();
    const spare = await fundOwn(
      env,
      400_000_000n,
      await fixture.lucidFor(scheduled.keyHash),
    );
    const held = largestCoin((await viewOf(env, lucid)).utxos);
    expect(refOf(held)).not.toBe(refOf(spare));
    await holdCoin(env, lucid, held);

    const forced = await run(
      env,
      retireOperatorProgram(
        lucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        {
          operatorKeyHash: scheduled.keyHash,
          mode: "forced-inactivity",
          economics: ECONOMICS,
        },
      ),
    );
    if (forced._tag === "Left") throw forced.left;
    expect(forced.right.retiredBondLovelace).toBe(800_000_000n);
    expectUntouched(env, refOf(held));
    expect(collateralsOf(lastAccepted(env))).toContain(refOf(spare));
    await expectLanded(env, forced.right.txHash);
  }, 600_000);

  it("slashes a duplicate registration with collateral from the view, never the larger coin a live intent holds, and it lands", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid: operator, contracts, operatorKeyHash, emulator } = fixture;
    await forceRegisterOperator({
      fixture,
      operatorLucid: operator,
      operatorKeyHash,
    });
    emulator.awaitSlot(3);
    await forceRegisterOperator({
      fixture,
      operatorLucid: operator,
      operatorKeyHash,
    });

    const slasher = generateEmulatorAccount({ lovelace: 0n });
    const env = await attach({
      emulator,
      own: slasher,
      payee: generateEmulatorAccount({ lovelace: 0n }),
      protocol: {
        contracts,
        referenceScriptAddresses: [
          await fixture.referenceScriptsLucid.wallet().address(),
        ],
      },
    });
    const lucid = await env.wallet();
    await resyncWallet(operator);
    const held = await fundOwn(env, 2_000_000_000n, operator);
    await resyncWallet(operator);
    const spare = await fundOwn(env, 1_000_000_000n, operator);
    await holdCoin(env, lucid, held);

    const slashed = await run(
      env,
      slashDuplicateOperatorProgram(
        lucid,
        contracts,
        await fixture.referenceScriptsLucid.wallet().address(),
        { operatorKeyHash, economics: ECONOMICS },
      ),
    );
    if (slashed._tag === "Left") throw slashed.left;
    expect(slashed.right.feeLovelace).toBe(500_000_000n);
    expectUntouched(env, refOf(held));
    expect(collateralsOf(lastAccepted(env))).toContain(refOf(spare));
    await expectLanded(env, slashed.right.txHash);
  }, 600_000);
});
