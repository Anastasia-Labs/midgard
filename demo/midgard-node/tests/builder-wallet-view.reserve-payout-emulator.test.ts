/**
 * The reserve payout steps funded from the node's wallet view (plan §8.5,
 * I2b) on the real compiled validators in the emulator, with the node's
 * follower store and intent journal attached to the settling wallet:
 *
 * - absorb, initialize, add funds and conclude each select their fee input,
 *   coins and collateral from the view: a coin a live own intent spends is
 *   never an input or collateral of any of them, though the provider's
 *   wallet still offers it, and all four land;
 * - with every own coin held by a live intent, the absorption is refused by
 *   name before anything is built.
 */
import {
  generateEmulatorAccount,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import type { IntentJournal } from "../src/services/intent-journal.js";
import {
  submitAbsorbConfirmedDepositToReserveProgram,
  submitAddReserveFundsToPayoutProgram,
  submitConcludePayoutProgram,
  submitInitializePayoutProgram,
} from "../src/transactions/reserve-payout.js";
import {
  causeTrail,
  collateralsOf,
  holdCoin,
  holdWholeWallet,
  inputsOf,
  refOf,
  viewOf,
} from "./helpers/builder-wallet-view.js";
import {
  attachIntentFollower,
  type IntentEmulator,
} from "./helpers/intent-journal-emulator.js";
import {
  DROP_ALL_TIMEOUT_MS,
  testDatabases,
} from "./helpers/l1-events-store.js";
import { makeReserveLifecycleBuilderFixture } from "./reserve-payout-builders.make-reserve-lifecycle-builder-fixture.js";
import { findUtxoWithUnit } from "./reserve-payout-builders.submit-with-wallet.js";

const databases = testDatabases();
const opened: IntentEmulator[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((env) => env.close()));
});
afterAll(async () => {
  await databases.dropAll();
}, DROP_ALL_TIMEOUT_MS);

type Fixture = Awaited<ReturnType<typeof makeReserveLifecycleBuilderFixture>>;

/**
 * The fixture's operator pays a fresh settling wallet one output per amount
 * from its largest plain coin (never a reference-script output), and the
 * node's follower is attached to the settling wallet afterwards.
 */
const settlingNode = async (
  fixture: Fixture,
  amounts: readonly bigint[],
): Promise<IntentEmulator> => {
  const settler = generateEmulatorAccount({ lovelace: 0n });
  const [source] = (await fixture.lucid.utxosAt(fixture.operator.address))
    .filter(
      (utxo) =>
        utxo.scriptRef == null &&
        Object.keys(utxo.assets).every((unit) => unit === "lovelace"),
    )
    .sort((a, b) =>
      (b.assets.lovelace ?? 0n) > (a.assets.lovelace ?? 0n) ? 1 : -1,
    );
  const funding = amounts.reduce(
    (tx, lovelace) => tx.pay.ToAddress(settler.address, { lovelace }),
    fixture.lucid.newTx().collectFrom([source!]),
  );
  const signed = await (
    await funding.complete({
      presetWalletInputs: [source!],
      coinSelection: false,
    })
  ).sign
    .withWallet()
    .complete();
  await fixture.lucid.awaitTx(await signed.submit());
  const env = await attachIntentFollower(databases, {
    emulator: fixture.emulator,
    own: settler,
    payee: generateEmulatorAccount({ lovelace: 0n }),
    protocol: {
      contracts: fixture.contracts,
      referenceScriptAddresses: [fixture.referenceScriptsAddress],
    },
  });
  opened.push(env);
  expect(await env.stage.run()).toEqual([]);
  return env;
};

const run = <A, E>(
  env: IntentEmulator,
  effect: Effect.Effect<A, E, IntentJournal>,
) => Effect.runPromise(Effect.either(Effect.provide(effect, env.journalLayer)));

const lastAccepted = (env: IntentEmulator): string =>
  [...env.accepted.values()].at(-1)!.toString("hex");

/** Runs one step; it must land, never touching `held`, its block followed. */
const step = async (
  env: IntentEmulator,
  held: UTxO,
  effect: Effect.Effect<string, unknown, IntentJournal>,
): Promise<string> => {
  const outcome = await run(env, effect);
  if (outcome._tag === "Left") throw outcome.left;
  const cbor = lastAccepted(env);
  expect(inputsOf(cbor)).not.toContain(refOf(held));
  expect(collateralsOf(cbor)).not.toContain(refOf(held));
  await env.follow();
  return outcome.right;
};

describe("reserve payout steps funded from the node wallet view", () => {
  it("absorbs, initializes, funds and concludes from the view, never touching a coin a live intent holds, and all four land", async () => {
    const fixture = await makeReserveLifecycleBuilderFixture();
    const env = await settlingNode(fixture, [60_000_000n, 30_000_000n]);
    const lucid: LucidEvolution = await env.wallet();
    const [held] = (await viewOf(env, lucid)).utxos.filter(
      (utxo) => utxo.assets.lovelace === 60_000_000n,
    );
    await holdCoin(env, lucid, held!);
    expect((await lucid.wallet().getUtxos()).map(refOf)).toContain(
      refOf(held!),
    );
    const {
      contracts,
      deposit,
      depositMembershipProof,
      hubOracleRefInput,
      payoutUnit,
      referenceScripts,
      referenceScriptsAddress,
      reserveAddress,
      settlementRefInput,
      withdrawal,
      withdrawalMembershipProof,
    } = fixture;

    const hashes = [
      await step(
        env,
        held!,
        submitAbsorbConfirmedDepositToReserveProgram(lucid, contracts, {
          deposit,
          hubOracleRefInput,
          membershipProof: depositMembershipProof,
          referenceScriptsAddress,
          nowMs: fixture.emulator.now(),
          referenceScripts,
          settlementRefInput,
        }),
      ),
    ];
    const reserveInput = (await lucid.utxosAt(reserveAddress)).find(
      (utxo) =>
        utxo.assets.lovelace === 8_000_000n &&
        Object.keys(utxo.assets).length === 1,
    )!;
    hashes.push(
      await step(
        env,
        held!,
        submitInitializePayoutProgram(lucid, contracts, {
          hubOracleRefInput,
          membershipProof: withdrawalMembershipProof,
          referenceScriptsAddress,
          nowMs: fixture.emulator.now(),
          referenceScripts,
          settlementRefInput,
          withdrawal,
        }),
      ),
    );
    hashes.push(
      await step(
        env,
        held!,
        submitAddReserveFundsToPayoutProgram(
          lucid,
          contracts,
          {
            hubOracleRefInput,
            payoutInput: findUtxoWithUnit(
              await lucid.utxosAt(contracts.payout.spendingScriptAddress),
              payoutUnit,
            ),
            referenceScripts,
            reserveInput,
          },
          withdrawal.idCbor,
        ),
      ),
    );
    hashes.push(
      await step(
        env,
        held!,
        submitConcludePayoutProgram(
          lucid,
          contracts,
          {
            hubOracleRefInput,
            payoutInput: findUtxoWithUnit(
              await lucid.utxosAt(contracts.payout.spendingScriptAddress),
              payoutUnit,
            ),
            referenceScripts,
          },
          withdrawal.idCbor,
        ),
      ),
    );

    expect(await env.stage.run()).toEqual([]);
    for (const hash of hashes)
      expect(
        env.stage.lastReport()!.entry(Buffer.from(hash, "hex"))?.status,
      ).toMatchObject({ kind: "landed" });
  }, 600_000);

  it("refuses the absorption by name when a live intent holds every own coin", async () => {
    const fixture = await makeReserveLifecycleBuilderFixture();
    const env = await settlingNode(fixture, [60_000_000n]);
    const lucid = await env.wallet();
    await holdWholeWallet(env, lucid, "inputs");
    const before = env.accepted.size;
    const refused = await run(
      env,
      submitAbsorbConfirmedDepositToReserveProgram(lucid, fixture.contracts, {
        deposit: fixture.deposit,
        hubOracleRefInput: fixture.hubOracleRefInput,
        membershipProof: fixture.depositMembershipProof,
        referenceScriptsAddress: fixture.referenceScriptsAddress,
        nowMs: fixture.emulator.now(),
        referenceScripts: fixture.referenceScripts,
        settlementRefInput: fixture.settlementRefInput,
      }),
    );
    expect(refused._tag).toBe("Left");
    expect(causeTrail(refused)).toContain("wallet_view_empty");
    expect(env.accepted.size).toBe(before);
  }, 600_000);
});
