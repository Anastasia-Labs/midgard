/**
 * Operator-wallet funding by coin selection over the whole operator wallet
 * view (plan §8.5), on the real compiled validators in the emulator:
 *
 * - a wallet whose every output carries a foreign token funds registration,
 *   activation and deregistration, and gets its tokens back in change;
 * - a wallet whose plain coins alone cannot fund registration spends its
 *   token output, and an empty wallet-input set is refused rather than
 *   left to coin selection, which would read the provider wallet;
 * - the reference-script wallet top-up is funded the same way, and pays the
 *   reference-script wallet plain ADA;
 * - with a follower attached, an output a live own intent spends is never
 *   an input or collateral of registration, activation or deregistration;
 * - the funding preflight counts the view's lovelace less the minimum ADA
 *   its tokens need in change.
 */
import "./helpers/follower-emulator-installed.js";

import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { unfundedSkipReason } from "../src/fibers/operator-watchdog.js";
import type { IntentJournal } from "../src/services/intent-journal.js";
import { isOperatorCommandRefusal } from "../src/transactions/operators/commands.js";
import {
  OperatorFundingShortfall,
  OperatorWalletInputsEmpty,
  presetOperatorWalletInputs,
  requireOperatorFundingProgram,
} from "../src/transactions/operators/funding-preflight.js";
import { ensureReferenceScriptWalletWorkingCapital } from "../src/transactions/reference-scripts.ensure-reference-script-wallet-working-capital.js";
import {
  activateOperatorProgram,
  deregisterOperatorProgram,
  registerOperatorProgram,
} from "../src/transactions/register-active-operator.js";
import {
  collateralsOf,
  fundOwn,
  holdCoin,
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
import { testDatabases } from "./helpers/l1-events-store.js";
import {
  amountOf,
  isPlain,
  lovelaceIn,
  makeTokenBearingWallet,
} from "./helpers/token-bearing-wallet.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  EMULATOR_REQUIRED_BOND_LOVELACE,
  initOperatorLifecycleFixture,
} from "./operator-lifecycle-emulator.build-operator-lifecycle-snapshot.js";
import {
  advanceEmulatorPastRegistrationDelay,
  assertOperatorActivatedState,
  fetchRegisteredOperatorNodes,
} from "./operator-lifecycle-emulator.register-second-operator-behind-active-first.js";

const databases = testDatabases();
const opened: IntentEmulator[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((env) => env.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

describe("operator lifecycle funded from a token-only operator wallet", () => {
  it("registers, with the tokens back in change", async () => {
    const { lucid, referenceScriptsLucid, contracts } =
      await initOperatorLifecycleFixture();
    const address = await lucid.wallet().address();
    const unit = await makeTokenBearingWallet(lucid, {});

    const registered = await runWithoutFollower(
      registerOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(registered.registerTxHash).toHaveLength(64);
    expect(await fetchRegisteredOperatorNodes(lucid, contracts)).not.toEqual(
      [],
    );
    expect(amountOf(await lucid.utxosAt(address), unit)).toBe(3n);
  }, 600_000);

  it("activates, with the tokens back in change", async () => {
    const {
      emulator,
      lucid,
      referenceScriptsLucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    } = await initOperatorLifecycleFixture();
    const address = await lucid.wallet().address();
    await runWithoutFollower(
      registerOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    const unit = await makeTokenBearingWallet(lucid, {});

    advanceEmulatorPastRegistrationDelay(emulator);
    const activated = await runWithoutFollower(
      activateOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(activated.activateTxHash).toHaveLength(64);
    await assertOperatorActivatedState({
      lucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    });
    expect(amountOf(await lucid.utxosAt(address), unit)).toBe(3n);
  }, 600_000);

  it("deregisters, with the tokens back in change", async () => {
    const { lucid, referenceScriptsLucid, contracts } =
      await initOperatorLifecycleFixture();
    const address = await lucid.wallet().address();
    await runWithoutFollower(
      registerOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    const unit = await makeTokenBearingWallet(lucid, {});

    const deregistered = await runWithoutFollower(
      deregisterOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(deregistered.deregisterTxHash).toHaveLength(64);
    expect(await fetchRegisteredOperatorNodes(lucid, contracts)).toEqual([]);
    expect(amountOf(await lucid.utxosAt(address), unit)).toBe(3n);
  }, 600_000);

  it("registers from a mixed wallet whose plain coins alone cannot fund it, spending the token output", async () => {
    const { lucid, referenceScriptsLucid, contracts } =
      await initOperatorLifecycleFixture();
    const address = await lucid.wallet().address();
    const unit = await makeTokenBearingWallet(lucid, {
      outputs: 1,
      keep: 1_000_000_000n,
      plainLovelace: 2_000_000n,
    });
    const tokenOutputs = (await lucid.utxosAt(address)).filter(
      (utxo) => !isPlain(utxo),
    );
    expect(tokenOutputs).toHaveLength(1);

    const registered = await runWithoutFollower(
      registerOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(registered.registerTxHash).toHaveLength(64);
    expect(await fetchRegisteredOperatorNodes(lucid, contracts)).not.toEqual(
      [],
    );
    const after = await lucid.utxosAt(address);
    expect(after.map(refOf)).not.toContain(refOf(tokenOutputs[0]!));
    expect(amountOf(after, unit)).toBe(1n);
  }, 600_000);

  it("refuses an empty wallet-input set by name, an unfunded skip for the watchdog", () => {
    const refused = Effect.runSync(
      Effect.either(presetOperatorWalletInputs("register-operator", [])),
    );
    expect(Either.isLeft(refused)).toBe(true);
    const error = Either.isLeft(refused) ? refused.left : undefined;
    expect(error).toBeInstanceOf(OperatorWalletInputsEmpty);
    expect(isOperatorCommandRefusal(error)).toBe(true);
    expect(unfundedSkipReason(error)).toBe("wallet_inputs_empty");
    expect(unfundedSkipReason(new Error("script failed"))).toBeUndefined();
  });
});

describe("reference-script wallet top-up from the operator wallet view", () => {
  const topUpFixture = async () => {
    const operator = generateEmulatorAccount({ lovelace: 500_000_000n });
    const reference = generateEmulatorAccount({ lovelace: 0n });
    const emulator = new Emulator(
      [operator, reference],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const operatorLucid = await Lucid(emulator, "Custom");
    operatorLucid.selectWallet.fromSeed(operator.seedPhrase);
    const referenceLucid = await Lucid(emulator, "Custom");
    referenceLucid.selectWallet.fromSeed(reference.seedPhrase);
    return { operator, reference, operatorLucid, referenceLucid };
  };

  it("funds the top-up from a token-only operator wallet and pays plain ADA", async () => {
    const { operator, reference, operatorLucid, referenceLucid } =
      await topUpFixture();
    const unit = await makeTokenBearingWallet(operatorLucid, {});

    await runWithoutFollower(
      ensureReferenceScriptWalletWorkingCapital(
        operatorLucid,
        referenceLucid,
        "token-only",
        100_000_000n,
        new Set(),
      ),
    );

    const received = await referenceLucid.utxosAt(reference.address);
    expect(received.length).toBeGreaterThan(0);
    expect(received.every(isPlain)).toBe(true);
    expect(lovelaceIn(received)).toBeGreaterThanOrEqual(100_000_000n);
    expect(amountOf(await operatorLucid.utxosAt(operator.address), unit)).toBe(
      3n,
    );
  }, 120_000);

  it("never spends a reserved initialization nonce of the funding wallet", async () => {
    const { operator, reference, operatorLucid, referenceLucid } =
      await topUpFixture();
    const own = (await operatorLucid.utxosAt(operator.address))[0]!;
    // Split the wallet so the nonce is one coin among several.
    const split = await operatorLucid
      .newTx()
      .pay.ToAddress(operator.address, { lovelace: 150_000_000n })
      .pay.ToAddress(operator.address, { lovelace: 150_000_000n })
      .complete({ presetWalletInputs: [own] });
    await operatorLucid.awaitTx(
      await (await split.sign.withWallet().complete()).submit(),
    );
    const coins = await operatorLucid.utxosAt(operator.address);
    const nonce = largestCoin(coins);

    await runWithoutFollower(
      ensureReferenceScriptWalletWorkingCapital(
        operatorLucid,
        referenceLucid,
        "reserved-nonce",
        100_000_000n,
        new Set([`${nonce.txHash}#${nonce.outputIndex.toString()}`]),
      ),
    );

    expect(
      lovelaceIn(await referenceLucid.utxosAt(reference.address)),
    ).toBeGreaterThanOrEqual(100_000_000n);
    expect(
      (await operatorLucid.utxosAt(operator.address)).map(refOf),
    ).toContain(refOf(nonce));
  }, 120_000);
});

describe("operator lifecycle over the view with a live own intent", () => {
  const attachOperator = async () => {
    const fixture = await initOperatorLifecycleFixture();
    const own = {
      seedPhrase: fixture.operatorSeedPhrase,
      address: await fixture.lucid.wallet().address(),
    };
    const env = await attachIntentFollower(databases, {
      emulator: fixture.emulator,
      own,
      payee: generateEmulatorAccount({ lovelace: 0n }),
      protocol: {
        contracts: fixture.contracts,
        referenceScriptAddresses: [
          await fixture.referenceScriptsLucid.wallet().address(),
        ],
      },
    });
    opened.push(env);
    expect(await env.stage.run()).toEqual([]);
    const lucid = await env.wallet();
    // A second coin that can fund the bond once the largest is held.
    await fundOwn(env, 2_000_000_000n, fixture.referenceScriptsLucid);
    return { fixture, env, lucid };
  };

  /** Holds the view's largest coin, which coin and collateral selection try first. */
  const holdLargest = async (env: IntentEmulator, lucid: LucidEvolution) => {
    const held = largestCoin((await viewOf(env, lucid)).utxos);
    await holdCoin(env, lucid, held);
    return refOf(held);
  };

  const run = <A, E>(
    env: IntentEmulator,
    effect: Effect.Effect<A, E, IntentJournal>,
  ) => Effect.runPromise(Effect.provide(effect, env.journalLayer));

  const lastAccepted = (env: IntentEmulator): string =>
    [...env.accepted.values()].at(-1)!.toString("hex");

  const expectUntouched = (env: IntentEmulator, held: string) => {
    const cbor = lastAccepted(env);
    expect(inputsOf(cbor)).not.toContain(held);
    expect(collateralsOf(cbor)).not.toContain(held);
  };

  const expectLanded = async (env: IntentEmulator, txHash: string | null) => {
    if (txHash === null) throw new Error("expected a submitted transaction");
    await env.follow();
    expect(await env.stage.run()).toEqual([]);
    expect(
      env.stage.lastReport()!.entry(Buffer.from(txHash, "hex"))?.status,
    ).toMatchObject({ kind: "landed" });
  };

  it("registers and activates without touching the coin a live intent spends", async () => {
    const { fixture, env, lucid } = await attachOperator();
    const held = await holdLargest(env, lucid);
    const registered = await run(
      env,
      registerOperatorProgram(
        lucid,
        fixture.contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        fixture.referenceScriptsLucid,
      ),
    );
    expectUntouched(env, held);
    await expectLanded(env, registered.registerTxHash);

    // The follower's pass may have sent the first holding intent; hold
    // another coin for the activation, beside a spare that can fund it.
    await fundOwn(env, 200_000_000n, fixture.referenceScriptsLucid);
    const heldForActivation = await holdLargest(env, lucid);
    advanceEmulatorPastRegistrationDelay(fixture.emulator);
    const activated = await run(
      env,
      activateOperatorProgram(
        lucid,
        fixture.contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        fixture.referenceScriptsLucid,
      ),
    );
    expectUntouched(env, heldForActivation);
    await expectLanded(env, activated.activateTxHash);
  }, 600_000);

  it("deregisters without touching the coin a live intent spends", async () => {
    const { fixture, env, lucid } = await attachOperator();
    const registered = await run(
      env,
      registerOperatorProgram(
        lucid,
        fixture.contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        fixture.referenceScriptsLucid,
      ),
    );
    await expectLanded(env, registered.registerTxHash);
    const held = await holdLargest(env, lucid);
    const deregistered = await run(
      env,
      deregisterOperatorProgram(
        lucid,
        fixture.contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        fixture.referenceScriptsLucid,
      ),
    );
    expectUntouched(env, held);
    await expectLanded(env, deregistered.deregisterTxHash);
  }, 600_000);
});

describe("funding preflight over the whole view", () => {
  const preflightFixture = async () => {
    const account = generateEmulatorAccount({ lovelace: 100_000_000n });
    const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    return { lucid, address: account.address };
  };

  const preflight = (lucid: LucidEvolution, lockedLovelace: bigint) =>
    runWithoutFollower(
      Effect.either(
        requireOperatorFundingProgram(lucid, {
          label: "test-command",
          lockedLovelace,
          feeHeadroomLovelace: 0n,
        }),
      ),
    );

  it("counts a token-only wallet's lovelace less the token change, refusing one lovelace past it", async () => {
    const { lucid, address } = await preflightFixture();
    await makeTokenBearingWallet(lucid, { keep: 60_000_000n });
    const total = lovelaceIn(await lucid.utxosAt(address));

    const probe = await preflight(lucid, 0n);
    if (Either.isLeft(probe)) throw probe.left;
    const { availableLovelace, tokenChangeLovelace } = probe.right;
    expect(tokenChangeLovelace).toBeGreaterThan(0n);
    expect(availableLovelace).toBe(total - tokenChangeLovelace);

    const exact = await preflight(lucid, availableLovelace);
    if (Either.isLeft(exact)) throw exact.left;
    expect(exact.right).toMatchObject({
      availableLovelace,
      tokenChangeLovelace,
      requiredLovelace: availableLovelace,
      shortfallLovelace: 0n,
    });

    const over = await preflight(lucid, availableLovelace + 1n);
    expect(Either.isLeft(over)).toBe(true);
    const refusal = (over as Either.Left<unknown, never>).left;
    expect(refusal).toBeInstanceOf(OperatorFundingShortfall);
    expect((refusal as OperatorFundingShortfall).preflight).toMatchObject({
      label: "test-command",
      availableLovelace,
      tokenChangeLovelace,
      requiredLovelace: availableLovelace + 1n,
      shortfallLovelace: 1n,
    });
  }, 120_000);

  it("keeps no token change for a plain wallet", async () => {
    const { lucid, address } = await preflightFixture();
    const total = lovelaceIn(await lucid.utxosAt(address));
    const exact = await preflight(lucid, total);
    if (Either.isLeft(exact)) throw exact.left;
    expect(exact.right).toMatchObject({
      availableLovelace: total,
      tokenChangeLovelace: 0n,
    });
    expect(Either.isLeft(await preflight(lucid, total + 1n))).toBe(true);
  }, 120_000);
});
