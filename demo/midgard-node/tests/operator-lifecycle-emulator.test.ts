import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/transactions/initialization.js";
import "../src/transactions/register-active-operator.js";
import "../src/transactions/register-active-operator/clock.js";
import "../src/transactions/script-reward-registration.js";
import "../src/transactions/utils.js";
import "./helpers/real-midgard-contracts.js";
import "./operator-lifecycle-emulator.build-operator-lifecycle-snapshot.js";
import "./operator-lifecycle-emulator.register-second-operator-behind-active-first.js";

import * as SDK from "@al-ft/midgard-sdk";
import { createReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  activateOperatorProgram,
  deployReferenceScriptCommandProgram,
  deregisterOperatorProgram,
  OperatorRegistrationRefusal,
  registerAndActivateOperatorProgram,
  registerOperatorProgram,
} from "../src/transactions/register-active-operator.js";
import * as LifecycleClock from "../src/transactions/register-active-operator/clock.js";
import { inspectSignedTxValidityInterval } from "../src/transactions/utils.js";
import { selectNodeWallet } from "../src/transactions/utils.wallet-view.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import {
  EMULATOR_PROTOCOL_PARAMETERS,
  EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
  EMULATOR_REQUIRED_BOND_LOVELACE,
  fragmentOperatorWalletUtxos,
  initOperatorLifecycleFixture,
  loadOperatorContracts,
} from "./operator-lifecycle-emulator.build-operator-lifecycle-snapshot.js";
import {
  advanceEmulatorPastRegistrationDelay,
  assertOperatorActivatedState,
  churnOperatorWalletUtxos,
  describePosixTime,
  fetchRegisteredOperatorNodes,
  registerSecondOperatorBehindActiveFirst,
} from "./operator-lifecycle-emulator.register-second-operator-behind-active-first.js";

describe("operator lifecycle emulator", () => {
  it("early activation restores an empty set only for its earliest registration", async () => {
    const {
      emulator,
      lucid,
      referenceScriptsLucid,
      contracts,
      operatorKeyHash,
      activeNodeUnit,
    } = await initOperatorLifecycleFixture();
    const second = generateEmulatorAccount({ lovelace: 0n });
    const funding = await lucid
      .newTx()
      .pay.ToAddress(second.address, { lovelace: 4_000_000_000n })
      .complete({ localUPLCEval: true });
    await lucid.awaitTx(
      await (await funding.sign.withWallet().complete()).submit(),
    );
    const secondLucid = await Lucid(emulator, "Custom");
    secondLucid.selectWallet.fromSeed(second.seedPhrase);
    for (const operatorLucid of [lucid, secondLucid]) {
      await runWithoutFollower(
        registerOperatorProgram(
          operatorLucid,
          contracts,
          EMULATOR_REQUIRED_BOND_LOVELACE,
          referenceScriptsLucid,
        ),
      );
    }
    const registeredNodes = await Promise.all(
      (
        await lucid.utxosAt(contracts.registeredOperators.spendingScriptAddress)
      ).map((utxo) =>
        Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(utxo)),
      ),
    );
    const registration = registeredNodes.find(
      (node) =>
        node.key !== "Empty" &&
        Data.castFrom(node.data, SDK.RegisteredOperatorDatum).operator ===
          operatorKeyHash,
    );
    if (registration === undefined || registration.key === "Empty")
      throw new Error("Missing first registration");
    const firstActivationTime = BigInt(`0x${registration.key.Key.key}`);
    expect(BigInt(emulator.now())).toBeLessThan(firstActivationTime);

    // Force an early interval only in the adversarial builder: the deployed
    // validator must reject a newer registration while the earliest is pending.
    // The program refuses an early activation locally before it builds, so the
    // adversary also lies about the clock to reach the builder at all.
    const originalBuild = SDK.buildActivateOperatorTx;
    const rejectForcedEarlyActivation = async (expectedEmpty: boolean) => {
      const clock = vi
        .spyOn(LifecycleClock, "resolveL1NowMsOrRefuse")
        .mockImplementation((lucid) =>
          Effect.succeed(
            LifecycleClock.currentTimeMsForLucidOrEmulatorFallback(lucid) +
              365n * 24n * 60n * 60n * 1000n,
          ),
        );
      const build = vi
        .spyOn(SDK, "buildActivateOperatorTx")
        .mockImplementation((parameters) => {
          expect(parameters.activeInsertionAnchor.datum.next === "Empty").toBe(
            expectedEmpty,
          );
          expect(parameters.validFrom).toBeGreaterThan(BigInt(emulator.now()));
          return originalBuild({
            ...parameters,
            validFrom: BigInt(Math.max(0, emulator.now() - 60_000)),
          });
        });
      try {
        await expect(
          runWithoutFollower(
            activateOperatorProgram(
              secondLucid,
              contracts,
              EMULATOR_REQUIRED_BOND_LOVELACE,
              referenceScriptsLucid,
            ),
          ),
        ).rejects.toThrow(
          "Failed to build activation transaction with final redeemer context",
        );
        expect(build).toHaveBeenCalled();
        expect(clock).toHaveBeenCalled();
      } finally {
        build.mockRestore();
        clock.mockRestore();
      }
    };
    await rejectForcedEarlyActivation(true);

    const submit = vi.spyOn(emulator, "submitTx");
    try {
      const activated = await runWithoutFollower(
        activateOperatorProgram(
          lucid,
          contracts,
          EMULATOR_REQUIRED_BOND_LOVELACE,
          referenceScriptsLucid,
        ),
      );
      expect(activated.activateTxHash).toHaveLength(64);
      expect(BigInt(emulator.now())).toBeLessThan(firstActivationTime);
      const submittedCbor = submit.mock.calls[0]?.[0];
      if (submittedCbor === undefined)
        throw new Error("Activation did not submit");
      expect(
        inspectSignedTxValidityInterval(submittedCbor).invalidBeforeSlot,
      ).toBeLessThan(emulator.slot);
      expect(
        await lucid.utxosAtWithUnit(
          contracts.activeOperators.spendingScriptAddress,
          activeNodeUnit,
        ),
      ).toHaveLength(1);
    } finally {
      submit.mockRestore();
    }

    // The remaining registration is now earliest, but the active set is no
    // longer empty: the same early interval must still fail on chain.
    await rejectForcedEarlyActivation(false);
  }, 180_000);

  it("activates operators before, between and after existing keys without duplicate activation", async () => {
    const { emulator, lucid, referenceScriptsLucid, contracts } =
      await initOperatorLifecycleFixture();
    const accounts = Array.from({ length: 4 }, () =>
      generateEmulatorAccount({ lovelace: 0n }),
    )
      .map((account) => ({
        ...account,
        key: paymentCredentialOf(account.address).hash,
      }))
      .sort((left, right) => left.key.localeCompare(right.key));
    let funding = lucid.newTx();
    for (const account of accounts) {
      funding = funding.pay.ToAddress(account.address, {
        lovelace: 4_000_000_000n,
      });
    }
    const funded = await funding.complete({ localUPLCEval: true });
    const signed = await funded.sign.withWallet().complete();
    await lucid.awaitTx(await signed.submit());

    // Empty list, before its first node, after its last node, then middle.
    const insertionOrder = [1, 0, 3, 2];
    const activatedKeys: string[] = [];
    for (const index of insertionOrder) {
      const account = accounts[index]!;
      const operatorLucid = await Lucid(emulator, "Custom");
      operatorLucid.selectWallet.fromSeed(account.seedPhrase);
      const registered = await runWithoutFollower(
        registerOperatorProgram(
          operatorLucid,
          contracts,
          EMULATOR_REQUIRED_BOND_LOVELACE,
          referenceScriptsLucid,
        ),
      );
      expect(registered.registerTxHash).toHaveLength(64);
      advanceEmulatorPastRegistrationDelay(emulator);
      const activated = await runWithoutFollower(
        activateOperatorProgram(
          operatorLucid,
          contracts,
          EMULATOR_REQUIRED_BOND_LOVELACE,
          referenceScriptsLucid,
        ),
      );
      expect(activated.activateTxHash).toHaveLength(64);
      activatedKeys.push(account.key);
      activatedKeys.sort();
      const activeUtxos = await operatorLucid.utxosAt(
        contracts.activeOperators.spendingScriptAddress,
      );
      const nodes = await Promise.all(
        activeUtxos
          .filter((utxo) =>
            Object.entries(utxo.assets).some(
              ([unit, quantity]) =>
                unit.startsWith(contracts.activeOperators.policyId) &&
                quantity === 1n,
            ),
          )
          .map(
            async (utxo) =>
              await Effect.runPromise(SDK.getLinkedListNodeViewFromUTxO(utxo)),
          ),
      );
      expect(nodes).toHaveLength(activatedKeys.length + 1);
      const orderedKeys = [null, ...activatedKeys];
      for (let position = 0; position < orderedKeys.length; position += 1) {
        const key = orderedKeys[position];
        const found = nodes.find((entry) =>
          key === null
            ? entry.key === "Empty"
            : entry.key !== "Empty" && entry.key.Key.key === key,
        );
        expect(found).toBeDefined();
        const next = activatedKeys[position];
        expect(found!.next).toEqual(
          next === undefined ? "Empty" : { Key: { key: next } },
        );
      }
      const priorOutRefs = activeUtxos
        .map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`)
        .sort();
      const duplicate = await runWithoutFollower(
        activateOperatorProgram(
          operatorLucid,
          contracts,
          EMULATOR_REQUIRED_BOND_LOVELACE,
          referenceScriptsLucid,
        ),
      );
      expect(duplicate.activateTxHash).toBeNull();
      expect(
        (
          await operatorLucid.utxosAt(
            contracts.activeOperators.spendingScriptAddress,
          )
        )
          .map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`)
          .sort(),
      ).toEqual(priorOutRefs);
    }
  });

  it("funds and signs the dedicated reference-script wallet from its view after external replenishment, whatever the instance pins", async () => {
    const operator = generateEmulatorAccount({
      lovelace: 200_000_000n,
    });
    const referenceScripts = generateEmulatorAccount({
      lovelace: 0n,
    });
    const emulator = new Emulator(
      [operator, referenceScripts],
      EMULATOR_PROTOCOL_PARAMETERS,
    );
    const fundingLucid = await Lucid(emulator, "Custom");
    fundingLucid.selectWallet.fromSeed(operator.seedPhrase);
    const referenceScriptsLucid = await Lucid(emulator, "Custom");
    selectNodeWallet(referenceScriptsLucid, referenceScripts.seedPhrase);

    const oneShotNonce = (await fundingLucid.wallet().getUtxos())[0];
    if (!oneShotNonce) {
      throw new Error("Expected at least one operator wallet UTxO in emulator");
    }
    const referenceScriptAuth = await createReferenceScriptAuthPolicy(
      referenceScriptsLucid,
      emulator.now(),
      EMULATOR_REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
    );
    const contracts = await loadOperatorContracts(
      {
        txHash: oneShotNonce.txHash,
        outputIndex: oneShotNonce.outputIndex,
      },
      referenceScriptAuth,
    );

    // A stale pin on the instance: the view never reads it.
    referenceScriptsLucid.overrideUTxOs([]);
    try {
      const staleReferenceWalletUtxos = await referenceScriptsLucid
        .wallet()
        .getUtxos();
      const stalePlainBalance = staleReferenceWalletUtxos
        .filter((utxo) => utxo.scriptRef === undefined)
        .reduce((total, utxo) => total + (utxo.assets.lovelace ?? 0n), 0n);
      expect(stalePlainBalance).toEqual(0n);

      const published = await runWithoutFollower(
        deployReferenceScriptCommandProgram(
          referenceScriptsLucid,
          contracts,
          "active-operators",
          contracts.referenceScriptAuth,
          fundingLucid,
        ),
      );
      expect(published).toHaveLength(2);

      const referenceScriptAddress = await referenceScriptsLucid
        .wallet()
        .address();
      const liveReferenceWalletUtxos = await referenceScriptsLucid.utxosAt(
        referenceScriptAddress,
      );
      const liveReferenceScriptCount = liveReferenceWalletUtxos.filter(
        (utxo) => utxo.scriptRef !== undefined,
      ).length;
      const livePlainBalance = liveReferenceWalletUtxos
        .filter((utxo) => utxo.scriptRef === undefined)
        .reduce((total, utxo) => total + (utxo.assets.lovelace ?? 0n), 0n);

      expect(liveReferenceScriptCount).toBeGreaterThanOrEqual(2);
      expect(livePlainBalance).toBeGreaterThan(0n);
      // The node pins nothing: the stale pin is as the test left it.
      expect(await referenceScriptsLucid.wallet().getUtxos()).toEqual([]);
    } finally {
      referenceScriptsLucid.clearUTxOOverride();
    }
  }, 240_000);

  it("deep-clones the authenticated deployment snapshot for each scenario", async () => {
    const first = await initOperatorLifecycleFixture();
    const second = await initOperatorLifecycleFixture();
    const firstOutRef = Object.keys(first.emulator.ledger)[0];
    if (firstOutRef === undefined) {
      throw new Error("Authenticated deployment snapshot has no ledger state");
    }
    const firstEntry = first.emulator.ledger[firstOutRef];
    const secondEntry = second.emulator.ledger[firstOutRef];
    if (firstEntry === undefined || secondEntry === undefined) {
      throw new Error("Cloned deployment snapshot lost its first ledger entry");
    }

    expect(first.emulator).not.toBe(second.emulator);
    expect(first.emulator.ledger).not.toBe(second.emulator.ledger);
    expect(firstEntry).not.toBe(secondEntry);
    firstEntry.spent = true;
    first.emulator.awaitSlot(1);
    expect(secondEntry.spent).toBe(false);
    expect(second.emulator.slot).toBe(first.emulator.slot - 1);
  });

  it("runs register-only then activate-only using offchain lifecycle programs", async () => {
    const {
      emulator,
      lucid,
      referenceScriptsLucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    } = await initOperatorLifecycleFixture();

    const registerResult = await runWithoutFollower(
      registerOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(registerResult.registerTxHash).toHaveLength(64);

    const registeredNodeUtxosAfterRegister = await fetchRegisteredOperatorNodes(
      lucid,
      contracts,
    );
    expect(registeredNodeUtxosAfterRegister.length).toBeGreaterThan(0);

    const activeNodeUtxosBeforeActivate = await lucid.utxosAtWithUnit(
      contracts.activeOperators.spendingScriptAddress,
      activeNodeUnit,
    );
    expect(activeNodeUtxosBeforeActivate.length).toEqual(0);

    advanceEmulatorPastRegistrationDelay(emulator);
    const activateResult = await runWithoutFollower(
      activateOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(activateResult.activateTxHash).toHaveLength(64);

    await assertOperatorActivatedState({
      lucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    });
  });

  it("resumes register-active-operator from an already registered inactive operator without deregistering", async () => {
    const {
      emulator,
      lucid,
      referenceScriptsLucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    } = await initOperatorLifecycleFixture();

    const registerResult = await runWithoutFollower(
      registerOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(registerResult.registerTxHash).toHaveLength(64);

    const registeredNodeUtxosAfterRegister = await fetchRegisteredOperatorNodes(
      lucid,
      contracts,
    );
    expect(registeredNodeUtxosAfterRegister.length).toBeGreaterThan(0);

    advanceEmulatorPastRegistrationDelay(emulator);
    const resumedResult = await runWithoutFollower(
      registerAndActivateOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(resumedResult.registerTxHash).toBeNull();
    expect(resumedResult.activateTxHash).toHaveLength(64);

    await assertOperatorActivatedState({
      lucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    });
  });

  it("runs deregister-only after register-only and removes registered node", async () => {
    const { lucid, referenceScriptsLucid, contracts } =
      await initOperatorLifecycleFixture();

    const registerResult = await runWithoutFollower(
      registerOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(registerResult.registerTxHash).toHaveLength(64);

    const registeredNodeUtxos = await fetchRegisteredOperatorNodes(
      lucid,
      contracts,
    );
    expect(registeredNodeUtxos.length).toBeGreaterThan(0);

    const deregisterResult = await runWithoutFollower(
      deregisterOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(deregisterResult.deregisterTxHash).toHaveLength(64);

    const registeredNodeUtxosAfterDeregister =
      await fetchRegisteredOperatorNodes(lucid, contracts);
    expect(registeredNodeUtxosAfterDeregister.length).toEqual(0);
  });

  it("fails activate-only when the operator is not registered", async () => {
    const { lucid, referenceScriptsLucid, contracts } =
      await initOperatorLifecycleFixture();

    await expect(
      runWithoutFollower(
        activateOperatorProgram(
          lucid,
          contracts,
          EMULATOR_REQUIRED_BOND_LOVELACE,
          referenceScriptsLucid,
        ),
      ),
    ).rejects.toThrow(
      /found no registered node for operator .*; run register-operator first/,
    );
  });

  it("refuses activate-only before the activation time, naming the time, without submitting", async () => {
    const fixture = await initOperatorLifecycleFixture();
    const { emulator, referenceScriptsLucid, contracts } = fixture;
    const { secondLucid, secondKeyHash, activationTime, secondActiveNodeUnit } =
      await registerSecondOperatorBehindActiveFirst(fixture);
    const nowMs = BigInt(emulator.now());

    const submit = vi.spyOn(emulator, "submitTx");
    try {
      const outcome = await runWithoutFollower(
        Effect.either(
          activateOperatorProgram(
            secondLucid,
            contracts,
            EMULATOR_REQUIRED_BOND_LOVELACE,
            referenceScriptsLucid,
          ),
        ),
      );
      if (Either.isRight(outcome)) {
        throw new Error("Expected activation to be refused");
      }
      if (!(outcome.left instanceof OperatorRegistrationRefusal)) {
        throw new Error(
          `Expected a registration refusal, got ${String(outcome.left)}`,
        );
      }
      expect(outcome.left.message).toEqual(
        `Operator ${secondKeyHash} cannot be activated before its activation time ${describePosixTime(activationTime)}; the chain time is ${describePosixTime(nowMs)}`,
      );
      expect(outcome.left.message).not.toContain("\n");
      expect(submit).not.toHaveBeenCalled();
    } finally {
      submit.mockRestore();
    }
    // The emulator clock did not move, so the refusal did not wait.
    expect(BigInt(emulator.now())).toEqual(nowMs);
    expect(
      await secondLucid.utxosAtWithUnit(
        contracts.activeOperators.spendingScriptAddress,
        secondActiveNodeUnit,
      ),
    ).toHaveLength(0);
    expect(
      (await fetchRegisteredOperatorNodes(secondLucid, contracts)).length,
    ).toEqual(1);
  }, 240_000);

  it("activates once the chain time reaches the activation time", async () => {
    const fixture = await initOperatorLifecycleFixture();
    const { emulator, referenceScriptsLucid, contracts } = fixture;
    const { secondLucid, secondKeyHash, activationTime, secondActiveNodeUnit } =
      await registerSecondOperatorBehindActiveFirst(fixture);

    // Advance to the first slot at or after the activation time, not beyond.
    const slotsUntilActivation = Math.ceil(
      Number(activationTime - BigInt(emulator.now())) / 1000,
    );
    emulator.awaitSlot(slotsUntilActivation);
    const nowMs = BigInt(emulator.now());
    expect(nowMs).toBeGreaterThanOrEqual(activationTime);
    expect(nowMs - activationTime).toBeLessThan(1000n);

    const activated = await runWithoutFollower(
      activateOperatorProgram(
        secondLucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(activated.activateTxHash).toHaveLength(64);
    await assertOperatorActivatedState({
      lucid: secondLucid,
      contracts,
      activeNodeUnit: secondActiveNodeUnit,
      operatorKeyHash: secondKeyHash,
    });
  }, 240_000);

  it("runs register-only then activate-only with fragmented wallet UTxOs to stress coin selection", async () => {
    const {
      emulator,
      lucid,
      referenceScriptsLucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    } = await initOperatorLifecycleFixture();

    await fragmentOperatorWalletUtxos(lucid, {
      outputs: 20,
      lovelacePerOutput: 3_000_000n,
    });
    const walletUtxosBeforeRegister = await lucid.wallet().getUtxos();
    expect(walletUtxosBeforeRegister.length).toBeGreaterThanOrEqual(12);

    const registerResult = await runWithoutFollower(
      registerOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(registerResult.registerTxHash).toHaveLength(64);

    await fragmentOperatorWalletUtxos(lucid, {
      outputs: 24,
      lovelacePerOutput: 2_500_000n,
    });
    const walletUtxosBeforeActivate = await lucid.wallet().getUtxos();
    expect(walletUtxosBeforeActivate.length).toBeGreaterThanOrEqual(14);

    advanceEmulatorPastRegistrationDelay(emulator);
    const activateResult = await runWithoutFollower(
      activateOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(activateResult.activateTxHash).toHaveLength(64);

    await assertOperatorActivatedState({
      lucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    });
  }, 240_000);

  it("runs register-only then activate-only with fragmented wallet UTxOs to stress automatic coin selection", async () => {
    const {
      emulator,
      lucid,
      referenceScriptsLucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    } = await initOperatorLifecycleFixture();

    await fragmentOperatorWalletUtxos(lucid, {
      outputs: 28,
      lovelacePerOutput: 2_500_000n,
    });
    const walletUtxosBeforeOnboarding = await lucid.wallet().getUtxos();
    expect(walletUtxosBeforeOnboarding.length).toBeGreaterThanOrEqual(16);

    const registerResult = await runWithoutFollower(
      registerOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(registerResult.registerTxHash).toHaveLength(64);
    advanceEmulatorPastRegistrationDelay(emulator);
    const activateResult = await runWithoutFollower(
      activateOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(activateResult.activateTxHash).toHaveLength(64);

    await assertOperatorActivatedState({
      lucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    });
  }, 240_000);

  // The authenticated deployment is cloned per profile; the timeout covers
  // only profile-specific fragmentation and lifecycle transactions.
  it("runs register-only then activate-only across varied fragmentation profiles", async () => {
    const profiles = [
      {
        registerOutputs: 18,
        registerLovelacePerOutput: 2_200_000n,
        activateOutputs: 26,
        activateLovelacePerOutput: 1_900_000n,
      },
      {
        registerOutputs: 24,
        registerLovelacePerOutput: 1_900_000n,
        activateOutputs: 16,
        activateLovelacePerOutput: 3_000_000n,
      },
      {
        registerOutputs: 12,
        registerLovelacePerOutput: 4_000_000n,
        activateOutputs: 32,
        activateLovelacePerOutput: 1_600_000n,
      },
    ] as const;

    for (const profile of profiles) {
      const {
        emulator,
        lucid,
        referenceScriptsLucid,
        contracts,
        activeNodeUnit,
        operatorKeyHash,
      } = await initOperatorLifecycleFixture();

      await fragmentOperatorWalletUtxos(lucid, {
        outputs: profile.registerOutputs,
        lovelacePerOutput: profile.registerLovelacePerOutput,
      });
      const walletUtxosBeforeRegister = await lucid.wallet().getUtxos();
      expect(walletUtxosBeforeRegister.length).toBeGreaterThanOrEqual(
        Math.max(8, Math.floor(profile.registerOutputs / 3)),
      );

      const registerResult = await runWithoutFollower(
        registerOperatorProgram(
          lucid,
          contracts,
          EMULATOR_REQUIRED_BOND_LOVELACE,
          referenceScriptsLucid,
        ),
      );
      expect(registerResult.registerTxHash).toHaveLength(64);

      await fragmentOperatorWalletUtxos(lucid, {
        outputs: profile.activateOutputs,
        lovelacePerOutput: profile.activateLovelacePerOutput,
      });
      const walletUtxosBeforeActivate = await lucid.wallet().getUtxos();
      expect(walletUtxosBeforeActivate.length).toBeGreaterThanOrEqual(
        Math.max(8, Math.floor(profile.activateOutputs / 3)),
      );

      advanceEmulatorPastRegistrationDelay(emulator);
      const activateResult = await runWithoutFollower(
        activateOperatorProgram(
          lucid,
          contracts,
          EMULATOR_REQUIRED_BOND_LOVELACE,
          referenceScriptsLucid,
        ),
      );
      expect(activateResult.activateTxHash).toHaveLength(64);

      await assertOperatorActivatedState({
        lucid,
        contracts,
        activeNodeUnit,
        operatorKeyHash,
      });
    }
  }, 240_000);

  // The authenticated deployment is cloned per profile; the timeout covers
  // only profile-specific churn and lifecycle transactions.
  it("runs repeated onboarding with aggressive UTxO churn to stress auto coin selection", async () => {
    const churnProfiles = [
      { outputs: 14, lovelacePerOutput: 2_100_000n },
      { outputs: 20, lovelacePerOutput: 1_800_000n },
      { outputs: 10, lovelacePerOutput: 3_300_000n },
    ] as const;

    for (const profile of churnProfiles) {
      const {
        emulator,
        lucid,
        referenceScriptsLucid,
        contracts,
        activeNodeUnit,
        operatorKeyHash,
      } = await initOperatorLifecycleFixture();

      await fragmentOperatorWalletUtxos(lucid, {
        outputs: profile.outputs,
        lovelacePerOutput: profile.lovelacePerOutput,
      });
      await fragmentOperatorWalletUtxos(lucid, {
        outputs: profile.outputs + 6,
        lovelacePerOutput: profile.lovelacePerOutput - 200_000n,
      });
      const walletUtxosBeforeOnboarding = await lucid.wallet().getUtxos();
      expect(walletUtxosBeforeOnboarding.length).toBeGreaterThanOrEqual(
        Math.max(8, Math.floor((profile.outputs + 6) / 2)),
      );

      const registerResult = await runWithoutFollower(
        registerOperatorProgram(
          lucid,
          contracts,
          EMULATOR_REQUIRED_BOND_LOVELACE,
          referenceScriptsLucid,
        ),
      );
      expect(registerResult.registerTxHash).toHaveLength(64);
      advanceEmulatorPastRegistrationDelay(emulator);
      const activateResult = await runWithoutFollower(
        activateOperatorProgram(
          lucid,
          contracts,
          EMULATOR_REQUIRED_BOND_LOVELACE,
          referenceScriptsLucid,
        ),
      );
      expect(activateResult.activateTxHash).toHaveLength(64);

      await assertOperatorActivatedState({
        lucid,
        contracts,
        activeNodeUnit,
        operatorKeyHash,
      });
    }
  }, 240_000);

  it("runs register-only then activate-only after deterministic wallet churn to stress auto coin selection index drift", async () => {
    const {
      emulator,
      lucid,
      referenceScriptsLucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    } = await initOperatorLifecycleFixture();

    await churnOperatorWalletUtxos(lucid, { seed: 0xa11ce, rounds: 2 });

    const registerResult = await runWithoutFollower(
      registerOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(registerResult.registerTxHash).toHaveLength(64);

    await churnOperatorWalletUtxos(lucid, { seed: 0xb0b, rounds: 2 });

    advanceEmulatorPastRegistrationDelay(emulator);
    const activateResult = await runWithoutFollower(
      activateOperatorProgram(
        lucid,
        contracts,
        EMULATOR_REQUIRED_BOND_LOVELACE,
        referenceScriptsLucid,
      ),
    );
    expect(activateResult.activateTxHash).toHaveLength(64);

    await assertOperatorActivatedState({
      lucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    });
  }, 420_000);

  // The authenticated deployment is cloned per profile; the timeout covers
  // only deterministic churn and lifecycle transactions.
  it("runs register-only then activate-only across deterministic churn profiles to reproduce coin-selection drift", async () => {
    const churnProfiles = [
      { seed: 0x101, rounds: 2 },
      { seed: 0x202, rounds: 2 },
      { seed: 0x303, rounds: 2 },
    ] as const;

    for (const profile of churnProfiles) {
      const {
        emulator,
        lucid,
        referenceScriptsLucid,
        contracts,
        activeNodeUnit,
        operatorKeyHash,
      } = await initOperatorLifecycleFixture();

      await churnOperatorWalletUtxos(lucid, {
        seed: profile.seed,
        rounds: profile.rounds,
      });

      const registerResult = await runWithoutFollower(
        registerOperatorProgram(
          lucid,
          contracts,
          EMULATOR_REQUIRED_BOND_LOVELACE,
          referenceScriptsLucid,
        ),
      );
      expect(registerResult.registerTxHash).toHaveLength(64);
      advanceEmulatorPastRegistrationDelay(emulator);
      const activateResult = await runWithoutFollower(
        activateOperatorProgram(
          lucid,
          contracts,
          EMULATOR_REQUIRED_BOND_LOVELACE,
          referenceScriptsLucid,
        ),
      );
      expect(activateResult.activateTxHash).toHaveLength(64);

      await assertOperatorActivatedState({
        lucid,
        contracts,
        activeNodeUnit,
        operatorKeyHash,
      });
    }
  }, 240_000);
});
