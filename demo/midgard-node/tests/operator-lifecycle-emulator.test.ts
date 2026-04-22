import { describe, expect, it } from "vitest";
import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  UTxO,
  generateEmulatorAccount,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { withRealStateQueueAndOperatorContracts } from "@/services/midgard-contracts.js";
import {
  buildAtomicProtocolInitTxProgram,
  ensureAtomicProtocolInitReferenceScriptsProgram,
} from "@/transactions/initialization.js";
import {
  activateOperatorProgram,
  deployReferenceScriptCommandProgram,
  deregisterOperatorProgram,
  registerOperatorProgram,
} from "@/transactions/register-active-operator.js";

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

// Keep fragmented UTxOs large enough so Lucid's default collateral selector
// can satisfy collateral + collateral-return constraints within max inputs (3).
const MIN_COLLATERAL_SAFE_FRAGMENT_LOVELACE = 2_300_000n;
const EMPTY_FRAUD_PROOF_CATALOGUE_ROOT = "00".repeat(32);

const loadOperatorContracts = (oneShotOutRef: {
  txHash: string;
  outputIndex: number;
}) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const placeholder = yield* AlwaysSucceedsContract;
      return yield* withRealStateQueueAndOperatorContracts(
        "Preprod",
        placeholder,
        oneShotOutRef,
      );
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

const buildOperatorAwareInitializationTx = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  referenceScriptsLucid: Awaited<ReturnType<typeof Lucid>>,
  contracts: SDK.MidgardValidators,
  nonceUtxo: UTxO,
) => {
  const referenceScripts = await Effect.runPromise(
    ensureAtomicProtocolInitReferenceScriptsProgram(
      referenceScriptsLucid,
      contracts,
    ),
  );
  return Effect.runPromise(
    buildAtomicProtocolInitTxProgram(
      lucid,
      contracts,
      {
        HUB_ORACLE_ONE_SHOT_TX_HASH: nonceUtxo.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonceUtxo.outputIndex,
      },
      EMPTY_FRAUD_PROOF_CATALOGUE_ROOT,
      undefined,
      referenceScripts,
    ),
  );
};

/**
 * Initializes the shared fixture used by operator-lifecycle emulator tests.
 */
const initOperatorLifecycleFixture = async () => {
  const operator = generateEmulatorAccount({
    lovelace: 30_000_000_000n,
  });
  const referenceScripts = generateEmulatorAccount({
    lovelace: 20_000_000_000n,
  });
  const emulator = new Emulator(
    [operator, referenceScripts],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const lucid = await Lucid(emulator, "Custom");
  const referenceScriptsLucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);
  referenceScriptsLucid.selectWallet.fromSeed(referenceScripts.seedPhrase);

  const nonceUtxo = (await lucid.wallet().getUtxos())[0];
  if (!nonceUtxo) {
    throw new Error("Expected at least one wallet UTxO in emulator");
  }
  const contracts = await loadOperatorContracts({
    txHash: nonceUtxo.txHash,
    outputIndex: nonceUtxo.outputIndex,
  });
  const initTx = await buildOperatorAwareInitializationTx(
    lucid,
    referenceScriptsLucid,
    contracts,
    nonceUtxo,
  );
  const initCompleted = await initTx.complete({ localUPLCEval: true });
  const initSigned = await initCompleted.sign.withWallet().complete();
  const initTxHash = await initSigned.submit();
  await lucid.awaitTx(initTxHash);

  const operatorAddress = await lucid.wallet().address();
  const paymentCredential = paymentCredentialOf(operatorAddress);
  if (paymentCredential?.type !== "Key") {
    throw new Error("Expected operator wallet payment credential to be Key");
  }
  const operatorKeyHash = paymentCredential.hash;

  const activeNodeUnit = toUnit(
    contracts.activeOperators.policyId,
    SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
  );

  return {
    emulator,
    lucid,
    referenceScriptsLucid,
    contracts,
    operatorKeyHash,
    activeNodeUnit,
  };
};

const reconcileLiveWalletUtxos = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  utxos: readonly UTxO[],
): Promise<readonly UTxO[]> => {
  if (utxos.length === 0) {
    return [];
  }
  const uniqueOutRefs = Array.from(
    new Map(
      utxos.map((utxo) => [
        `${utxo.txHash}#${utxo.outputIndex.toString()}`,
        {
          txHash: utxo.txHash,
          outputIndex: utxo.outputIndex,
        },
      ]),
    ).values(),
  );
  return lucid.utxosByOutRef(uniqueOutRefs);
};

const fragmentOperatorWalletUtxos = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  {
    outputs,
    lovelacePerOutput,
  }: {
    outputs: number;
    lovelacePerOutput: bigint;
  },
) => {
  if (outputs <= 0) {
    throw new Error("fragmentOperatorWalletUtxos requires outputs > 0");
  }
  const effectiveLovelacePerOutput =
    lovelacePerOutput < MIN_COLLATERAL_SAFE_FRAGMENT_LOVELACE
      ? MIN_COLLATERAL_SAFE_FRAGMENT_LOVELACE
      : lovelacePerOutput;
  const operatorAddress = await lucid.wallet().address();
  const liveWalletInputs = await reconcileLiveWalletUtxos(
    lucid,
    await lucid.wallet().getUtxos(),
  ).then((utxos) => utxos.filter((utxo) => utxo.scriptRef === undefined));
  let tx = lucid.newTx();
  for (let index = 0; index < outputs; index += 1) {
    tx = tx.pay.ToAddress(operatorAddress, {
      lovelace: effectiveLovelacePerOutput,
    });
  }
  const completed = await tx.complete({
    localUPLCEval: true,
    presetWalletInputs: [...liveWalletInputs],
  });
  const signed = await completed.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/**
 * Builds a deterministic random-number generator for repeatable tests.
 */
const mkDeterministicRng = (seed: number) => {
  let state = seed >>> 0;
  return () => {
    state = (state * 1664525 + 1013904223) >>> 0;
    return state / 0x1_0000_0000;
  };
};

const churnOperatorWalletUtxos = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  {
    seed,
    rounds,
  }: {
    seed: number;
    rounds: number;
  },
) => {
  const rng = mkDeterministicRng(seed);
  for (let round = 0; round < rounds; round += 1) {
    const outputs = 12 + Math.floor(rng() * 30);
    const lovelacePerOutput = BigInt(1_900_000 + Math.floor(rng() * 2_200_000));
    await fragmentOperatorWalletUtxos(lucid, { outputs, lovelacePerOutput });
    const secondOutputs = 8 + Math.floor(rng() * 18);
    const secondLovelacePerOutput = BigInt(
      1_900_000 + Math.floor(rng() * 1_600_000),
    );
    await fragmentOperatorWalletUtxos(lucid, {
      outputs: secondOutputs,
      lovelacePerOutput: secondLovelacePerOutput,
    });
    await reconcileLiveWalletUtxos(lucid, await lucid.wallet().getUtxos());
  }
};

const assertOperatorActivatedState = async ({
  lucid,
  contracts,
  activeNodeUnit,
  operatorKeyHash,
}: {
  lucid: Awaited<ReturnType<typeof Lucid>>;
  contracts: SDK.MidgardValidators;
  activeNodeUnit: string;
  operatorKeyHash: string;
}) => {
  const activeNodeUtxosAfterActivate = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeNodeUnit,
  );
  expect(activeNodeUtxosAfterActivate.length).toBeGreaterThan(0);
  const activeNodeDatum = await Effect.runPromise(
    SDK.getNodeDatumFromUTxO(activeNodeUtxosAfterActivate[0]),
  );
  expect(activeNodeDatum.key).toEqual({ Key: { key: operatorKeyHash } });

  const registeredNodeUtxosAfterActivate = await fetchRegisteredOperatorNodes(
    lucid,
    contracts,
  );
  expect(registeredNodeUtxosAfterActivate.length).toEqual(0);
};

const fetchRegisteredOperatorNodes = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  contracts: SDK.MidgardValidators,
): Promise<readonly UTxO[]> => {
  const utxos = await lucid.utxosAt(
    contracts.registeredOperators.spendingScriptAddress,
  );
  return utxos.filter((utxo) =>
    Object.keys(utxo.assets).some((unit) => {
      if (!unit.startsWith(contracts.registeredOperators.policyId)) {
        return false;
      }
      const assetName = unit.slice(contracts.registeredOperators.policyId.length);
      return assetName.startsWith(SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX);
    }),
  );
};

const advanceEmulatorPastRegistrationDelay = (emulator: Emulator): void => {
  emulator.awaitSlot(180);
};

describe("operator lifecycle emulator", () => {
  it(
    "refreshes the dedicated reference-script wallet from provider state after external replenishment",
    async () => {
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
      referenceScriptsLucid.selectWallet.fromSeed(referenceScripts.seedPhrase);

      const oneShotNonce = (await fundingLucid.wallet().getUtxos())[0];
      if (!oneShotNonce) {
        throw new Error("Expected at least one operator wallet UTxO in emulator");
      }
      const contracts = await loadOperatorContracts({
        txHash: oneShotNonce.txHash,
        outputIndex: oneShotNonce.outputIndex,
      });

      referenceScriptsLucid.overrideUTxOs([]);
      const staleReferenceWalletUtxos =
        await referenceScriptsLucid.wallet().getUtxos();
      const stalePlainBalance = staleReferenceWalletUtxos
        .filter((utxo) => utxo.scriptRef === undefined)
        .reduce((total, utxo) => total + (utxo.assets.lovelace ?? 0n), 0n);
      expect(stalePlainBalance).toEqual(0n);

      const published = await Effect.runPromise(
        deployReferenceScriptCommandProgram(
          referenceScriptsLucid,
          contracts,
          "active-operators",
          fundingLucid,
        ),
      );
      expect(published).toHaveLength(2);

      const referenceScriptAddress =
        await referenceScriptsLucid.wallet().address();
      const liveReferenceWalletUtxos =
        await referenceScriptsLucid.utxosAt(referenceScriptAddress);
      const liveReferenceScriptCount = liveReferenceWalletUtxos.filter(
        (utxo) => utxo.scriptRef !== undefined,
      ).length;
      const livePlainBalance = liveReferenceWalletUtxos
        .filter((utxo) => utxo.scriptRef === undefined)
        .reduce((total, utxo) => total + (utxo.assets.lovelace ?? 0n), 0n);

      expect(liveReferenceScriptCount).toBeGreaterThanOrEqual(2);
      expect(livePlainBalance).toBeGreaterThan(0n);
      expect(await referenceScriptsLucid.wallet().getUtxos()).not.toEqual([]);
    },
    240_000,
  );

  it("runs register-only then activate-only using offchain lifecycle programs", async () => {
    const {
      emulator,
      lucid,
      referenceScriptsLucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    } = await initOperatorLifecycleFixture();

    const registerResult = await Effect.runPromise(
      registerOperatorProgram(lucid, contracts, 5_000_000n, referenceScriptsLucid),
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
    const activateResult = await Effect.runPromise(
      activateOperatorProgram(lucid, contracts, 5_000_000n, referenceScriptsLucid),
    );
    expect(activateResult.activateTxHash).toHaveLength(64);

    await assertOperatorActivatedState({
      lucid,
      contracts,
      activeNodeUnit,
      operatorKeyHash,
    });
  });

  it("runs deregister-only after register-only and removes registered node", async () => {
    const { lucid, referenceScriptsLucid, contracts, operatorKeyHash } =
      await initOperatorLifecycleFixture();

    const registerResult = await Effect.runPromise(
      registerOperatorProgram(lucid, contracts, 5_000_000n, referenceScriptsLucid),
    );
    expect(registerResult.registerTxHash).toHaveLength(64);

    const registeredNodeUtxos = await fetchRegisteredOperatorNodes(
      lucid,
      contracts,
    );
    expect(registeredNodeUtxos.length).toBeGreaterThan(0);

    const deregisterResult = await Effect.runPromise(
      deregisterOperatorProgram(
        lucid,
        contracts,
        5_000_000n,
        referenceScriptsLucid,
      ),
    );
    expect(deregisterResult.deregisterTxHash).toHaveLength(64);

    const registeredNodeUtxosAfterDeregister = await fetchRegisteredOperatorNodes(
      lucid,
      contracts,
    );
    expect(registeredNodeUtxosAfterDeregister.length).toEqual(0);
  });

  it("fails activate-only when the operator is not registered", async () => {
    const { lucid, referenceScriptsLucid, contracts } =
      await initOperatorLifecycleFixture();

    await expect(
      Effect.runPromise(
        activateOperatorProgram(
          lucid,
          contracts,
          5_000_000n,
          referenceScriptsLucid,
        ),
      ),
    ).rejects.toThrow();
  });

  it(
    "runs register-only then activate-only with fragmented wallet UTxOs to stress coin selection",
    async () => {
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

      const registerResult = await Effect.runPromise(
        registerOperatorProgram(
          lucid,
          contracts,
          5_000_000n,
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
      const activateResult = await Effect.runPromise(
        activateOperatorProgram(
          lucid,
          contracts,
          5_000_000n,
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
    },
    240_000,
  );

  it(
    "runs register-only then activate-only with fragmented wallet UTxOs to stress automatic coin selection",
    async () => {
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

      const registerResult = await Effect.runPromise(
        registerOperatorProgram(
          lucid,
          contracts,
          5_000_000n,
          referenceScriptsLucid,
        ),
      );
      expect(registerResult.registerTxHash).toHaveLength(64);
      advanceEmulatorPastRegistrationDelay(emulator);
      const activateResult = await Effect.runPromise(
        activateOperatorProgram(
          lucid,
          contracts,
          5_000_000n,
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
    },
    240_000,
  );

  it(
    "runs register-only then activate-only across varied fragmentation profiles",
    async () => {
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

        const registerResult = await Effect.runPromise(
          registerOperatorProgram(
            lucid,
            contracts,
            5_000_000n,
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
        const activateResult = await Effect.runPromise(
          activateOperatorProgram(
            lucid,
            contracts,
            5_000_000n,
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
    },
    360_000,
  );

  it(
    "runs repeated onboarding with aggressive UTxO churn to stress auto coin selection",
    async () => {
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

        const registerResult = await Effect.runPromise(
          registerOperatorProgram(
            lucid,
            contracts,
            5_000_000n,
            referenceScriptsLucid,
          ),
        );
        expect(registerResult.registerTxHash).toHaveLength(64);
        advanceEmulatorPastRegistrationDelay(emulator);
        const activateResult = await Effect.runPromise(
          activateOperatorProgram(
            lucid,
            contracts,
            5_000_000n,
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
    },
    360_000,
  );

  it(
    "runs register-only then activate-only after deterministic wallet churn to stress auto coin selection index drift",
    async () => {
      const {
        emulator,
        lucid,
        referenceScriptsLucid,
        contracts,
        activeNodeUnit,
        operatorKeyHash,
      } = await initOperatorLifecycleFixture();

      await churnOperatorWalletUtxos(lucid, { seed: 0xa11ce, rounds: 2 });

      const registerResult = await Effect.runPromise(
        registerOperatorProgram(
          lucid,
          contracts,
          5_000_000n,
          referenceScriptsLucid,
        ),
      );
      expect(registerResult.registerTxHash).toHaveLength(64);

      await churnOperatorWalletUtxos(lucid, { seed: 0xb0b, rounds: 2 });

      advanceEmulatorPastRegistrationDelay(emulator);
      const activateResult = await Effect.runPromise(
        activateOperatorProgram(
          lucid,
          contracts,
          5_000_000n,
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
    },
    420_000,
  );

  it(
    "runs register-only then activate-only across deterministic churn profiles to reproduce coin-selection drift",
    async () => {
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

        const registerResult = await Effect.runPromise(
          registerOperatorProgram(
            lucid,
            contracts,
            5_000_000n,
            referenceScriptsLucid,
          ),
        );
        expect(registerResult.registerTxHash).toHaveLength(64);
        advanceEmulatorPastRegistrationDelay(emulator);
        const activateResult = await Effect.runPromise(
          activateOperatorProgram(
            lucid,
            contracts,
            5_000_000n,
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
    },
    420_000,
  );
});
