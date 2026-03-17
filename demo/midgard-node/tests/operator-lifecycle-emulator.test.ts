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
  activateOperatorProgram,
  deregisterOperatorProgram,
  registerAndActivateOperatorProgram,
  registerOperatorProgram,
} from "@/transactions/register-active-operator.js";

const LARGE_TX_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

// Keep fragmented UTxOs large enough so Lucid's default collateral selector
// can satisfy collateral + collateral-return constraints within max inputs (3).
const MIN_COLLATERAL_SAFE_FRAGMENT_LOVELACE = 2_300_000n;

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
  contracts: SDK.MidgardValidators,
  nonceUtxo: UTxO,
) => {
  const genesisTime = BigInt(Date.now() + SDK.VALIDITY_RANGE_BUFFER);
  const hubOracleTx = await Effect.runPromise(
    SDK.incompleteHubOracleInitTxProgram(lucid, {
      hubOracleMintValidator: contracts.hubOracle,
      validators: contracts,
      oneShotNonceUTxO: nonceUtxo,
    }),
  );
  const stateQueueTx = await Effect.runPromise(
    SDK.incompleteInitStateQueueTxProgram(lucid, {
      validator: contracts.stateQueue,
      genesisTime,
    }),
  );
  const registeredOperatorsTx = await Effect.runPromise(
    SDK.incompleteRegisteredOperatorInitTxProgram(lucid, {
      validator: contracts.registeredOperators,
    }),
  );
  const activeOperatorsTx = await Effect.runPromise(
    SDK.incompleteActiveOperatorInitTxProgram(lucid, {
      validator: contracts.activeOperators,
    }),
  );
  const retiredOperatorsTx = await Effect.runPromise(
    SDK.incompleteRetiredOperatorInitTxProgram(lucid, {
      validator: contracts.retiredOperators,
    }),
  );

  return lucid
    .newTx()
    .validTo(Number(genesisTime))
    .compose(hubOracleTx)
    .compose(stateQueueTx)
    .compose(registeredOperatorsTx)
    .compose(activeOperatorsTx)
    .compose(retiredOperatorsTx);
};

const initOperatorLifecycleFixture = async () => {
  const operator = generateEmulatorAccount({
    lovelace: 30_000_000_000n,
  });
  const emulator = new Emulator([operator], LARGE_TX_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);

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

  const registeredNodeUnit = toUnit(
    contracts.registeredOperators.policyId,
    SDK.NODE_ASSET_NAME + operatorKeyHash,
  );
  const activeNodeUnit = toUnit(
    contracts.activeOperators.policyId,
    SDK.NODE_ASSET_NAME + operatorKeyHash,
  );

  return {
    lucid,
    contracts,
    operatorKeyHash,
    registeredNodeUnit,
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
  );
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
  registeredNodeUnit,
  activeNodeUnit,
  operatorKeyHash,
}: {
  lucid: Awaited<ReturnType<typeof Lucid>>;
  contracts: SDK.MidgardValidators;
  registeredNodeUnit: string;
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

  const registeredNodeUtxosAfterActivate = await lucid.utxosAtWithUnit(
    contracts.registeredOperators.spendingScriptAddress,
    registeredNodeUnit,
  );
  expect(registeredNodeUtxosAfterActivate.length).toEqual(0);
};

describe("operator lifecycle emulator", () => {
  it("runs register-only then activate-only using offchain lifecycle programs", async () => {
    const {
      lucid,
      contracts,
      registeredNodeUnit,
      activeNodeUnit,
      operatorKeyHash,
    } = await initOperatorLifecycleFixture();

    const registerResult = await Effect.runPromise(
      registerOperatorProgram(lucid, contracts, 5_000_000n),
    );
    expect(registerResult.registerTxHash).toHaveLength(64);

    const registeredNodeUtxosAfterRegister = await lucid.utxosAtWithUnit(
      contracts.registeredOperators.spendingScriptAddress,
      registeredNodeUnit,
    );
    expect(registeredNodeUtxosAfterRegister.length).toBeGreaterThan(0);

    const activeNodeUtxosBeforeActivate = await lucid.utxosAtWithUnit(
      contracts.activeOperators.spendingScriptAddress,
      activeNodeUnit,
    );
    expect(activeNodeUtxosBeforeActivate.length).toEqual(0);

    const activateResult = await Effect.runPromise(
      activateOperatorProgram(lucid, contracts, 5_000_000n),
    );
    expect(activateResult.activateTxHash).toHaveLength(64);

    await assertOperatorActivatedState({
      lucid,
      contracts,
      registeredNodeUnit,
      activeNodeUnit,
      operatorKeyHash,
    });
  });

  it("runs deregister-only after register-only and removes registered node", async () => {
    const { lucid, contracts, registeredNodeUnit } =
      await initOperatorLifecycleFixture();

    const registerResult = await Effect.runPromise(
      registerOperatorProgram(lucid, contracts, 5_000_000n),
    );
    expect(registerResult.registerTxHash).toHaveLength(64);

    const registeredNodeUtxos = await lucid.utxosAtWithUnit(
      contracts.registeredOperators.spendingScriptAddress,
      registeredNodeUnit,
    );
    expect(registeredNodeUtxos.length).toBeGreaterThan(0);

    const deregisterResult = await Effect.runPromise(
      deregisterOperatorProgram(lucid, contracts, 5_000_000n),
    );
    expect(deregisterResult.deregisterTxHash).toHaveLength(64);

    const registeredNodeUtxosAfterDeregister = await lucid.utxosAtWithUnit(
      contracts.registeredOperators.spendingScriptAddress,
      registeredNodeUnit,
    );
    expect(registeredNodeUtxosAfterDeregister.length).toEqual(0);
  });

  it("fails activate-only when the operator is not registered", async () => {
    const { lucid, contracts } = await initOperatorLifecycleFixture();

    await expect(
      Effect.runPromise(activateOperatorProgram(lucid, contracts, 5_000_000n)),
    ).rejects.toThrow();
  });

  it(
    "runs register-only then activate-only with fragmented wallet UTxOs to stress coin selection",
    async () => {
      const {
        lucid,
        contracts,
        registeredNodeUnit,
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
        registerOperatorProgram(lucid, contracts, 5_000_000n),
      );
      expect(registerResult.registerTxHash).toHaveLength(64);

      await fragmentOperatorWalletUtxos(lucid, {
        outputs: 24,
        lovelacePerOutput: 2_500_000n,
      });
      const walletUtxosBeforeActivate = await lucid.wallet().getUtxos();
      expect(walletUtxosBeforeActivate.length).toBeGreaterThanOrEqual(14);

      const activateResult = await Effect.runPromise(
        activateOperatorProgram(lucid, contracts, 5_000_000n),
      );
      expect(activateResult.activateTxHash).toHaveLength(64);

      await assertOperatorActivatedState({
        lucid,
        contracts,
        registeredNodeUnit,
        activeNodeUnit,
        operatorKeyHash,
      });
    },
    240_000,
  );

  it(
    "runs register-and-activate with fragmented wallet UTxOs to stress automatic coin selection",
    async () => {
      const {
        lucid,
        contracts,
        registeredNodeUnit,
        activeNodeUnit,
        operatorKeyHash,
      } = await initOperatorLifecycleFixture();

      await fragmentOperatorWalletUtxos(lucid, {
        outputs: 28,
        lovelacePerOutput: 2_500_000n,
      });
      const walletUtxosBeforeOnboarding = await lucid.wallet().getUtxos();
      expect(walletUtxosBeforeOnboarding.length).toBeGreaterThanOrEqual(16);

      const onboardingResult = await Effect.runPromise(
        registerAndActivateOperatorProgram(lucid, contracts, 5_000_000n),
      );
      expect(onboardingResult.registerTxHash).toHaveLength(64);
      expect(onboardingResult.activateTxHash).toHaveLength(64);

      await assertOperatorActivatedState({
        lucid,
        contracts,
        registeredNodeUnit,
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
          lucid,
          contracts,
          registeredNodeUnit,
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
          registerOperatorProgram(lucid, contracts, 5_000_000n),
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

        const activateResult = await Effect.runPromise(
          activateOperatorProgram(lucid, contracts, 5_000_000n),
        );
        expect(activateResult.activateTxHash).toHaveLength(64);

        await assertOperatorActivatedState({
          lucid,
          contracts,
          registeredNodeUnit,
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
          lucid,
          contracts,
          registeredNodeUnit,
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

        const onboardingResult = await Effect.runPromise(
          registerAndActivateOperatorProgram(lucid, contracts, 5_000_000n),
        );
        expect(onboardingResult.registerTxHash).toHaveLength(64);
        expect(onboardingResult.activateTxHash).toHaveLength(64);

        await assertOperatorActivatedState({
          lucid,
          contracts,
          registeredNodeUnit,
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
        lucid,
        contracts,
        registeredNodeUnit,
        activeNodeUnit,
        operatorKeyHash,
      } = await initOperatorLifecycleFixture();

      await churnOperatorWalletUtxos(lucid, { seed: 0xa11ce, rounds: 2 });

      const registerResult = await Effect.runPromise(
        registerOperatorProgram(lucid, contracts, 5_000_000n),
      );
      expect(registerResult.registerTxHash).toHaveLength(64);

      await churnOperatorWalletUtxos(lucid, { seed: 0xb0b, rounds: 2 });

      const activateResult = await Effect.runPromise(
        activateOperatorProgram(lucid, contracts, 5_000_000n),
      );
      expect(activateResult.activateTxHash).toHaveLength(64);

      await assertOperatorActivatedState({
        lucid,
        contracts,
        registeredNodeUnit,
        activeNodeUnit,
        operatorKeyHash,
      });
    },
    420_000,
  );

  it(
    "runs register-and-activate across deterministic churn profiles to reproduce coin-selection drift",
    async () => {
      const churnProfiles = [
        { seed: 0x101, rounds: 2 },
        { seed: 0x202, rounds: 2 },
        { seed: 0x303, rounds: 2 },
      ] as const;

      for (const profile of churnProfiles) {
        const {
          lucid,
          contracts,
          registeredNodeUnit,
          activeNodeUnit,
          operatorKeyHash,
        } = await initOperatorLifecycleFixture();

        await churnOperatorWalletUtxos(lucid, {
          seed: profile.seed,
          rounds: profile.rounds,
        });

        const onboardingResult = await Effect.runPromise(
          registerAndActivateOperatorProgram(lucid, contracts, 5_000_000n),
        );
        expect(onboardingResult.registerTxHash).toHaveLength(64);
        expect(onboardingResult.activateTxHash).toHaveLength(64);

        await assertOperatorActivatedState({
          lucid,
          contracts,
          registeredNodeUnit,
          activeNodeUnit,
          operatorKeyHash,
        });
      }
    },
    420_000,
  );
});
