import { describe, expect, it } from "vitest";
import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  UTxO,
  generateEmulatorAccount,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import {
  withRealStateQueueAndOperatorContracts,
  withRealStateQueueContracts,
} from "@/services/midgard-contracts.js";
import {
  deploySchedulerAndHubProgram,
  fetchHubOracleWitness,
  isSchedulerInitialized,
} from "@/transactions/initialization.js";
import { registerAndActivateOperatorProgram } from "@/transactions/register-active-operator.js";

const loadContracts = (oneShotOutRef: {
  txHash: string;
  outputIndex: number;
}) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const placeholder = yield* AlwaysSucceedsContract;
      return yield* withRealStateQueueContracts(
        "Preprod",
        placeholder,
        oneShotOutRef,
      );
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

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

const buildPhase1AndStateQueueInitializationTx = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  contracts: SDK.MidgardValidators,
  nonceUtxo: UTxO,
) => {
  const genesisTime = BigInt(Date.now() + SDK.VALIDITY_RANGE_BUFFER);
  const hubAndSchedulerTx = await Effect.runPromise(
    SDK.incompleteHubAndSchedulerInitTxProgram(lucid, {
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

  return lucid
    .newTx()
    .validTo(Number(genesisTime))
    .compose(hubAndSchedulerTx)
    .compose(stateQueueTx);
};

const buildOperatorAwareInitializationTx = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  contracts: SDK.MidgardValidators,
  nonceUtxo: UTxO,
) => {
  const tx = await buildPhase1AndStateQueueInitializationTx(
    lucid,
    contracts,
    nonceUtxo,
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
  return tx
    .compose(registeredOperatorsTx)
    .compose(activeOperatorsTx)
    .compose(retiredOperatorsTx);
};

/**
 * Builds a Lucid emulator instance for initialization tests.
 */
const initEmulatorLucid = async () => {
  const operator = generateEmulatorAccount({
    lovelace: 30_000_000_000n,
  });
  const emulator = new Emulator([operator], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(operator.seedPhrase);
  const nonceUtxo = (await lucid.wallet().getUtxos())[0];
  if (!nonceUtxo) {
    throw new Error("Expected at least one wallet UTxO in emulator");
  }
  return { lucid, nonceUtxo };
};

describe("initialization emulator", () => {
  it("deploys hub-oracle and scheduler together with the real scripts", async () => {
    const { lucid, nonceUtxo } = await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const txHash = await Effect.runPromise(
      deploySchedulerAndHubProgram(lucid, contracts, {
        HUB_ORACLE_ONE_SHOT_TX_HASH: nonceUtxo.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonceUtxo.outputIndex,
      }),
    );

    const hubOracleWitness = await Effect.runPromise(
      fetchHubOracleWitness(lucid, contracts),
    );
    const schedulerInitialized = await Effect.runPromise(
      isSchedulerInitialized(lucid, contracts.scheduler),
    );
    const schedulerUtxos = await lucid.utxosAtWithUnit(
      contracts.scheduler.spendingScriptAddress,
      toUnit(contracts.scheduler.policyId, SDK.SCHEDULER_ASSET_NAME),
    );
    const schedulerDatum = Data.from(
      schedulerUtxos[0]!.datum!,
      SDK.SchedulerDatum,
    );

    expect(txHash).toHaveLength(64);
    expect(hubOracleWitness).not.toBeNull();
    expect(schedulerInitialized).toBe(true);
    expect(schedulerDatum).toEqual({
      operator: "",
      startTime: 0n,
    });
  });

  it("returns already-deployed when hub-oracle and scheduler are already initialized", async () => {
    const { lucid, nonceUtxo } = await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const firstDeployment = await Effect.runPromise(
      deploySchedulerAndHubProgram(lucid, contracts, {
        HUB_ORACLE_ONE_SHOT_TX_HASH: nonceUtxo.txHash,
        HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonceUtxo.outputIndex,
      }),
    );
    expect(firstDeployment).toHaveLength(64);

    await expect(
      Effect.runPromise(
        deploySchedulerAndHubProgram(lucid, contracts, {
          HUB_ORACLE_ONE_SHOT_TX_HASH: nonceUtxo.txHash,
          HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: nonceUtxo.outputIndex,
        }),
      ),
    ).resolves.toEqual("already-deployed");
  });

  it("rejects scheduler initialization that does not use the canonical empty datum", async () => {
    const { lucid, nonceUtxo } = await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const hubAndSchedulerTx = await Effect.runPromise(
      SDK.incompleteHubAndSchedulerInitTxProgram(lucid, {
        validators: contracts,
        oneShotNonceUTxO: nonceUtxo,
        schedulerDatum: {
          operator: "11".repeat(28),
          startTime: 1n,
        },
      }),
    );

    await expect(
      lucid
        .newTx()
        .validTo(Number(BigInt(Date.now() + SDK.VALIDITY_RANGE_BUFFER)))
        .compose(hubAndSchedulerTx)
        .complete({ localUPLCEval: true }),
    ).rejects.toThrow();
  });

  it("initializes protocol when phase-1 and state_queue use real onchain scripts", async () => {
    const { lucid, nonceUtxo } = await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const initTx = await buildPhase1AndStateQueueInitializationTx(
      lucid,
      contracts,
      nonceUtxo,
    );
    const completed = await initTx.complete({ localUPLCEval: true });
    const signed = await completed.sign.withWallet().complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);

    const latest = await Effect.runPromise(
      SDK.fetchLatestCommittedBlockProgram(lucid, {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      }),
    );

    expect(txHash).toHaveLength(64);
    expect(latest.datum.key).toEqual("Empty");
    expect(latest.datum.next).toEqual("Empty");
    expect(latest.utxo.assets.lovelace ?? 0n).toBeGreaterThanOrEqual(
      5_000_000n,
    );
  });

  it("rejects re-initialization when the hub_oracle one-shot nonce is already consumed", async () => {
    const { lucid, nonceUtxo } = await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const firstInit = await buildPhase1AndStateQueueInitializationTx(
      lucid,
      contracts,
      nonceUtxo,
    );
    const firstSigned = await (
      await firstInit.complete({ localUPLCEval: true })
    ).sign
      .withWallet()
      .complete();
    const firstTxHash = await firstSigned.submit();
    await lucid.awaitTx(firstTxHash);
    const walletUtxosAfterFirstInit = await lucid.wallet().getUtxos();
    expect(
      walletUtxosAfterFirstInit.some(
        (utxo) =>
          utxo.txHash === nonceUtxo.txHash &&
          utxo.outputIndex === nonceUtxo.outputIndex,
      ),
    ).toBe(false);

    await expect(
      (async () => {
        const secondInit = await buildPhase1AndStateQueueInitializationTx(
          lucid,
          contracts,
          nonceUtxo,
        );
        const secondSigned = await (
          await secondInit.complete({ localUPLCEval: true })
        ).sign
          .withWallet()
          .complete();
        const secondTxHash = await secondSigned.submit();
        await lucid.awaitTx(secondTxHash);
      })(),
    ).rejects.toThrow();
  });

  it("registers and activates the operator with real operator contracts", async () => {
    const { lucid, nonceUtxo } = await initEmulatorLucid();
    const contracts = await loadOperatorContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const initTx = await buildOperatorAwareInitializationTx(
      lucid,
      contracts,
      nonceUtxo,
    );
    const completed = await initTx.complete({ localUPLCEval: true });
    const signed = await completed.sign.withWallet().complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);

    const onboardingResult = await Effect.runPromise(
      registerAndActivateOperatorProgram(lucid, contracts, 5_000_000n),
    );
    expect(onboardingResult.activateTxHash).toHaveLength(64);

    const operatorAddress = await lucid.wallet().address();
    const paymentCredential = paymentCredentialOf(operatorAddress);
    expect(paymentCredential?.type).toEqual("Key");
    const operatorKeyHash = paymentCredential?.hash ?? "";
    const operatorNodeUnit = toUnit(
      contracts.activeOperators.policyId,
      SDK.NODE_ASSET_NAME + operatorKeyHash,
    );
    const operatorNodeUtxos = await lucid.utxosAtWithUnit(
      contracts.activeOperators.spendingScriptAddress,
      operatorNodeUnit,
    );

    expect(operatorNodeUtxos.length).toBeGreaterThan(0);
    const operatorNodeDatum = await Effect.runPromise(
      SDK.getNodeDatumFromUTxO(operatorNodeUtxos[0]),
    );
    expect(operatorNodeDatum.key).toEqual({ Key: { key: operatorKeyHash } });
  });
});
