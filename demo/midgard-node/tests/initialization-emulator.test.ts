import { describe, expect, it, vi } from "vitest";
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
  ensureAtomicProtocolInitReferenceScriptsProgram,
  buildAtomicProtocolInitTxProgram,
  fetchProtocolDeploymentStatus,
  fetchHubOracleWitness,
  isSchedulerInitialized,
} from "@/transactions/initialization.js";
import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "@/transactions/register-active-operator.js";
import { verifyNodeRuntimeReferenceScriptsProgram } from "@/transactions/reference-scripts.js";

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

const buildAtomicInitializationTx = async (
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
 * Builds a Lucid emulator instance for initialization tests.
 */
const initEmulatorLucid = async () => {
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
  return { emulator, lucid, referenceScriptsLucid, nonceUtxo };
};

describe("initialization emulator", () => {
  it("builds the hub-oracle mint fragment in isolation", async () => {
    const { lucid, nonceUtxo } = await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const hubOracleTx = await Effect.runPromise(
      SDK.incompleteHubOracleInitTxProgram(lucid, {
        hubOracleMintValidator: contracts.hubOracle,
        validators: contracts,
        oneShotNonceUTxO: nonceUtxo,
      }),
    );

    await expect(
      hubOracleTx.complete({ localUPLCEval: true }),
    ).resolves.toBeDefined();
  });

  it("builds the SDK atomic init transaction from explicit inputs only", async () => {
    const { emulator, lucid, nonceUtxo } = await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });
    const validFrom = BigInt(emulator.now());
    const validTo = validFrom + 7n * 60n * 1000n;
    const outputAssets: Record<string, bigint>[] = [];
    const calls: {
      validFrom?: number;
      validTo?: number;
      collected?: UTxO[];
    } = {};
    const txBuilder: any = {};
    Object.assign(txBuilder, {
      validFrom: vi.fn((value: number) => {
        calls.validFrom = value;
        return txBuilder;
      }),
      validTo: vi.fn((value: number) => {
        calls.validTo = value;
        return txBuilder;
      }),
      collectFrom: vi.fn((utxos: UTxO[]) => {
        calls.collected = utxos;
        return txBuilder;
      }),
      mintAssets: vi.fn(() => txBuilder),
      pay: {
        ToAddressWithData: vi.fn(
          (
            _address: unknown,
            _datum: unknown,
            assets: Record<string, bigint>,
          ) => {
            outputAssets.push(assets);
            return txBuilder;
          },
        ),
        ToContract: vi.fn(
          (
            _address: unknown,
            _datum: unknown,
            assets: Record<string, bigint>,
          ) => {
            outputAssets.push(assets);
            return txBuilder;
          },
        ),
      },
      readFrom: vi.fn(() => txBuilder),
      attach: {
        MintingPolicy: vi.fn(() => txBuilder),
        Script: vi.fn(() => txBuilder),
      },
    });
    const wallet = vi.fn(() => {
      throw new Error("SDK initialization builder must not fetch wallet UTxOs");
    });
    const fakeLucid = {
      config: () => lucid.config(),
      newTx: () => txBuilder,
      wallet,
    } as unknown as typeof lucid;

    const dateNowSpy = vi
      .spyOn(Date, "now")
      .mockReturnValue(Number(validTo) + 123_456_789);

    try {
      const initTx = await Effect.runPromise(
        SDK.unsignedInitializationTxProgram(fakeLucid, {
          midgardValidators: contracts,
          fraudProofCatalogueMerkleRoot: EMPTY_FRAUD_PROOF_CATALOGUE_ROOT,
          oneShotNonceUTxO: nonceUtxo,
          validityRange: { validFrom, validTo },
        }),
      );

      expect(initTx).toBe(txBuilder);
      expect(calls.validFrom).toBe(Number(validFrom));
      expect(calls.validTo).toBe(Number(validTo));
      expect(calls.collected).toEqual([nonceUtxo]);
      expect(outputAssets).toHaveLength(7);
      expect(
        outputAssets.every((assets) => !("lovelace" in assets)),
      ).toBe(true);
      expect(wallet).not.toHaveBeenCalled();
    } finally {
      dateNowSpy.mockRestore();
    }
  });

  it("deploys the canonical real protocol roots atomically", async () => {
    const { lucid, referenceScriptsLucid, nonceUtxo } =
      await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const initTx = await buildAtomicInitializationTx(
      lucid,
      referenceScriptsLucid,
      contracts,
      nonceUtxo,
    );
    const signed = await (
      await initTx.complete({ localUPLCEval: true })
    ).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);

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
    const status = await Effect.runPromise(
      fetchProtocolDeploymentStatus(lucid, contracts),
    );
    const runtimeReferenceScripts = await Effect.runPromise(
      verifyNodeRuntimeReferenceScriptsProgram(
        lucid,
        await referenceScriptsLucid.wallet().address(),
        contracts,
      ),
    );
    const runtimeReferenceScriptNames = runtimeReferenceScripts.map(
      ({ name }) => name,
    );

    expect(txHash).toHaveLength(64);
    expect(hubOracleWitness).not.toBeNull();
    expect(schedulerInitialized).toBe(true);
    expect(schedulerDatum).toEqual("NoActiveOperators");
    expect(status.complete).toBe(true);
    expect(runtimeReferenceScriptNames).toContain("state-queue spending");
    expect(runtimeReferenceScriptNames).toContain("deposit minting");
    expect(runtimeReferenceScriptNames).toContain("settlement minting");
  });

  it("reports already initialized when the atomic protocol root set exists", async () => {
    const { lucid, referenceScriptsLucid, nonceUtxo } =
      await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const initTx = await buildAtomicInitializationTx(
      lucid,
      referenceScriptsLucid,
      contracts,
      nonceUtxo,
    );
    const signed = await (
      await initTx.complete({ localUPLCEval: true })
    ).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);

    const status = await Effect.runPromise(
      fetchProtocolDeploymentStatus(lucid, contracts),
    );
    expect(status.complete).toBe(true);
    expect(status.missingComponents).toEqual([]);
  });

  it("detects partial real deployment as non-empty and incomplete", async () => {
    const { lucid, nonceUtxo } = await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const hubOracleTx = await Effect.runPromise(
      SDK.incompleteHubOracleInitTxProgram(lucid, {
        hubOracleMintValidator: contracts.hubOracle,
        validators: contracts,
        oneShotNonceUTxO: nonceUtxo,
      }),
    );
    const signed = await (
      await hubOracleTx.complete({ localUPLCEval: true })
    ).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);

    const status = await Effect.runPromise(
      fetchProtocolDeploymentStatus(lucid, contracts),
    );
    expect(status.empty).toBe(false);
    expect(status.complete).toBe(false);
    expect(status.missingComponents).toContain("scheduler");
    expect(status.missingComponents).toContain("state-queue");
  });

  it("initializes state_queue when all real protocol roots are minted atomically", async () => {
    const { lucid, referenceScriptsLucid, nonceUtxo } =
      await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const initTx = await buildAtomicInitializationTx(
      lucid,
      referenceScriptsLucid,
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
    expect(
      latest.utxo.assets[
        toUnit(contracts.stateQueue.policyId, SDK.STATE_QUEUE_ROOT_ASSET_NAME)
      ],
    ).toEqual(1n);
    expect(latest.utxo.assets.lovelace ?? 0n).toBeGreaterThan(0n);
  });

  it("rejects re-initialization when the hub_oracle one-shot nonce is already consumed", async () => {
    const { lucid, referenceScriptsLucid, nonceUtxo } =
      await initEmulatorLucid();
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const firstInit = await buildAtomicInitializationTx(
      lucid,
      referenceScriptsLucid,
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
        const secondInit = await buildAtomicInitializationTx(
          lucid,
          referenceScriptsLucid,
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
    const { emulator, lucid, referenceScriptsLucid, nonceUtxo } =
      await initEmulatorLucid();
    const contracts = await loadOperatorContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const initTx = await buildAtomicInitializationTx(
      lucid,
      referenceScriptsLucid,
      contracts,
      nonceUtxo,
    );
    const completed = await initTx.complete({ localUPLCEval: true });
    const signed = await completed.sign.withWallet().complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);

    const registrationResult = await Effect.runPromise(
      registerOperatorProgram(
        lucid,
        contracts,
        5_000_000n,
        referenceScriptsLucid,
      ),
    );
    emulator.awaitSlot(180);
    const onboardingResult = await Effect.runPromise(
      activateOperatorProgram(
        lucid,
        contracts,
        5_000_000n,
        referenceScriptsLucid,
      ),
    );
    expect(registrationResult.registerTxHash).toHaveLength(64);
    expect(onboardingResult.activateTxHash).toHaveLength(64);

    const operatorAddress = await lucid.wallet().address();
    const paymentCredential = paymentCredentialOf(operatorAddress);
    expect(paymentCredential?.type).toEqual("Key");
    const operatorKeyHash = paymentCredential?.hash ?? "";
    const operatorNodeUnit = toUnit(
      contracts.activeOperators.policyId,
      SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
    );
    const operatorNodeUtxos = await lucid.utxosAtWithUnit(
      contracts.activeOperators.spendingScriptAddress,
      operatorNodeUnit,
    );

    expect(operatorNodeUtxos.length).toBeGreaterThan(0);
    const operatorNodeDatum = await Effect.runPromise(
      SDK.getLinkedListNodeViewFromUTxO(operatorNodeUtxos[0]),
    );
    expect(operatorNodeDatum.key).toEqual({ Key: { key: operatorKeyHash } });
  });
});
