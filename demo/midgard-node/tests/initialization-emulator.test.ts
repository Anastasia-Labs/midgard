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
import {
  withRealStateQueueAndOperatorContracts,
  withRealStateQueueContracts,
} from "@/services/midgard-contracts.js";
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

const LARGE_TX_PROTOCOL_PARAMETERS = {
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

const buildHubAndStateInitializationTx = async (
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

  return lucid
    .newTx()
    .validTo(Number(genesisTime))
    .compose(hubOracleTx)
    .compose(stateQueueTx);
};

const buildOperatorAwareInitializationTx = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  contracts: SDK.MidgardValidators,
  nonceUtxo: UTxO,
) => {
  const tx = await buildHubAndStateInitializationTx(
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

describe("initialization emulator", () => {
  it("initializes protocol when state_queue and hub_oracle use real onchain scripts", async () => {
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
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const initTx = await buildHubAndStateInitializationTx(
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
    const contracts = await loadContracts({
      txHash: nonceUtxo.txHash,
      outputIndex: nonceUtxo.outputIndex,
    });

    const firstInit = await buildHubAndStateInitializationTx(
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
        const secondInit = await buildHubAndStateInitializationTx(
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
