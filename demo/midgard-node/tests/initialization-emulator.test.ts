import { describe, expect, it } from "vitest";
import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  Lucid,
  generateEmulatorAccount,
  paymentCredentialOf,
  toUnit,
} from "@lucid-evolution/lucid";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { withRealStateQueueContracts } from "@/services/midgard-contracts.js";
import { ensureActiveOperatorWitnessNodeProgram } from "@/transactions/bootstrap-active-operator.js";

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

describe("initialization emulator", () => {
  it("initializes protocol when state_queue and hub_oracle use real onchain scripts", async () => {
    const operator = generateEmulatorAccount({
      lovelace: 30_000_000_000n,
    });
    const emulator = new Emulator([operator]);
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

    const initTx = await Effect.runPromise(
      SDK.incompleteInitializationTxProgram(lucid, {
        midgardValidators: contracts,
        fraudProofCatalogueMerkleRoot: "00".repeat(32),
      }),
    );
    const completed = await initTx.complete({ localUPLCEval: false });
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
    const emulator = new Emulator([operator]);
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

    const firstInit = await Effect.runPromise(
      SDK.incompleteInitializationTxProgram(lucid, {
        midgardValidators: contracts,
        fraudProofCatalogueMerkleRoot: "00".repeat(32),
      }),
    );
    const firstSigned = await (
      await firstInit.complete({ localUPLCEval: false })
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
        const secondInit = await Effect.runPromise(
          SDK.incompleteInitializationTxProgram(lucid, {
            midgardValidators: contracts,
            fraudProofCatalogueMerkleRoot: "00".repeat(32),
          }),
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

  it("bootstraps active-operators witness node for real state_queue commits", async () => {
    const operator = generateEmulatorAccount({
      lovelace: 30_000_000_000n,
    });
    const emulator = new Emulator([operator]);
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

    const initTx = await Effect.runPromise(
      SDK.incompleteInitializationTxProgram(lucid, {
        midgardValidators: contracts,
        fraudProofCatalogueMerkleRoot: "00".repeat(32),
      }),
    );
    const completed = await initTx.complete({ localUPLCEval: false });
    const signed = await completed.sign.withWallet().complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash);

    await Effect.runPromise(
      ensureActiveOperatorWitnessNodeProgram(lucid, contracts),
    );

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
