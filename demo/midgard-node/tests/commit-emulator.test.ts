import { describe, expect, it } from "vitest";
import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Assets,
  Data,
  Emulator,
  Lucid,
  generateEmulatorAccount,
  paymentCredentialOf,
  toUnit,
  type TxBuilder,
} from "@lucid-evolution/lucid";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { withRealStateQueueContracts } from "@/services/midgard-contracts.js";
import {
  deriveStateQueueCommitLayout,
  encodeActiveOperatorCommitRedeemer,
  encodeStateQueueCommitRedeemer,
} from "@/workers/utils/commit-redeemers.js";

const MIN_OUTPUT_LOVELACE = 5_000_000n;

const submitAndAwait = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  txBuilder: TxBuilder,
  options?: { localUPLCEval: boolean },
) => {
  const completed = await txBuilder.complete({
    localUPLCEval: options?.localUPLCEval ?? true,
  });
  const signed = await completed.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

const loadContracts = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      const placeholder = yield* AlwaysSucceedsContract;
      const oneShotOutRef = {
        txHash: "00".repeat(32),
        outputIndex: 0,
      } as const;
      const real = yield* withRealStateQueueContracts(
        "Preprod",
        placeholder,
        oneShotOutRef,
      );
      return { placeholder, real };
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

const selectLargestWalletUtxo = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
) =>
  (await lucid.wallet().getUtxos()).sort((a, b) => {
    const lovelaceA = a.assets.lovelace ?? 0n;
    const lovelaceB = b.assets.lovelace ?? 0n;
    if (lovelaceA === lovelaceB) {
      return a.txHash === b.txHash
        ? a.outputIndex - b.outputIndex
        : a.txHash.localeCompare(b.txHash);
    }
    return lovelaceA > lovelaceB ? -1 : 1;
  })[0];

describe("commit emulator", () => {
  it("commits with the real state_queue contract (no always-succeeds state_queue)", async () => {
    const { placeholder, real } = await loadContracts();
    const initialHeaderHash = "11".repeat(28);
    const stateQueueRootUnit = toUnit(
      real.stateQueue.policyId,
      SDK.NODE_ASSET_NAME,
    );
    const initialHeaderUnit = toUnit(
      real.stateQueue.policyId,
      SDK.NODE_ASSET_NAME + initialHeaderHash,
    );
    const schedulerUnit = toUnit(placeholder.scheduler.policyId, "");
    const seededAssets: Assets = {
      [stateQueueRootUnit]: 1n,
      [initialHeaderUnit]: 1n,
      [schedulerUnit]: 1n,
    };
    const operator = generateEmulatorAccount({
      lovelace: 30_000_000_000n,
      ...seededAssets,
    });
    const emulator = new Emulator([operator]);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(operator.seedPhrase);
    const provider = lucid.config().provider as { time: number; slot: number };
    const slotLength = 1000;
    const zeroTime = provider.time - provider.slot * slotLength;
    const currentSlot = lucid.unixTimeToSlot(provider.time);
    const targetSlot = currentSlot + 120;
    const alignedEndTime = BigInt(zeroTime + targetSlot * slotLength);
    const initialHeader: SDK.Header = {
      prevUtxosRoot: "11".repeat(32),
      utxosRoot: "22".repeat(32),
      transactionsRoot: "33".repeat(32),
      depositsRoot: "44".repeat(32),
      withdrawalsRoot: "55".repeat(32),
      startTime: alignedEndTime - 1000n,
      endTime: alignedEndTime,
      prevHeaderHash: "00".repeat(28),
      operatorVkey: "aa".repeat(28),
      protocolVersion: 0n,
    };

    const operatorAddress = await lucid.wallet().address();
    const operatorCredential = paymentCredentialOf(operatorAddress);
    expect(operatorCredential?.type).toBe("Key");
    const operatorKeyHash = operatorCredential!.hash;

    const activeOperatorUnit = toUnit(
      placeholder.activeOperators.policyId,
      SDK.NODE_ASSET_NAME + operatorKeyHash,
    );
    const commitEndTime = initialHeader.endTime;
    const schedulerStartTime = initialHeader.endTime;

    const confirmedStateDatum: SDK.ConfirmedState = {
      headerHash: initialHeaderHash,
      prevHeaderHash: "00".repeat(28),
      utxoRoot: initialHeader.utxosRoot,
      startTime: initialHeader.startTime,
      endTime: initialHeader.endTime,
      protocolVersion: initialHeader.protocolVersion,
    };
    const stateQueueRootDatum: SDK.StateQueueDatum = {
      key: "Empty",
      next: { Key: { key: initialHeaderHash } },
      data: Data.castTo(confirmedStateDatum, SDK.ConfirmedState),
    };
    const stateQueueHeaderDatum: SDK.StateQueueDatum = {
      key: { Key: { key: initialHeaderHash } },
      next: "Empty",
      data: Data.castTo(initialHeader, SDK.Header),
    };
    const activeOperatorDatum: SDK.StateQueueDatum = {
      key: { Key: { key: operatorKeyHash } },
      next: "Empty",
      data: "00",
    };

    const bootstrapTx = lucid
      .newTx()
      .mintAssets({ [activeOperatorUnit]: 1n }, Data.void())
      .pay.ToContract(
        real.stateQueue.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(stateQueueRootDatum, SDK.StateQueueDatum),
        },
        {
          lovelace: MIN_OUTPUT_LOVELACE,
          [stateQueueRootUnit]: 1n,
        },
      )
      .pay.ToContract(
        real.stateQueue.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(stateQueueHeaderDatum, SDK.StateQueueDatum),
        },
        {
          lovelace: MIN_OUTPUT_LOVELACE,
          [initialHeaderUnit]: 1n,
        },
      )
      .pay.ToContract(
        placeholder.scheduler.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to<SDK.SchedulerDatum>(
            {
              operator: operatorKeyHash,
              startTime: schedulerStartTime,
            },
            SDK.SchedulerDatum,
          ),
        },
        {
          lovelace: MIN_OUTPUT_LOVELACE,
          [schedulerUnit]: 1n,
        },
      )
      .pay.ToContract(
        placeholder.activeOperators.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(activeOperatorDatum, SDK.StateQueueDatum),
        },
        {
          lovelace: MIN_OUTPUT_LOVELACE,
          [activeOperatorUnit]: 1n,
        },
      )
      .attach.Script(placeholder.scheduler.mintingScript)
      .attach.Script(placeholder.activeOperators.mintingScript);

    await submitAndAwait(lucid, bootstrapTx);

    const stateQueueFetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: real.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: real.stateQueue.policyId,
    };
    const latestBlock = await Effect.runPromise(
      SDK.fetchLatestCommittedBlockProgram(lucid, stateQueueFetchConfig),
    );
    expect(latestBlock.datum.key).not.toEqual("Empty");

    const { nodeDatum: updatedNodeDatum, header: newHeader } =
      await Effect.runPromise(
        SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
          lucid,
          latestBlock.datum,
          "11".repeat(32),
          "22".repeat(32),
          "00".repeat(32),
          "00".repeat(32),
          commitEndTime,
        ),
      );
    const newHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeader(newHeader),
    );

    const appendedNodeDatum: SDK.StateQueueDatum = {
      key: updatedNodeDatum.next,
      next: "Empty",
      data: Data.castTo(newHeader, SDK.Header),
    };

    const schedulerRefInput = (
      await lucid.utxosAtWithUnit(
        placeholder.scheduler.spendingScriptAddress,
        schedulerUnit,
      )
    )[0];
    const activeOperatorInput = (
      await lucid.utxosAtWithUnit(
        placeholder.activeOperators.spendingScriptAddress,
        activeOperatorUnit,
      )
    )[0];

    expect(schedulerRefInput?.datum).toBeDefined();
    expect(activeOperatorInput?.datum).toBeDefined();
    const feeInput = await selectLargestWalletUtxo(lucid);
    expect(feeInput).toBeDefined();

    const commitLayout = deriveStateQueueCommitLayout({
      latestBlockInput: latestBlock.utxo,
      activeOperatorInput: activeOperatorInput!,
      txInputs: [latestBlock.utxo, activeOperatorInput!, feeInput!],
    });

    const commitMintAssets = {
      [toUnit(real.stateQueue.policyId, SDK.NODE_ASSET_NAME + newHeaderHash)]:
        1n,
    };
    const commitOutputAssets = {
      lovelace: MIN_OUTPUT_LOVELACE,
      ...commitMintAssets,
    };

    const commitTx = lucid
      .newTx()
      .validTo(Number(commitEndTime))
      .collectFrom([feeInput!])
      .collectFrom([latestBlock.utxo], Data.void())
      .pay.ToContract(
        real.stateQueue.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(appendedNodeDatum, SDK.StateQueueDatum),
        },
        commitOutputAssets,
      )
      .pay.ToContract(
        real.stateQueue.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(updatedNodeDatum, SDK.StateQueueDatum),
        },
        latestBlock.utxo.assets,
      )
      .readFrom([schedulerRefInput!])
      .collectFrom(
        [activeOperatorInput!],
        encodeActiveOperatorCommitRedeemer(commitLayout),
      )
      .pay.ToContract(
        activeOperatorInput!.address,
        {
          kind: "inline",
          value: activeOperatorInput!.datum!,
        },
        activeOperatorInput!.assets,
      )
      .attach.Script(placeholder.activeOperators.spendingScript)
      .addSignerKey(operatorKeyHash)
      .mintAssets(
        commitMintAssets,
        encodeStateQueueCommitRedeemer(operatorKeyHash, commitLayout),
      )
      .attach.Script(real.stateQueue.spendingScript)
      .attach.Script(real.stateQueue.mintingScript);

    await submitAndAwait(lucid, commitTx, { localUPLCEval: true });

    const committedTail = await Effect.runPromise(
      SDK.fetchLatestCommittedBlockProgram(lucid, stateQueueFetchConfig),
    );
    const committedHeader = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(committedTail.datum),
    );
    const committedHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeader(committedHeader),
    );

    expect(committedHeaderHash).toEqual(newHeaderHash);
    expect(committedHeader.endTime).toEqual(commitEndTime);
  });

  it("commits first header from confirmed-state root with real state_queue", async () => {
    const { placeholder, real } = await loadContracts();
    const stateQueueRootUnit = toUnit(
      real.stateQueue.policyId,
      SDK.NODE_ASSET_NAME,
    );
    const schedulerUnit = toUnit(placeholder.scheduler.policyId, "");
    const seededAssets: Assets = {
      [stateQueueRootUnit]: 1n,
      [schedulerUnit]: 1n,
    };
    const operator = generateEmulatorAccount({
      lovelace: 30_000_000_000n,
      ...seededAssets,
    });
    const emulator = new Emulator([operator]);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(operator.seedPhrase);

    const provider = lucid.config().provider as { time: number; slot: number };
    const slotLength = 1000;
    const zeroTime = provider.time - provider.slot * slotLength;
    const currentSlot = lucid.unixTimeToSlot(provider.time);
    const targetSlot = currentSlot + 120;
    const alignedEndTime = BigInt(zeroTime + targetSlot * slotLength);

    const operatorAddress = await lucid.wallet().address();
    const operatorCredential = paymentCredentialOf(operatorAddress);
    expect(operatorCredential?.type).toBe("Key");
    const operatorKeyHash = operatorCredential!.hash;

    const activeOperatorUnit = toUnit(
      placeholder.activeOperators.policyId,
      SDK.NODE_ASSET_NAME + operatorKeyHash,
    );

    const confirmedStateDatum: SDK.ConfirmedState = {
      headerHash: "00".repeat(28),
      prevHeaderHash: "00".repeat(28),
      utxoRoot: "11".repeat(32),
      startTime: alignedEndTime - 1000n,
      endTime: alignedEndTime,
      protocolVersion: 0n,
    };
    const stateQueueRootDatum: SDK.StateQueueDatum = {
      key: "Empty",
      next: "Empty",
      data: Data.castTo(confirmedStateDatum, SDK.ConfirmedState),
    };
    const activeOperatorDatum: SDK.StateQueueDatum = {
      key: { Key: { key: operatorKeyHash } },
      next: "Empty",
      data: "00",
    };

    const bootstrapTx = lucid
      .newTx()
      .mintAssets({ [activeOperatorUnit]: 1n }, Data.void())
      .pay.ToContract(
        real.stateQueue.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(stateQueueRootDatum, SDK.StateQueueDatum),
        },
        {
          lovelace: MIN_OUTPUT_LOVELACE,
          [stateQueueRootUnit]: 1n,
        },
      )
      .pay.ToContract(
        placeholder.scheduler.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to<SDK.SchedulerDatum>(
            {
              operator: operatorKeyHash,
              startTime: alignedEndTime,
            },
            SDK.SchedulerDatum,
          ),
        },
        {
          lovelace: MIN_OUTPUT_LOVELACE,
          [schedulerUnit]: 1n,
        },
      )
      .pay.ToContract(
        placeholder.activeOperators.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(activeOperatorDatum, SDK.StateQueueDatum),
        },
        {
          lovelace: MIN_OUTPUT_LOVELACE,
          [activeOperatorUnit]: 1n,
        },
      )
      .attach.Script(placeholder.scheduler.mintingScript)
      .attach.Script(placeholder.activeOperators.mintingScript);

    await submitAndAwait(lucid, bootstrapTx);

    const stateQueueFetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: real.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: real.stateQueue.policyId,
    };
    const latestBlock = await Effect.runPromise(
      SDK.fetchLatestCommittedBlockProgram(lucid, stateQueueFetchConfig),
    );
    expect(latestBlock.datum.key).toEqual("Empty");

    const { nodeDatum: updatedNodeDatum, header: newHeader } =
      await Effect.runPromise(
        SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
          lucid,
          latestBlock.datum,
          "11".repeat(32),
          "22".repeat(32),
          "00".repeat(32),
          "00".repeat(32),
          alignedEndTime,
        ),
      );
    const newHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeader(newHeader),
    );
    const appendedNodeDatum: SDK.StateQueueDatum = {
      key: updatedNodeDatum.next,
      next: "Empty",
      data: Data.castTo(newHeader, SDK.Header),
    };

    const schedulerRefInput = (
      await lucid.utxosAtWithUnit(
        placeholder.scheduler.spendingScriptAddress,
        schedulerUnit,
      )
    )[0];
    const activeOperatorInput = (
      await lucid.utxosAtWithUnit(
        placeholder.activeOperators.spendingScriptAddress,
        activeOperatorUnit,
      )
    )[0];
    const feeInput = await selectLargestWalletUtxo(lucid);

    expect(schedulerRefInput?.datum).toBeDefined();
    expect(activeOperatorInput?.datum).toBeDefined();
    expect(feeInput).toBeDefined();

    const commitLayout = deriveStateQueueCommitLayout({
      latestBlockInput: latestBlock.utxo,
      activeOperatorInput: activeOperatorInput!,
      txInputs: [latestBlock.utxo, activeOperatorInput!, feeInput!],
    });
    const commitMintAssets = {
      [toUnit(real.stateQueue.policyId, SDK.NODE_ASSET_NAME + newHeaderHash)]:
        1n,
    };
    const commitOutputAssets = {
      lovelace: MIN_OUTPUT_LOVELACE,
      ...commitMintAssets,
    };

    const commitTx = lucid
      .newTx()
      .validTo(Number(alignedEndTime))
      .collectFrom([feeInput!])
      .collectFrom([latestBlock.utxo], Data.void())
      .pay.ToContract(
        real.stateQueue.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(appendedNodeDatum, SDK.StateQueueDatum),
        },
        commitOutputAssets,
      )
      .pay.ToContract(
        real.stateQueue.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(updatedNodeDatum, SDK.StateQueueDatum),
        },
        latestBlock.utxo.assets,
      )
      .readFrom([schedulerRefInput!])
      .collectFrom(
        [activeOperatorInput!],
        encodeActiveOperatorCommitRedeemer(commitLayout),
      )
      .pay.ToContract(
        activeOperatorInput!.address,
        {
          kind: "inline",
          value: activeOperatorInput!.datum!,
        },
        activeOperatorInput!.assets,
      )
      .attach.Script(placeholder.activeOperators.spendingScript)
      .addSignerKey(operatorKeyHash)
      .mintAssets(
        commitMintAssets,
        encodeStateQueueCommitRedeemer(operatorKeyHash, commitLayout),
      )
      .attach.Script(real.stateQueue.spendingScript)
      .attach.Script(real.stateQueue.mintingScript);

    await submitAndAwait(lucid, commitTx, { localUPLCEval: true });

    const committedTail = await Effect.runPromise(
      SDK.fetchLatestCommittedBlockProgram(lucid, stateQueueFetchConfig),
    );
    const committedHeader = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(committedTail.datum),
    );
    const committedHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeader(committedHeader),
    );

    expect(committedHeaderHash).toEqual(newHeaderHash);
    expect(committedHeader.endTime).toEqual(alignedEndTime);
    expect(committedTail.utxo.assets.lovelace ?? 0n).toBeGreaterThanOrEqual(
      MIN_OUTPUT_LOVELACE,
    );

    const secondCommitEndTime = alignedEndTime;
    const { nodeDatum: updatedSecondNodeDatum, header: secondHeader } =
      await Effect.runPromise(
        SDK.updateLatestBlocksDatumAndGetTheNewHeaderProgram(
          lucid,
          committedTail.datum,
          "33".repeat(32),
          "44".repeat(32),
          "00".repeat(32),
          "00".repeat(32),
          secondCommitEndTime,
        ),
      );
    const secondHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeader(secondHeader),
    );
    const secondAppendedNodeDatum: SDK.StateQueueDatum = {
      key: updatedSecondNodeDatum.next,
      next: "Empty",
      data: Data.castTo(secondHeader, SDK.Header),
    };

    const schedulerRefInput2 = (
      await lucid.utxosAtWithUnit(
        placeholder.scheduler.spendingScriptAddress,
        schedulerUnit,
      )
    )[0];
    const activeOperatorInput2 = (
      await lucid.utxosAtWithUnit(
        placeholder.activeOperators.spendingScriptAddress,
        activeOperatorUnit,
      )
    )[0];
    const feeInput2 = await selectLargestWalletUtxo(lucid);

    expect(schedulerRefInput2?.datum).toBeDefined();
    expect(activeOperatorInput2?.datum).toBeDefined();
    expect(feeInput2).toBeDefined();

    const secondCommitLayout = deriveStateQueueCommitLayout({
      latestBlockInput: committedTail.utxo,
      activeOperatorInput: activeOperatorInput2!,
      txInputs: [committedTail.utxo, activeOperatorInput2!, feeInput2!],
    });
    const secondCommitMintAssets = {
      [toUnit(
        real.stateQueue.policyId,
        SDK.NODE_ASSET_NAME + secondHeaderHash,
      )]: 1n,
    };
    const secondCommitOutputAssets = {
      lovelace: MIN_OUTPUT_LOVELACE,
      ...secondCommitMintAssets,
    };

    const secondCommitTx = lucid
      .newTx()
      .validTo(Number(secondCommitEndTime))
      .collectFrom([feeInput2!])
      .collectFrom([committedTail.utxo], Data.void())
      .pay.ToContract(
        real.stateQueue.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(secondAppendedNodeDatum, SDK.StateQueueDatum),
        },
        secondCommitOutputAssets,
      )
      .pay.ToContract(
        real.stateQueue.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(updatedSecondNodeDatum, SDK.StateQueueDatum),
        },
        committedTail.utxo.assets,
      )
      .readFrom([schedulerRefInput2!])
      .collectFrom(
        [activeOperatorInput2!],
        encodeActiveOperatorCommitRedeemer(secondCommitLayout),
      )
      .pay.ToContract(
        activeOperatorInput2!.address,
        {
          kind: "inline",
          value: activeOperatorInput2!.datum!,
        },
        activeOperatorInput2!.assets,
      )
      .attach.Script(placeholder.activeOperators.spendingScript)
      .addSignerKey(operatorKeyHash)
      .mintAssets(
        secondCommitMintAssets,
        encodeStateQueueCommitRedeemer(operatorKeyHash, secondCommitLayout),
      )
      .attach.Script(real.stateQueue.spendingScript)
      .attach.Script(real.stateQueue.mintingScript);

    await submitAndAwait(lucid, secondCommitTx, { localUPLCEval: true });

    const secondCommittedTail = await Effect.runPromise(
      SDK.fetchLatestCommittedBlockProgram(lucid, stateQueueFetchConfig),
    );
    const secondCommittedHeader = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(secondCommittedTail.datum),
    );
    const secondCommittedHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeader(secondCommittedHeader),
    );

    expect(secondCommittedHeaderHash).toEqual(secondHeaderHash);
    expect(secondCommittedHeader.endTime).toEqual(secondCommitEndTime);
  });
});
