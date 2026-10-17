import {
  addAssets as addAssetMaps,
  normalizeAssets,
} from "@al-ft/midgard-core/assets";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { type Assets } from "@lucid-evolution/lucid";

import { compareOutRefs, outRefLabel } from "../tx-context.js";
import { type NodeUtxo, walletNetworkFromId } from "./command-utils.js";
import {
  type BuiltTransferTx,
  DEFAULT_TERMINAL_DRAIN_FEE_CAP_LOVELACE,
  DEFAULT_TERMINAL_DRAIN_MAX_FEE_ITERATIONS,
  makeTransferMidgard,
  privateKeyHash,
  type PrivateKeyInput,
  subtractAssetMaps,
  toBuiltTransferTx,
  toMidgardUtxo,
  type TransferConsensusProfile,
  type TransferNetworkName,
} from "./transfer-build-core.make-static-midgard-provider.js";

/**
 * Builds a fully signed Midgard-native transfer transaction with explicit
 * change handling.
 */
export const buildTransferTx = async ({
  senderAddress,
  destinationAddress,
  signer,
  selectedInputs,
  requestedAssets,
  network,
  networkId,
  fee = 0n,
  consensusProfile = MIDGARD_CONSENSUS_PROFILE,
}: {
  readonly senderAddress: string;
  readonly destinationAddress: string;
  readonly signer: PrivateKeyInput;
  readonly selectedInputs: readonly NodeUtxo[];
  readonly requestedAssets: Readonly<Assets>;
  readonly network?: TransferNetworkName;
  readonly networkId: bigint;
  readonly fee?: bigint;
  readonly consensusProfile?: TransferConsensusProfile;
}): Promise<BuiltTransferTx> => {
  if (selectedInputs.length === 0) {
    throw new Error("Cannot build a transfer without selected inputs.");
  }
  const orderedInputs = [...selectedInputs].sort(compareOutRefs);

  const selectedTotal = orderedInputs.reduce<Readonly<Assets>>(
    (acc, utxo) => addAssetMaps(acc, utxo.assets),
    {},
  );
  const changeAssets = subtractAssetMaps(
    selectedTotal,
    addAssetMaps(requestedAssets, fee > 0n ? { lovelace: fee } : {}),
  );

  const midgard = await makeTransferMidgard({
    senderAddress,
    signer,
    utxos: orderedInputs,
    network: network ?? walletNetworkFromId(networkId),
    networkId,
    minFeeA: 0n,
    minFeeB: 0n,
    consensusProfile,
  });
  let txBuilder = midgard
    .newTx()
    .collectFrom(orderedInputs.map(toMidgardUtxo))
    .addSigner(privateKeyHash(signer))
    .pay.ToAddress(destinationAddress, requestedAssets);
  if (Object.keys(changeAssets).length > 0) {
    txBuilder = txBuilder.pay.ToAddress(senderAddress, changeAssets);
  }
  const completed = await txBuilder.complete({ fee });
  const signed = await completed.sign();
  return toBuiltTransferTx({
    signed,
    senderAddress,
    destinationAddress,
    availableUtxos: orderedInputs,
    requestedAssets,
    changeAssets,
  });
};

/**
 * Builds a signed Midgard-native transfer with fee convergence over the exact
 * bytes submitted to the node. Native body construction, deterministic wallet
 * input selection, change output creation, fee convergence, and body-hash
 * signing are delegated to lucid-midgard.
 */
export const buildTransferTxWithMinFee = async ({
  senderAddress,
  destinationAddress,
  signer,
  availableUtxos,
  requestedAssets,
  network,
  networkId,
  minFeeA,
  minFeeB,
  maxSubmitTxCborBytes,
  consensusProfile = MIDGARD_CONSENSUS_PROFILE,
}: {
  readonly senderAddress: string;
  readonly destinationAddress: string;
  readonly signer: PrivateKeyInput;
  readonly availableUtxos: readonly NodeUtxo[];
  readonly requestedAssets: Readonly<Assets>;
  readonly network?: TransferNetworkName;
  readonly networkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly maxSubmitTxCborBytes?: number;
  readonly consensusProfile?: TransferConsensusProfile;
}): Promise<BuiltTransferTx> => {
  if (availableUtxos.length === 0) {
    throw new Error("Cannot build a transfer without available inputs.");
  }
  const orderedAvailableUtxos = [...availableUtxos].sort(compareOutRefs);
  const midgard = await makeTransferMidgard({
    senderAddress,
    signer,
    utxos: orderedAvailableUtxos,
    network: network ?? walletNetworkFromId(networkId),
    networkId,
    minFeeA,
    minFeeB,
    ...(maxSubmitTxCborBytes === undefined ? {} : { maxSubmitTxCborBytes }),
    consensusProfile,
  });
  const completed = await midgard
    .newTx()
    .addSigner(privateKeyHash(signer))
    .pay.ToAddress(destinationAddress, requestedAssets)
    .complete({
      changeAddress: senderAddress,
      feePolicy: "provider",
    });
  const signed = await completed.sign();
  return toBuiltTransferTx({
    signed,
    senderAddress,
    destinationAddress,
    availableUtxos: orderedAvailableUtxos,
    requestedAssets,
  });
};

/**
 * Builds an exact terminal sweep of every source input. The signed byte length
 * participates in the fee calculation, so the fee is raised monotonically
 * until the transaction pays at least the protocol minimum. No source change
 * output is permitted.
 */
export const buildTerminalDrainTx = async ({
  senderAddress,
  destinationAddress,
  signer,
  availableUtxos,
  network,
  networkId,
  minFeeA,
  minFeeB,
  feeCap = DEFAULT_TERMINAL_DRAIN_FEE_CAP_LOVELACE,
  maxFeeIterations = DEFAULT_TERMINAL_DRAIN_MAX_FEE_ITERATIONS,
  consensusProfile = MIDGARD_CONSENSUS_PROFILE,
}: {
  readonly senderAddress: string;
  readonly destinationAddress: string;
  readonly signer: PrivateKeyInput;
  readonly availableUtxos: readonly NodeUtxo[];
  readonly network?: TransferNetworkName;
  readonly networkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly feeCap?: bigint;
  readonly maxFeeIterations?: number;
  readonly consensusProfile?: TransferConsensusProfile;
}): Promise<BuiltTransferTx> => {
  if (availableUtxos.length === 0) {
    throw new Error("Cannot build a terminal drain without available inputs.");
  }
  if (minFeeA < 0n || minFeeB < 0n || feeCap < 0n) {
    throw new Error("Terminal drain fee parameters must be non-negative.");
  }
  if (!Number.isSafeInteger(maxFeeIterations) || maxFeeIterations <= 0) {
    throw new Error(
      "Terminal drain maxFeeIterations must be a positive safe integer.",
    );
  }
  const orderedInputs = [...availableUtxos].sort(compareOutRefs);
  for (const utxo of orderedInputs) {
    if (
      Object.entries(utxo.assets).some(
        ([unit, quantity]) => unit !== "lovelace" && quantity !== 0n,
      )
    ) {
      throw new Error(
        `Terminal drain input ${outRefLabel(utxo)} contains non-ADA assets.`,
      );
    }
  }
  const totalLovelace = orderedInputs.reduce(
    (total, utxo) => total + (utxo.assets.lovelace ?? 0n),
    0n,
  );
  let fee = 0n;
  for (let iteration = 0; iteration < maxFeeIterations; iteration += 1) {
    const requested = totalLovelace - fee;
    if (requested <= 0n) {
      throw new Error(
        `Terminal drain source balance ${totalLovelace.toString()} cannot pay fee ${fee.toString()}.`,
      );
    }
    const midgard = await makeTransferMidgard({
      senderAddress,
      signer,
      utxos: orderedInputs,
      network: network ?? walletNetworkFromId(networkId),
      networkId,
      minFeeA: 0n,
      minFeeB: fee,
      consensusProfile,
    });
    const completed = await midgard
      .newTx()
      .collectFrom(orderedInputs.map(toMidgardUtxo))
      .addSigner(privateKeyHash(signer))
      .pay.ToAddress(destinationAddress, { lovelace: requested })
      .complete({ changeAddress: senderAddress, feePolicy: "provider" });
    const signed = await completed.sign();
    const built = toBuiltTransferTx({
      signed,
      senderAddress,
      destinationAddress,
      availableUtxos: orderedInputs,
      requestedAssets: { lovelace: requested },
      changeAssets: signed.metadata.changeAssets ?? {},
    });
    if (built.selectedInputs.length !== orderedInputs.length) {
      throw new Error(
        "Terminal drain builder did not select every source input.",
      );
    }
    if (Object.keys(normalizeAssets(built.changeAssets)).length !== 0) {
      throw new Error(
        "Terminal drain builder unexpectedly produced source change.",
      );
    }
    const requiredFee = minFeeA * BigInt(built.txCbor.length) + minFeeB;
    if (requiredFee > feeCap) {
      throw new Error(
        `Terminal drain required fee ${requiredFee.toString()} exceeds cap ${feeCap.toString()}.`,
      );
    }
    if (fee >= requiredFee) return { ...built, fee };
    fee = requiredFee;
  }
  throw new Error(
    `Terminal drain fee did not converge within ${maxFeeIterations.toString()} iterations.`,
  );
};
