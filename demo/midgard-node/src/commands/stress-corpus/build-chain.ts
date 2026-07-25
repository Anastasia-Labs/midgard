import { createHash } from "node:crypto";

import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { CML, type Network } from "@lucid-evolution/lucid";

import {
  decodeNodeUtxo,
  deriveWalletInfo,
  type NodeUtxo,
} from "@/commands/command-utils.js";
import type { OpenLoopCorpusRow } from "@/commands/stress-open-loop.js";
import { buildTransferTxWithMinFee } from "@/commands/transfer-build-core.js";
import { outRefLabel } from "@/tx-context.js";

export type CorpusFundingUtxo = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly outputCborHex: string;
};

export type CorpusFeeParams = {
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
};

export type BuildCorpusChainInput = {
  readonly seedPhrase: string;
  readonly walletId: string;
  readonly fundingUtxo: CorpusFundingUtxo;
  readonly depth: number;
  readonly amountLovelace: bigint;
  readonly feeParams: CorpusFeeParams;
  readonly network: Network;
  readonly networkId: bigint;
  readonly maxSubmitTxCborBytes: number;
  readonly corpusSliceId: string;
  readonly planShape?: OpenLoopCorpusRow["planShape"];
  readonly terminalChangeFloorLovelace?: bigint;
};

export type BuiltCorpusChain = {
  readonly walletId: string;
  readonly rows: readonly OpenLoopCorpusRow[];
  readonly terminalChangeLovelace: bigint;
};

const sha256Hex = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

const outRefCborHex = (txHash: string, outputIndex: number): string =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(txHash),
      BigInt(outputIndex),
    ).to_cbor_bytes(),
  ).toString("hex");

export const nodeUtxoFromCorpusFunding = (
  funding: CorpusFundingUtxo,
): NodeUtxo =>
  decodeNodeUtxo({
    outref: outRefCborHex(funding.txHash, funding.outputIndex),
    outputCbor: funding.outputCborHex,
  });

const outputUtxoFromBuiltTx = ({
  txHash,
  outputIndex,
  outputCbor,
}: {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly outputCbor: Buffer;
}): NodeUtxo =>
  decodeNodeUtxo({
    outref: outRefCborHex(txHash, outputIndex),
    outputCbor: outputCbor.toString("hex"),
  });

const requirePositiveSafeInteger = (value: number, fieldName: string): void => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${fieldName} must be a positive safe integer.`);
  }
};

export const buildCorpusChain = async (
  input: BuildCorpusChainInput,
): Promise<BuiltCorpusChain> => {
  requirePositiveSafeInteger(input.depth, "depth");
  requirePositiveSafeInteger(
    input.maxSubmitTxCborBytes,
    "maxSubmitTxCborBytes",
  );
  if (input.amountLovelace <= 0n) {
    throw new Error("amountLovelace must be greater than zero.");
  }
  const wallet = deriveWalletInfo(
    { seedPhrase: input.seedPhrase, resolvedFrom: input.walletId },
    input.network,
  );
  let currentInput = nodeUtxoFromCorpusFunding(input.fundingUtxo);
  if (currentInput.address !== wallet.address) {
    throw new Error(
      `Funding UTxO for ${input.walletId} belongs to ${currentInput.address}, not derived wallet ${wallet.address}.`,
    );
  }

  const rows: OpenLoopCorpusRow[] = [];
  let parentTxHash: string | null = null;
  for (let step = 0; step < input.depth; step += 1) {
    const built = await buildTransferTxWithMinFee({
      senderAddress: wallet.address,
      destinationAddress: wallet.address,
      signer: wallet.privateKey,
      availableUtxos: [currentInput],
      requestedAssets: { lovelace: input.amountLovelace },
      network: input.network,
      networkId: input.networkId,
      minFeeA: input.feeParams.minFeeA,
      minFeeB: input.feeParams.minFeeB,
      maxSubmitTxCborBytes: input.maxSubmitTxCborBytes,
    });
    if (built.txCbor.length > input.maxSubmitTxCborBytes) {
      throw new Error(
        `Built tx ${built.txIdHex} is ${built.txCbor.length.toString()} bytes, above maxSubmitTxCborBytes ${input.maxSubmitTxCborBytes.toString()}.`,
      );
    }
    const expectedFee =
      input.feeParams.minFeeA * BigInt(built.txCbor.length) +
      input.feeParams.minFeeB;
    if (built.fee !== expectedFee) {
      throw new Error(
        `Built tx ${built.txIdHex} fee ${built.fee.toString()} does not match minFeeA*bytes+minFeeB ${expectedFee.toString()}.`,
      );
    }
    const outputs = decodeMidgardNativeByteListPreimage(
      decodeMidgardNativeTxFullV1FromCanonicalCbor(built.txCbor).body
        .outputsPreimageCbor,
      "native.outputs",
    );
    if (outputs.length < 2) {
      throw new Error(
        `Built tx ${built.txIdHex} must have destination output 0 and change output 1.`,
      );
    }
    const outputOutrefs = outputs.map(
      (_output, outputIndex) => `${built.txIdHex}#${outputIndex.toString()}`,
    );
    rows.push({
      txHash: built.txIdHex,
      canonicalCborHex: built.txHex,
      canonicalCborSha256: sha256Hex(built.txCbor),
      canonicalCborByteLength: built.txCbor.length,
      senderWalletId: input.walletId,
      selectedInputOutref: outRefLabel(currentInput),
      outputOutrefs,
      planShape: input.planShape ?? "chain",
      parentTxHash,
      corpusSliceId: input.corpusSliceId,
    });
    parentTxHash = built.txIdHex;
    currentInput = outputUtxoFromBuiltTx({
      txHash: built.txIdHex,
      outputIndex: 1,
      outputCbor: outputs[1]!,
    });
  }

  const terminalChangeLovelace = currentInput.assets.lovelace ?? 0n;
  const floor = input.terminalChangeFloorLovelace ?? input.amountLovelace;
  if (terminalChangeLovelace < floor) {
    throw new Error(
      `Terminal change for ${input.walletId} is ${terminalChangeLovelace.toString()} lovelace, below floor ${floor.toString()}.`,
    );
  }
  return {
    walletId: input.walletId,
    rows,
    terminalChangeLovelace,
  };
};
