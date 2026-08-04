import fs from "node:fs";

import { CML, walletFromSeed } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { encode as cborEncode } from "cborg";
import dotenv from "dotenv";

const MIDGARD_NATIVE_TX_VERSION = 1n;
const MIDGARD_POSIX_TIME_NONE = -1n;
const MIDGARD_NETWORK_ID_PREPROD = 0n;
const TX_IS_VALID_CODE = 0n;
const HASH32_LEN = 32;

const EMPTY_CBOR_LIST = Buffer.from([0x80]);
const EMPTY_CBOR_NULL = Buffer.from([0xf6]);

export const parseEnv = (filename) => {
  const raw = fs.readFileSync(filename, "utf8");
  return dotenv.parse(raw);
};

export const makeWalletsFromEnv = (env) => {
  const keys = [
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_A",
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_B",
    "TESTNET_GENESIS_WALLET_SEED_PHRASE_C",
  ];

  return keys
    .map((key) => {
      const seed = env[key];
      if (!seed || seed.trim().length === 0) {
        return null;
      }
      const wallet = walletFromSeed(seed.trim(), { network: "Preprod" });
      return {
        key,
        seed: seed.trim(),
        address: wallet.address,
        signer: CML.PrivateKey.from_bech32(wallet.paymentKey),
      };
    })
    .filter((wallet) => wallet !== null);
};

export const encodeCbor = (value) => Buffer.from(cborEncode(value));

export const hash32 = (value) =>
  Buffer.from(blake2b(value, { dkLen: HASH32_LEN }));

export const encodeByteListPreimage = (items) =>
  encodeCbor(items.map((item) => Buffer.from(item)));

export const toOutRefCbor = (txId, outputIndex) =>
  Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(txId),
      BigInt(outputIndex),
    ).to_cbor_bytes(),
  );

export const decodeCoin = (outputHex) => {
  const output = CML.TransactionOutput.from_cbor_bytes(
    Buffer.from(outputHex, "hex"),
  );
  return output.amount().coin();
};

export const outputHasMultiAssets = (outputCbor) =>
  CML.TransactionOutput.from_cbor_bytes(outputCbor).amount().has_multiassets();

export const withOutputCoin = (outputCbor, coin) => {
  const output = CML.TransactionOutput.from_cbor_bytes(outputCbor);
  const currentAmount = output.amount();
  const nextAmount = currentAmount.has_multiassets()
    ? CML.Value.new(coin, currentAmount.multi_asset())
    : CML.Value.from_coin(coin);
  output.set_amount(nextAmount);
  return Buffer.from(output.to_cbor_bytes());
};

export const lovelaceOnlyOutputForTemplate = (templateOutputCbor, coin) => {
  const template = CML.TransactionOutput.from_cbor_bytes(templateOutputCbor);
  return Buffer.from(
    CML.TransactionOutput.new(
      template.address(),
      CML.Value.from_coin(coin),
    ).to_cbor_bytes(),
  );
};

const buildNativeSignedOutputs = ({
  spendOutRefCbor,
  outputCbors,
  signer,
  fee,
  txIdMode,
}) => {
  const spendInputsPreimageCbor = encodeByteListPreimage([spendOutRefCbor]);
  const referenceInputsPreimageCbor = EMPTY_CBOR_LIST;
  const outputsPreimageCbor = encodeByteListPreimage(outputCbors);
  const requiredObserversPreimageCbor = EMPTY_CBOR_LIST;
  const requiredSignersPreimageCbor = encodeByteListPreimage([
    Buffer.from(signer.to_public().hash().to_raw_bytes()),
  ]);
  const mintPreimageCbor = EMPTY_CBOR_LIST;

  const scriptIntegrityHash = hash32(EMPTY_CBOR_NULL);
  const auxiliaryDataHash = hash32(EMPTY_CBOR_NULL);

  const bodyCompact = [
    hash32(spendInputsPreimageCbor),
    hash32(referenceInputsPreimageCbor),
    hash32(outputsPreimageCbor),
    fee,
    MIDGARD_POSIX_TIME_NONE,
    MIDGARD_POSIX_TIME_NONE,
    hash32(requiredObserversPreimageCbor),
    hash32(requiredSignersPreimageCbor),
    hash32(mintPreimageCbor),
    scriptIntegrityHash,
    auxiliaryDataHash,
    MIDGARD_NETWORK_ID_PREPROD,
  ];

  const bodyHash = hash32(encodeCbor(bodyCompact));
  const witness = CML.make_vkey_witness(
    CML.TransactionHash.from_raw_bytes(bodyHash),
    signer,
  );

  const addrTxWitsPreimageCbor = encodeByteListPreimage([
    Buffer.from(witness.to_cbor_bytes()),
  ]);
  const scriptTxWitsPreimageCbor = EMPTY_CBOR_LIST;
  const redeemerTxWitsPreimageCbor = EMPTY_CBOR_LIST;

  const witnessCompact = [
    hash32(addrTxWitsPreimageCbor),
    hash32(scriptTxWitsPreimageCbor),
    hash32(redeemerTxWitsPreimageCbor),
  ];

  const compact = [
    MIDGARD_NATIVE_TX_VERSION,
    bodyHash,
    hash32(encodeCbor(witnessCompact)),
    TX_IS_VALID_CODE,
  ];

  const bodyFull = [
    bodyCompact[0],
    spendInputsPreimageCbor,
    bodyCompact[1],
    referenceInputsPreimageCbor,
    bodyCompact[2],
    outputsPreimageCbor,
    bodyCompact[3],
    bodyCompact[4],
    bodyCompact[5],
    bodyCompact[6],
    requiredObserversPreimageCbor,
    bodyCompact[7],
    requiredSignersPreimageCbor,
    bodyCompact[8],
    mintPreimageCbor,
    bodyCompact[9],
    bodyCompact[10],
    bodyCompact[11],
  ];

  const witnessFull = [
    witnessCompact[0],
    addrTxWitsPreimageCbor,
    witnessCompact[1],
    scriptTxWitsPreimageCbor,
    witnessCompact[2],
    redeemerTxWitsPreimageCbor,
  ];

  const txCbor = encodeCbor([
    MIDGARD_NATIVE_TX_VERSION,
    compact,
    bodyFull,
    witnessFull,
  ]);

  return {
    bodyHash,
    txId: txIdMode === "compact" ? hash32(encodeCbor(compact)) : bodyHash,
    txHex: txCbor.toString("hex"),
  };
};

export const buildNativeSignedOneToOneWithFee = ({
  spendOutRefCbor,
  outputCbor,
  signer,
  fee,
  txIdMode = "body",
}) => {
  const tx = buildNativeSignedOutputs({
    spendOutRefCbor,
    outputCbors: [outputCbor],
    signer,
    fee,
    txIdMode,
  });
  return {
    txId: tx.txId,
    txHex: tx.txHex,
    nextOutRef: toOutRefCbor(tx.txId, 0),
    outputCbor,
    fee,
  };
};

export const buildNativeSignedOneToOne = ({
  spendOutRefCbor,
  outputCbor,
  signer,
  txIdMode = "compact",
}) =>
  buildNativeSignedOneToOneWithFee({
    spendOutRefCbor,
    outputCbor,
    signer,
    fee: 0n,
    txIdMode,
  });

export const buildNativeSignedSplitWithFee = ({
  spendOutRefCbor,
  inputOutputCbor,
  signer,
  outputCount,
  fee,
}) => {
  const input = CML.TransactionOutput.from_cbor_bytes(inputOutputCbor);
  if (input.amount().has_multiassets()) {
    throw new Error("fanout setup only supports lovelace-only source UTxOs");
  }
  const inputCoin = input.amount().coin();
  if (inputCoin <= fee) {
    throw new Error(
      `fanout source coin ${inputCoin.toString()} cannot cover fee ${fee.toString()}`,
    );
  }
  const available = inputCoin - fee;
  const outputCountBig = BigInt(outputCount);
  const baseCoin = available / outputCountBig;
  const remainder = available % outputCountBig;
  if (baseCoin <= 0n) {
    throw new Error(
      `fanout source coin ${inputCoin.toString()} too small for ${outputCount} outputs`,
    );
  }

  const outputs = Array.from({ length: outputCount }, (_, index) =>
    lovelaceOnlyOutputForTemplate(
      inputOutputCbor,
      baseCoin + (BigInt(index) < remainder ? 1n : 0n),
    ),
  );
  const tx = buildNativeSignedOutputs({
    spendOutRefCbor,
    outputCbors: outputs,
    signer,
    fee,
    txIdMode: "body",
  });
  return {
    txId: tx.txId,
    txHex: tx.txHex,
    outputs: outputs.map((outputCbor, outputIndex) => {
      const spendOutRefCbor = toOutRefCbor(tx.txId, outputIndex);
      return {
        outputCbor,
        spendOutRefCbor,
        outRefHex: spendOutRefCbor.toString("hex"),
        outputIndex,
      };
    }),
    fee,
  };
};

export const buildNativeSignedSplit = ({
  spendOutRefCbor,
  inputOutputCbor,
  signer,
  outputCount,
  minFeeA,
  minFeeB,
}) => {
  let fee = minFeeB;
  for (let iteration = 0; iteration < 12; iteration += 1) {
    const tx = buildNativeSignedSplitWithFee({
      spendOutRefCbor,
      inputOutputCbor,
      signer,
      outputCount,
      fee,
    });
    const requiredFee =
      minFeeA * BigInt(Buffer.from(tx.txHex, "hex").length) + minFeeB;
    if (requiredFee === fee) {
      return tx;
    }
    fee = requiredFee;
  }
  throw new Error("failed to converge native fanout transaction min fee");
};

export const buildNativeSignedOneToOneWithMinFee = ({
  spendOutRefCbor,
  inputOutputCbor,
  signer,
  minFeeA,
  minFeeB,
}) => {
  let fee = minFeeB;
  for (let iteration = 0; iteration < 12; iteration += 1) {
    const inputCoin = CML.TransactionOutput.from_cbor_bytes(inputOutputCbor)
      .amount()
      .coin();
    if (inputCoin <= fee) {
      throw new Error(
        `input coin ${inputCoin.toString()} cannot cover fee ${fee.toString()}`,
      );
    }
    const outputCbor = withOutputCoin(inputOutputCbor, inputCoin - fee);
    const tx = buildNativeSignedOneToOneWithFee({
      spendOutRefCbor,
      outputCbor,
      signer,
      fee,
      txIdMode: "body",
    });
    const requiredFee =
      minFeeA * BigInt(Buffer.from(tx.txHex, "hex").length) + minFeeB;
    if (requiredFee === fee) {
      return tx;
    }
    fee = requiredFee;
  }
  throw new Error("failed to converge native stress transaction min fee");
};
