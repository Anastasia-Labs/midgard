import {
  computeHash32,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalBinary,
  decodeSingleCbor,
  encodeAddrTxWitsBinary,
  encodeMidgardMint,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCompact,
  encodeOutputsBinary,
  encodeRedeemerTxWitsBinary,
  encodeRequiredObserversBinary,
  encodeRequiredSignersBinary,
  encodeReferenceInputsBinary,
  encodeScriptTxWitsBinary,
  encodeSpendInputsBinary,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import { CML } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  LucidMidgard,
  type MidgardProvider,
  type MidgardUtxo,
  type MidgardWallet,
  type OutRef,
  outRefToCbor,
  type Redeemer,
  walletFromPrivateKey,
  walletFromSeedPhrase,
} from "../../src/index.js";

export const HIGH_CARDINALITY_FIXTURE_NAME =
  "high-cardinality-combined-v1" as const;

export const HIGH_CARDINALITY_COUNTS = {
  spendInputs: 8,
  referenceInputs: 4,
  outputs: 12,
  mintPolicies: 6,
  spendRedeemers: 3,
  mintRedeemers: 6,
  observerRedeemers: 2,
  receiveRedeemers: 2,
  totalRedeemers: 13,
} as const;

export const SIZE_BALANCED_FIXTURE_NAME = "size-balanced-15_5k-v1" as const;

export const SIZE_BALANCED_TARGET_FULL_TX_CBOR_BYTES = 15_872;
export const SIZE_BALANCED_FULL_TX_CBOR_TOLERANCE_BYTES = 256;
export const SIZE_BALANCED_MAX_LIST_LENGTH = 255;
export const SIZE_BALANCED_MAX_FEE = 10_000_000n;
export const SIZE_BALANCED_FEE = 5_000_000n;

export const SIZE_BALANCED_COUNTS = {
  spendInputs: 48,
  referenceInputs: 32,
  outputs: 48,
  mintPolicies: 24,
  spendRedeemers: 8,
  mintRedeemers: 24,
  observerRedeemers: 18,
  receiveRedeemers: 18,
  totalRedeemers: 68,
  requiredSigners: 17,
  addrWitnesses: 17,
  scriptWitnesses: 68,
} as const;

export type NativeTxFixtureSizes = {
  readonly fullTxCborBytes: number;
  readonly compactTxCborBytes: number;
  readonly compactBodyCborBytes: number;
  readonly fee: string;
  readonly preimages: {
    readonly spendInputs: number;
    readonly referenceInputs: number;
    readonly outputs: number;
    readonly requiredObservers: number;
    readonly requiredSigners: number;
    readonly mint: number;
    readonly addrTxWits: number;
    readonly scriptTxWits: number;
    readonly redeemerTxWits: number;
  };
};

export type HighCardinalityNativeTxFixture = {
  readonly name: typeof HIGH_CARDINALITY_FIXTURE_NAME;
  readonly txIdHex: string;
  readonly fullTxCborHex: string;
  readonly compactTxCborHex: string;
  readonly compactBodyCborHex: string;
  readonly counts: typeof HIGH_CARDINALITY_COUNTS;
  readonly sizes: NativeTxFixtureSizes;
  readonly mintPolicyIdsInTxInfoOrder: readonly string[];
  readonly redeemerPointers: readonly string[];
  readonly preimages: {
    readonly spendInputsCborHex: string;
    readonly referenceInputsCborHex: string;
    readonly outputsCborHex: string;
    readonly requiredObserversCborHex: string;
    readonly requiredSignersCborHex: string;
    readonly mintCborHex: string;
    readonly addrTxWitsCborHex: string;
    readonly scriptTxWitsCborHex: string;
    readonly redeemerTxWitsCborHex: string;
  };
  readonly hashes: {
    readonly spendInputsHashHex: string;
    readonly referenceInputsHashHex: string;
    readonly outputsHashHex: string;
    readonly requiredObserversHashHex: string;
    readonly requiredSignersHashHex: string;
    readonly mintHashHex: string;
    readonly addrTxWitsHashHex: string;
    readonly scriptTxWitsHashHex: string;
    readonly redeemerTxWitsHashHex: string;
    readonly witnessSetHashHex: string;
  };
};

export type SizeBalancedNativeTxFixture = {
  readonly name: typeof SIZE_BALANCED_FIXTURE_NAME;
  readonly txIdHex: string;
  readonly fullTxCborHex: string;
  readonly compactTxCborHex: string;
  readonly compactBodyCborHex: string;
  readonly counts: typeof SIZE_BALANCED_COUNTS;
  readonly targetFullTxCborBytes: typeof SIZE_BALANCED_TARGET_FULL_TX_CBOR_BYTES;
  readonly fullTxCborToleranceBytes: typeof SIZE_BALANCED_FULL_TX_CBOR_TOLERANCE_BYTES;
  readonly maxListLength: typeof SIZE_BALANCED_MAX_LIST_LENGTH;
  readonly maxFee: string;
  readonly sizes: NativeTxFixtureSizes;
  readonly mintPolicyIdsInTxInfoOrder: readonly string[];
  readonly redeemerPointers: readonly string[];
  readonly preimages: HighCardinalityNativeTxFixture["preimages"];
  readonly hashes: HighCardinalityNativeTxFixture["hashes"];
};

const seedPhrase =
  "test test test test test test test test test test test junk";
const baseScriptBytes = Buffer.from("4e4d01000033222220051200120011", "hex");

const fakeProvider: MidgardProvider = {
  getUtxos: async () => [],
  getUtxoByOutRef: async () => undefined,
  getProtocolInfo: async () => ({
    apiVersion: 1,
    network: "Preview",
    midgardNativeTxVersion: 1,
    currentSlot: 0n,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: { minFeeA: 0n, minFeeB: 0n },
    submissionLimits: { maxSubmitTxCborBytes: 32768 },
    validation: {
      strictnessProfile: "phase1_midgard",
      localValidationIsAuthoritative: false,
    },
  }),
  getProtocolParameters: async () => ({
    minFeeA: 0n,
    minFeeB: 0n,
    networkId: 0n,
  }),
  getCurrentSlot: async () => 0n,
  submitTx: async () => ({
    txId: "00".repeat(32),
    status: "queued",
    httpStatus: 202,
    duplicate: false,
  }),
  getTxStatus: async (txId) => ({ kind: "queued", txId }),
  diagnostics: () => ({
    endpoint: "memory://high-cardinality-fixture",
    protocolInfoSource: "node",
  }),
};

const makeOutRef = (byte: number, outputIndex = 0): OutRef => ({
  txHash: byte.toString(16).padStart(2, "0").repeat(32),
  outputIndex,
});

const scriptAddress = (scriptHash: string): string =>
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_script(CML.ScriptHash.from_hex(scriptHash)),
  )
    .to_address()
    .to_bech32();

const scriptBytes = (domain: string, index: number): Buffer =>
  Buffer.concat([
    baseScriptBytes,
    Buffer.from(domain, "utf8"),
    Buffer.from([index]),
  ]);

const paddedScriptBytes = (
  domain: string,
  index: number,
  totalBytes: number,
): Buffer => {
  const prefix = scriptBytes(domain, index);
  if (prefix.length > totalBytes) {
    throw new Error(`${domain} script prefix exceeds requested byte length`);
  }
  return Buffer.concat([
    prefix,
    blake2b(Buffer.from(`${domain}:${index.toString(10)}:padding`, "utf8"), {
      dkLen: totalBytes - prefix.length,
    }),
  ]);
};

const plutusV3Hash = (bytes: Uint8Array): string =>
  CML.Script.new_plutus_v3(CML.PlutusV3Script.from_raw_bytes(bytes))
    .hash()
    .to_hex();

const midgardV1Hash = (bytes: Uint8Array): string =>
  Buffer.from(
    blake2b(Buffer.concat([Buffer.from([0x80]), Buffer.from(bytes)]), {
      dkLen: 28,
    }),
  ).toString("hex");

const makeUtxo = (
  ref: OutRef,
  address: string,
  assets: Readonly<Record<string, bigint>>,
  options: { readonly scriptRef?: InstanceType<typeof CML.Script> } = {},
): MidgardUtxo =>
  decodeMidgardUtxo({
    outRef: ref,
    outRefCbor: outRefToCbor(ref),
    outputCbor: encodeMidgardTxOutput(address, assets, options),
  });

const redeemer = (value: bigint, mem = 1n, steps = 2n): Redeemer => ({
  data: CML.PlutusData.new_integer(CML.BigInteger.from_str(value.toString())),
  exUnits: { mem, steps },
});

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

const asArray = (value: unknown, label: string): readonly unknown[] => {
  if (!Array.isArray(value)) {
    throw new Error(`${label} must decode to an array`);
  }
  return value;
};

const redeemerPointers = (preimageCbor: Uint8Array): readonly string[] =>
  asArray(decodeSingleCbor(preimageCbor), "redeemer preimage").map((entry) => {
    const fields = asArray(entry, "redeemer entry");
    return `${fields[0]!.toString()}:${fields[1]!.toString()}`;
  });

const makeAssetName = (prefix: number, index: number): string =>
  `${prefix.toString(16).padStart(2, "0")}${index
    .toString(16)
    .padStart(2, "0")}`;

const deterministicPrivateKey = (
  domain: string,
  index: number,
): InstanceType<typeof CML.PrivateKey> =>
  CML.PrivateKey.from_normal_bytes(
    blake2b(Buffer.from(`${domain}:${index.toString(10)}`, "utf8"), {
      dkLen: 32,
    }),
  );

const walletFromDeterministicPrivateKey = (
  domain: string,
  index: number,
): MidgardWallet => {
  const privateKey = deterministicPrivateKey(domain, index);
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(privateKey.to_public().hash()),
  )
    .to_address()
    .to_bech32();
  return walletFromPrivateKey(privateKey, address, { expectedNetworkId: 0 });
};

const fixtureSizes = (
  tx: ReturnType<typeof decodeMidgardNativeTxFullFromCanonicalBinary>,
  compactTxCbor: Uint8Array,
  compactBodyCbor: Uint8Array,
  fullTxCbor: Uint8Array,
): NativeTxFixtureSizes => ({
  fullTxCborBytes: fullTxCbor.length,
  compactTxCborBytes: compactTxCbor.length,
  compactBodyCborBytes: compactBodyCbor.length,
  fee: tx.body.fee.toString(10),
  preimages: {
    spendInputs: encodeSpendInputsBinary(tx.body.spendInputs).length,
    referenceInputs: encodeReferenceInputsBinary(tx.body.referenceInputs)
      .length,
    outputs: encodeOutputsBinary(tx.body.outputs).length,
    requiredObservers: encodeRequiredObserversBinary(tx.body.requiredObservers)
      .length,
    requiredSigners: encodeRequiredSignersBinary(tx.body.requiredSigners)
      .length,
    mint: encodeMidgardMint(tx.body.mint).length,
    addrTxWits: encodeAddrTxWitsBinary(tx.witnessSet).length,
    scriptTxWits: encodeScriptTxWitsBinary(tx.witnessSet).length,
    redeemerTxWits: encodeRedeemerTxWitsBinary(tx.witnessSet).length,
  },
});

export const buildHighCardinalityNativeTxFixture =
  async (): Promise<HighCardinalityNativeTxFixture> => {
    const wallet = walletFromSeedPhrase(seedPhrase, {
      network: "Preview",
      expectedNetworkId: 0,
    });
    const walletAddress = await wallet.address();
    const walletKeyHash = await wallet.keyHash();

    const spendScripts = [0, 1, 2].map((index) => {
      const bytes = scriptBytes("spend", index);
      return { bytes, hash: plutusV3Hash(bytes) };
    });
    const mintScripts = [0, 1, 2, 3, 4, 5].map((index) => {
      const bytes = scriptBytes("mint", index);
      return { bytes, hash: plutusV3Hash(bytes) };
    });
    const observerScripts = [0, 1].map((index) => {
      const bytes = scriptBytes("observer", index);
      return { bytes, hash: plutusV3Hash(bytes) };
    });
    const receiveScripts = [0, 1].map((index) => {
      const bytes = scriptBytes("receive", index);
      return { bytes, hash: midgardV1Hash(bytes) };
    });

    let builder = (
      await LucidMidgard.new(fakeProvider, {
        network: "Preview",
        networkId: 0,
      })
    ).newTx();

    for (const script of [
      ...spendScripts,
      ...mintScripts,
      ...observerScripts,
    ]) {
      builder = builder.attach.Script({
        kind: "plutus-v3",
        language: "PlutusV3",
        script: script.bytes,
      });
    }
    for (const script of receiveScripts) {
      builder = builder.attach.Script({
        kind: "midgard-v1",
        language: "MidgardV1",
        script: script.bytes,
      });
    }

    const pubKeyInputs = [0x31, 0x11, 0x27, 0x05, 0x19].map((byte) =>
      makeUtxo(makeOutRef(byte), walletAddress, { lovelace: 10_000_000n }),
    );
    const scriptInputs = spendScripts.map((script, index) =>
      makeUtxo(
        makeOutRef([0x22, 0x09, 0x3a][index]!),
        scriptAddress(script.hash),
        { lovelace: 10_000_000n },
      ),
    );
    const referenceInputUtxos = [0x70, 0x45, 0x66, 0x52].map((byte) =>
      makeUtxo(makeOutRef(byte), walletAddress, { lovelace: 2_000_000n }),
    );

    builder = builder
      .collectFrom(pubKeyInputs)
      .collectFrom([scriptInputs[0]!], redeemer(101n))
      .collectFrom([scriptInputs[1]!], redeemer(102n))
      .collectFrom([scriptInputs[2]!], redeemer(103n))
      .readFrom(referenceInputUtxos)
      .addSignerKey(walletKeyHash);

    const mintedOutputs: Record<string, bigint>[] = [];
    for (let index = 0; index < mintScripts.length; index += 1) {
      const script = mintScripts[index]!;
      const firstAssetName = makeAssetName(0xa0 + index, index);
      const secondAssetName = makeAssetName(0xb0 + index, index);
      const firstUnit = `${script.hash}${firstAssetName}`;
      const secondUnit = `${script.hash}${secondAssetName}`;
      const firstQuantity = BigInt(index + 1);
      const secondQuantity = BigInt(index + 11);

      builder = builder.mintAssets(
        script.hash,
        {
          [firstAssetName]: firstQuantity,
          [secondAssetName]: secondQuantity,
        },
        redeemer(BigInt(200 + index)),
      );
      mintedOutputs.push({
        lovelace: 4_000_000n,
        [firstUnit]: firstQuantity,
        [secondUnit]: secondQuantity,
      });
    }

    for (const script of observerScripts) {
      builder = builder.observe(script.hash, redeemer(300n));
    }
    for (let index = 0; index < receiveScripts.length; index += 1) {
      builder = builder.receiveRedeemer(
        receiveScripts[index]!.hash,
        redeemer(BigInt(400 + index)),
      );
    }

    for (const value of mintedOutputs) {
      builder = builder.pay.ToAddress(walletAddress, value);
    }
    builder = builder.pay
      .ToAddress(
        walletAddress,
        { lovelace: 4_000_000n },
        {
          datum: CML.PlutusData.new_integer(CML.BigInteger.from_str("77")),
        },
      )
      .pay.ToAddress(
        walletAddress,
        { lovelace: 4_000_000n },
        {
          scriptRef: {
            type: "PlutusV3",
            script: scriptBytes("output-plutus-ref", 0).toString("hex"),
          },
        },
      )
      .pay.ToAddress(
        walletAddress,
        { lovelace: 4_000_000n },
        {
          scriptRef: {
            type: "MidgardV1",
            script: scriptBytes("output-midgard-ref", 0).toString("hex"),
          },
        },
      )
      .pay.ToProtectedAddress(scriptAddress(receiveScripts[0]!.hash), {
        lovelace: 4_000_000n,
      })
      .pay.ToProtectedAddress(scriptAddress(receiveScripts[1]!.hash), {
        lovelace: 4_000_000n,
      })
      .pay.ToAddress(walletAddress, { lovelace: 36_000_000n });

    const completed = await builder.complete();
    const signed = await completed.sign(wallet);
    const tx = decodeMidgardNativeTxFullFromCanonicalBinary(signed.txCbor);
    const compactTxCbor = encodeMidgardNativeTxCompact(tx.compact);
    const compactBodyCbor = encodeMidgardNativeTxBodyCompact(
      tx.compact.transactionBody,
    );

    const spendInputs = tx.body.spendInputs;
    const decodedReferenceInputs = tx.body.referenceInputs;
    const outputs = tx.body.outputs;
    const mint = tx.body.mint;
    const redeemers = redeemerPointers(tx.witnessSet.redeemerTxWits);

    if (
      spendInputs.length !== HIGH_CARDINALITY_COUNTS.spendInputs ||
      decodedReferenceInputs.length !==
        HIGH_CARDINALITY_COUNTS.referenceInputs ||
      outputs.length !== HIGH_CARDINALITY_COUNTS.outputs ||
      mint.size !== HIGH_CARDINALITY_COUNTS.mintPolicies ||
      redeemers.length !== HIGH_CARDINALITY_COUNTS.totalRedeemers
    ) {
      throw new Error("High-cardinality native tx fixture shape drifted");
    }

    const spendInputsBytes = encodeSpendInputsBinary(spendInputs);
    const referenceInputsBytes = encodeReferenceInputsBinary(
      decodedReferenceInputs,
    );
    const outputsBytes = encodeOutputsBinary(outputs);
    const requiredObserversBytes = encodeRequiredObserversBinary(
      tx.body.requiredObservers,
    );
    const requiredSignersBytes = encodeRequiredSignersBinary(
      tx.body.requiredSigners,
    );
    const mintBytes = encodeMidgardMint(mint);
    const addrTxWitsBytes = encodeAddrTxWitsBinary(tx.witnessSet);
    const scriptTxWitsBytes = encodeScriptTxWitsBinary(tx.witnessSet);
    const redeemerTxWitsBytes = encodeRedeemerTxWitsBinary(tx.witnessSet);

    return {
      name: HIGH_CARDINALITY_FIXTURE_NAME,
      txIdHex: hex(computeMidgardNativeTxId(tx)),
      fullTxCborHex: hex(signed.txCbor),
      compactTxCborHex: hex(compactTxCbor),
      compactBodyCborHex: hex(compactBodyCbor),
      counts: HIGH_CARDINALITY_COUNTS,
      sizes: fixtureSizes(tx, compactTxCbor, compactBodyCbor, signed.txCbor),
      mintPolicyIdsInTxInfoOrder: [...mint.keys()],
      redeemerPointers: redeemers,
      preimages: {
        spendInputsCborHex: hex(spendInputsBytes),
        referenceInputsCborHex: hex(referenceInputsBytes),
        outputsCborHex: hex(outputsBytes),
        requiredObserversCborHex: hex(requiredObserversBytes),
        requiredSignersCborHex: hex(requiredSignersBytes),
        mintCborHex: hex(mintBytes),
        addrTxWitsCborHex: hex(addrTxWitsBytes),
        scriptTxWitsCborHex: hex(scriptTxWitsBytes),
        redeemerTxWitsCborHex: hex(redeemerTxWitsBytes),
      },
      hashes: {
        spendInputsHashHex: hex(tx.compact.transactionBody.spendInputsHash),
        referenceInputsHashHex: hex(
          tx.compact.transactionBody.referenceInputsHash,
        ),
        outputsHashHex: hex(tx.compact.transactionBody.outputsHash),
        requiredObserversHashHex: hex(
          tx.compact.transactionBody.requiredObserversHash,
        ),
        requiredSignersHashHex: hex(
          tx.compact.transactionBody.requiredSignersHash,
        ),
        mintHashHex: hex(tx.compact.transactionBody.mintHash),
        addrTxWitsHashHex: hex(computeHash32(addrTxWitsBytes)),
        scriptTxWitsHashHex: hex(computeHash32(scriptTxWitsBytes)),
        redeemerTxWitsHashHex: hex(computeHash32(redeemerTxWitsBytes)),
        witnessSetHashHex: hex(tx.compact.transactionWitnessSetHash),
      },
    };
  };

export const buildSizeBalancedNativeTxFixture =
  async (): Promise<SizeBalancedNativeTxFixture> => {
    const primaryWallet = walletFromSeedPhrase(seedPhrase, {
      network: "Preview",
      expectedNetworkId: 0,
    });
    const primaryAddress = await primaryWallet.address();
    const primaryKeyHash = await primaryWallet.keyHash();
    const extraSignerWallets = Array.from(
      { length: SIZE_BALANCED_COUNTS.requiredSigners - 1 },
      (_, index) => walletFromDeterministicPrivateKey("size-balanced", index),
    );
    const extraSignerKeyHashes = await Promise.all(
      extraSignerWallets.map((wallet) => wallet.keyHash()),
    );

    const spendScripts = Array.from(
      { length: SIZE_BALANCED_COUNTS.spendRedeemers },
      (_, index) => {
        const bytes = paddedScriptBytes("balanced-spend", index, 64);
        return { bytes, hash: plutusV3Hash(bytes) };
      },
    );
    const mintScripts = Array.from(
      { length: SIZE_BALANCED_COUNTS.mintPolicies },
      (_, index) => {
        const bytes = paddedScriptBytes("balanced-mint", index, 64);
        return { bytes, hash: plutusV3Hash(bytes) };
      },
    );
    const observerScripts = Array.from(
      { length: SIZE_BALANCED_COUNTS.observerRedeemers },
      (_, index) => {
        const bytes = paddedScriptBytes("balanced-observe", index, 64);
        return { bytes, hash: plutusV3Hash(bytes) };
      },
    );
    const receiveScripts = Array.from(
      { length: SIZE_BALANCED_COUNTS.receiveRedeemers },
      (_, index) => {
        const bytes = paddedScriptBytes("balanced-receive", index, 64);
        return { bytes, hash: midgardV1Hash(bytes) };
      },
    );

    let builder = (
      await LucidMidgard.new(fakeProvider, {
        network: "Preview",
        networkId: 0,
      })
    ).newTx();

    for (const script of [
      ...spendScripts,
      ...mintScripts,
      ...observerScripts,
    ]) {
      builder = builder.attach.Script({
        kind: "plutus-v3",
        language: "PlutusV3",
        script: script.bytes,
      });
    }
    for (const script of receiveScripts) {
      builder = builder.attach.Script({
        kind: "midgard-v1",
        language: "MidgardV1",
        script: script.bytes,
      });
    }

    const pubKeyInputCount =
      SIZE_BALANCED_COUNTS.spendInputs - SIZE_BALANCED_COUNTS.spendRedeemers;
    const inputLovelace = 20_000_000n;
    const pubKeyInputs = Array.from({ length: pubKeyInputCount }, (_, index) =>
      makeUtxo(makeOutRef(0x01 + index), primaryAddress, {
        lovelace: inputLovelace,
      }),
    );
    const scriptInputs = spendScripts.map((script, index) =>
      makeUtxo(makeOutRef(0x70 + index), scriptAddress(script.hash), {
        lovelace: inputLovelace,
      }),
    );
    const referenceInputUtxos = Array.from(
      { length: SIZE_BALANCED_COUNTS.referenceInputs },
      (_, index) =>
        makeUtxo(makeOutRef(0xa0 + index), primaryAddress, {
          lovelace: 2_000_000n,
        }),
    );

    builder = builder.collectFrom(pubKeyInputs);
    for (let index = 0; index < scriptInputs.length; index += 1) {
      builder = builder.collectFrom(
        [scriptInputs[index]!],
        redeemer(BigInt(1000 + index)),
      );
    }
    builder = builder
      .readFrom(referenceInputUtxos)
      .addSignerKey(primaryKeyHash);
    for (const keyHash of extraSignerKeyHashes) {
      builder = builder.addSignerKey(keyHash);
    }

    const mintedOutputs: Record<string, bigint>[] = [];
    for (let index = 0; index < mintScripts.length; index += 1) {
      const script = mintScripts[index]!;
      const firstAssetName = makeAssetName(0x10 + index, index);
      const secondAssetName = makeAssetName(0x40 + index, index);
      const firstUnit = `${script.hash}${firstAssetName}`;
      const secondUnit = `${script.hash}${secondAssetName}`;
      const firstQuantity = BigInt(index + 1);
      const secondQuantity = BigInt(index + 101);

      builder = builder.mintAssets(
        script.hash,
        {
          [firstAssetName]: firstQuantity,
          [secondAssetName]: secondQuantity,
        },
        redeemer(BigInt(2000 + index)),
      );
      mintedOutputs.push({
        lovelace: 4_000_000n,
        [firstUnit]: firstQuantity,
        [secondUnit]: secondQuantity,
      });
    }

    for (let index = 0; index < observerScripts.length; index += 1) {
      builder = builder.observe(
        observerScripts[index]!.hash,
        redeemer(BigInt(3000 + index)),
      );
    }
    for (let index = 0; index < receiveScripts.length; index += 1) {
      builder = builder.receiveRedeemer(
        receiveScripts[index]!.hash,
        redeemer(BigInt(4000 + index)),
      );
    }

    for (const value of mintedOutputs) {
      builder = builder.pay.ToAddress(primaryAddress, value);
    }
    for (const script of receiveScripts) {
      builder = builder.pay.ToProtectedAddress(scriptAddress(script.hash), {
        lovelace: 4_000_000n,
      });
    }
    for (let index = 0; index < 5; index += 1) {
      builder = builder.pay.ToAddress(
        primaryAddress,
        { lovelace: 4_000_000n },
        index < 4 && index % 2 === 0
          ? {
              datum: CML.PlutusData.new_integer(
                CML.BigInteger.from_str((index + 1).toString()),
              ),
            }
          : undefined,
      );
    }

    const inputTotal = BigInt(SIZE_BALANCED_COUNTS.spendInputs) * inputLovelace;
    const fixedOutputLovelace =
      BigInt(
        SIZE_BALANCED_COUNTS.mintPolicies +
          SIZE_BALANCED_COUNTS.receiveRedeemers +
          5,
      ) * 4_000_000n;
    const finalOutputLovelace =
      inputTotal - fixedOutputLovelace - SIZE_BALANCED_FEE;
    if (finalOutputLovelace <= 0n) {
      throw new Error("Size-balanced fixture is not lovelace-balanced");
    }
    builder = builder.pay.ToAddress(primaryAddress, {
      lovelace: finalOutputLovelace,
    });

    const completed = await builder.complete({ fee: SIZE_BALANCED_FEE });
    let signed = await completed.sign(primaryWallet);
    for (const wallet of extraSignerWallets) {
      signed = await signed.sign(wallet);
    }
    const phaseAReport = await signed.validate("phase-a");
    if (
      phaseAReport.rejected.length > 0 ||
      !phaseAReport.acceptedTxIds.includes(signed.txIdHex)
    ) {
      throw new Error(
        "Size-balanced native tx fixture failed Phase A validation",
      );
    }

    const tx = decodeMidgardNativeTxFullFromCanonicalBinary(signed.txCbor);
    const compactTxCbor = encodeMidgardNativeTxCompact(tx.compact);
    const compactBodyCbor = encodeMidgardNativeTxBodyCompact(
      tx.compact.transactionBody,
    );

    const spendInputs = tx.body.spendInputs;
    const decodedReferenceInputs = tx.body.referenceInputs;
    const outputs = tx.body.outputs;
    const requiredObservers = tx.body.requiredObservers;
    const requiredSigners = tx.body.requiredSigners;
    const mint = tx.body.mint;
    const addrTxWits = tx.witnessSet.addrTxWits;
    const scriptTxWits = tx.witnessSet.scriptTxWits;
    const redeemers = redeemerPointers(tx.witnessSet.redeemerTxWits);

    const expectedLengths = {
      spendInputs: SIZE_BALANCED_COUNTS.spendInputs,
      referenceInputs: SIZE_BALANCED_COUNTS.referenceInputs,
      outputs: SIZE_BALANCED_COUNTS.outputs,
      requiredObservers: SIZE_BALANCED_COUNTS.observerRedeemers,
      requiredSigners: SIZE_BALANCED_COUNTS.requiredSigners,
      mintPolicies: SIZE_BALANCED_COUNTS.mintPolicies,
      addrWitnesses: SIZE_BALANCED_COUNTS.addrWitnesses,
      scriptWitnesses: SIZE_BALANCED_COUNTS.scriptWitnesses,
      totalRedeemers: SIZE_BALANCED_COUNTS.totalRedeemers,
    };
    const actualLengths = {
      spendInputs: spendInputs.length,
      referenceInputs: decodedReferenceInputs.length,
      outputs: outputs.length,
      requiredObservers: requiredObservers.length,
      requiredSigners: requiredSigners.length,
      mintPolicies: mint.size,
      addrWitnesses: addrTxWits.length,
      scriptWitnesses: scriptTxWits.length,
      totalRedeemers: redeemers.length,
    };
    if (JSON.stringify(actualLengths) !== JSON.stringify(expectedLengths)) {
      throw new Error(
        `Size-balanced native tx fixture shape drifted: ${JSON.stringify(
          actualLengths,
        )}`,
      );
    }

    const sizes = fixtureSizes(
      tx,
      compactTxCbor,
      compactBodyCbor,
      signed.txCbor,
    );
    const lowerBound =
      SIZE_BALANCED_TARGET_FULL_TX_CBOR_BYTES -
      SIZE_BALANCED_FULL_TX_CBOR_TOLERANCE_BYTES;
    const upperBound =
      SIZE_BALANCED_TARGET_FULL_TX_CBOR_BYTES +
      SIZE_BALANCED_FULL_TX_CBOR_TOLERANCE_BYTES;
    if (
      sizes.fullTxCborBytes < lowerBound ||
      sizes.fullTxCborBytes > upperBound
    ) {
      throw new Error(
        `Size-balanced native tx fixture size ${sizes.fullTxCborBytes.toString()} is outside target range ${lowerBound.toString()}-${upperBound.toString()}`,
      );
    }
    if (tx.body.fee > SIZE_BALANCED_MAX_FEE) {
      throw new Error("Size-balanced native tx fixture fee exceeds max fee");
    }
    for (const [field, count] of Object.entries(actualLengths)) {
      if (count > SIZE_BALANCED_MAX_LIST_LENGTH) {
        throw new Error(`${field} count exceeds max fixture list length`);
      }
    }

    return {
      name: SIZE_BALANCED_FIXTURE_NAME,
      txIdHex: hex(computeMidgardNativeTxId(tx)),
      fullTxCborHex: hex(signed.txCbor),
      compactTxCborHex: hex(compactTxCbor),
      compactBodyCborHex: hex(compactBodyCbor),
      counts: SIZE_BALANCED_COUNTS,
      targetFullTxCborBytes: SIZE_BALANCED_TARGET_FULL_TX_CBOR_BYTES,
      fullTxCborToleranceBytes: SIZE_BALANCED_FULL_TX_CBOR_TOLERANCE_BYTES,
      maxListLength: SIZE_BALANCED_MAX_LIST_LENGTH,
      maxFee: SIZE_BALANCED_MAX_FEE.toString(10),
      sizes,
      mintPolicyIdsInTxInfoOrder: [...mint.keys()],
      redeemerPointers: redeemers,
      preimages: {
        spendInputsCborHex: hex(encodeSpendInputsBinary(spendInputs)),
        referenceInputsCborHex: hex(
          encodeReferenceInputsBinary(decodedReferenceInputs),
        ),
        outputsCborHex: hex(encodeOutputsBinary(outputs)),
        requiredObserversCborHex: hex(
          encodeRequiredObserversBinary(requiredObservers),
        ),
        requiredSignersCborHex: hex(encodeRequiredSignersBinary(requiredSigners)),
        mintCborHex: hex(encodeMidgardMint(mint)),
        addrTxWitsCborHex: hex(encodeAddrTxWitsBinary(tx.witnessSet)),
        scriptTxWitsCborHex: hex(encodeScriptTxWitsBinary(tx.witnessSet)),
        redeemerTxWitsCborHex: hex(encodeRedeemerTxWitsBinary(tx.witnessSet)),
      },
      hashes: {
        spendInputsHashHex: hex(tx.compact.transactionBody.spendInputsHash),
        referenceInputsHashHex: hex(
          tx.compact.transactionBody.referenceInputsHash,
        ),
        outputsHashHex: hex(tx.compact.transactionBody.outputsHash),
        requiredObserversHashHex: hex(
          tx.compact.transactionBody.requiredObserversHash,
        ),
        requiredSignersHashHex: hex(
          tx.compact.transactionBody.requiredSignersHash,
        ),
        mintHashHex: hex(tx.compact.transactionBody.mintHash),
        addrTxWitsHashHex: hex(
          computeHash32(encodeAddrTxWitsBinary(tx.witnessSet)),
        ),
        scriptTxWitsHashHex: hex(
          computeHash32(encodeScriptTxWitsBinary(tx.witnessSet)),
        ),
        redeemerTxWitsHashHex: hex(
          computeHash32(encodeRedeemerTxWitsBinary(tx.witnessSet)),
        ),
        witnessSetHashHex: hex(tx.compact.transactionWitnessSetHash),
      },
    };
  };

export const stableFixtureJson = (
  fixture: HighCardinalityNativeTxFixture | SizeBalancedNativeTxFixture,
): string => `${JSON.stringify(fixture, null, 2)}\n`;

const aikenByteString = (hexValue: string): string => `#"${hexValue}"`;

const pointerPairs = (pointers: readonly string[]): string =>
  pointers
    .map((pointer) => {
      const [tag, index] = pointer.split(":");
      return `Pair(${tag}, ${index})`;
    })
    .join(", ");

export const renderHighCardinalityAikenTest = (
  fixture: HighCardinalityNativeTxFixture,
): string => `// Generated by demo/lucid-midgard/scripts/generate-native-tx-aiken-fixtures.ts.
// Do not edit by hand.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/fuzz
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact, encode_native_tx_body_compact,
  encode_native_tx_compact, verify_native_tx_compact,
  verify_native_tx_compact_cbor,
}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_address_witnesses_preimage_cbor,
  decode_midgard_tx_byte_list_preimage_cbor,
  decode_midgard_tx_inputs_preimage_cbor,
  decode_midgard_tx_mint_preimage_cbor,
  decode_midgard_tx_outputs_preimage_cbor,
  decode_midgard_tx_redeemer_witnesses_preimage_cbor,
  decode_midgard_tx_script_witnesses_preimage_cbor,
  encode_address_witness_preimage, encode_input_preimage,
  encode_mint_preimage, encode_output_preimage,
  encode_redeemer_witness_preimage, encode_script_witness_preimage,
}
use midgard/fraud_proofs/native_tx/transaction.{
  decode_midgard_transaction, encode_midgard_transaction,
  midgard_transaction_to_compact, midgard_transaction_witness_set_to_compact,
  partial_view_from_compact_and_preimages,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardRedeemerPurpose, MidgardRedeemerWitness,
  MidgardTransactionBodyPartialPreimages,
  MidgardTransactionWitnessSetPartialPreimages, MintRedeemer, NativeTxCompact,
  ReceiveRedeemer, RewardRedeemer, SpendRedeemer,
}

const high_cardinality_tx_cbor = ${aikenByteString(fixture.fullTxCborHex)}

const high_cardinality_compact_cbor = ${aikenByteString(fixture.compactTxCborHex)}

const high_cardinality_tx_id = ${aikenByteString(fixture.txIdHex)}

const high_cardinality_spend_inputs_preimage_cbor =
  ${aikenByteString(fixture.preimages.spendInputsCborHex)}

const high_cardinality_reference_inputs_preimage_cbor =
  ${aikenByteString(fixture.preimages.referenceInputsCborHex)}

const high_cardinality_outputs_preimage_cbor =
  ${aikenByteString(fixture.preimages.outputsCborHex)}

const high_cardinality_required_observers_preimage_cbor =
  ${aikenByteString(fixture.preimages.requiredObserversCborHex)}

const high_cardinality_required_signers_preimage_cbor =
  ${aikenByteString(fixture.preimages.requiredSignersCborHex)}

const high_cardinality_mint_preimage_cbor =
  ${aikenByteString(fixture.preimages.mintCborHex)}

const high_cardinality_addr_wits_preimage_cbor =
  ${aikenByteString(fixture.preimages.addrTxWitsCborHex)}

const high_cardinality_script_wits_preimage_cbor =
  ${aikenByteString(fixture.preimages.scriptTxWitsCborHex)}

const high_cardinality_redeemer_wits_preimage_cbor =
  ${aikenByteString(fixture.preimages.redeemerTxWitsCborHex)}

fn sample_high_cardinality_tx_cbor(_size: Int) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_tx_cbor)
}

fn sample_high_cardinality_compact_cbor(_size: Int) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_compact_cbor)
}

fn sample_high_cardinality_compact_fixture(
  _size: Int,
) -> Fuzzer<Pair<NativeTxCompact, ByteArray>> {
  fuzz.constant(
    Pair(
      decode_native_tx_compact(high_cardinality_compact_cbor),
      high_cardinality_compact_cbor,
    ),
  )
}

fn sample_high_cardinality_spend_inputs_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_spend_inputs_preimage_cbor)
}

fn sample_high_cardinality_reference_inputs_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_reference_inputs_preimage_cbor)
}

fn sample_high_cardinality_outputs_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_outputs_preimage_cbor)
}

fn sample_high_cardinality_required_observers_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_required_observers_preimage_cbor)
}

fn sample_high_cardinality_required_signers_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_required_signers_preimage_cbor)
}

fn sample_high_cardinality_mint_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_mint_preimage_cbor)
}

fn sample_high_cardinality_addr_wits_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_addr_wits_preimage_cbor)
}

fn sample_high_cardinality_script_wits_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_script_wits_preimage_cbor)
}

fn sample_high_cardinality_redeemer_wits_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(high_cardinality_redeemer_wits_preimage_cbor)
}

fn redeemer_purpose_tag(purpose: MidgardRedeemerPurpose) -> Int {
  when purpose is {
    SpendRedeemer -> 0
    MintRedeemer -> 1
    RewardRedeemer -> 3
    ReceiveRedeemer -> 6
    _ -> -1
  }
}

fn redeemer_pointer_tags(
  redeemers: List<MidgardRedeemerWitness>,
) -> List<Pair<Int, Int>> {
  redeemers
    |> list.map(fn(redeemer) {
      Pair(redeemer_purpose_tag(redeemer.purpose), redeemer.index)
    })
}

bench high_cardinality_decode_midgard_transaction_bench(
  tx_cbor: ByteArray via sample_high_cardinality_tx_cbor,
) {
  let decoded = decode_midgard_transaction(tx_cbor)
  and {
    decoded.version == 1,
    list.length(decoded.body.inputs) == ${fixture.counts.spendInputs},
    list.length(decoded.body.reference_inputs) == ${fixture.counts.referenceInputs},
    list.length(decoded.body.outputs) == ${fixture.counts.outputs},
    list.length(decoded.witness_set.redeemer_tx_wits) == ${fixture.counts.totalRedeemers},
  }
}

bench high_cardinality_decode_native_tx_compact_bench(
  compact_cbor: ByteArray via sample_high_cardinality_compact_cbor,
) {
  let compact = decode_native_tx_compact(compact_cbor)
  compact.validity_code == 0
}

bench high_cardinality_verify_native_tx_compact_reencode_bench(
  fixture: Pair<NativeTxCompact, ByteArray> via sample_high_cardinality_compact_fixture,
) {
  let Pair(compact, compact_cbor) = fixture
  let verified =
    verify_native_tx_compact(high_cardinality_tx_id, compact, compact_cbor)
  verified.tx_compact.validity_code == 0
}

bench high_cardinality_verify_native_tx_compact_cbor_bench(
  compact_cbor: ByteArray via sample_high_cardinality_compact_cbor,
) {
  let verified =
    verify_native_tx_compact_cbor(high_cardinality_tx_id, compact_cbor)
  verified.tx_compact.validity_code == 0
}

bench high_cardinality_decode_then_verify_native_tx_compact_reencode_bench(
  compact_cbor: ByteArray via sample_high_cardinality_compact_cbor,
) {
  let compact = decode_native_tx_compact(compact_cbor)
  let verified =
    verify_native_tx_compact(high_cardinality_tx_id, compact, compact_cbor)
  verified.tx_compact.validity_code == 0
}

bench high_cardinality_decode_spend_inputs_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_spend_inputs_preimage_cbor,
) {
  list.length(decode_midgard_tx_inputs_preimage_cbor(preimage_cbor)) == ${fixture.counts.spendInputs}
}

bench high_cardinality_decode_reference_inputs_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_reference_inputs_preimage_cbor,
) {
  list.length(decode_midgard_tx_inputs_preimage_cbor(preimage_cbor)) == ${fixture.counts.referenceInputs}
}

bench high_cardinality_decode_outputs_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_outputs_preimage_cbor,
) {
  list.length(decode_midgard_tx_outputs_preimage_cbor(preimage_cbor)) == ${fixture.counts.outputs}
}

bench high_cardinality_decode_required_observers_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_required_observers_preimage_cbor,
) {
  list.length(decode_midgard_tx_byte_list_preimage_cbor(preimage_cbor)) == 2
}

bench high_cardinality_decode_required_signers_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_required_signers_preimage_cbor,
) {
  list.length(decode_midgard_tx_byte_list_preimage_cbor(preimage_cbor)) == 1
}

bench high_cardinality_decode_mint_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_mint_preimage_cbor,
) {
  let mint = decode_midgard_tx_mint_preimage_cbor(preimage_cbor)
  list.length(builtin.un_map_data(mint)) == ${fixture.counts.mintPolicies}
}

bench high_cardinality_decode_address_witnesses_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_addr_wits_preimage_cbor,
) {
  list.length(decode_midgard_tx_address_witnesses_preimage_cbor(preimage_cbor)) == 1
}

bench high_cardinality_decode_script_witnesses_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_script_wits_preimage_cbor,
) {
  list.length(decode_midgard_tx_script_witnesses_preimage_cbor(preimage_cbor)) == 13
}

bench high_cardinality_decode_redeemer_witnesses_preimage_bench(
  preimage_cbor: ByteArray via sample_high_cardinality_redeemer_wits_preimage_cbor,
) {
  list.length(decode_midgard_tx_redeemer_witnesses_preimage_cbor(preimage_cbor)) == ${fixture.counts.totalRedeemers}
}

test high_cardinality_lucid_midgard_native_tx_decodes() {
  let tx_cbor = high_cardinality_tx_cbor
  let compact_cbor = high_cardinality_compact_cbor
  let tx_id = high_cardinality_tx_id
  let spend_inputs_preimage_cbor =
    high_cardinality_spend_inputs_preimage_cbor
  let reference_inputs_preimage_cbor =
    high_cardinality_reference_inputs_preimage_cbor
  let outputs_preimage_cbor = high_cardinality_outputs_preimage_cbor
  let required_observers_preimage_cbor =
    high_cardinality_required_observers_preimage_cbor
  let required_signers_preimage_cbor =
    high_cardinality_required_signers_preimage_cbor
  let mint_preimage_cbor = high_cardinality_mint_preimage_cbor
  let addr_wits_preimage_cbor =
    high_cardinality_addr_wits_preimage_cbor
  let script_wits_preimage_cbor =
    high_cardinality_script_wits_preimage_cbor
  let redeemer_wits_preimage_cbor =
    high_cardinality_redeemer_wits_preimage_cbor
  let decoded_spend_inputs =
    decode_midgard_tx_inputs_preimage_cbor(spend_inputs_preimage_cbor)
  let decoded_reference_inputs =
    decode_midgard_tx_inputs_preimage_cbor(reference_inputs_preimage_cbor)
  let decoded_outputs =
    decode_midgard_tx_outputs_preimage_cbor(outputs_preimage_cbor)
  let decoded_mint = decode_midgard_tx_mint_preimage_cbor(mint_preimage_cbor)
  let decoded_addr_wits =
    decode_midgard_tx_address_witnesses_preimage_cbor(addr_wits_preimage_cbor)
  let decoded_script_wits =
    decode_midgard_tx_script_witnesses_preimage_cbor(script_wits_preimage_cbor)
  let decoded_redeemer_wits =
    decode_midgard_tx_redeemer_witnesses_preimage_cbor(
      redeemer_wits_preimage_cbor,
    )
  expect encode_input_preimage(decoded_spend_inputs) == spend_inputs_preimage_cbor
  expect
    encode_input_preimage(decoded_reference_inputs) == reference_inputs_preimage_cbor
  expect encode_output_preimage(decoded_outputs) == outputs_preimage_cbor
  expect encode_mint_preimage(decoded_mint) == mint_preimage_cbor
  expect
    encode_address_witness_preimage(decoded_addr_wits) == addr_wits_preimage_cbor
  expect
    encode_script_witness_preimage(decoded_script_wits) == script_wits_preimage_cbor
  expect
    encode_redeemer_witness_preimage(decoded_redeemer_wits) == redeemer_wits_preimage_cbor

  let decoded = decode_midgard_transaction(tx_cbor)
  let compact = midgard_transaction_to_compact(decoded)
  let compact_from_cbor = decode_native_tx_compact(compact_cbor)
  let verified = verify_native_tx_compact(tx_id, compact_from_cbor, compact_cbor)
  let verified_from_cbor = verify_native_tx_compact_cbor(tx_id, compact_cbor)
  let witness_set_compact =
    midgard_transaction_witness_set_to_compact(decoded.witness_set)
  let body_preimages =
    MidgardTransactionBodyPartialPreimages {
      inputs: Some(spend_inputs_preimage_cbor),
      reference_inputs: Some(reference_inputs_preimage_cbor),
      outputs: Some(outputs_preimage_cbor),
      required_observers: Some(required_observers_preimage_cbor),
      required_signers: Some(required_signers_preimage_cbor),
      mint: Some(mint_preimage_cbor),
    }
  let witness_set_preimages =
    MidgardTransactionWitnessSetPartialPreimages {
      addr_tx_wits: Some(addr_wits_preimage_cbor),
      script_tx_wits: Some(script_wits_preimage_cbor),
      redeemer_tx_wits: Some(redeemer_wits_preimage_cbor),
    }
  let partial =
    partial_view_from_compact_and_preimages(
      verified,
      body_preimages,
      Some(witness_set_compact),
      witness_set_preimages,
    )
  expect Some(partial_body) = partial.body
  expect Some(partial_witness_set) = partial.witness_set

  and {
    verified_from_cbor == verified,
    encode_midgard_transaction(decoded) == tx_cbor,
    compact == compact_from_cbor,
    encode_native_tx_compact(compact) == compact_cbor,
    blake2b_256(encode_native_tx_body_compact(compact.body)) == tx_id,
    list.length(decoded.body.inputs) == ${fixture.counts.spendInputs},
    list.length(decoded.body.reference_inputs) == ${fixture.counts.referenceInputs},
    list.length(decoded.body.outputs) == ${fixture.counts.outputs},
    list.length(builtin.un_map_data(decoded.body.mint)) == ${fixture.counts.mintPolicies},
    list.length(decoded.witness_set.addr_tx_wits) == 1,
    list.length(decoded.witness_set.script_tx_wits) == 13,
    list.length(decoded.witness_set.redeemer_tx_wits) == ${fixture.counts.totalRedeemers},
    list.count(
      decoded.witness_set.redeemer_tx_wits,
      fn(redeemer) { redeemer.purpose == SpendRedeemer },
    ) == ${fixture.counts.spendRedeemers},
    list.count(
      decoded.witness_set.redeemer_tx_wits,
      fn(redeemer) { redeemer.purpose == MintRedeemer },
    ) == ${fixture.counts.mintRedeemers},
    list.count(
      decoded.witness_set.redeemer_tx_wits,
      fn(redeemer) { redeemer.purpose == RewardRedeemer },
    ) == ${fixture.counts.observerRedeemers},
    list.count(
      decoded.witness_set.redeemer_tx_wits,
      fn(redeemer) { redeemer.purpose == ReceiveRedeemer },
    ) == ${fixture.counts.receiveRedeemers},
    redeemer_pointer_tags(decoded.witness_set.redeemer_tx_wits) == [
      ${pointerPairs(fixture.redeemerPointers)}
    ],
    encode_input_preimage(decoded_spend_inputs) == spend_inputs_preimage_cbor,
    encode_input_preimage(decoded_reference_inputs) == reference_inputs_preimage_cbor,
    encode_output_preimage(decoded_outputs) == outputs_preimage_cbor,
    encode_mint_preimage(decoded_mint) == mint_preimage_cbor,
    encode_address_witness_preimage(decoded_addr_wits) == addr_wits_preimage_cbor,
    encode_script_witness_preimage(decoded_script_wits) == script_wits_preimage_cbor,
    encode_redeemer_witness_preimage(decoded_redeemer_wits) == redeemer_wits_preimage_cbor,
    list.length(decoded_spend_inputs) == ${fixture.counts.spendInputs},
    list.length(decoded_reference_inputs) == ${fixture.counts.referenceInputs},
    list.length(decoded_outputs) == ${fixture.counts.outputs},
    list.length(decode_midgard_tx_byte_list_preimage_cbor(required_observers_preimage_cbor)) == 2,
    list.length(decode_midgard_tx_byte_list_preimage_cbor(required_signers_preimage_cbor)) == 1,
    list.length(builtin.un_map_data(decoded_mint)) == ${fixture.counts.mintPolicies},
    list.length(decoded_addr_wits) == 1,
    list.length(decoded_script_wits) == 13,
    list.length(decoded_redeemer_wits) == ${fixture.counts.totalRedeemers},
    blake2b_256(spend_inputs_preimage_cbor) == compact.body.spend_inputs_hash,
    blake2b_256(reference_inputs_preimage_cbor) == compact.body.reference_inputs_hash,
    blake2b_256(outputs_preimage_cbor) == compact.body.outputs_hash,
    blake2b_256(required_observers_preimage_cbor) == compact.body.required_observers_hash,
    blake2b_256(required_signers_preimage_cbor) == compact.body.required_signers_hash,
    blake2b_256(mint_preimage_cbor) == compact.body.mint_hash,
    partial_body.inputs == Some(decoded.body.inputs),
    partial_body.reference_inputs == Some(decoded.body.reference_inputs),
    partial_body.outputs == Some(decoded.body.outputs),
    partial_body.required_observers == Some(decoded.body.required_observers),
    partial_body.required_signers == Some(decoded.body.required_signers),
    partial_body.mint == Some(decoded.body.mint),
    partial_witness_set.addr_tx_wits == Some(decoded.witness_set.addr_tx_wits),
    partial_witness_set.script_tx_wits == Some(decoded.witness_set.script_tx_wits),
    partial_witness_set.redeemer_tx_wits == Some(
      decoded.witness_set.redeemer_tx_wits,
    ),
  }
}
`;

export const renderSizeBalancedAikenTest = (
  fixture: SizeBalancedNativeTxFixture,
): string => `// Generated by demo/lucid-midgard/scripts/generate-native-tx-aiken-fixtures.ts.
// Do not edit by hand.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/fuzz
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact, encode_native_tx_body_compact,
  encode_native_tx_compact, verify_native_tx_compact,
  verify_native_tx_compact_cbor,
}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_address_witnesses_preimage_cbor,
  decode_midgard_tx_byte_list_preimage_cbor,
  decode_midgard_tx_inputs_preimage_cbor,
  decode_midgard_tx_mint_preimage_cbor,
  decode_midgard_tx_outputs_preimage_cbor,
  decode_midgard_tx_redeemer_witnesses_preimage_cbor,
  decode_midgard_tx_script_witnesses_preimage_cbor,
}
use midgard/fraud_proofs/native_tx/transaction.{
  decode_midgard_transaction, encode_midgard_transaction,
  midgard_transaction_to_compact,
}
use midgard/fraud_proofs/native_tx/types.{MidgardTransaction, NativeTxCompact}

const size_balanced_tx_cbor = ${aikenByteString(fixture.fullTxCborHex)}

const size_balanced_compact_cbor = ${aikenByteString(fixture.compactTxCborHex)}

const size_balanced_tx_id = ${aikenByteString(fixture.txIdHex)}

const size_balanced_target_tx_cbor_bytes = ${fixture.targetFullTxCborBytes}

const size_balanced_full_tx_cbor_bytes = ${fixture.sizes.fullTxCborBytes}

const size_balanced_compact_tx_cbor_bytes = ${fixture.sizes.compactTxCborBytes}

const size_balanced_fee = ${fixture.sizes.fee}

const size_balanced_spend_inputs_preimage_cbor =
  ${aikenByteString(fixture.preimages.spendInputsCborHex)}

const size_balanced_reference_inputs_preimage_cbor =
  ${aikenByteString(fixture.preimages.referenceInputsCborHex)}

const size_balanced_outputs_preimage_cbor =
  ${aikenByteString(fixture.preimages.outputsCborHex)}

const size_balanced_required_observers_preimage_cbor =
  ${aikenByteString(fixture.preimages.requiredObserversCborHex)}

const size_balanced_required_signers_preimage_cbor =
  ${aikenByteString(fixture.preimages.requiredSignersCborHex)}

const size_balanced_mint_preimage_cbor =
  ${aikenByteString(fixture.preimages.mintCborHex)}

const size_balanced_addr_wits_preimage_cbor =
  ${aikenByteString(fixture.preimages.addrTxWitsCborHex)}

const size_balanced_script_wits_preimage_cbor =
  ${aikenByteString(fixture.preimages.scriptTxWitsCborHex)}

const size_balanced_redeemer_wits_preimage_cbor =
  ${aikenByteString(fixture.preimages.redeemerTxWitsCborHex)}

fn sample_size_balanced_tx_cbor(_size: Int) -> Fuzzer<ByteArray> {
  fuzz.constant(size_balanced_tx_cbor)
}

fn sample_size_balanced_compact_cbor(_size: Int) -> Fuzzer<ByteArray> {
  fuzz.constant(size_balanced_compact_cbor)
}

fn sample_size_balanced_decoded_tx(
  _size: Int,
) -> Fuzzer<MidgardTransaction> {
  fuzz.constant(decode_midgard_transaction(size_balanced_tx_cbor))
}

fn sample_size_balanced_compact_fixture(
  _size: Int,
) -> Fuzzer<Pair<NativeTxCompact, ByteArray>> {
  fuzz.constant(
    Pair(
      decode_native_tx_compact(size_balanced_compact_cbor),
      size_balanced_compact_cbor,
    ),
  )
}

fn sample_size_balanced_spend_inputs_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(size_balanced_spend_inputs_preimage_cbor)
}

fn sample_size_balanced_reference_inputs_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(size_balanced_reference_inputs_preimage_cbor)
}

fn sample_size_balanced_outputs_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(size_balanced_outputs_preimage_cbor)
}

fn sample_size_balanced_required_observers_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(size_balanced_required_observers_preimage_cbor)
}

fn sample_size_balanced_required_signers_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(size_balanced_required_signers_preimage_cbor)
}

fn sample_size_balanced_mint_preimage_cbor(_size: Int) -> Fuzzer<ByteArray> {
  fuzz.constant(size_balanced_mint_preimage_cbor)
}

fn sample_size_balanced_addr_wits_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(size_balanced_addr_wits_preimage_cbor)
}

fn sample_size_balanced_script_wits_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(size_balanced_script_wits_preimage_cbor)
}

fn sample_size_balanced_redeemer_wits_preimage_cbor(
  _size: Int,
) -> Fuzzer<ByteArray> {
  fuzz.constant(size_balanced_redeemer_wits_preimage_cbor)
}

bench size_balanced_decode_midgard_transaction_bench(
  tx_cbor: ByteArray via sample_size_balanced_tx_cbor,
) {
  let decoded = decode_midgard_transaction(tx_cbor)
  and {
    decoded.version == 1,
    decoded.body.fee == size_balanced_fee,
    list.length(decoded.body.inputs) == ${fixture.counts.spendInputs},
    list.length(decoded.body.reference_inputs) == ${fixture.counts.referenceInputs},
    list.length(decoded.body.outputs) == ${fixture.counts.outputs},
    list.length(decoded.witness_set.addr_tx_wits) == ${fixture.counts.addrWitnesses},
    list.length(decoded.witness_set.script_tx_wits) == ${fixture.counts.scriptWitnesses},
    list.length(decoded.witness_set.redeemer_tx_wits) == ${fixture.counts.totalRedeemers},
  }
}

bench size_balanced_encode_midgard_transaction_bench(
  decoded: MidgardTransaction via sample_size_balanced_decoded_tx,
) {
  let encoded = encode_midgard_transaction(decoded)
  and {
    bytearray.length(encoded) == size_balanced_full_tx_cbor_bytes,
    encoded == size_balanced_tx_cbor,
  }
}

bench size_balanced_decode_native_tx_compact_bench(
  compact_cbor: ByteArray via sample_size_balanced_compact_cbor,
) {
  let compact = decode_native_tx_compact(compact_cbor)
  compact.validity_code == 0
}

bench size_balanced_verify_native_tx_compact_reencode_bench(
  fixture: Pair<NativeTxCompact, ByteArray> via sample_size_balanced_compact_fixture,
) {
  let Pair(compact, compact_cbor) = fixture
  let verified =
    verify_native_tx_compact(size_balanced_tx_id, compact, compact_cbor)
  verified.tx_compact.validity_code == 0
}

bench size_balanced_verify_native_tx_compact_cbor_bench(
  compact_cbor: ByteArray via sample_size_balanced_compact_cbor,
) {
  let verified =
    verify_native_tx_compact_cbor(size_balanced_tx_id, compact_cbor)
  verified.tx_compact.validity_code == 0
}

bench size_balanced_decode_spend_inputs_preimage_bench(
  preimage_cbor: ByteArray via sample_size_balanced_spend_inputs_preimage_cbor,
) {
  list.length(decode_midgard_tx_inputs_preimage_cbor(preimage_cbor)) == ${fixture.counts.spendInputs}
}

bench size_balanced_decode_reference_inputs_preimage_bench(
  preimage_cbor: ByteArray via sample_size_balanced_reference_inputs_preimage_cbor,
) {
  list.length(decode_midgard_tx_inputs_preimage_cbor(preimage_cbor)) == ${fixture.counts.referenceInputs}
}

bench size_balanced_decode_outputs_preimage_bench(
  preimage_cbor: ByteArray via sample_size_balanced_outputs_preimage_cbor,
) {
  list.length(decode_midgard_tx_outputs_preimage_cbor(preimage_cbor)) == ${fixture.counts.outputs}
}

bench size_balanced_decode_required_observers_preimage_bench(
  preimage_cbor: ByteArray via sample_size_balanced_required_observers_preimage_cbor,
) {
  list.length(decode_midgard_tx_byte_list_preimage_cbor(preimage_cbor)) == ${fixture.counts.observerRedeemers}
}

bench size_balanced_decode_required_signers_preimage_bench(
  preimage_cbor: ByteArray via sample_size_balanced_required_signers_preimage_cbor,
) {
  list.length(decode_midgard_tx_byte_list_preimage_cbor(preimage_cbor)) == ${fixture.counts.requiredSigners}
}

bench size_balanced_decode_mint_preimage_bench(
  preimage_cbor: ByteArray via sample_size_balanced_mint_preimage_cbor,
) {
  let mint = decode_midgard_tx_mint_preimage_cbor(preimage_cbor)
  list.length(builtin.un_map_data(mint)) == ${fixture.counts.mintPolicies}
}

bench size_balanced_decode_address_witnesses_preimage_bench(
  preimage_cbor: ByteArray via sample_size_balanced_addr_wits_preimage_cbor,
) {
  list.length(decode_midgard_tx_address_witnesses_preimage_cbor(preimage_cbor)) == ${fixture.counts.addrWitnesses}
}

bench size_balanced_decode_script_witnesses_preimage_bench(
  preimage_cbor: ByteArray via sample_size_balanced_script_wits_preimage_cbor,
) {
  list.length(decode_midgard_tx_script_witnesses_preimage_cbor(preimage_cbor)) == ${fixture.counts.scriptWitnesses}
}

bench size_balanced_decode_redeemer_witnesses_preimage_bench(
  preimage_cbor: ByteArray via sample_size_balanced_redeemer_wits_preimage_cbor,
) {
  list.length(decode_midgard_tx_redeemer_witnesses_preimage_cbor(preimage_cbor)) == ${fixture.counts.totalRedeemers}
}

test size_balanced_lucid_midgard_native_tx_decodes() {
  let tx_cbor = size_balanced_tx_cbor
  let compact_cbor = size_balanced_compact_cbor
  let tx_id = size_balanced_tx_id
  let decoded = decode_midgard_transaction(tx_cbor)
  let compact = midgard_transaction_to_compact(decoded)
  let compact_from_cbor = decode_native_tx_compact(compact_cbor)
  let verified = verify_native_tx_compact(tx_id, compact_from_cbor, compact_cbor)
  let verified_from_cbor = verify_native_tx_compact_cbor(tx_id, compact_cbor)
  let decoded_spend_inputs =
    decode_midgard_tx_inputs_preimage_cbor(size_balanced_spend_inputs_preimage_cbor)
  let decoded_reference_inputs =
    decode_midgard_tx_inputs_preimage_cbor(size_balanced_reference_inputs_preimage_cbor)
  let decoded_outputs =
    decode_midgard_tx_outputs_preimage_cbor(size_balanced_outputs_preimage_cbor)
  let decoded_required_observers =
    decode_midgard_tx_byte_list_preimage_cbor(
      size_balanced_required_observers_preimage_cbor,
    )
  let decoded_required_signers =
    decode_midgard_tx_byte_list_preimage_cbor(
      size_balanced_required_signers_preimage_cbor,
    )
  let decoded_mint =
    decode_midgard_tx_mint_preimage_cbor(size_balanced_mint_preimage_cbor)
  let decoded_addr_wits =
    decode_midgard_tx_address_witnesses_preimage_cbor(
      size_balanced_addr_wits_preimage_cbor,
    )
  let decoded_script_wits =
    decode_midgard_tx_script_witnesses_preimage_cbor(
      size_balanced_script_wits_preimage_cbor,
    )
  let decoded_redeemer_wits =
    decode_midgard_tx_redeemer_witnesses_preimage_cbor(
      size_balanced_redeemer_wits_preimage_cbor,
    )

  and {
    bytearray.length(tx_cbor) == size_balanced_full_tx_cbor_bytes,
    bytearray.length(compact_cbor) == size_balanced_compact_tx_cbor_bytes,
    size_balanced_full_tx_cbor_bytes >= size_balanced_target_tx_cbor_bytes - ${fixture.fullTxCborToleranceBytes},
    size_balanced_full_tx_cbor_bytes <= size_balanced_target_tx_cbor_bytes + ${fixture.fullTxCborToleranceBytes},
    verified_from_cbor == verified,
    compact == compact_from_cbor,
    verified.tx_compact == compact_from_cbor,
    encode_midgard_transaction(decoded) == tx_cbor,
    encode_native_tx_compact(compact) == compact_cbor,
    blake2b_256(encode_native_tx_body_compact(compact.body)) == tx_id,
    decoded.body.fee == size_balanced_fee,
    decoded.body.fee <= ${fixture.maxFee},
    list.length(decoded.body.inputs) == ${fixture.counts.spendInputs},
    list.length(decoded.body.reference_inputs) == ${fixture.counts.referenceInputs},
    list.length(decoded.body.outputs) == ${fixture.counts.outputs},
    list.length(decoded.body.required_observers) == ${fixture.counts.observerRedeemers},
    list.length(decoded.body.required_signers) == ${fixture.counts.requiredSigners},
    list.length(builtin.un_map_data(decoded.body.mint)) == ${fixture.counts.mintPolicies},
    list.length(decoded.witness_set.addr_tx_wits) == ${fixture.counts.addrWitnesses},
    list.length(decoded.witness_set.script_tx_wits) == ${fixture.counts.scriptWitnesses},
    list.length(decoded.witness_set.redeemer_tx_wits) == ${fixture.counts.totalRedeemers},
    list.length(decoded_spend_inputs) == ${fixture.counts.spendInputs},
    list.length(decoded_reference_inputs) == ${fixture.counts.referenceInputs},
    list.length(decoded_outputs) == ${fixture.counts.outputs},
    list.length(decoded_required_observers) == ${fixture.counts.observerRedeemers},
    list.length(decoded_required_signers) == ${fixture.counts.requiredSigners},
    list.length(builtin.un_map_data(decoded_mint)) == ${fixture.counts.mintPolicies},
    list.length(decoded_addr_wits) == ${fixture.counts.addrWitnesses},
    list.length(decoded_script_wits) == ${fixture.counts.scriptWitnesses},
    list.length(decoded_redeemer_wits) == ${fixture.counts.totalRedeemers},
    blake2b_256(size_balanced_spend_inputs_preimage_cbor) == compact.body.spend_inputs_hash,
    blake2b_256(size_balanced_reference_inputs_preimage_cbor) == compact.body.reference_inputs_hash,
    blake2b_256(size_balanced_outputs_preimage_cbor) == compact.body.outputs_hash,
    blake2b_256(size_balanced_required_observers_preimage_cbor) == compact.body.required_observers_hash,
    blake2b_256(size_balanced_required_signers_preimage_cbor) == compact.body.required_signers_hash,
    blake2b_256(size_balanced_mint_preimage_cbor) == compact.body.mint_hash,
  }
}
`;
