import { blake2b } from "@noble/hashes/blake2.js";
import { CML } from "@lucid-evolution/lucid";
import {
  computeHash32,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeTxFull,
  decodeMidgardUtxo,
  decodeSingleCbor,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCompact,
  encodeMidgardTxOutput,
  LucidMidgard,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  outRefToCbor,
  walletFromSeedPhrase,
  type MidgardProvider,
  type MidgardUtxo,
  type OutRef,
  type Redeemer,
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

export type HighCardinalityNativeTxFixture = {
  readonly name: typeof HIGH_CARDINALITY_FIXTURE_NAME;
  readonly txIdHex: string;
  readonly fullTxCborHex: string;
  readonly compactTxCborHex: string;
  readonly compactBodyCborHex: string;
  readonly counts: typeof HIGH_CARDINALITY_COUNTS;
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

const seedPhrase = "test test test test test test test test test test test junk";
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

const asMap = (value: unknown, label: string): ReadonlyMap<unknown, unknown> => {
  if (!(value instanceof Map)) {
    throw new Error(`${label} must decode to a map`);
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
    builder = builder
      .pay.ToAddress(
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
    const tx = decodeMidgardNativeTxFull(signed.txCbor);
    const compactTxCbor = encodeMidgardNativeTxCompact(tx.compact);
    const compactBodyCbor = encodeMidgardNativeTxBodyCompact(
      tx.compact.transactionBody,
    );

    const spendInputs = asArray(
      decodeSingleCbor(tx.body.spendInputsPreimageCbor),
      "spend inputs",
    );
    const decodedReferenceInputs = asArray(
      decodeSingleCbor(tx.body.referenceInputsPreimageCbor),
      "reference inputs",
    );
    const outputs = asArray(
      decodeSingleCbor(tx.body.outputsPreimageCbor),
      "outputs",
    );
    const mint = asMap(decodeSingleCbor(tx.body.mintPreimageCbor), "mint");
    const redeemers = redeemerPointers(tx.witnessSet.redeemerTxWitsPreimageCbor);

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

    return {
      name: HIGH_CARDINALITY_FIXTURE_NAME,
      txIdHex: hex(computeMidgardNativeTxIdFromFull(tx)),
      fullTxCborHex: hex(signed.txCbor),
      compactTxCborHex: hex(compactTxCbor),
      compactBodyCborHex: hex(compactBodyCbor),
      counts: HIGH_CARDINALITY_COUNTS,
      mintPolicyIdsInTxInfoOrder: [...mint.keys()].map((policy) =>
        hex(policy as Uint8Array),
      ),
      redeemerPointers: redeemers,
      preimages: {
        spendInputsCborHex: hex(tx.body.spendInputsPreimageCbor),
        referenceInputsCborHex: hex(tx.body.referenceInputsPreimageCbor),
        outputsCborHex: hex(tx.body.outputsPreimageCbor),
        requiredObserversCborHex: hex(tx.body.requiredObserversPreimageCbor),
        requiredSignersCborHex: hex(tx.body.requiredSignersPreimageCbor),
        mintCborHex: hex(tx.body.mintPreimageCbor),
        addrTxWitsCborHex: hex(tx.witnessSet.addrTxWitsPreimageCbor),
        scriptTxWitsCborHex: hex(tx.witnessSet.scriptTxWitsPreimageCbor),
        redeemerTxWitsCborHex: hex(tx.witnessSet.redeemerTxWitsPreimageCbor),
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
          computeHash32(tx.witnessSet.addrTxWitsPreimageCbor),
        ),
        scriptTxWitsHashHex: hex(
          computeHash32(tx.witnessSet.scriptTxWitsPreimageCbor),
        ),
        redeemerTxWitsHashHex: hex(
          computeHash32(tx.witnessSet.redeemerTxWitsPreimageCbor),
        ),
        witnessSetHashHex: hex(tx.compact.transactionWitnessSetHash),
      },
    };
  };

export const stableFixtureJson = (
  fixture: HighCardinalityNativeTxFixture,
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
  MidgardTransactionWitnessSetPartialPreimages, MintRedeemer, ReceiveRedeemer,
  RewardRedeemer, SpendRedeemer,
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
