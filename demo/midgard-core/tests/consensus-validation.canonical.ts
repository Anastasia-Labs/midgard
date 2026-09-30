import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardCekProgramEnvelope,
  encodeMidgardFieldPreimageForField,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScriptListPreimage,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardAddressFromText,
  type MidgardNativeScript,
  type MidgardNativeTxCanonical,
  type MidgardTxOutput,
  protectMidgardAddress,
} from "../src/index.js";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "../src/plutus-data-cbor.js";

export const canonicalDataBytes = (payload: Buffer): Buffer =>
  Buffer.from(
    aikenSerialisedPlutusDataCborPreservingMapOrder(
      encodeCbor(payload).toString("hex"),
    ),
    "hex",
  );

export const address = midgardAddressFromText(
  "addr1q9ynxme7c0tcmmvgk2tjuv63aw7zk9tk6yqkaqd48ulhkyl5f6v47dp5rc7286z5f57339d0c79khw4y3lwxzm8ywkzs02spk6",
);

export const cekProgramEnvelope = (
  nodeCount = 3n,
  materialByteLength = 144n,
): Buffer =>
  encodeMidgardCekProgramEnvelope({
    uplcVersion: [1n, 1n, 0n],
    termRoot: Buffer.alloc(32, 0x33),
    nodeCount,
    materialByteLength,
  });

export const output = (
  overrides: Partial<MidgardTxOutput> = {},
): MidgardTxOutput => ({
  address: protectMidgardAddress(address),
  value: { lovelace: 2_000_000n, assets: new Map() },
  script_ref: {
    language: "MidgardV1",
    scriptBytes: cekProgramEnvelope(),
  },
  ...overrides,
});

export const canonical = (
  version = MIDGARD_NATIVE_TX_VERSION,
): MidgardNativeTxCanonical => ({
  version,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: EMPTY_CBOR_LIST,
    referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
    outputsPreimageCbor: encodeCbor([encodeMidgardTxOutput(output())]),
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: encodeCbor([Buffer.alloc(28, 7)]),
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    // §5.6: the enveloped per-policy item list, not the retired raw map.
    mintPreimageCbor: encodeMidgardFieldPreimageForField({
      fieldIndex: 5,
      items: [
        {
          policyId: Buffer.alloc(28, 8),
          assets: [{ assetName: Buffer.from("asset", "ascii"), quantity: 1n }],
        },
      ],
    }),
    scriptIntegrityHash: Buffer.alloc(32, 9),
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
      { language: "MidgardV1", scriptBytes: cekProgramEnvelope() },
    ]),
    redeemerTxWitsPreimageCbor: encodeMidgardFieldPreimageForField({
      fieldIndex: 8,
      items: [
        {
          purpose: "Spend",
          index: 0n,
          redeemerCbor: Buffer.from([0x80]),
          executionUnits: { memory: 0n, steps: 0n },
        },
      ],
    }),
  },
});

export const nestedNativeScript = (depth: number): MidgardNativeScript => {
  let script: MidgardNativeScript = {
    type: "sig",
    keyHash: Buffer.alloc(28, 0x44),
  };
  for (let index = 1; index < depth; index += 1) {
    script = { type: "all", scripts: [script] };
  }
  return script;
};
