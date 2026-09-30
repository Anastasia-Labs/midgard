import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardCekBlobChunk,
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekTermNode,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  hashMidgardCekProgramMaterialPreimage,
  hashMidgardCekTermNode,
  materializeMidgardForcedTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { CML, type LucidEvolution } from "@lucid-evolution/lucid";

import * as SDK from "../src/index.js";

export const transactionCbor = (): Buffer =>
  encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: encodeCbor(
          [0x11, 0x22].map((fill) =>
            encodeMidgardTxOutput({
              address: Buffer.concat([
                Buffer.from([0x60]),
                Buffer.alloc(28, fill),
              ]),
              value: { lovelace: 2_000_000n, assets: new Map() },
              datum: {
                kind: "inline",
                cbor: Buffer.from(
                  aikenSerialisedPlutusDataCborPreservingMapOrder(
                    encodeCbor(Buffer.alloc(5_000, fill)).toString("hex"),
                  ),
                  "hex",
                ),
              },
            }),
          ),
        ),
        fee: 0n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        mintPreimageCbor: EMPTY_CBOR_LIST,
        scriptIntegrityHash: EMPTY_NULL_ROOT,
        auxiliaryDataHash: EMPTY_NULL_ROOT,
        networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    }),
  );

export const cekProgramMaterialAddress = CML.Address.from_raw_bytes(
  Buffer.concat([Buffer.from([0x70]), Buffer.alloc(28, 0x42)]),
).to_bech32();

export const cekProgramMaterialContracts = {
  cekProgramMaterial: {
    spendingScriptAddress: cekProgramMaterialAddress,
  },
} as Pick<SDK.MidgardValidators, "cekProgramMaterial">;

export const cekProgramMaterialPublication = (bytes: number) => {
  const preimage = encodeMidgardCekBlobChunk(Buffer.alloc(bytes, 0x5a));
  return SDK.deriveCekProgramMaterialPublications([
    {
      kind: "blobChunk",
      root: hashMidgardCekProgramMaterialPreimage("blobChunk", preimage),
      preimage,
    },
  ])[0]!;
};

export const completeCekPublicationInput = () => {
  const term = { kind: "error" } as const;
  const preimage = encodeMidgardCekTermNode(term);
  const root = hashMidgardCekTermNode(term);
  const entry = { kind: "term" as const, root, preimage };
  const envelopeCbor = encodeMidgardCekProgramEnvelope({
    uplcVersion: [1n, 1n, 0n],
    termRoot: root,
    nodeCount: 1n,
    materialByteLength: BigInt(preimage.length),
  });
  return {
    envelopeCbor,
    entry,
    sidecarCbor: encodeMidgardCekProgramMaterialSidecar([entry]),
  };
};

export const materialPublicationLucid = ({
  coinsPerUtxoByte,
  fundedLovelace,
  outputs,
}: {
  readonly coinsPerUtxoByte: bigint;
  readonly fundedLovelace: bigint[];
  readonly outputs?: {
    address: string;
    datum: unknown;
    lovelace: bigint;
  }[];
}): LucidEvolution => {
  const tx = {
    pay: {
      ToAddressWithData: (
        address: string,
        datum: unknown,
        assets: { readonly lovelace: bigint },
      ) => {
        fundedLovelace.push(assets.lovelace);
        outputs?.push({ address, datum, lovelace: assets.lovelace });
        return tx;
      },
    },
    complete: async () => ({}),
  };
  return {
    config: () => ({ protocolParameters: { coinsPerUtxoByte } }),
    newTx: () => tx,
  } as unknown as LucidEvolution;
};
