import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardForcedTxCanonical,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  materializeMidgardForcedTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
export const transactionCbor = (): Buffer =>
  encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      body: {
        // One spend input: the tx-order mint admits no forced tx without one.
        spendInputsPreimageCbor: encodeCbor([
          encodeMidgardSpendInputItem({
            txId: Buffer.alloc(32, 0x33),
            outputIndex: 0,
          }),
        ]),
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
