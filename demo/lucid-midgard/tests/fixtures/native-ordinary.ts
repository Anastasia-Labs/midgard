/**
 * The **declared construction** of the ordinary native-transaction golden — the
 * small, unremarkable transaction `native-tx.test.ak` uses to pin the whole
 * decode / compact / commit path for a shape a reader can hold in their head.
 *
 * This was the near-miss #588 names: the generator's ordinary branch already
 * re-derived the compact form, the transaction id and the field commitments from
 * `golden_core_native_full_tx_cbor`, but that full-CBOR literal was itself
 * hand-maintained. So the branch regenerated everything *except* its own seed,
 * and moving the seed — as #586's out-ref change had to — meant editing ~250
 * hex characters by hand inside an Aiken module.
 *
 * The construction below is what that literal now derives from. It is
 * deliberately minimal and asymmetric rather than pretty: one spend input at
 * output index 0 and one reference input at output index 1 (so the fixed-index
 * §5.3 encoding is exercised at two different indices), exactly one output
 * carrying both ada and one multi-asset entry, a non-zero fee, an upper validity
 * bound but no lower one, distinct script-integrity and auxiliary-data hashes,
 * and every one of the six remaining fields empty. Every one of those choices is
 * something the Aiken module asserts, which is why they are stated here.
 *
 * Writer: `pnpm --dir demo/lucid-midgard run fixtures:native-ordinary:sync`
 * (`tests/native-ordinary-fixture.test.ts`), then
 * `pnpm --dir demo/lucid-midgard run fixtures:native-compact` to rebind the
 * Aiken goldens derived from it.
 */

import {
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec";

import {
  deriveNativeTxFixtureFacets,
  type NativeTxFixtureEnvelope,
} from "./native-tx-fixture-shape.js";

export const ORDINARY_FIXTURE_NAME = "ordinary-core-golden-v1" as const;

export const ORDINARY_PRODUCER =
  "pnpm --dir demo/lucid-midgard run fixtures:native-ordinary:sync";

/**
 * Every input to the construction. The repeated-byte identifiers are chosen so
 * that a misread offset shows up as an obviously wrong byte rather than as a
 * plausible-looking hash: `11…` is the spend input, `22…` the reference input,
 * `33…` the script-integrity hash, `44…` the auxiliary-data hash.
 */
export const ORDINARY_PARAMETERS = {
  spendInputTxIdByte: 0x11,
  spendInputOutputIndex: 0,
  referenceInputTxIdByte: 0x22,
  /** Deliberately not 0: the §5.3 fixed index has to be exercised twice over. */
  referenceInputOutputIndex: 1,
  /** A 29-byte enterprise key address: header `60` ‖ 28-byte payment hash. */
  outputAddressPaymentByte: 0xaa,
  outputLovelace: 2_000_000n,
  outputPolicyIdByte: 0x7a,
  /** `MID`, three ASCII bytes — a short asset name, not the 32-byte maximum. */
  outputAssetName: "MID",
  outputAssetQuantity: 5n,
  fee: 42n,
  validityIntervalEnd: 100n,
  networkId: 0n,
  scriptIntegrityHashByte: 0x33,
  auxiliaryDataHashByte: 0x44,
} as const;

export type OrdinaryNativeTxFixture = NativeTxFixtureEnvelope<{
  readonly name: typeof ORDINARY_FIXTURE_NAME;
  readonly producer: string;
}>;

const repeated = (byte: number, length: number): Buffer =>
  Buffer.alloc(length, byte);

/** The `80` every empty field wears, spelled once. */
const EMPTY_FIELD_PREIMAGE_CBOR = encodeCbor([]);

export const buildOrdinaryNativeTxFixture = (): OrdinaryNativeTxFixture => {
  const parameters = ORDINARY_PARAMETERS;
  const canonical: MidgardNativeTxCanonical = {
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor([
        encodeMidgardSpendInputItem({
          txId: repeated(parameters.spendInputTxIdByte, 32),
          outputIndex: parameters.spendInputOutputIndex,
        }),
      ]),
      referenceInputsPreimageCbor: encodeCbor([
        encodeMidgardSpendInputItem({
          txId: repeated(parameters.referenceInputTxIdByte, 32),
          outputIndex: parameters.referenceInputOutputIndex,
        }),
      ]),
      outputsPreimageCbor: encodeCbor([
        encodeMidgardTxOutput({
          address: Buffer.concat([
            Buffer.from([0x60]),
            repeated(parameters.outputAddressPaymentByte, 28),
          ]),
          value: {
            lovelace: parameters.outputLovelace,
            assets: new Map([
              [
                repeated(parameters.outputPolicyIdByte, 28).toString("hex"),
                new Map([
                  [
                    Buffer.from(parameters.outputAssetName, "ascii").toString(
                      "hex",
                    ),
                    parameters.outputAssetQuantity,
                  ],
                ]),
              ],
            ]),
          },
        }),
      ]),
      fee: parameters.fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: parameters.validityIntervalEnd,
      requiredObserversPreimageCbor: EMPTY_FIELD_PREIMAGE_CBOR,
      requiredSignersPreimageCbor: EMPTY_FIELD_PREIMAGE_CBOR,
      mintPreimageCbor: EMPTY_FIELD_PREIMAGE_CBOR,
      scriptIntegrityHash: repeated(parameters.scriptIntegrityHashByte, 32),
      auxiliaryDataHash: repeated(parameters.auxiliaryDataHashByte, 32),
      networkId: parameters.networkId,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_FIELD_PREIMAGE_CBOR,
      scriptTxWitsPreimageCbor: EMPTY_FIELD_PREIMAGE_CBOR,
      redeemerTxWitsPreimageCbor: EMPTY_FIELD_PREIMAGE_CBOR,
    },
  };

  const facets = deriveNativeTxFixtureFacets(
    encodeMidgardNativeTxCanonical(
      materializeMidgardNativeTxFromCanonical(canonical),
    ),
  );
  if (
    facets.mintPolicyIdsInTxInfoOrder.length !== 0 ||
    facets.redeemerPointers.length !== 0
  ) {
    throw new Error("ordinary native tx golden is no longer witness-free");
  }

  return {
    name: ORDINARY_FIXTURE_NAME,
    producer: ORDINARY_PRODUCER,
    txIdHex: facets.txIdHex,
    fullTxCborHex: facets.fullTxCborHex,
    compactTxCborHex: facets.compactTxCborHex,
    compactBodyCborHex: facets.compactBodyCborHex,
    sizes: facets.sizes,
    mintPolicyIdsInTxInfoOrder: facets.mintPolicyIdsInTxInfoOrder,
    redeemerPointers: facets.redeemerPointers,
    preimages: facets.preimages,
    hashes: facets.hashes,
  };
};
