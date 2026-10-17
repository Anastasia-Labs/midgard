/**
 * What survives of the spend-input witness module after #604.
 *
 * The test this replaces published a 180-input typed witness UTxO through
 * `ensureSpendInputsReferenceWitness` and pinned its min-Ada. That publication
 * is deleted: §8's carriage ladder replaced it with the §8.5 nothing-but-bytes
 * publication that `field-opening.ts` builds, and the redeemer indices it fed
 * (`tx1_spend_inputs_ref_input_index` and its twin) no longer exist on-chain.
 *
 * The min-Ada calculation itself is still live — `publish-proof-chunks.ts` uses
 * it — and so is the canonical witness decoding, so both stay pinned here. The
 * §8 publication route has its own coverage in
 * `tests/submit-input-no-idx-step-02.test.ts` and the input-no-idx emulator leg.
 */
import {
  encodeMidgardFieldPreimage,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core/codec";
import {
  CML,
  credentialToAddress,
  Data,
  keyHashToCredential,
  PROTOCOL_PARAMETERS_DEFAULT,
  type ProtocolParameters,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  minimumLovelaceForInlineDatumOutput,
  resolveProtocolParameters,
  spendInputsWitnessFromCbors,
} from "../src/spend-input-witness.js";

/**
 * A fixed enterprise payment-key address. The priced output's size — and so
 * its min-Ada — depends on the address bytes, so the scenario states them
 * rather than drawing a fresh wallet.
 */
const ADDRESS = credentialToAddress(
  "Preprod",
  keyHashToCredential("ab".repeat(28)),
);

const inputCbor = (index: number): string =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(index.toString(16).padStart(64, "0"), "hex"),
    outputIndex: index,
  }).toString("hex");

const inlineDatumOutput = (
  datum: string,
  lovelace: bigint,
): CML.TransactionOutput =>
  CML.TransactionOutput.new(
    CML.Address.from_bech32(ADDRESS),
    CML.Value.from_coin(lovelace),
    CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
    undefined,
  );

/**
 * The Babbage/Conway ledger rule (CIP-55): an output is fundable when its coin
 * is at least `coinsPerUtxoByte * (160 + |serialized output|)`. This is a
 * different rule from the one under test — `minimumLovelaceForInlineDatumOutput`
 * searches for the fixpoint of `CML.min_ada_required` — so it can decide
 * whether the fixpoint the module returns is the right number, and does not
 * merely restate it.
 */
const ledgerMinimumLovelace = (
  datum: string,
  lovelace: bigint,
  coinsPerUtxoByte: bigint,
): bigint =>
  (160n + BigInt(inlineDatumOutput(datum, lovelace).to_cbor_bytes().length)) *
  coinsPerUtxoByte;

describe("spend-input witness decoding", () => {
  it("decodes a high-cardinality witness into its exact §5.3 out-refs", () => {
    const witness = spendInputsWitnessFromCbors(
      Array.from({ length: 180 }, (_, index) => inputCbor(index + 1)),
      "test.inputs",
    );

    expect(witness.inputs).toHaveLength(180);
    // The scenario numbers item i by i+1 in both halves of the out-ref, so the
    // decoded pair states which item it came from. A decoder that dropped,
    // reordered, or truncated items — or that read the index off the wrong
    // byte offset — cannot reproduce this list.
    expect(witness.inputs).toEqual(
      Array.from({ length: 180 }, (_, index) => ({
        tx_id: (index + 1).toString(16).padStart(64, "0"),
        output_index: BigInt(index + 1),
      })),
    );
  });

  it("refuses a spend-input item that is not the canonical 38-byte §5.3 form", () => {
    const canonical = inputCbor(1);
    // Start from the demonstrated-valid item above and change exactly one
    // thing: re-encode the output index in CBOR's shortest form (`01`) instead
    // of the §5.3 fixed `19 <uint16>`. Everything else — array header, byte
    // string tag, tx id — stays valid.
    expect(canonical).toMatch(/190001$/u);
    const shortestFormIndex = `${canonical.slice(0, -6)}01`;
    expect(() =>
      spendInputsWitnessFromCbors([shortestFormIndex], "test.inputs"),
    ).toThrow(/test\.inputs\[0\] is not valid Midgard §5\.3 TxOutRef CBOR/u);

    // A three-element array of the right total length is refused for the same
    // contract by a different route.
    expect(() =>
      spendInputsWitnessFromCbors([`83${canonical.slice(2)}00`], "test.inputs"),
    ).toThrow(/test\.inputs\[0\] is not valid Midgard §5\.3 TxOutRef CBOR/u);

    // Accept/reject pairing: the untouched item still decodes.
    expect(
      spendInputsWitnessFromCbors([canonical], "test.inputs").inputs,
    ).toEqual([{ tx_id: "00".repeat(31).concat("01"), output_index: 1n }]);
  });
});

describe("inline-datum min-Ada pricing", () => {
  it("prices a §8.5 nothing-but-bytes witness datum at the exact ledger minimum", () => {
    const witness = spendInputsWitnessFromCbors(
      Array.from({ length: 180 }, (_, index) => inputCbor(index + 1)),
      "test.inputs",
    );
    // #604 deleted the module's typed `datum` field along with the publication
    // that consumed it, so what is priced here is the shape min-Ada is still
    // asked about: a §8.5 nothing-but-bytes inline datum over the §5.1 preimage
    // of the same 180 items.
    const datum = Data.to(
      encodeMidgardFieldPreimage(
        witness.inputs.map((input) =>
          encodeMidgardSpendInputItem({
            txId: Buffer.from(input.tx_id, "hex"),
            outputIndex: Number(input.output_index),
          }),
        ),
      ).toString("hex"),
    );
    const { coinsPerUtxoByte } = PROTOCOL_PARAMETERS_DEFAULT;

    const lovelace = minimumLovelaceForInlineDatumOutput({
      address: ADDRESS,
      datum,
      coinsPerUtxoByte,
    });

    // Sufficiency: the returned coin funds the output that carries it.
    expect(lovelace).toBe(
      ledgerMinimumLovelace(datum, lovelace, coinsPerUtxoByte),
    );
    // Minimality: one lovelace less does not fund the output it would sit in,
    // so the fixpoint is not merely a safe over-estimate.
    expect(
      ledgerMinimumLovelace(datum, lovelace - 1n, coinsPerUtxoByte),
    ).toBeGreaterThan(lovelace - 1n);
  }, 30_000);
});

describe("protocol-parameter resolution", () => {
  it("prefers the parameters Lucid was constructed with over a provider round trip", async () => {
    const configured = {
      ...PROTOCOL_PARAMETERS_DEFAULT,
      coinsPerUtxoByte: 1234n,
    } satisfies ProtocolParameters;
    let providerCalls = 0;
    const lucid = {
      config: () => ({
        protocolParameters: configured,
        provider: {
          getProtocolParameters: async () => {
            providerCalls += 1;
            return PROTOCOL_PARAMETERS_DEFAULT;
          },
        },
      }),
    };

    await expect(
      resolveProtocolParameters(lucid as never),
    ).resolves.toStrictEqual(configured);
    // Lucid caches parameters at construction; re-reading them from the
    // provider is what silently relaxed a pinned `maxTxSize` in the emulator
    // scenarios this helper feeds.
    expect(providerCalls).toBe(0);
  });

  it("falls back to the provider only when Lucid carries no parameters", async () => {
    const fromProvider = {
      ...PROTOCOL_PARAMETERS_DEFAULT,
      coinsPerUtxoByte: 4321n,
    } satisfies ProtocolParameters;
    const lucid = {
      config: () => ({
        provider: { getProtocolParameters: async () => fromProvider },
      }),
    };

    await expect(
      resolveProtocolParameters(lucid as never),
    ).resolves.toStrictEqual(fromProvider);
  });

  it("refuses to guess when neither parameters nor a provider are configured", async () => {
    await expect(
      resolveProtocolParameters({ config: () => ({}) } as never),
    ).rejects.toThrow(/Lucid provider is not configured/u);
  });
});
