import {
  midgardCarriageDataByteStringBytes,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core/codec/native-tx-carriage";
import {
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  type ResolvedCarriageReferenceInput,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  deriveFieldPreimageCertification,
  FieldPreimageCertificate,
  minimumLovelaceForFieldPreimageCertificate,
  minimumLovelaceForFieldPreimagePublication,
} from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  type MintingPolicy,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { CERTIFY_REDEEMER_BYTES } from "./field-preimage-carriage-fit-emulator.8-3-erratum-e1-the-publishable-frontier-is-enforced-at-the-real-max-tx-size.js";
import {
  FIELD_INDEX,
  fieldPreimage,
  itemCountForPreimageBytes,
  keyHash,
  MAX_L1_TX_BYTES,
  publicationOutputFor,
  publish,
  PUBLISHER_ADDRESS,
  PUBLISHER_KEY,
  setupEmulator,
  TX_ID,
} from "./field-preimage-carriage-fit-emulator.publish.js";

describe("§8.6 Phase-4 exit measurement — certificate min-Ada and the one-transaction question", () => {
  it("reports min-Ada at the sizes the ladder really uses", async () => {
    const harness = await setupEmulator();
    const protocolParameters = harness.lucid.config().protocolParameters;
    if (protocolParameters === undefined) {
      throw new Error("emulator protocol parameters are unavailable");
    }
    const { coinsPerUtxoByte } = protocolParameters;

    const cornerPlan = planMidgardFieldCarriage({
      owner: keyHash(PUBLISHER_KEY),
      txId: TX_ID,
      fieldIndex: FIELD_INDEX,
      preimage: fieldPreimage(
        itemCountForPreimageBytes(
          MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
        ),
      ),
    });
    const certification = deriveFieldPreimageCertification(cornerPlan);
    // The certificate lives at the validator's own address. Its *code* comes
    // from the blueprint #579 regenerates, but min-Ada depends only on the
    // output's serialised size — address, value, inline datum — so a stand-in
    // script address of the right shape measures the same number the real one
    // will. Stated rather than hidden: this is the one figure here that does
    // not come from the deployed script, and it does not need to.
    const certificatePolicyId = "22".repeat(28);
    const certificateAddress = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_script(CML.ScriptHash.from_hex(certificatePolicyId)),
    )
      .to_address()
      .to_bech32();

    const certificateMinAda = minimumLovelaceForFieldPreimageCertificate({
      certificateAddress,
      certification,
      certificatePolicyId,
      coinsPerUtxoByte,
    });

    const chunkMinAda = minimumLovelaceForFieldPreimagePublication({
      publisherAddress: PUBLISHER_ADDRESS,
      output: publicationOutputFor(cornerPlan, 0),
      coinsPerUtxoByte,
    });
    const raggedMinAda = minimumLovelaceForFieldPreimagePublication({
      publisherAddress: PUBLISHER_ADDRESS,
      output: publicationOutputFor(cornerPlan, 2),
      coinsPerUtxoByte,
    });

    console.log(
      JSON.stringify({
        measurement: "certificate-min-ada-v1",
        coinsPerUtxoByte: coinsPerUtxoByte.toString(),
        certificateChunkDigests: certification.chunkCount,
        certificateDatumBytes: certification.datumCbor.length / 2,
        certificateMinAdaLovelace: certificateMinAda.toString(),
        fullChunkMinAdaLovelace: chunkMinAda.toString(),
        raggedChunkMinAdaLovelace: raggedMinAda.toString(),
        certifyRedeemerBytes: CERTIFY_REDEEMER_BYTES,
      }),
    );

    // §8.10 quotes these three numbers as a table, so they are asserted rather
    // than only printed — a console.log is a report, not a gate, and the table
    // it feeds is the deliverable.
    expect(coinsPerUtxoByte).toBe(4_310n);
    expect(certification.chunkCount).toBe(3);
    // 210 bytes since #606: the datum gained the 32-byte mint-welded
    // `field_hash` plus its 2-byte CBOR head, 176 → 210.
    //
    // The min-Ada movement is **not** those 34 bytes, and the difference is
    // worth stating because the naive attribution is off by the one thing #606
    // changed twice. min-Ada is charged on the whole serialised output, and the
    // output carries the asset name as well as the datum: the name shrank from
    // a 32-byte `blake2b_256(field_index ‖ tx_id)` digest to the 27-byte
    // constant `MIDGARD_FIELD_PREIMAGE_CERT`. So the net is +34 − 5 = +29 bytes,
    // and at 4,310 lovelace/byte that is 124,990 lovelace — exactly the
    // 1.9395 → 2.0645 ADA the pin below records (1,939,500 → 2,064,490). The
    // 5-byte half is measured, not asserted from the constant's length: minting
    // the same manifest under a 32-byte name costs 2,086,040, which is 21,550 =
    // 5 × 4,310 more than this line pins.
    expect(certification.datumCbor.length / 2).toBe(210);
    // The other half of the arithmetic above, so neither term is prose.
    expect(certification.assetNameHex.length / 2).toBe(27);
    expect(certificateMinAda).toBe(2_064_490n);
    expect(certificateMinAda - 1_939_500n).toBe(29n * coinsPerUtxoByte);
    expect(chunkMinAda).toBe(68_231_610n);
    expect(raggedMinAda).toBe(11_869_740n);

    // The datum a full chunk carries is 15,624 bytes, not the 15,148 of
    // payload: min-Ada is charged on the serialised output, so the ≈3.125 %
    // Plutus Data chunking cost is deposit as well as transaction size. §8.10's
    // table labelled the datum column with the payload figure until this line
    // existed to contradict it. (The three deposit figures moved with §8.3
    // erratum E1's repair of `K`: the full chunk was 15,900 B of payload in a
    // 16,400-byte datum at 71.5762 ADA, and the ragged tail 963 B in 996 at
    // 5.1849 ADA. The manifest is unmoved — three digests either way.)
    expect(midgardCarriageDataByteStringBytes(MIDGARD_CHUNK_BYTES_K)).toBe(
      15_624,
    );
    expect(publicationOutputFor(cornerPlan, 0).datumCbor.length / 2).toBe(
      15_624,
    );
    expect(publicationOutputFor(cornerPlan, 2).datumCbor.length / 2).toBe(
      2_547,
    );

    // A manifest is small: three digests, a length and an owner. The whole
    // point of tier 3 is that the *certified* object is tiny even though the
    // preimage is not.
    expect(certificateMinAda).toBeLessThan(chunkMinAda);
    expect(raggedMinAda).toBeLessThan(chunkMinAda);
  });

  it("shows last-chunk publication and certification cannot share a transaction", async () => {
    const harness = await setupEmulator();
    const cornerPlan = planMidgardFieldCarriage({
      owner: keyHash(PUBLISHER_KEY),
      txId: TX_ID,
      fieldIndex: FIELD_INDEX,
      preimage: fieldPreimage(
        itemCountForPreimageBytes(
          MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
        ),
      ),
    });

    // Reason one, and it is structural rather than a matter of size. §8.6
    // resolves chunks from *reference inputs*, and the Cardano ledger resolves
    // reference inputs against the UTxO set as it stands before the
    // transaction. An output the same transaction creates is therefore not
    // available to it, at any size, under any protocol parameters. The builder
    // makes that concrete by requiring resolved chunk UTxOs rather than
    // datums: there is no way to hand it a chunk that does not yet exist.
    const lastChunk = publicationOutputFor(cornerPlan, 2);
    const published = await publish({
      harness,
      publication: lastChunk,
      signerKey: PUBLISHER_KEY,
      address: PUBLISHER_ADDRESS,
    });
    expect(published.utxo.txHash).not.toBe("");

    // Reason two, independent of the first and measured: even if a chunk could
    // be self-referenced, the bytes do not fit. A full-K publication alone is
    // already most of maxTxSize, and a certification adds the manifest output,
    // the §8.6 redeemer and the minting-policy witness on top.
    const fullChunkPublication = await publish({
      harness,
      publication: publicationOutputFor(cornerPlan, 0),
      signerKey: PUBLISHER_KEY,
      address: PUBLISHER_ADDRESS,
      submit: false,
    });
    const redeemerBytes = CERTIFY_REDEEMER_BYTES;
    const certification = deriveFieldPreimageCertification(cornerPlan);
    const certificateOutputBytes = certification.datumCbor.length / 2;

    console.log(
      JSON.stringify({
        measurement: "publication-plus-certification-fit-v1",
        maxL1TxBytes: MAX_L1_TX_BYTES,
        fullChunkPublicationBytes: fullChunkPublication.signedBytes,
        certifyRedeemerBytes: redeemerBytes,
        certificateDatumBytes: certificateOutputBytes,
        combinedLowerBound:
          fullChunkPublication.signedBytes +
          redeemerBytes +
          certificateOutputBytes,
        fitsOneTransaction: false,
        structuralReason:
          "reference inputs resolve against the pre-transaction UTxO set",
      }),
    );

    expect(
      fullChunkPublication.signedBytes + redeemerBytes + certificateOutputBytes,
    ).toBeGreaterThan(MAX_L1_TX_BYTES);
  }, 120_000);
});

/**
 * A stand-in certificate minting policy: an always-satisfiable native script.
 *
 * **What this is and is not.** The compiled `field_preimage_certificate`
 * validator landed in #573, but the blueprint that would carry its code is
 * regenerated by #579, so nothing in this repository can put *that* script on
 * an emulator ledger yet. Its content rules — the §3 tx-id re-derivation, the
 * positional field-hash extraction, `total_length`, the per-chunk digests, the
 * output shape — are proved by `validators/field-preimage-certificate-handlers.test.ak`
 * and are proved nowhere here.
 *
 * What the stand-in buys is everything that is not the script's arithmetic: a
 * certificate token that really exists under a `(tx_id, field_index)` asset
 * name, a manifest that really sits as an inline datum at a script address, and
 * a consuming step that really recovers it from the ledger. Before this, the
 * certificate half of every tier-3 read in this file came out of the in-memory
 * plan, so the reference input a step resolves had never been round-tripped
 * through a UTxO at all.
 */
export const standInCertificatePolicy = (): {
  readonly policy: MintingPolicy;
  readonly policyId: string;
  readonly address: string;
} => {
  const script = CML.NativeScript.new_script_all(CML.NativeScriptList.new());
  const policyId = script.hash().to_hex();
  return {
    policy: {
      type: "Native",
      script: Buffer.from(script.to_cbor_bytes()).toString("hex"),
    },
    policyId,
    address: CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_script(CML.ScriptHash.from_hex(policyId)),
    )
      .to_address()
      .to_bech32(),
  };
};

/**
 * Recovers a tier-3 certificate reference input the way a step builder must:
 * from the **ledger**, by decoding the manifest out of the UTxO's inline datum
 * and taking the asset name off the token the UTxO actually carries.
 *
 * Nothing here consults the plan. That is the point — a resolver that read the
 * certificate out of the plan it was built from would agree with itself no
 * matter what the mint had done.
 */
export const resolveCertificateFromLedger = (
  utxo: UTxO,
  policyId: string,
): ResolvedCarriageReferenceInput => {
  const datum = utxo.datum;
  if (datum === undefined || datum === null) {
    throw new Error("certificate UTxO carries no inline datum");
  }
  const decoded = Data.from(datum, FieldPreimageCertificate);
  const units = Object.keys(utxo.assets).filter(
    (unit) => unit !== "lovelace" && unit.startsWith(policyId),
  );
  const unit = units[0];
  if (units.length !== 1 || unit === undefined) {
    throw new Error("certificate output must carry exactly one policy asset");
  }
  return {
    certificate: {
      owner: Buffer.from(decoded.owner, "hex"),
      txId: Buffer.from(decoded.tx_id, "hex"),
      fieldIndex: Number(decoded.field_index),
      fieldHash: Buffer.from(decoded.field_hash, "hex"),
      totalLength: Number(decoded.total_length),
      chunkDigests: decoded.chunk_digests.map((digest) =>
        Buffer.from(digest, "hex"),
      ),
    },
    certificateAssetName: Buffer.from(unit.slice(policyId.length), "hex"),
  };
};
