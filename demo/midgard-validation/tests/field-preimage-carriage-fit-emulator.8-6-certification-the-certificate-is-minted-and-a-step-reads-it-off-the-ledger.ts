import {
  healMidgardFieldCarriage,
  layOutMidgardFieldCarriage,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core/codec/native-tx-carriage";
import {
  authenticatedMidgardFieldView,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  midgardFieldItemAt,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  buildUnsignedFieldPreimageCertificationProgram,
  deriveFieldPreimageCertification,
  fieldPreimagePublicationDatumCbor,
  fieldPreimagePublicationOutputs,
  resolveChunkReferenceIndices,
  retireFieldPreimageCertificateRedeemer,
} from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  resolveCertificateFromLedger,
  standInCertificatePolicy,
} from "./field-preimage-carriage-fit-emulator.8-6-phase-4-exit-measurement-certificate-min-ada-and-the-one-transaction-question.js";
import {
  FIELD_INDEX,
  fieldPreimage,
  type Harness,
  HEALER_KEY,
  itemCountForPreimageBytes,
  itemPayloadAt,
  keyHash,
  MAX_L1_TX_BYTES,
  publish,
  PUBLISHER_ADDRESS,
  PUBLISHER_KEY,
  setupEmulator,
  TX_ID,
} from "./field-preimage-carriage-fit-emulator.publish.js";

describe("§8.6 certification — the certificate is minted, and a step reads it off the ledger", () => {
  let harness: Harness;

  beforeAll(async () => {
    // Inflated for the chunk publications only (§8.3 E1); the certification
    // transaction itself is small and its size is asserted against the real
    // limit below.
    harness = await setupEmulator();
  });

  it("publishes, certifies, and resolves the manifest back out of the UTxO set", async () => {
    const itemCount = itemCountForPreimageBytes(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
    const preimage = fieldPreimage(itemCount);
    const plan = planMidgardFieldCarriage({
      owner: keyHash(PUBLISHER_KEY),
      txId: TX_ID,
      fieldIndex: FIELD_INDEX,
      preimage,
    });
    const chunkUtxos: UTxO[] = [];
    for (const publication of fieldPreimagePublicationOutputs(plan)) {
      const result = await publish({
        harness,
        publication,
        signerKey: PUBLISHER_KEY,
        address: PUBLISHER_ADDRESS,
      });
      chunkUtxos.push(result.utxo);
    }

    const { policy, policyId, address } = standInCertificatePolicy();
    harness.lucid.selectWallet.fromPrivateKey(PUBLISHER_KEY.to_bech32());

    // The SDK's certification builder, driven end to end. It derives the
    // manifest, the asset name, the min-Ada, the redeemer and — through
    // `resolveChunkReferenceIndices` — the positional chunk indices.
    const unsigned = await Effect.runPromise(
      buildUnsignedFieldPreimageCertificationProgram(harness.lucid, {
        sourceKind: 0n,
        plan,
        certificatePolicyId: policyId,
        certificateAddress: address,
        certificateWitness: {
          kind: "inline_emulator_only",
          certificateScript: policy,
        },
        // Deliberately out of ledger order: the indices the redeemer carries
        // must be indices into the *sorted* reference-input list, and a builder
        // that used the order it was handed would be right until the first
        // transaction whose UTxOs sorted differently.
        chunkUtxos: [...chunkUtxos].reverse(),
        compactCbor: "ab".repeat(400),
        witnessSetCompactCbor: "cd".repeat(100),
      }),
    );
    const signed = await unsigned.sign.withWallet().complete();
    const certificationBytes = signed.toCBOR().length / 2;
    await harness.lucid.awaitTx(await signed.submit());

    // A certification is small — it re-carries no chunk bytes — and therefore
    // fits the real limit even though the publications it references do not.
    expect(certificationBytes).toBeLessThanOrEqual(MAX_L1_TX_BYTES);

    const certificateUtxos = await harness.lucid.utxosAt(address);
    expect(certificateUtxos.length).toBe(1);
    const certificateUtxo = certificateUtxos[0];
    if (certificateUtxo === undefined) {
      throw new Error("certificate UTxO was not found");
    }

    // The manifest comes back off the ledger and matches what was planned —
    // decoded from CBOR, with the asset name taken from the minted token rather
    // than from the plan.
    const resolvedCertificate = resolveCertificateFromLedger(
      certificateUtxo,
      policyId,
    );
    expect(resolvedCertificate).toEqual({
      certificate: plan.certificate,
      certificateAssetName: plan.certificateAssetName,
    });

    // And the whole tier-3 read runs with that ledger-resolved certificate in
    // the certificate slot, rather than the plan's own copy.
    const lastIndex = itemCount - 1;
    const layout = layOutMidgardFieldCarriage({ plan });
    const view = authenticatedMidgardFieldView({
      fieldIndex: plan.fieldIndex,
      txId: plan.txId,
      expectedCommitment: plan.commitment,
      carriage: layout.carriage,
      referenceInputs: [
        resolvedCertificate,
        ...plan.publications.map((publication) => ({
          inlineDatumBytes: publication.bytes,
        })),
      ],
    });
    expect(midgardFieldItemAt(view, lastIndex)).toEqual(
      itemPayloadAt(preimage, lastIndex),
    );

    // §8.7's recertify half: a second identity mints an interchangeable
    // certificate over the same chunks. Only the `owner` differs, and no
    // consuming step reads it.
    //
    // **Healing is pinned on the datum, not the token (#606).** Under the
    // retired derivation the token name was `blake2b_256(field_index ‖ tx_id)`
    // and "same name" was the healing property. Since the owner ruling of
    // 2026-08-16 `deriveFieldPreimageCertification` returns a module-level
    // constant unconditionally, so an assertion that two plans agree on
    // `assetNameHex` holds for *any* two plans in the repo and says nothing
    // about healing. What the ruling actually promises is "same bytes ⇒ same
    // datum (modulo `owner`)", and that is what these rows check.
    const healedPlan = healMidgardFieldCarriage({
      healer: keyHash(HEALER_KEY),
      txId: TX_ID,
      fieldIndex: FIELD_INDEX,
      preimage,
    });
    const originalCertificate = plan.certificate;
    const healedCertificate = healedPlan.certificate;
    if (originalCertificate === null || healedCertificate === null) {
      throw new Error("a tier-3 plan must carry a certificate");
    }
    // The mint-welded commitment — the value the §8.8 door now holds against
    // the commitment it anchored itself — is the same bytes' hash either way.
    expect(healedCertificate.fieldHash).toEqual(originalCertificate.fieldHash);
    expect(healedCertificate.fieldHash).toEqual(plan.commitment);
    // And so is every other content-bound field; `owner` is the only
    // difference, which is exactly what "interchangeable at consumption" means.
    expect(healedCertificate.owner).not.toEqual(originalCertificate.owner);
    expect({
      ...healedCertificate,
      owner: originalCertificate.owner,
    }).toEqual(originalCertificate);
    // At the wire level: the healed manifest's datum differs from the
    // original's only because the reclaim authority does, and the two become
    // byte-identical the moment the owners agree.
    expect(deriveFieldPreimageCertification(healedPlan).datumCbor).not.toBe(
      deriveFieldPreimageCertification(plan).datumCbor,
    );
    expect(
      deriveFieldPreimageCertification({
        ...healedPlan,
        certificate: { ...healedCertificate, owner: originalCertificate.owner },
      }).datumCbor,
    ).toBe(deriveFieldPreimageCertification(plan).datumCbor);
    // The guard that keeps the rows above from going vacuous a second time: a
    // certificate over *different* bytes of a *different* transaction is the
    // same token name and a different manifest. If the token ever discriminated
    // this pair, the constant name would have come back.
    const unrelatedPreimage = Buffer.from(preimage);
    unrelatedPreimage[unrelatedPreimage.length - 1] ^= 0xff;
    const unrelatedPlan = planMidgardFieldCarriage({
      owner: keyHash(PUBLISHER_KEY),
      txId: Buffer.alloc(32, 0x5e),
      fieldIndex: FIELD_INDEX,
      preimage: unrelatedPreimage,
    });
    expect(deriveFieldPreimageCertification(unrelatedPlan).assetNameHex).toBe(
      deriveFieldPreimageCertification(plan).assetNameHex,
    );
    expect(unrelatedPlan.certificate?.fieldHash).not.toEqual(
      originalCertificate.fieldHash,
    );
    expect(deriveFieldPreimageCertification(unrelatedPlan).datumCbor).not.toBe(
      deriveFieldPreimageCertification(plan).datumCbor,
    );

    // The burn half of §8.7's yank mode is **not** exercised here and cannot
    // be: retirement spends the certificate output, and that needs the compiled
    // spend handler #579's blueprint carries. `certificate_spend_cost` and
    // `certificate_spend_*` in the Aiken suite are where the burn path is
    // proved. What is checked here is that the off-chain half of it — the
    // `Retire` redeemer an off-chain burner emits — is the frozen Constr tag 1
    // the policy branches on.
    expect(retireFieldPreimageCertificateRedeemer()).toBe("d87a80");
  }, 300_000);

  it("orders reference-input indices the way the ledger does, not the way it was handed them", () => {
    const plan = planMidgardFieldCarriage({
      owner: keyHash(PUBLISHER_KEY),
      txId: TX_ID,
      fieldIndex: FIELD_INDEX,
      preimage: fieldPreimage(
        itemCountForPreimageBytes(
          MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
        ),
      ),
    });
    // Three synthetic UTxOs whose canonical (txHash, outputIndex) order is the
    // reverse of the §8.4 chunk order, so a resolver that returned positions in
    // the order it was given would return [0, 1, 2] and be wrong.
    const utxos: UTxO[] = plan.publications.map((publication, offset) => ({
      txHash: `${(9 - offset).toString().repeat(2)}`.repeat(32).slice(0, 64),
      outputIndex: 0,
      address: PUBLISHER_ADDRESS,
      assets: { lovelace: 1n },
      datum: fieldPreimagePublicationDatumCbor(publication.bytes),
    }));
    expect(
      resolveChunkReferenceIndices({ plan, referenceInputs: utxos }),
    ).toEqual([2, 1, 0]);
    // Handing them in a different order changes nothing, which is the property.
    expect(
      resolveChunkReferenceIndices({
        plan,
        referenceInputs: [...utxos].reverse(),
      }),
    ).toEqual([2, 1, 0]);
  });

  it("refuses to locate a chunk that is not among the reference inputs", () => {
    const plan = planMidgardFieldCarriage({
      owner: keyHash(PUBLISHER_KEY),
      txId: TX_ID,
      fieldIndex: FIELD_INDEX,
      preimage: fieldPreimage(
        itemCountForPreimageBytes(
          MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
        ),
      ),
    });
    expect(() =>
      resolveChunkReferenceIndices({ plan, referenceInputs: [] }),
    ).toThrow("is not among the transaction's reference inputs");
  });
});
