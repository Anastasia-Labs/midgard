import {
  type MidgardFieldCarriagePlan,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core/codec/native-tx-carriage-v1";
import {
  CML,
  type MintingPolicy,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  deriveFieldPreimageCertification,
  fieldPreimagePublicationDatumCbor,
  requireFieldPreimageCertificateReferenceScript,
  resolveCertificateReferenceIndex,
  resolveFieldPreimageCertificationReferenceLayout,
} from "@/fraud-proof/field-preimage-carriage-v1.js";
import { FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX } from "@/native-tx-field-access-v1.js";

/**
 * §8.6 certificate selection under #606's **constant** asset name (owner
 * ruling, 2026-08-16).
 *
 * Before #606 the token name was `blake2b_256(field_index ‖ tx_id)`, so a
 * token-only lookup *was* a selection: one name, one certificate, no ambiguity.
 * The ruling retired that derivation — every certificate of the policy now
 * wears the same constant name and identity lives in the mint-verified datum,
 * which is why it records that "consumers disambiguate by datum, never by token
 * alone".
 *
 * {@link resolveCertificateReferenceIndex} is the single off-chain function
 * that ruling changed, and every other certificate fixture in this repo puts
 * exactly **one** certificate UTxO in the reference-input set — where a
 * token-only resolver and a datum-filtering one are indistinguishable. These
 * rows are the ones that can tell them apart: each puts two same-name
 * certificate tokens in the set and pins which index comes back.
 *
 * What breaks without the datum filter is not a cosmetic mis-selection. A step
 * disputing two tier-3 fields of one transaction carries two certificates; a
 * token-only match returns the canonically-first UTxO for both, the second
 * field's `cert_ref_input_index` names the first field's manifest, and the door
 * aborts at `expect certificate.field_index == field_index`
 * (`native-tx-field-access-v1.ak`) — the fault cannot be proved at all. The
 * `field_hash` conjunct is load-bearing for the same reason one rung down:
 * `(tx_id, field_index)` does not pin content, because a certificate minted
 * over fabricated bytes wears the fabricated commitment in its datum and is
 * otherwise a well-formed certificate for that field. Selecting it hands the
 * door a manifest whose anchored equality cannot hold.
 *
 * Each arrangement below is run twice with the two certificates' out-refs
 * swapped. A single arrangement only bites for whichever field does *not*
 * happen to sort first; running both makes every leg a discriminator rather
 * than an accident of canonical order.
 */

const CARRIAGE_OWNER = Buffer.alloc(28, 0x11);
const HEALING_OWNER = Buffer.alloc(28, 0x77);
const CARRIAGE_TX_ID = Buffer.alloc(32, 0x22);
const CERTIFICATE_POLICY_ID = "ab".repeat(28);
const FOREIGN_POLICY_ID = "cd".repeat(28);
const PROVER_KEY_ADDRESS = "addr_test1_prover_key";
const CERTIFICATE_ADDRESS = "addr_test1_field_preimage_certificate";

/** §8.4: above `chunk_bytes_k` (15,148), so both plans are tier 3. */
const TIER3_PREIMAGE_BYTES = 16_384;

const CERTIFICATE_UNIT = `${CERTIFICATE_POLICY_ID}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`;

const utxo = ({
  txHash,
  outputIndex,
  address,
  datum,
  assets,
  scriptRef,
}: {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly address: string;
  readonly datum: string;
  readonly assets?: Record<string, bigint>;
  readonly scriptRef?: MintingPolicy;
}): UTxO => ({
  txHash,
  outputIndex,
  address,
  assets: { lovelace: 5_000_000n, ...(assets ?? {}) },
  datum,
  ...(scriptRef === undefined ? {} : { scriptRef }),
});

const nativePolicy = (): {
  readonly policy: MintingPolicy;
  readonly policyId: string;
} => {
  const script = CML.NativeScript.new_script_pubkey(
    CML.PrivateKey.generate_ed25519().to_public().hash(),
  );
  const policy: MintingPolicy = {
    type: "Native",
    script: Buffer.from(script.to_cbor_bytes()).toString("hex"),
  };
  return { policy, policyId: validatorToScriptHash(policy) };
};

const planFor = ({
  fieldIndex,
  fill,
  owner = CARRIAGE_OWNER,
}: {
  readonly fieldIndex: number;
  readonly fill: number;
  readonly owner?: Buffer;
}): MidgardFieldCarriagePlan =>
  planMidgardFieldCarriage({
    owner,
    txId: CARRIAGE_TX_ID,
    fieldIndex,
    preimage: Buffer.alloc(TIER3_PREIMAGE_BYTES, fill),
  });

/** The certificate UTxO a plan mints, placed at a chosen out-ref. */
const certificateUtxoFor = (
  plan: MidgardFieldCarriagePlan,
  txHash: string,
  outputIndex = 0,
): UTxO => {
  const certification = deriveFieldPreimageCertification(plan);
  // #606: the name is branding. Every certificate below is the *same* unit, so
  // nothing in these rows can be selected by token.
  expect(certification.assetNameHex).toBe(
    FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  );
  return utxo({
    txHash,
    outputIndex,
    address: CERTIFICATE_ADDRESS,
    datum: certification.datumCbor,
    assets: { [CERTIFICATE_UNIT]: 1n },
  });
};

const chunkUtxosFor = (
  plan: MidgardFieldCarriagePlan,
  txHash: string,
): readonly UTxO[] =>
  plan.publications.map((publication, offset) =>
    utxo({
      txHash,
      outputIndex: offset,
      address: PROVER_KEY_ADDRESS,
      datum: fieldPreimagePublicationDatumCbor(publication.bytes),
    }),
  );

/**
 * The published spending validator a real dispute step reads through
 * `readFrom`. It sorts ahead of everything else here, so no expected index in
 * this file is trivially zero.
 */
const SCRIPT_REFERENCE = utxo({
  txHash: "00".repeat(32),
  outputIndex: 0,
  address: "addr_test1_published_validator",
  datum: "d87980",
});

/**
 * The two certificate out-refs, in the two orders. `a1… < b2…` lexicographically
 * and both sort after every chunk and the script reference, so the certificate
 * placed at `a1…` is the canonically-first certificate — the one a token-only
 * resolver would return for *both* fields.
 */
const EARLIER_CERTIFICATE_TX_HASH = "a1".repeat(32);
const LATER_CERTIFICATE_TX_HASH = "b2".repeat(32);
const EARLIER_CERTIFICATE_INDEX = 5;
const LATER_CERTIFICATE_INDEX = 6;

const resolveFor = (
  plan: MidgardFieldCarriagePlan,
  referenceInputs: readonly UTxO[],
): number =>
  resolveCertificateReferenceIndex({
    certificatePolicyId: CERTIFICATE_POLICY_ID,
    txIdHex: plan.txId.toString("hex"),
    fieldIndex: plan.fieldIndex,
    fieldHashHex: plan.commitment.toString("hex"),
    referenceInputs,
    label: `field ${plan.fieldIndex.toString()}`,
  });

describe("§8.6 certificate selection under the constant asset name V1", () => {
  it("binds the strict certification ABI to the complete reference-input set", () => {
    const plan = planFor({ fieldIndex: 2, fill: 0xa5 });
    const chunks = chunkUtxosFor(plan, "10".repeat(32));
    const { policy, policyId } = nativePolicy();
    const policyReference = utxo({
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: "addr_test1_published_certificate_policy",
      datum: "d87980",
      scriptRef: policy,
    });
    const layout = resolveFieldPreimageCertificationReferenceLayout({
      plan,
      certificatePolicyId: policyId,
      certificatePolicyReferenceUtxo: policyReference,
      chunkUtxos: [...chunks].reverse(),
    });
    expect(layout.referenceInputs).toEqual([
      ...[...chunks].reverse(),
      policyReference,
    ]);
    // The published policy sorts first, so the Certify redeemer must name the
    // two chunks at indices 1 and 2 in the complete ledger reference set.
    expect(layout.chunkRefInputIndices).toEqual([1, 2]);
  });

  it("rejects a substituted or absent certificate-policy reference script", () => {
    const expected = nativePolicy();
    const substituted = nativePolicy();
    const reference = utxo({
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: "addr_test1_published_certificate_policy",
      datum: "d87980",
      scriptRef: substituted.policy,
    });
    expect(() =>
      requireFieldPreimageCertificateReferenceScript({
        certificatePolicyId: expected.policyId,
        referenceUtxo: reference,
      }),
    ).toThrow("reference script hashes to");
    expect(() =>
      requireFieldPreimageCertificateReferenceScript({
        certificatePolicyId: expected.policyId,
        referenceUtxo: { ...reference, scriptRef: undefined },
      }),
    ).toThrow("carries no reference script");
  });

  it("discriminates two same-name certificates of one transaction by field index", () => {
    // One step, two tier-3 fields of the same L2 transaction — the shape the
    // ruling calls out. Both certificates are the same unit.
    const fieldTwo = planFor({ fieldIndex: 2, fill: 0xa5 });
    const fieldFive = planFor({ fieldIndex: 5, fill: 0xa5 });
    expect(fieldTwo.tier).toBe("Certified");
    expect(fieldFive.tier).toBe("Certified");

    const chunks = [
      ...chunkUtxosFor(fieldTwo, "10".repeat(32)),
      ...chunkUtxosFor(fieldFive, "20".repeat(32)),
    ];

    for (const [earlier, later] of [
      [fieldTwo, fieldFive],
      [fieldFive, fieldTwo],
    ] as const) {
      const referenceInputs = [
        SCRIPT_REFERENCE,
        ...chunks,
        certificateUtxoFor(earlier, EARLIER_CERTIFICATE_TX_HASH),
        certificateUtxoFor(later, LATER_CERTIFICATE_TX_HASH),
      ];
      expect(resolveFor(earlier, referenceInputs)).toBe(
        EARLIER_CERTIFICATE_INDEX,
      );
      // The leg that bites: a token-only match would hand this field the
      // canonically-first certificate, which is the *other* field's manifest.
      expect(resolveFor(later, referenceInputs)).toBe(LATER_CERTIFICATE_INDEX);
      // Argument order is irrelevant — the resolver counts into the ledger's
      // canonically-sorted list, not the order a builder collected.
      expect(resolveFor(later, [...referenceInputs].reverse())).toBe(
        LATER_CERTIFICATE_INDEX,
      );
    }
  });

  it("discriminates by the welded field_hash when tx id and field index collide", () => {
    // Two certificates for the same `(tx_id, field_index)`, over different
    // bytes. Both are well-formed and mint-verified against their own chunks —
    // the mint welds `field_hash ↔ chunk_digests`, it does not know which bytes
    // the disputed field actually held. Only the commitment tells them apart,
    // and picking the wrong one gives the door a manifest whose anchored
    // equality cannot hold.
    const honest = planFor({ fieldIndex: 2, fill: 0xa5 });
    const fabricated = planFor({ fieldIndex: 2, fill: 0x5a });
    expect(honest.commitment.toString("hex")).not.toBe(
      fabricated.commitment.toString("hex"),
    );

    const chunks = [
      ...chunkUtxosFor(honest, "10".repeat(32)),
      ...chunkUtxosFor(fabricated, "20".repeat(32)),
    ];

    for (const [earlier, later] of [
      [honest, fabricated],
      [fabricated, honest],
    ] as const) {
      const referenceInputs = [
        SCRIPT_REFERENCE,
        ...chunks,
        certificateUtxoFor(earlier, EARLIER_CERTIFICATE_TX_HASH),
        certificateUtxoFor(later, LATER_CERTIFICATE_TX_HASH),
      ];
      expect(resolveFor(earlier, referenceInputs)).toBe(
        EARLIER_CERTIFICATE_INDEX,
      );
      expect(resolveFor(later, referenceInputs)).toBe(LATER_CERTIFICATE_INDEX);
    }
  });

  it("resolves deterministically when several certificates match the whole triple", () => {
    // §8.7 healing: the same preimage certified twice by two identities. The
    // datums differ only in `owner`, which no consuming step reads, so the two
    // are interchangeable — but "interchangeable" must still mean *one*
    // answer, the same one every time. Duplicates are admissible here (the
    // ruling records that same-triple certificates may coexist), so the
    // resolver must neither throw on them nor let the answer depend on the
    // order a builder happened to collect the set in.
    const plan = planFor({ fieldIndex: 2, fill: 0xa5 });
    const healed = planFor({ fieldIndex: 2, fill: 0xa5, owner: HEALING_OWNER });
    expect(deriveFieldPreimageCertification(healed).datumCbor).not.toBe(
      deriveFieldPreimageCertification(plan).datumCbor,
    );

    // Same tx hash, different output index — the tie-break the comparator falls
    // through to once the hashes are equal.
    const original = certificateUtxoFor(plan, "c1".repeat(32), 0);
    const republished = certificateUtxoFor(healed, "c1".repeat(32), 1);
    const chunks = chunkUtxosFor(plan, "10".repeat(32));
    const base = [SCRIPT_REFERENCE, ...chunks];

    const arrangements: readonly (readonly UTxO[])[] = [
      [...base, original, republished],
      [...base, republished, original],
      [republished, ...base, original],
      [original, republished, ...base],
    ];
    for (const referenceInputs of arrangements) {
      // 0 script reference, 1–2 chunks, then `c1…#0` before `c1…#1`.
      expect(resolveFor(plan, referenceInputs)).toBe(3);
    }
  });

  it("still requires the policy's token, and steps over datums that are not certificates", () => {
    const plan = planFor({ fieldIndex: 2, fill: 0xa5 });
    const chunks = chunkUtxosFor(plan, "10".repeat(32));

    // The right datum under a foreign policy's token is not this policy's
    // certificate, however well it reads.
    const impostor: UTxO = {
      ...certificateUtxoFor(plan, "a1".repeat(32)),
      assets: {
        lovelace: 5_000_000n,
        [`${FOREIGN_POLICY_ID}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`]:
          1n,
      },
    };
    expect(() =>
      resolveFor(plan, [SCRIPT_REFERENCE, ...chunks, impostor]),
    ).toThrowError(
      /^field 2 §8\.6 certificate \(policy [0-9a-f]+, tx [0-9a-f]+, field 2\) is not among the transaction's reference inputs$/,
    );

    // A UTxO carrying this policy's constant-name token over a datum that does
    // not decode as a certificate is skipped rather than fatal: the token no
    // longer implies the shape, so a decode failure is a non-match, not an
    // error.
    const undecodable = utxo({
      txHash: "a1".repeat(32),
      outputIndex: 0,
      address: CERTIFICATE_ADDRESS,
      datum: "d87980",
      assets: { [CERTIFICATE_UNIT]: 1n },
    });
    expect(
      resolveFor(plan, [
        SCRIPT_REFERENCE,
        ...chunks,
        undecodable,
        certificateUtxoFor(plan, "b2".repeat(32)),
      ]),
    ).toBe(4);
  });
});
