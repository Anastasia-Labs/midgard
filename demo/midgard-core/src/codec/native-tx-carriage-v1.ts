/**
 * The TypeScript twin of `onchain/aiken/lib/midgard/native-tx-carriage-v1.ak`
 * — the **publication half** of the `docs/spec/midgard-tx.md` §8 field-preimage
 * carriage ladder.
 *
 * `./native-tx-field-access-v1.js` is the consumer half: it takes carriage that
 * already exists and turns it into an authenticated view. This module is what
 * decides which carriage should exist in the first place, and it owns exactly
 * three things:
 *
 *   * **The plan (§8.1–§8.4).** Given a preimage and the `(tx_id, field_index)`
 *     it belongs to, which tier carries it, what has to be published, and what
 *     certificate — if any — has to be minted. Tier selection is a total
 *     function of the preimage's length, so it is never a builder's choice.
 *   * **The reference-input layout.** Tier-3 carriage names its manifest and
 *     its chunks by *position*, and a plan that produces indices separately
 *     from the reference-input list they index into is a plan with two places
 *     to get an off-by-one. {@link layOutMidgardFieldCarriage} emits both
 *     from one traversal, so the carriage a step's redeemer carries and the
 *     reference inputs the step resolves cannot disagree.
 *   * **Healing (§8.7).** Content addressing is the whole mechanism: because
 *     the §8.4 split is a pure function of the preimage bytes, an unrelated
 *     party who obtains the same preimage produces byte-identical publications
 *     and an interchangeable certificate. {@link healMidgardFieldCarriage}
 *     is that re-derivation, and
 *     {@link midgardFieldCarriagePlansAreInterchangeable} is the predicate a
 *     caller checks it by rather than trusting it.
 *
 * **What a tier is, and is not, visible to.** The tier is branched on *inside
 * this module* — three times, and each one is a place where the three tiers
 * really are three different objects: {@link planMidgardFieldCarriage}
 * (nothing, one UTxO or `n` UTxOs plus a certificate to publish),
 * {@link layOutMidgardFieldCarriage} (a different carriage constructor and a
 * different reference-input layout) and
 * {@link midgardFieldCarriagePlansAreInterchangeable} (only tier 3 has a
 * certificate to compare). What the claim is really about is the boundary, not
 * the count: **no caller of this module branches on the tier**, because
 * {@link layOutMidgardFieldCarriage} hands back a
 * {@link MidgardFieldCarriage} and the reference inputs it indexes, and
 * `authenticatedMidgardFieldViewV1` turns any of the three into the same
 * {@link MidgardFieldViewV1}. That is §8's simplest-fitting-first mandate
 * expressed as a type: there is no tier-shaped argument anywhere downstream of
 * this module. The one place tier survives into consumer-visible *behaviour*
 * is `midgardFieldItemCountV1`, which declines to answer for a variable-width
 * field under tier 3 because §7.4's arithmetic does not apply there and no
 * other check reconciles the declared count — a documented refusal, never a
 * different answer.
 *
 * **Nothing here authenticates anything.** Raw carriage is unauthenticated data
 * (§8.5) and a plan is a statement about bytes the caller already holds. The
 * `(tx_id, field_index)` a plan is built against is used to derive the
 * certificate and its asset name; it is *not* evidence that the preimage is
 * that field's. What proves that is the §4 commitment check the door runs on
 * the way back in, and — on-chain — the minting policy that would not have let
 * the certificate exist otherwise.
 */

import { MIDGARD_CONSENSUS_LIMITS } from "../consensus-profile-v1.js";
import {
  MidgardTxCodecError,
  type MidgardTxCodecErrorCode,
  MidgardTxCodecErrorCodes,
} from "./errors.js";
import {
  deriveMidgardFieldPreimageCertificate,
  exactMidgardFieldIndex,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  MIDGARD_MAX_TIER3_CHUNK_COUNT,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  type MidgardFieldCarriage,
  midgardFieldCommitment,
  type MidgardFieldPreimageCertificate,
  type ResolvedCarriageReferenceInput,
  selectMidgardFieldCarriageTier,
  splitMidgardFieldPreimageIntoChunks,
} from "./native-tx-field-access-v1.js";

/**
 * The type annotation sits on the binding rather than on the arrow so that
 * TypeScript's control-flow analysis treats a call as unreachable-after; that
 * is what lets the guards below narrow a nullable plan field instead of being
 * followed by a non-null assertion.
 */
const fail: (
  message: string,
  detail?: string,
  code?: MidgardTxCodecErrorCode,
) => never = (
  message,
  detail,
  code = MidgardTxCodecErrorCodes.SchemaMismatch,
) => {
  throw new MidgardTxCodecError(code, message, detail);
};

/** The tier a plan carries its preimage under. */
export type MidgardFieldCarriageTier = MidgardFieldCarriage["carriage"];

/**
 * One raw carriage UTxO a publisher has to create: a §8.5 nothing-but-bytes
 * inline datum at the publisher's own key address.
 *
 * `chunkIndex` is `0` for a tier-2 whole-preimage publication and the §8.4
 * chunk index under tier 3, so a caller that publishes in list order publishes
 * in the order the certificate's digest vector is written and the order tier-3
 * carriage indexes them. `digest` is the `blake2b_256` of `bytes` — under tier
 * 3 it is exactly the certificate's `chunkDigests[chunkIndex]`, and it is
 * carried here so a publisher can address a publication by content (§8.7)
 * without recomputing it.
 */
export type MidgardFieldPublication = {
  readonly chunkIndex: number;
  readonly bytes: Buffer;
  readonly digest: Buffer;
};

/**
 * Everything that has to exist on-chain for one field's preimage to reach a
 * dispute transaction, and nothing about how it gets there.
 *
 * A plan is pure: two callers who hold the same preimage and name the same
 * `(txId, fieldIndex)` produce plans that differ in `certificate.owner` and in
 * nothing else. That is what §8.7's healing rests on, and it is checkable —
 * see {@link midgardFieldCarriagePlansAreInterchangeable}.
 */
export type MidgardFieldCarriagePlan = {
  readonly tier: MidgardFieldCarriageTier;
  readonly fieldIndex: number;
  readonly txId: Buffer;
  readonly totalLength: number;
  /** §4. The flat commitment these bytes must match at the door. */
  readonly commitment: Buffer;
  /** Tier 1 carries the preimage in the step's redeemer; this is those bytes. */
  readonly inlinePreimage: Buffer | null;
  /** Empty under tier 1; one entry under tier 2; `n` chunks under tier 3. */
  readonly publications: readonly MidgardFieldPublication[];
  /** §8.6. Present under tier 3 only — the only tier that certifies. */
  readonly certificate: MidgardFieldPreimageCertificate | null;
  /**
   * §8.6's constant token name (#606) — the same for every certificate of the
   * policy; identity lives in the datum. Present under tier 3 only.
   */
  readonly certificateAssetName: Buffer | null;
};

/**
 * Plans the carriage for one field preimage — the entry point of the
 * publication half.
 *
 * The tier is chosen by {@link selectMidgardFieldCarriageTier}, which is a
 * partition rather than a preference (§8.4): a preimage that fits tier 1 has
 * exactly one admissible carriage and cannot be re-carried under tier 3 to
 * side-step the structural checks tiers 1–2 run at view construction. An empty
 * preimage is refused here rather than planned as a zero-byte publication: the
 * §5.1 empty field is `80`, one byte, and a genuinely empty byte string is a
 * caller's mistake in every case.
 *
 * `owner` is the min-Ada reclaim authority the certificate records (§8.6). It
 * is the only part of a plan that is the publisher's own choice, and no
 * consuming step reads it.
 *
 * `publish` demotes tier 1 to tier 2 and is the **one** tier choice §8 leaves
 * open. The on-chain partition §8.4 enforces is at the *top* of the ladder — the
 * door refuses a certificate whose `total_length ≤ K`, so a field that fits one
 * publication can never be re-carried as a one-chunk manifest. Below that
 * boundary tiers 1 and 2 are indistinguishable to the door: `whole_view` hashes
 * the same bytes against the same commitment whether they arrived in a redeemer
 * or in a referenced datum. What separates them is whose byte budget pays —
 * tier 1 spends the consuming transaction's, tier 2 spends a prior one's — and
 * that is a property of the consuming transaction, not of the preimage. See
 * `planTxOrderMaterialCarriageV1` in the SDK for the caller this exists for: a
 * forced order whose fields fit tier 1 individually but not all at once.
 */
export const planMidgardFieldCarriage = ({
  owner,
  txId,
  fieldIndex,
  preimage,
  publish = false,
}: {
  readonly owner: Uint8Array;
  readonly txId: Uint8Array;
  readonly fieldIndex: number;
  readonly preimage: Uint8Array;
  readonly publish?: boolean;
}): MidgardFieldCarriagePlan => {
  const bytes = Buffer.from(preimage);
  if (bytes.length === 0) {
    fail(
      "a field preimage is never empty — the §5.1 empty field is one byte (`80`)",
      "length=0",
    );
  }
  // Both bounds are enforced on **every** tier, not only where a certificate
  // datum would enforce them for us (only tier 3 builds one); a tier-1 or
  // tier-2 plan naming field 42 would otherwise be constructible here and
  // refused much later, at the door, in a caller that had already built a
  // transaction around it.
  const exactFieldIndex = exactMidgardFieldIndex(fieldIndex);
  const exactTxId = Buffer.from(txId);
  if (exactTxId.length !== 32) {
    fail("a transaction id is 32 bytes", `length=${exactTxId.length}`);
  }
  const selected = selectMidgardFieldCarriageTier(bytes.length);
  const tier = publish && selected === "Inline" ? "RawUtxo" : selected;
  const commitment = midgardFieldCommitment(bytes);
  const base = {
    tier,
    fieldIndex: exactFieldIndex,
    txId: exactTxId,
    totalLength: bytes.length,
    commitment,
  } as const;

  if (tier === "Inline") {
    return {
      ...base,
      inlinePreimage: bytes,
      publications: [],
      certificate: null,
      certificateAssetName: null,
    };
  }

  if (tier === "RawUtxo") {
    return {
      ...base,
      inlinePreimage: null,
      publications: [{ chunkIndex: 0, bytes, digest: commitment }],
      certificate: null,
      certificateAssetName: null,
    };
  }

  const certificate = deriveMidgardFieldPreimageCertificate({
    owner,
    txId,
    fieldIndex,
    preimage: bytes,
  });
  const chunks = splitMidgardFieldPreimageIntoChunks(bytes);
  if (chunks.length > MIDGARD_MAX_TIER3_CHUNK_COUNT) {
    fail(
      "§8.3 bounds tier-3 carriage at three chunks",
      `chunks=${chunks.length}`,
    );
  }
  return {
    ...base,
    inlinePreimage: null,
    publications: chunks.map((chunk, chunkIndex) => {
      const digest = certificate.chunkDigests[chunkIndex];
      if (digest === undefined) {
        // Unreachable: the digest vector is `chunks.map(commitment)` from the
        // same split. Checked rather than asserted, because a publication whose
        // digest did not come from its own bytes is exactly the object §8.7's
        // content addressing must never produce.
        return fail(
          "chunk digest vector is shorter than the split",
          `chunk=${chunkIndex}`,
        );
      }
      return { chunkIndex, bytes: chunk, digest };
    }),
    certificate,
    certificateAssetName: Buffer.from(
      MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
    ),
  };
};

/**
 * §8.7. Re-derives a plan's carriage the way an unrelated party would after a
 * publication was yanked: from the preimage bytes and the `(txId, fieldIndex)`
 * binding alone, under a new owner.
 *
 * This is deliberately not a "repair" that consults the original plan — it
 * takes the preimage and re-runs {@link planMidgardFieldCarriage}. Healing
 * works precisely because there is nothing to consult: the split is a pure
 * function of the bytes, so the healer's publications are byte-identical and
 * the yanked certificate still describes them. A function that copied anything
 * across from the original would hide the moment that stopped being true.
 */
export const healMidgardFieldCarriage = ({
  healer,
  txId,
  fieldIndex,
  preimage,
  publish = false,
}: {
  readonly healer: Uint8Array;
  readonly txId: Uint8Array;
  readonly fieldIndex: number;
  readonly preimage: Uint8Array;
  /**
   * Set when the carriage being healed is a tier-1-sized field that its consumer
   * demoted to tier 2. Without it the healer would re-plan the field as tier 1
   * and produce a plan with no publications — nothing to heal with.
   */
  readonly publish?: boolean;
}): MidgardFieldCarriagePlan =>
  planMidgardFieldCarriage({
    owner: healer,
    txId,
    fieldIndex,
    preimage,
    publish,
  });

/**
 * Whether two plans describe the same carriage — the property §8.7's healing
 * claim actually needs.
 *
 * Interchangeable means: same tier, same field, same transaction, same total
 * length, same commitment, byte-identical publications in the same order, and
 * — under tier 3 — the same digest vector and the same mint-welded
 * `fieldHash` (#606: the token name is one constant, so the datum is where
 * certificate identity lives). It deliberately does **not** compare
 * `certificate.owner`: the owner is the min-Ada reclaim authority, it differs
 * by construction when a second identity heals, and no consuming step reads
 * it. Two interchangeable plans mean either party's certificate verifies
 * against either party's chunks.
 */
export const midgardFieldCarriagePlansAreInterchangeable = (
  left: MidgardFieldCarriagePlan,
  right: MidgardFieldCarriagePlan,
): boolean => {
  if (
    left.tier !== right.tier ||
    left.fieldIndex !== right.fieldIndex ||
    left.totalLength !== right.totalLength ||
    !left.txId.equals(right.txId) ||
    !left.commitment.equals(right.commitment) ||
    left.publications.length !== right.publications.length
  ) {
    return false;
  }
  const publicationsAgree = left.publications.every((publication, index) => {
    // Checked rather than cast. The lengths were compared above, so an
    // undefined here is unreachable — but this predicate is what a caller uses
    // *instead of* trusting the healer, and a cast is the one construct that
    // would let a malformed plan answer `true` by reading a property off
    // `undefined` in a build where that did not throw.
    const other = right.publications[index];
    return (
      other !== undefined &&
      publication.chunkIndex === other.chunkIndex &&
      publication.bytes.equals(other.bytes) &&
      publication.digest.equals(other.digest)
    );
  });
  if (!publicationsAgree) {
    return false;
  }
  const leftCertificate = left.certificate;
  const rightCertificate = right.certificate;
  if (leftCertificate === null || rightCertificate === null) {
    // Tiers 1–2 have no certificate; two plans agree here only by both having
    // none, which the tier equality above has already established.
    return leftCertificate === rightCertificate;
  }
  const rightDigests = rightCertificate.chunkDigests;
  const digestsAgree =
    leftCertificate.chunkDigests.length === rightDigests.length &&
    leftCertificate.chunkDigests.every((digest, index) => {
      const other = rightDigests[index];
      return other !== undefined && digest.equals(other);
    });
  return (
    digestsAgree &&
    leftCertificate.totalLength === rightCertificate.totalLength &&
    leftCertificate.fieldHash.equals(rightCertificate.fieldHash) &&
    left.certificateAssetName !== null &&
    right.certificateAssetName !== null &&
    left.certificateAssetName.equals(right.certificateAssetName)
  );
};

/**
 * A carriage value together with the reference inputs its indices point into.
 *
 * The two are emitted together and never separately, because tier-3 carriage is
 * positional: `chunkRefInputIndices[k]` is an index into a list, and a builder
 * that assembled the list in one place and the indices in another would have
 * two chances to be off by one and no way to notice until a script refused.
 */
export type MidgardFieldCarriageLayout = {
  readonly carriage: MidgardFieldCarriage;
  /**
   * The carriage-bearing reference inputs, in the order this layout indexes
   * them. Empty under tier 1.
   */
  readonly referenceInputs: readonly ResolvedCarriageReferenceInput[];
  /**
   * Absolute reference-input index of each entry above, in the same order —
   * what a transaction builder needs in order to place them.
   */
  readonly referenceInputIndices: readonly number[];
};

/**
 * Lays a plan out against a transaction's reference-input list.
 *
 * `baseIndex` is the absolute index at which this field's carriage begins, so a
 * step disputing several fields lays each out after the last. Under tier 3 the
 * manifest goes first and the chunks follow in §8.4 order, which is the order
 * `chunkRefInputIndices` requires and the order a certificate's digest vector
 * is written in.
 *
 * The result is the point at which tier stops being visible: whatever comes
 * back, a consumer passes `carriage` and `referenceInputs` to
 * `authenticatedMidgardFieldViewV1` and reads items off the view.
 */
export const layOutMidgardFieldCarriage = ({
  plan,
  baseIndex = 0,
}: {
  readonly plan: MidgardFieldCarriagePlan;
  readonly baseIndex?: number;
}): MidgardFieldCarriageLayout => {
  if (!Number.isSafeInteger(baseIndex) || baseIndex < 0) {
    fail(
      "reference-input base index must be a non-negative integer",
      `${baseIndex}`,
    );
  }

  if (plan.tier === "Inline") {
    if (plan.inlinePreimage === null) {
      fail("tier-1 plan carries no preimage", `field=${plan.fieldIndex}`);
    }
    return {
      carriage: { carriage: "Inline", preimage: plan.inlinePreimage },
      referenceInputs: [],
      referenceInputIndices: [],
    };
  }

  if (plan.tier === "RawUtxo") {
    const publication = plan.publications[0];
    if (publication === undefined || plan.publications.length !== 1) {
      fail(
        "tier-2 plan must carry exactly one publication",
        `publications=${plan.publications.length}`,
      );
    }
    return {
      carriage: { carriage: "RawUtxo", refInputIndex: baseIndex },
      referenceInputs: [{ inlineDatumBytes: publication.bytes }],
      referenceInputIndices: [baseIndex],
    };
  }

  const certificate = plan.certificate;
  const certificateAssetName = plan.certificateAssetName;
  if (certificate === null || certificateAssetName === null) {
    fail("tier-3 plan carries no certificate", `field=${plan.fieldIndex}`);
  }
  const chunkIndices = plan.publications.map(
    (_, offset) => baseIndex + 1 + offset,
  );
  return {
    carriage: {
      carriage: "Certified",
      certRefInputIndex: baseIndex,
      chunkRefInputIndices: chunkIndices,
    },
    referenceInputs: [
      { certificate, certificateAssetName },
      ...plan.publications.map((publication) => ({
        inlineDatumBytes: publication.bytes,
      })),
    ],
    referenceInputIndices: [baseIndex, ...chunkIndices],
  };
};

// ---------------------------------------------------------------------------
// §8.3 erratum E1 — the publishable frontier
// ---------------------------------------------------------------------------

/**
 * The `maxTxSize` every carriage publication is judged against. Named here
 * rather than imported at each call site because a publication that does not
 * clear it is not a publication at all.
 */
const MAX_L1_TX_BYTES = MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes;

/**
 * The transaction-side allowance separating the *exact* frontier (a
 * publication that lands on `maxTxSize` to the byte) from the *reliable* one
 * (the same publication with room for the variability a real submission path
 * introduces). Carried over from the counted era's
 * `proofItemEnvelopeReliabilityReserveBytes` so the flat frontier is
 * comparable to the counted bound it supersedes.
 */
export const MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES = 512;

/**
 * The genuinely payload-independent cost of a §8.5 publication transaction:
 * body framing, one input, the change output, the fee, and one vkey witness.
 *
 * Measured, not assumed. Note that this is **not** the whole non-datum cost:
 * an inline datum is carried in the output as a CBOR byte string wrapping the
 * serialised Plutus Data, and that wrapper's own head grows with the datum, so
 * the third term lives in {@link midgardCarriagePublicationFramingBytes}
 * rather than here. An earlier revision folded a head of 3 into this constant
 * and published 248 as "fixed, payload-independent"; it is neither, and the
 * error ran in the unsafe direction above a 65,536-byte datum. See
 * {@link midgardCarriagePublicationFramingBytes}.
 */
export const MIDGARD_CARRIAGE_PUBLICATION_FIXED_FRAMING_BYTES = 245;

/** The Plutus Data byte-string chunk width; above it the encoding changes. */
const DATA_BYTE_STRING_CHUNK_BYTES = 64;

const cborHeadBytes = (length: number): number =>
  length < 24 ? 1 : length < 256 ? 2 : length < 65_536 ? 3 : 5;

/**
 * The non-payload cost of a §8.5 publication transaction carrying an
 * `encodedDatumBytes`-byte inline datum: the fixed framing plus the CBOR head
 * of the byte string the ledger wraps that datum in.
 *
 * **Why this is a function and not a constant.** The head is one byte below a
 * 24-byte datum, two below 256, three below 65,536 and five above it, so the
 * non-payload cost is 246, 247, 248 and 250 bytes respectively. Across the
 * whole of the carriage ladder the datum sits in `[256, 65_536)` and the answer
 * is a flat 248 — which is why an earlier revision mistook it for a constant —
 * but the flat region is bounded on both sides, and only the lower bound is
 * safe to be wrong about. Overstating a 22-byte publication by two bytes
 * refuses nothing that would have fitted; understating a publication whose
 * datum crosses 65,536 bytes would hand a caller a transaction the ledger
 * rejects, which is the exact failure §8.3 erratum E1 exists to stop. Modelling
 * the head removes the unsafe direction rather than documenting around it.
 *
 * Every value this function returns at a carriage-relevant size is pinned
 * against a real signed emulator transaction by
 * `§8.3 Phase-4 exit measurement — the tier-2 raw-UTxO bound` in
 * `demo/midgard-validation/tests/field-preimage-carriage-fit-emulator-v1.test.ts`,
 * which samples both sides of the 24-, 256- and 64-byte boundaries.
 */
export const midgardCarriagePublicationFramingBytes = (
  encodedDatumBytes: number,
): number =>
  MIDGARD_CARRIAGE_PUBLICATION_FIXED_FRAMING_BYTES +
  cborHeadBytes(encodedDatumBytes);

/**
 * How many bytes `payloadBytes` raw bytes occupy once encoded as a Plutus Data
 * byte string.
 *
 * At or below 64 bytes that is an ordinary definite byte string. **Strictly
 * above** it, Plutus Data serialisation switches to an indefinite-length string
 * of 64-byte definite chunks — `5f 5840 … ff` — and each chunk pays its own
 * two-byte head. That is the ≈3.125% payload-proportional cost that makes the
 * framing of a publication a function of its payload rather than a constant,
 * and it is the cost §8.3's erratum E1 quantifies.
 */
export const midgardCarriageDataByteStringBytes = (
  payloadBytes: number,
): number => {
  if (!Number.isSafeInteger(payloadBytes) || payloadBytes < 0) {
    fail("payload length must be a non-negative integer", `${payloadBytes}`);
  }
  if (payloadBytes <= DATA_BYTE_STRING_CHUNK_BYTES) {
    return cborHeadBytes(payloadBytes) + payloadBytes;
  }
  const fullChunks = Math.floor(payloadBytes / DATA_BYTE_STRING_CHUNK_BYTES);
  const remainder = payloadBytes % DATA_BYTE_STRING_CHUNK_BYTES;
  return (
    1 + // 5f — indefinite-length byte string
    fullChunks *
      (cborHeadBytes(DATA_BYTE_STRING_CHUNK_BYTES) +
        DATA_BYTE_STRING_CHUNK_BYTES) +
    (remainder === 0 ? 0 : cborHeadBytes(remainder) + remainder) +
    1 // ff — break
  );
};

/**
 * The size of the signed §8.5 publication transaction that carries
 * `payloadBytes` bytes — the quantity `maxTxSize` is actually applied to.
 *
 * **Three terms.**
 * {@link MIDGARD_CARRIAGE_PUBLICATION_FIXED_FRAMING_BYTES} of genuinely
 * fixed transaction framing, the CBOR head of the inline datum's byte-string
 * wrapper, and the payload's own Plutus Data encoding. Only the first is a
 * constant; the middle term is a step function of the second
 * ({@link midgardCarriagePublicationFramingBytes}), and collapsing the two
 * into a single 248 is what an earlier revision did — exact across the ladder,
 * two bytes conservative at or below a 22-byte payload, and two bytes
 * *optimistic* above a 63,547-byte one.
 *
 * With the head modelled the result is exact at every payload size, which is
 * the property a fail-closed guard needs: there is no size at which this
 * function reports a publication smaller than the ledger will see it. The
 * emulator measurement asserts the exactness against real signed transactions
 * rather than this comment claiming it.
 */
export const midgardCarriagePublicationBytes = (
  payloadBytes: number,
): number => {
  const datumBytes = midgardCarriageDataByteStringBytes(payloadBytes);
  return midgardCarriagePublicationFramingBytes(datumBytes) + datumBytes;
};

const largestPayloadWithin = (budget: number): number => {
  let payload = budget;
  while (payload > 0 && midgardCarriagePublicationBytes(payload) > budget) {
    payload -= 1;
  }
  return payload;
};

/**
 * §8.3 erratum E1. The largest payload whose signed publication lands **on**
 * `maxTxSize` — 15,644 bytes, in a 16,384-byte transaction.
 *
 * Derived from {@link midgardCarriagePublicationBytes} rather than written
 * down, so the frontier and the cost model can never disagree; the emulator
 * measurement pins both against real transactions.
 */
export const MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES =
  largestPayloadWithin(MAX_L1_TX_BYTES);

/**
 * §8.3 erratum E1. **The bound a publisher builds against**: the largest
 * payload whose signed publication clears `maxTxSize` with the 512-byte
 * reliability reserve — 15,148 bytes, in a 15,872-byte transaction.
 *
 * This is the operative limit on §8.5 raw carriage, and since E1's repair landed
 * it is **exactly** `chunk_bytes_k` ({@link MIDGARD_CHUNK_BYTES_K}): the
 * chunker cuts at the publishable frontier, so every chunk of every tier-3 plan
 * publishes, and a tier-2 preimage is admissible precisely when it fits one
 * publication. The two are asserted equal in
 * `demo/midgard-core/tests/native-tx-carriage-v1.test.ts` rather than left to
 * coincide — that assertion is the whole of E1's repair as a checkable property.
 *
 * Before the repair `K` read 15,900, a 15,900-byte chunk measured 16,648 signed
 * (264 over `maxTxSize`), and because the chunker cut at `K` the unpublishable
 * window was the whole of `(15,148, 32,768]` rather than a tier-2 sliver — tier 3
 * did not function at any preimage size. {@link
 * midgardFieldCarriagePublishability} is the guard that made that failure
 * visible at build time; it stays, because the frontier is a measurement and a
 * caller may still exceed it deliberately (a raised-limit measurement, or a
 * future `maxTxSize`).
 */
export const MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES = largestPayloadWithin(
  MAX_L1_TX_BYTES - MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES,
);

/** One publication that a plan requires and `maxTxSize` will not accept. */
export type MidgardUnpublishableChunk = {
  readonly chunkIndex: number;
  readonly byteLength: number;
  /** Size of the signed publication transaction these bytes would produce. */
  readonly publicationBytes: number;
  /** By how much it exceeds the budget it was judged against. */
  readonly overrunBytes: number;
};

/**
 * Whether every publication a plan requires can actually be published, and if
 * not, exactly which ones cannot and by how much.
 *
 * **Why this is a report and not an exception at plan time — and why §8.3 E1's
 * prohibition is worded against publication.** A plan is a statement about
 * bytes, and the §8.4 split is a pure function that healing and certification
 * both depend on reproducing; making {@link planMidgardFieldCarriage} refuse
 * would take the erratum's diagnosis away from the caller who needs it and make
 * the measurement that found it unrunnable. E1 therefore prohibits
 * **publishing** carriage above the frontier, not planning it, and the refusal
 * lives where a transaction is actually built: the SDK's
 * `buildUnsignedFieldPreimagePublicationV1Program` consumes this report.
 *
 * With E1's repair applied the chunker cuts at the frontier, so an honest §8.4
 * plan now reports `publishable: true` at every preimage size up to the §5.4
 * cap. What this function still catches is the two cases that are not an honest
 * plan at the compiled `K`: a chunk list assembled by hand or by an older
 * chunker, and a deliberate raised-limit measurement. Before the repair it
 * caught *every* tier-3 plan, which is what made the outage visible at build
 * time instead of at submission.
 *
 * `budgetBytes` defaults to the reliable frontier's transaction budget. A
 * caller measuring the frontier itself passes a larger one deliberately.
 */
export const midgardFieldCarriagePublishability = ({
  plan,
  budgetBytes = MAX_L1_TX_BYTES - MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES,
}: {
  readonly plan: MidgardFieldCarriagePlan;
  readonly budgetBytes?: number;
}): {
  readonly publishable: boolean;
  readonly budgetBytes: number;
  readonly unpublishableChunks: readonly MidgardUnpublishableChunk[];
} => {
  const unpublishableChunks = plan.publications.flatMap((publication) => {
    const publicationBytes = midgardCarriagePublicationBytes(
      publication.bytes.length,
    );
    return publicationBytes <= budgetBytes
      ? []
      : [
          {
            chunkIndex: publication.chunkIndex,
            byteLength: publication.bytes.length,
            publicationBytes,
            overrunBytes: publicationBytes - budgetBytes,
          },
        ];
  });
  return {
    publishable: unpublishableChunks.length === 0,
    budgetBytes,
    unpublishableChunks,
  };
};

/**
 * The §8.3 table, as the one place a builder asks "will this fit that tier?".
 *
 * Exposed as one frozen object rather than left to callers comparing against
 * the constants, because the tier-1 bound is still **provisional pending
 * Phase-4 measurement** (§8.3) and a re-pin has to move every call site at
 * once. `maxPublishableCarriageBytes` is the row erratum E1 added; since E1's
 * repair landed it **equals** `chunkBytesK`, which is what makes the ladder
 * publishable end to end, and the two are asserted equal in this module's tests.
 */
export const midgardFieldCarriageBounds = Object.freeze({
  maxTier1RedeemerPreimageBytes: MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  chunkBytesK: MIDGARD_CHUNK_BYTES_K,
  maxTransactionAggregateFieldBytes:
    MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  maxTier3ChunkCount: MIDGARD_MAX_TIER3_CHUNK_COUNT,
  maxPublishableCarriageBytes: MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
  exactPublishableCarriageBytes: MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES,
});
