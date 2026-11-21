import { MIDGARD_CONSENSUS_LIMITS } from "../consensus-profile.js";
import {
  fail,
  type MidgardFieldCarriagePlan,
} from "./native-tx-carriage.plan-midgard-field-carriage.js";
import {
  type MidgardFieldCarriage,
  type ResolvedCarriageReferenceInput,
} from "./native-tx-field-access.js";

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
export const MAX_L1_TX_BYTES =
  MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes;

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
export const DATA_BYTE_STRING_CHUNK_BYTES = 64;

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
 * `demo/midgard-validation/tests/field-preimage-carriage-fit-emulator.test.ts`,
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
