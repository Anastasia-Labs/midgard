import {
  MidgardTxCodecError,
  type MidgardTxCodecErrorCode,
  MidgardTxCodecErrorCodes,
} from "./errors.js";
import {
  deriveMidgardFieldPreimageCertificate,
  exactMidgardFieldIndex,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
  MIDGARD_MAX_TIER3_CHUNK_COUNT,
  type MidgardFieldCarriage,
  midgardFieldCommitment,
  type MidgardFieldPreimageCertificate,
  selectMidgardFieldCarriageTier,
  splitMidgardFieldPreimageIntoChunks,
} from "./native-tx-field-access.js";

/**
 * The type annotation sits on the binding rather than on the arrow so that
 * TypeScript's control-flow analysis treats a call as unreachable-after; that
 * is what lets the guards below narrow a nullable plan field instead of being
 * followed by a non-null assertion.
 */
export const fail: (
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
