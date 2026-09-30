/**
 * The builder-side half of the §8.8 field-opening door — what a rebound
 * fraud-proof step submitter needs in order to hand a validator one of the nine
 * committed fields of the transaction its computation thread is disputing.
 *
 * `@al-ft/midgard-sdk`'s `fraud-proof/field-opening.ts` owns the *wire*
 * shapes (`NativeTxAnchorV1`, `FieldOpening`, the §2.5 field table). This
 * module owns the *transaction* half: deriving the §5.1 preimage from canonical
 * item bytes, planning its §8 carriage, publishing that carriage when the tier
 * calls for it, and resolving the positional reference-input indices against the
 * transaction that will actually run the door.
 *
 * **Everything here refuses early what the door would abort on.** Each of the
 * checks below is one the on-chain door applies unconditionally (§7.3
 * abort-never-clamp), and a builder that discovers it from a
 * `Spend[0] the validator crashed` trace has already paid for a transaction and
 * — for a fault proof — may have burned an unrepeatable computation thread:
 *
 *   * the compact bytes re-derive to the id thread state anchored
 *     (`verify_native_tx_compact_cbor_v1`);
 *   * the §5.1 preimage hashes to the commitment the compact structure carries
 *     *at this field index* (`field_commitment_at`) — §4 removed field-index
 *     domain separation, so a preimage that opens the wrong slot is otherwise a
 *     perfectly well-formed value;
 *   * a witness-set field (§2.5 6–8) carries the transaction's own compact
 *     witness set, whose hash is the anchored `witness_set_hash`;
 *   * the §2.5 half pairs with the field (`field_pairs_with`, applied by the
 *     SDK's `fieldOpeningForField`; the former fields-6–8 tier-3 refusal
 *     lifted with #606's welded-hash repair).
 *
 * **The tier is never a caller's argument.** §8.4 partitions on the preimage's
 * own length, and `planMidgardFieldCarriage` is the only thing here that
 * decides it. `publish` is the single choice §8 leaves open — it demotes a
 * tier-1 preimage to a tier-2 publication so the step's own redeemer does not
 * have to carry the bytes — and it changes which transaction pays, never what
 * the door authenticates.
 */

import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "./json-file.js";
import "./runtime.js";
import "./workflow/transaction-boundary.js";
import "./field-opening.plan-fault-proof-field-opening.js";
import "./field-opening.publish-fault-proof-field-carriage.js";
import "./field-opening.certify-fault-proof-field-carriage.js";
export {
  type CertifiedFaultProofFieldCarriage,
  certifyFaultProofFieldCarriage,
  faultProofFieldCarriageReferenceOrder,
  fieldPreimageCertificateAddress,
  resolveFaultProofFieldPreimageCertificate,
} from "./field-opening.certify-fault-proof-field-carriage.js";
export {
  type FaultProofFieldOpeningPlan,
  parseNativeTxCompactCbor,
  planFaultProofFieldOpening,
} from "./field-opening.plan-fault-proof-field-opening.js";
export {
  faultProofFieldCarriage,
  faultProofFieldOpening,
  faultProofRawFieldCarriage,
  findMissingFaultProofFieldPublication,
  type MissingFaultProofFieldPublication,
  publishFaultProofFieldCarriage,
  resolveFaultProofFieldCarriagePublications,
} from "./field-opening.publish-fault-proof-field-carriage.js";
