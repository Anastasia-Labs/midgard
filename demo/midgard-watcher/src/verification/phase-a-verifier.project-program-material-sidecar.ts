import {
  decodeMidgardCekProgramMaterialDaEntry,
  encodeMidgardCekProgramMaterialSidecar,
  type MidgardCekProgramMaterialEntry,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core/cek-proof";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/native";
import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
  MIDGARD_CONSENSUS_PROFILE_ID,
  MIDGARD_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/consensus-profile";
import { collectMidgardAttachedProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import type { MidgardValidationPhaseName } from "@al-ft/midgard-core/validation-trace";
import type { Header } from "@al-ft/midgard-sdk";
import type {
  PhaseAConfig,
  RejectCode,
  RejectedTx,
} from "@al-ft/midgard-validation/types";

import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import {
  fail,
  WATCHER_PHASE_A_CANONICAL_REJECT_CODES,
  WATCHER_PHASE_A_VERIFIER_REASON_CODES,
  WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION,
  type WatcherPhaseARejection,
  type WatcherPhaseAVerifierReasonCode,
} from "./phase-a-verifier.watcher-phase-a-excluded-reject-code-justifications.js";
import {
  WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
  WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY,
  type WatcherRuleBundle,
} from "./rule-bundle.js";

export type WatcherPhaseAVerificationResult = Readonly<{
  schemaVersion: typeof WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION;
  action: "accept" | "reject" | "error";
  reasonCodes: readonly WatcherPhaseAVerifierReasonCode[];
  /** W23 rejection-selection rule that produced `selectedRejection`. */
  rejectionSelection: typeof WATCHER_RULE_BUNDLE_REJECTION_SELECTION;
  consensusProfileId: typeof MIDGARD_CONSENSUS_PROFILE_ID;
  /** Null for a queued-transaction evaluation with no block context. */
  headerHash: string | null;
  payloadEnvelopeSha256: string | null;
  payloadSha256: string | null;
  reconstructionDigest: string | null;
  ruleBundleCommitment: string | null;
  transactionCount: number;
  acceptedCount: number;
  /** Accepted transaction ids in canonical block order. */
  acceptedTxIds: readonly string[];
  /** Rejections in canonical block order (ascending `index`). */
  rejections: readonly WatcherPhaseARejection[];
  /** The W23-priority first fault: lowest `stagePriority`, then `index`. */
  selectedRejection: WatcherPhaseARejection | null;
  resultDigest: string;
}>;

export const digestResult = (
  result: Omit<WatcherPhaseAVerificationResult, "resultDigest">,
): WatcherPhaseAVerificationResult =>
  Object.freeze({
    ...result,
    resultDigest: watcherSha256CanonicalJson(result),
  });

export const orderReasonCodes = (
  codes: Iterable<WatcherPhaseAVerifierReasonCode>,
): readonly WatcherPhaseAVerifierReasonCode[] => {
  const present = new Set<string>(codes);
  return Object.freeze(
    WATCHER_PHASE_A_VERIFIER_REASON_CODES.filter((code) => present.has(code)),
  );
};

// ---------------------------------------------------------------------------
// Canonical verdict projection
// ---------------------------------------------------------------------------

export const HEX_32 = /^[0-9a-f]{64}$/u;

/**
 * Projects one canonical `RejectedTx` into the watcher's record shape.
 *
 * Nothing is reinterpreted: the code, stage, and detail are copied verbatim.
 * The only additions are the block position and the W23 stage priority. Every
 * failure path throws, and the caller turns a throw into an error result, so
 * an unrecognisable canonical rejection can never become an acceptance.
 */
export const watcherPhaseARejectionProjection = (input: {
  readonly rejected: RejectedTx;
  readonly index: number;
  readonly expectedTxId: string;
}): WatcherPhaseARejection => {
  const { rejected, index, expectedTxId } = input;
  const txId = rejected.txId.toString("hex");
  if (txId !== expectedTxId) {
    fail("rejection_tx_id_mismatch", `$.rejections[${index.toString()}].txId`);
  }
  const code: string = rejected.code;
  if (!WATCHER_PHASE_A_CANONICAL_REJECT_CODES.includes(code as RejectCode)) {
    fail("unknown_reject_code", `$.rejections[${index.toString()}].code`);
  }
  // One guard, not two: the W23 priority list is the canonical phase
  // enumeration in order, so "absent from the priority list" already covers a
  // missing stage and an unknown stage name. A second `in MidgardValidationPhase`
  // check would be unreachable, and unreachable guards are indistinguishable
  // from no guard at all under mutation.
  const stagePriority = WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY.indexOf(
    rejected.consensusPhase as MidgardValidationPhaseName,
  );
  if (stagePriority < 0) {
    fail("missing_rejection_stage", `$.rejections[${index.toString()}].stage`);
  }
  const stage = rejected.consensusPhase as MidgardValidationPhaseName;
  return Object.freeze({
    index,
    txId,
    code: rejected.code,
    stage,
    stagePriority,
    detail: rejected.detail,
  });
};

/**
 * `first_rejection_by_phase_then_program_counter_v1` at block scope: the
 * lowest canonical validation phase wins, and canonical block order breaks
 * ties.
 *
 * `rejections` is always built in ascending `index` order, so a strict `<` on
 * the phase priority already implements "earliest block position wins on a
 * tie" - the first rejection at the winning phase is reached first and is
 * never displaced. An explicit tie-break clause would be unreachable code.
 */
export const selectRejection = (
  rejections: readonly WatcherPhaseARejection[],
): WatcherPhaseARejection | null => {
  let selected: WatcherPhaseARejection | null = null;
  for (const rejection of rejections) {
    if (selected === null || rejection.stagePriority < selected.stagePriority) {
      selected = rejection;
    }
  }
  return selected;
};

// ---------------------------------------------------------------------------
// Phase A configuration from the L1-committed header and the W23 rule bundle
// ---------------------------------------------------------------------------

/**
 * Phase A is deterministic in the transaction bytes, the consensus profile,
 * and three header-committed parameters. Concurrency is pinned to 1 because a
 * verifier has no throughput requirement and the value must not influence a
 * digest.
 */
export const WATCHER_PHASE_A_CONCURRENCY = 1 as const;

/**
 * Arrival metadata is submission bookkeeping, not a validation input:
 * `validatePhaseASingle` copies `arrivalSeq`/`createdAt` into the accepted
 * candidate and never reads them. The watcher cannot recover the operator's
 * values from public DA, so it pins them: `arrivalSeq` is the canonical block
 * position and `createdAt` is the Unix epoch. Neither appears in the result.
 */
export const WATCHER_PHASE_A_CREATED_AT = new Date(0);

/**
 * Builds the canonical `PhaseAConfig` from the L1-committed header and the
 * W23 rule bundle. Nothing here is a policy choice by the watcher: the three
 * numeric parameters are header fields, and the profile is the exact compiled
 * V1 tuple the rule bundle commits to.
 */
export const makeWatcherPhaseAConfig = (input: {
  readonly header: Header;
  readonly ruleBundle: WatcherRuleBundle;
}): PhaseAConfig => {
  const { header, ruleBundle } = input;
  if (
    ruleBundle.consensusProfileId !== MIDGARD_CONSENSUS_PROFILE_ID ||
    ruleBundle.consensusProfileDigest !== MIDGARD_CONSENSUS_PROFILE_DIGEST ||
    ruleBundle.protocolVersion !== MIDGARD_PROTOCOL_VERSION
  ) {
    fail("rule_bundle_profile_mismatch", "$.ruleBundle.consensusProfileId");
  }
  if (
    ruleBundle.validation.rejectionSelection !==
      WATCHER_RULE_BUNDLE_REJECTION_SELECTION ||
    ruleBundle.validation.phasePriority.length !==
      WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY.length ||
    ruleBundle.validation.phasePriority.some(
      (phase, index) =>
        phase !== WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY[index],
    )
  ) {
    fail("rule_bundle_profile_mismatch", "$.ruleBundle.validation");
  }
  if (header.protocolVersion !== BigInt(ruleBundle.protocolVersion)) {
    fail("header_protocol_version_mismatch", "$.header.protocolVersion");
  }
  return Object.freeze({
    expectedNetworkId: header.expectedNetworkId,
    minFeeA: header.minFeeA,
    minFeeB: header.minFeeB,
    concurrency: WATCHER_PHASE_A_CONCURRENCY,
    strictnessProfile: MIDGARD_CONSENSUS_PROFILE_ID,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  });
};

// ---------------------------------------------------------------------------
// Program-material projection
// ---------------------------------------------------------------------------

export const EMPTY_SIDECAR = encodeMidgardCekProgramMaterialSidecar([]);

/**
 * Reconstructs the per-transaction CEK program-material sidecar from the
 * block-wide, content-addressed `cek_program_material` set.
 *
 * The operator's per-transaction sidecars are NOT recoverable from public DA:
 * the block merges and deduplicates them by content root
 * (`mergeMidgardCekProgramMaterialSidecarsV1`, cek-proof.ts:3856). Handing the
 * merged superset to Phase A would be wrong in the unsafe direction's mirror
 * image - it would reject honest transactions, because
 * `verifyMidgardCekProgramMaterialBundle` requires every supplied node to be
 * reachable from the transaction's own program envelopes.
 *
 * So the watcher projects the superset down to the reachable subset, and it
 * does so with the canonical reachability computation itself: the envelopes
 * come from `collectMidgardAttachedProgramEnvelopes` and the reachable roots
 * come from `verifyMidgardCekProgramMaterialBundle(..., allowUnreachable)`.
 * There is no watcher-authored traversal. If that canonical computation throws
 * for any reason, the complete block-wide set is handed to Phase A unchanged,
 * so the canonical validator - not this function - renders the verdict.
 */
export const projectProgramMaterialSidecar = (
  txCbor: Buffer,
  blockMaterial: readonly MidgardCekProgramMaterialEntry[],
  sourceKind: "normal" | "forced",
): Buffer => {
  if (blockMaterial.length === 0) {
    return EMPTY_SIDECAR;
  }
  try {
    const canonicalTx = (
      sourceKind === "forced"
        ? decodeMidgardForcedTxFullFromCanonicalCbor
        : decodeMidgardNativeTxFullFromCanonicalCbor
    )(txCbor);
    const envelopes = collectMidgardAttachedProgramEnvelopes(canonicalTx);
    const verifications = verifyMidgardCekProgramMaterialBundle(
      envelopes,
      blockMaterial,
      { allowUnreachable: true },
    );
    const reached = new Set<string>();
    for (const verification of verifications) {
      for (const root of verification.reachableRoots) {
        reached.add(root);
      }
    }
    return encodeMidgardCekProgramMaterialSidecar(
      blockMaterial.filter((entry) =>
        reached.has(Buffer.from(entry.root).toString("hex")),
      ),
    );
  } catch {
    return encodeMidgardCekProgramMaterialSidecar(blockMaterial);
  }
};

/** Decodes the block-wide DA program-material entries, or fails closed. */
export const decodeBlockProgramMaterial = (
  entries: readonly (readonly [string, string])[],
): readonly MidgardCekProgramMaterialEntry[] => {
  try {
    return Object.freeze(
      entries.map(([rootHex, valueHex]) =>
        decodeMidgardCekProgramMaterialDaEntry(
          Buffer.from(rootHex, "hex"),
          Buffer.from(valueHex, "hex"),
        ),
      ),
    );
  } catch {
    return fail(
      "malformed_program_material",
      "$.payload.block_body.cek_program_material",
    );
  }
};

// ---------------------------------------------------------------------------
// Queued-transaction derivation
// ---------------------------------------------------------------------------

export type WatcherPhaseABlockTransaction = Readonly<{
  /** 32-byte canonical transaction id, lowercase hex. */
  txId: string;
  /** Exact canonical transaction CBOR from `transaction_preimages`. */
  txCbor: Buffer;
  sourceKind?: "normal" | "forced";
}>;
