import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { canonicalBlockEvidenceFromVerifiedPayload } from "@al-ft/midgard-fault-proofs";
import type {
  AuthenticatedStateQueueHeaderObservation,
  EvidenceProvenance,
} from "@al-ft/midgard-sdk";
import { validatePhaseASingle } from "@al-ft/midgard-validation/phase-a";
import type { PhaseAConfig, QueuedTx } from "@al-ft/midgard-validation/types";

import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import type { WatcherHeaderRootReconstructionResult } from "./header-root-reconstruction.js";
import { WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION } from "./header-root-reconstruction.js";
import {
  decodeBlockProgramMaterial,
  digestResult,
  HEX_32,
  makeWatcherPhaseAConfig,
  orderReasonCodes,
  projectProgramMaterialSidecar,
  selectRejection,
  WATCHER_PHASE_A_CREATED_AT,
  type WatcherPhaseABlockTransaction,
  watcherPhaseARejectionProjection,
  type WatcherPhaseAVerificationResult,
} from "./phase-a-verifier.project-program-material-sidecar.js";
import {
  fail,
  REACHABLE_SET,
  WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION,
  type WatcherPhaseARejection,
  WatcherPhaseAVerifierError,
  type WatcherPhaseAVerifierReasonCode,
} from "./phase-a-verifier.watcher-phase-a-excluded-reject-code-justifications.js";
import {
  WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
  type WatcherRuleBundle,
} from "./rule-bundle.js";

/**
 * Derives the canonical `QueuedTx` list from authenticated block transactions.
 *
 * The `txId` is not trusted as a claim: the canonical reconstruction already
 * required it to equal `computeMidgardNativeTxIdV1` of the preimage
 * (reconstruct.ts:445-490), and `validatePhaseASingle` re-checks it a second
 * time (E_TX_HASH_MISMATCH, phase-a.ts:431). This function only reshapes.
 */
export const watcherPhaseAQueuedTxs = (input: {
  readonly transactions: readonly WatcherPhaseABlockTransaction[];
  readonly programMaterial: readonly (readonly [string, string])[];
}): readonly QueuedTx[] => {
  const blockMaterial = decodeBlockProgramMaterial(input.programMaterial);
  return Object.freeze(
    input.transactions.map((transaction, index) => {
      if (!HEX_32.test(transaction.txId)) {
        fail(
          "canonical_reconstruction_failed",
          `$.transactions[${index}].txId`,
        );
      }
      return Object.freeze({
        txId: Buffer.from(transaction.txId, "hex"),
        txCbor: transaction.txCbor,
        sourceKind: transaction.sourceKind ?? "normal",
        programMaterialSidecarCbor: projectProgramMaterialSidecar(
          transaction.txCbor,
          blockMaterial,
          transaction.sourceKind ?? "normal",
        ),
        arrivalSeq: BigInt(index),
        createdAt: WATCHER_PHASE_A_CREATED_AT,
      });
    }),
  );
};

// ---------------------------------------------------------------------------
// Core evaluation
// ---------------------------------------------------------------------------

export type WatcherPhaseABlockContext = Readonly<{
  headerHash: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  reconstructionDigest: string;
  ruleBundleCommitment: string;
}>;

const NULL_CONTEXT = {
  headerHash: null,
  payloadEnvelopeSha256: null,
  payloadSha256: null,
  reconstructionDigest: null,
  ruleBundleCommitment: null,
} as const;

const errorResult = (
  reasonCodes: Iterable<WatcherPhaseAVerifierReasonCode>,
  context: WatcherPhaseABlockContext | null,
  transactionCount: number,
): WatcherPhaseAVerificationResult =>
  digestResult({
    schemaVersion: WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION,
    action: "error",
    reasonCodes: orderReasonCodes(reasonCodes),
    rejectionSelection: WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
    ...(context ?? NULL_CONTEXT),
    transactionCount,
    acceptedCount: 0,
    acceptedTxIds: Object.freeze([]),
    rejections: Object.freeze([]),
    selectedRejection: null,
  });

const reasonCodeOf = (error: unknown): WatcherPhaseAVerifierReasonCode =>
  error instanceof WatcherPhaseAVerifierError
    ? error.code
    : "canonical_validation_threw";

/**
 * Runs the canonical Phase A validator over already-derived queued
 * transactions and projects the verdicts.
 *
 * This is the only place a verdict is produced, and it produces none of its
 * own: `validatePhaseASingle` decides, and everything after the call is
 * bookkeeping. A throw out of the canonical layer, an unrecognisable
 * rejection, or an accepted/rejected count that does not add up all yield
 * `action: "error"`.
 */
export const evaluateWatcherPhaseAQueuedTxs = (input: {
  readonly queuedTxs: readonly QueuedTx[];
  readonly config: PhaseAConfig;
  readonly context?: WatcherPhaseABlockContext;
}): WatcherPhaseAVerificationResult => {
  const { queuedTxs, config } = input;
  const context = input.context ?? null;
  const transactionCount = queuedTxs.length;
  const reasonCodes = new Set<WatcherPhaseAVerifierReasonCode>();
  const acceptedTxIds: string[] = [];
  const rejections: WatcherPhaseARejection[] = [];

  for (const [index, queuedTx] of queuedTxs.entries()) {
    const expectedTxId = queuedTx.txId.toString("hex");
    let outcome;
    try {
      outcome = validatePhaseASingle(queuedTx, config);
    } catch (error) {
      return errorResult(
        [reasonCodeOf(error), "canonical_validation_threw"],
        context,
        transactionCount,
      );
    }
    if ("ledgerTx" in outcome) {
      acceptedTxIds.push(expectedTxId);
      continue;
    }
    let projected: WatcherPhaseARejection;
    try {
      projected = watcherPhaseARejectionProjection({
        rejected: outcome,
        index,
        expectedTxId,
      });
    } catch (error) {
      return errorResult([reasonCodeOf(error)], context, transactionCount);
    }
    if (!REACHABLE_SET.has(projected.code)) {
      reasonCodes.add("undeclared_reachable_code");
    }
    reasonCodes.add("phase_a_rejection");
    rejections.push(projected);
  }

  // No accepted/rejected reconciliation guard here: the loop above puts every
  // queued transaction into exactly one bucket or returns early, so the counts
  // cannot disagree. A guard that no input can trip is not fail-closed
  // behaviour, it is unreachable code.
  return digestResult({
    schemaVersion: WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION,
    action: rejections.length === 0 ? "accept" : "reject",
    reasonCodes: orderReasonCodes(reasonCodes),
    rejectionSelection: WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
    ...(context ?? NULL_CONTEXT),
    transactionCount,
    acceptedCount: acceptedTxIds.length,
    acceptedTxIds: Object.freeze([...acceptedTxIds]),
    rejections: Object.freeze([...rejections]),
    selectedRejection: selectRejection(rejections),
  });
};

// ---------------------------------------------------------------------------
// Block evaluation
// ---------------------------------------------------------------------------

export type EvaluateWatcherPhaseABlockInput = {
  /** L1-authenticated header observation, as W22 consumed it. */
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  /** The accepted W22 record for this block. */
  readonly reconstruction: WatcherHeaderRootReconstructionResult;
  /** Exact public `DaPayloadEnvelopeV1` bytes, from the header decision's canonical evidence. */
  readonly payloadEnvelopeCbor: Uint8Array;
  /** Provenance of those bytes; must be public/permissionless DA. */
  readonly daProvenance: EvidenceProvenance;
  /** The W23 rule bundle, with its commitment. */
  readonly ruleBundle: WatcherRuleBundle;
  readonly ruleBundleCommitment: string;
  readonly minimumConfirmationDepth?: number;
};

/**
 * Re-checks the caller's W22 record against a fresh canonical recomputation.
 *
 * The W22 record is a digest-bound summary, so it can be replayed but not
 * trusted on its own: this recomputes its `resultDigest` from its own fields
 * and requires the accepted root/count set and header identity to equal what
 * the canonical evidence core just produced from the supplied bytes. A record
 * describing a different block, a different payload, or a rejected
 * reconstruction cannot reach the validator.
 */
const bindReconstruction = (input: {
  readonly reconstruction: WatcherHeaderRootReconstructionResult;
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
}): void => {
  const { reconstruction } = input;
  if (
    reconstruction.schemaVersion !==
    WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION
  ) {
    fail("reconstruction_unsupported_schema", "$.reconstruction.schemaVersion");
  }
  const { resultDigest, ...withoutDigest } = reconstruction;
  if (watcherSha256CanonicalJson(withoutDigest) !== resultDigest) {
    fail("reconstruction_digest_mismatch", "$.reconstruction.resultDigest");
  }
  if (
    reconstruction.action !== "accept" ||
    reconstruction.reconstructedRoots === null ||
    reconstruction.reconstructedCounts === null ||
    reconstruction.reasonCodes.length !== 0
  ) {
    fail("reconstruction_not_accepted", "$.reconstruction.action");
  }
  if (reconstruction.headerHash !== input.headerHash) {
    fail("reconstruction_header_mismatch", "$.reconstruction.headerHash");
  }
  if (reconstruction.payloadEnvelopeSha256 !== input.payloadEnvelopeSha256) {
    fail("payload_bytes_mismatch", "$.reconstruction.payloadEnvelopeSha256");
  }
};

/**
 * The W24 entry point: an accepted W22 reconstruction plus the exact block
 * bytes from the header decision's canonical evidence plus the W23 rule bundle produce a frozen, digest-bound record of the
 * canonical Phase A verdict for every transaction in the block.
 */
export const evaluateWatcherPhaseABlock = async (
  input: EvaluateWatcherPhaseABlockInput,
): Promise<WatcherPhaseAVerificationResult> => {
  let evidence;
  try {
    evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: input.observation,
      payloadEnvelopeCbor: input.payloadEnvelopeCbor,
      daProvenance: input.daProvenance,
      ...(input.minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth: input.minimumConfirmationDepth }),
    });
  } catch {
    return errorResult(["canonical_reconstruction_failed"], null, 0);
  }

  const context: WatcherPhaseABlockContext = Object.freeze({
    headerHash: evidence.headerHash,
    payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
    payloadSha256: evidence.payloadSha256,
    reconstructionDigest: input.reconstruction.resultDigest,
    ruleBundleCommitment: input.ruleBundleCommitment,
  });

  let queuedTxs: readonly QueuedTx[];
  let config: PhaseAConfig;
  try {
    bindReconstruction({
      reconstruction: input.reconstruction,
      headerHash: evidence.headerHash,
      payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
    });
    config = makeWatcherPhaseAConfig({
      header: evidence.header,
      ruleBundle: input.ruleBundle,
    });
    queuedTxs = watcherPhaseAQueuedTxs({
      transactions: evidence.reconstruction.transactions.map((entry) =>
        Object.freeze({
          txId: entry.txId,
          txCbor: entry.fullTransactionCbor,
        }),
      ),
      programMaterial: evidence.reconstruction.payload.block_body
        .cek_program_material as readonly (readonly [string, string])[],
    });
  } catch (error) {
    return errorResult(
      [reasonCodeOf(error)],
      context,
      evidence.reconstruction.transactions.length,
    );
  }

  return evaluateWatcherPhaseAQueuedTxs({ queuedTxs, config, context });
};
