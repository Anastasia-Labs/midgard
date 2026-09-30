import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import {
  DaPayloadsDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { Database, Lucid, MidgardContracts } from "../services/index.js";
import { backfillMissingDaPayloadsFromFinalizedJournals } from "../workers/commit-block-header/da-payload-backfill.js";
import {
  evidence,
  optionRecordEvidence,
  type ReconciliationEvidence,
  type ReconciliationResult,
  type ReconciliationStatus,
  result,
} from "./reconcile.parse-reconciliation-result.js";
import {
  fetchCanonicalStateQueueHeaderHashes,
  fetchCanonicalStateQueueHeaders,
} from "./reconcile.reconcile-phas-registered-program.js";
import {
  type CanonicalDaAttestationDecision,
  type CanonicalDaAttestationObservation,
} from "./reconcile.reconcile-reference-scripts-complete-program.js";

export const classifyCanonicalDaAttestation = ({
  headerHash,
  localPayloadPresent,
  observations,
}: {
  readonly headerHash: string;
  readonly localPayloadPresent: boolean;
  readonly observations: readonly CanonicalDaAttestationObservation[];
}): CanonicalDaAttestationDecision => {
  const matches = observations.filter(
    (observation) => observation.datumHeaderHash === headerHash,
  );
  if (matches.length === 0) {
    return {
      status: "ambiguous",
      reason: "canonical_header_absent",
      nextAction:
        "The configured Cardano source has no exact canonical state-queue node for this header; do not claim DA attestation from local or committee-node-only evidence.",
    };
  }
  if (matches.length !== 1) {
    return {
      status: "blocked",
      reason: "canonical_header_not_unique",
      nextAction:
        "The configured Cardano source returned multiple canonical state-queue nodes for this header; resolve the inconsistent L1 view before continuing.",
    };
  }

  const matched = matches[0]!;
  if (matched.computedHeaderHash !== headerHash) {
    return {
      status: "blocked",
      reason: "header_hash_mismatch",
      nextAction:
        "The canonical state-queue datum key does not match its recomputed HeaderV1 hash; quarantine this observation and reconcile the L1 source.",
    };
  }
  if (matched.daAvailability !== SDK.NO_DA_ATTESTATION) {
    return {
      status: "satisfied",
      reason: "attestation_applied",
      nextAction: null,
    };
  }
  if (!localPayloadPresent) {
    return {
      status: "blocked",
      reason: "local_payload_missing",
      nextAction:
        "The canonical state-queue node is not DA-attested and the exact local payload is missing; restore the canonical payload before attestation.",
    };
  }
  return {
    status: "pending",
    reason: "attestation_pending",
    nextAction:
      "The exact canonical state-queue node is present but has no on-chain DA-attestation marker yet; wait for the canonical committee/submitter pipeline.",
  };
};

const fetchCanonicalDaAttestationObservations = Effect.gen(function* () {
  const canonicalHeaders = yield* fetchCanonicalStateQueueHeaders;
  const observations: CanonicalDaAttestationObservation[] = [];
  for (const canonicalHeader of canonicalHeaders) {
    const node = yield* SDK.getStateQueueNodeFromStateQueueDatum(
      canonicalHeader.utxo.datum,
    );
    observations.push({
      datumHeaderHash: canonicalHeader.headerHash,
      computedHeaderHash: yield* SDK.hashBlockHeader(node.header),
      daAvailability: node.da_attestation,
      outRef: canonicalHeader.outRef,
    });
  }
  return observations;
});

export const reconcileDaAttestedProgram = (options: {
  readonly headerHash: Buffer;
  readonly committeeUrl?: string;
  readonly deploymentFingerprint?: string;
  readonly repair: boolean;
}): Effect.Effect<
  ReconciliationResult,
  DatabaseError,
  Database | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const { headerHash, repair } = options;
    const headerHashHex = headerHash.toString("hex");
    let localPayload = yield* DaPayloadsDB.retrieveByHeaderHash(headerHash);
    const repairActions: string[] = [];
    let backfillSkipped: readonly { readonly reason: string }[] = [];
    if (Option.isNone(localPayload) && repair) {
      const backfill = yield* backfillMissingDaPayloadsFromFinalizedJournals({
        headerHash,
        limit: 1,
      });
      repairActions.push("backfill_missing_da_payload");
      backfillSkipped = backfill.skipped;
      if (backfill.backfilled.includes(headerHashHex)) {
        localPayload = yield* DaPayloadsDB.retrieveByHeaderHash(headerHash);
      }
    }

    const evidenceEntries: ReconciliationEvidence[] = [
      evidence(
        "local_da_payload",
        Option.isNone(localPayload)
          ? { present: false, headerHash: headerHashHex }
          : {
              present: true,
              headerHash: headerHashHex,
              consensusProfileId:
                localPayload.value[DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID],
              payloadSha256:
                localPayload.value[
                  DaPayloadsDB.Columns.PAYLOAD_SHA256
                ].toString("hex"),
            },
      ),
    ];
    if (backfillSkipped.length > 0) {
      evidenceEntries.push(
        evidence("da_payload_backfill_skipped", {
          reasons: backfillSkipped.map((entry) => entry.reason),
        }),
      );
    }

    const contracts = yield* MidgardContracts;
    const canonicalAttempt = yield* Effect.either(
      fetchCanonicalDaAttestationObservations,
    );
    if (canonicalAttempt._tag === "Left") {
      evidenceEntries.push(
        evidence("canonical_l1_da_attestation_query_error", {
          stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
          stateQueuePolicyId: contracts.stateQueue.policyId,
          availabilityPolicyId: contracts.availabilityChallenge.policyId,
          error: formatUnknownError(canonicalAttempt.left, {
            includeCause: true,
          }),
        }),
      );
      return result({
        milestone: "da-attested",
        target: { headerHash: headerHashHex },
        status: "blocked",
        evidence: evidenceEntries,
        repairActions,
        nextAction:
          "The configured Cardano source could not prove the canonical state-queue attestation marker; restore a consistent L1 query path before continuing.",
      });
    }

    const decision = classifyCanonicalDaAttestation({
      headerHash: headerHashHex,
      localPayloadPresent: Option.isSome(localPayload),
      observations: canonicalAttempt.right,
    });
    evidenceEntries.push(
      evidence("canonical_l1_da_attestation", {
        source: "configured_cardano_l1_query",
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
        availabilityPolicyId: contracts.availabilityChallenge.policyId,
        targetHeaderHash: headerHashHex,
        decisionReason: decision.reason,
        observations: canonicalAttempt.right,
      }),
    );
    return result({
      milestone: "da-attested",
      target: { headerHash: headerHashHex },
      status: decision.status,
      evidence: evidenceEntries,
      repairActions,
      nextAction:
        decision.status === "satisfied"
          ? null
          : backfillSkipped.some((entry) =>
                entry.reason.includes("journal excluded by status: abandoned"),
              )
            ? "Canonical journal is abandoned locally; revive it through confirmation recovery and complete local finalization before DA payload backfill."
            : decision.nextAction,
    });
  });

export const reconcileBlockCommittedProgram = ({
  headerHash,
}: {
  readonly headerHash: Buffer;
}): Effect.Effect<
  ReconciliationResult,
  unknown,
  Database | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const headerHashHex = headerHash.toString("hex");
    const journal =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
    const canonicalHeaders = yield* fetchCanonicalStateQueueHeaderHashes;
    const canonical = canonicalHeaders.includes(headerHashHex);
    const journalStatus = Option.isSome(journal)
      ? journal.value[PendingBlockFinalizationsDB.Columns.STATUS]
      : null;
    const status: ReconciliationStatus =
      canonical ||
      journalStatus === PendingBlockFinalizationsDB.Status.Finalized
        ? "satisfied"
        : journalStatus === null
          ? "ambiguous"
          : "pending";
    return result({
      milestone: "block-committed",
      target: { headerHash: headerHashHex },
      status,
      evidence: [
        evidence("canonical_state_queue", {
          containsHeader: canonical,
          headers: canonicalHeaders,
        }),
        optionRecordEvidence(journal),
      ],
      nextAction:
        status === "pending"
          ? "Wait for block confirmation/local finalization worker or inspect state-queue lease."
          : status === "ambiguous"
            ? "No local journal or canonical state-queue evidence exists for this header."
            : null,
    });
  });
