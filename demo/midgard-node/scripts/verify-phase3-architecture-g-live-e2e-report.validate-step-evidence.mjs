import { SHA256 } from "./phase3-architecture-g-closure-lib.mjs";
import {
  OWNER_EPOCH,
  positiveInteger,
  ready,
  TX_HASH,
  zeroInteger,
} from "./verify-phase3-architecture-g-live-e2e-report.validate-step-evidence-shape.mjs";

export const validateStepEvidence = (stepId, evidence, context, reasons) => {
  switch (stepId) {
    case "fresh-deployment-preflight": {
      if (
        evidence?.runMode !== "fresh" ||
        evidence?.engine !== "architecture_g" ||
        evidence?.localUplc !== true ||
        evidence?.provider !== "l1_node" ||
        evidence?.cleanDeployment !== true ||
        !ready(evidence?.readiness)
      ) {
        reasons.push("fresh deployment preflight is incomplete");
      }
      break;
    }
    case "deposit-projection": {
      if (
        !TX_HASH.test(evidence?.txHash ?? "") ||
        typeof evidence?.eventId !== "string" ||
        evidence.eventId.length === 0 ||
        evidence?.confirmed !== true ||
        evidence?.projected !== true ||
        !Number.isSafeInteger(evidence?.balanceBeforeLovelace) ||
        !Number.isSafeInteger(evidence?.balanceAfterLovelace) ||
        evidence.balanceAfterLovelace <= evidence.balanceBeforeLovelace
      ) {
        reasons.push("deposit confirmation/projection evidence is incomplete");
      }
      break;
    }
    case "l2-submit": {
      const transactions = Array.isArray(evidence?.transactions)
        ? evidence.transactions
        : [];
      const hashes = transactions.map(({ txHash }) => txHash);
      if (
        transactions.length < 2 ||
        new Set(hashes).size !== transactions.length ||
        transactions.some(
          ({ txHash, status }) =>
            !TX_HASH.test(txHash ?? "") ||
            !["accepted", "committed"].includes(status),
        ) ||
        evidence?.submissionErrors !== 0
      ) {
        reasons.push(
          "L2 submission evidence does not contain two clean admissions",
        );
      } else context.l2TxHashes = hashes;
      break;
    }
    case "da-attestation": {
      const headers = Array.isArray(evidence?.headers) ? evidence.headers : [];
      if (
        headers.length === 0 ||
        headers.some(
          (header) =>
            !TX_HASH.test(header?.headerHash ?? "") ||
            !SHA256.test(header?.payloadMetadataSha256 ?? "") ||
            !SHA256.test(header?.payloadCborSha256 ?? "") ||
            !["attested", "merged"].includes(header?.watcherStatus) ||
            !Array.isArray(header?.attestationTxHashes) ||
            header.attestationTxHashes.length === 0 ||
            header.attestationTxHashes.some((hash) => !TX_HASH.test(hash)),
        )
      ) {
        reasons.push("DA payload and attestation evidence is incomplete");
      } else context.headerHashes = headers.map(({ headerHash }) => headerHash);
      break;
    }
    case "merge-finalization": {
      const committed = Array.isArray(evidence?.committedTxHashes)
        ? evidence.committedTxHashes
        : [];
      const finalized = Array.isArray(evidence?.finalizedHeaderHashes)
        ? evidence.finalizedHeaderHashes
        : [];
      if (
        evidence?.automaticMerge !== true ||
        context.l2TxHashes.some((hash) => !committed.includes(hash)) ||
        context.headerHashes.some((hash) => !finalized.includes(hash)) ||
        !zeroInteger(evidence?.stateQueueDepth) ||
        !zeroInteger(evidence?.unfinishedMutationJobs)
      ) {
        reasons.push("automatic merge/finalization evidence is incomplete");
      }
      break;
    }
    case "db-balance": {
      const counts = evidence?.counts;
      const assertions = Array.isArray(evidence?.balanceAssertions)
        ? evidence.balanceAssertions
        : [];
      if (
        !positiveInteger(counts?.consumedDeposits) ||
        !Number.isSafeInteger(counts?.acceptedAdmissions) ||
        counts.acceptedAdmissions < 2 ||
        !positiveInteger(counts?.immutableRows) ||
        !positiveInteger(counts?.confirmedLedgerRows) ||
        !zeroInteger(counts?.mempoolRows) ||
        !zeroInteger(counts?.processedMempoolRows) ||
        !zeroInteger(counts?.blockRows) ||
        !zeroInteger(counts?.unfinishedMutationJobs) ||
        assertions.length < 3 ||
        assertions.some(
          (entry) =>
            typeof entry?.addressHash !== "string" ||
            entry.addressHash.length === 0 ||
            !Number.isSafeInteger(entry?.expectedLovelace) ||
            entry.actualLovelace !== entry.expectedLovelace,
        )
      ) {
        reasons.push("DB residue or exact balance assertions failed");
      }
      break;
    }
    case "owner-child-restart": {
      if (
        evidence?.signal !== "SIGKILL" ||
        !positiveInteger(evidence?.ownerPidBefore) ||
        !positiveInteger(evidence?.ownerPidAfter) ||
        evidence.ownerPidBefore === evidence.ownerPidAfter ||
        !positiveInteger(evidence?.nodePid) ||
        !Number.isSafeInteger(evidence?.childRestartsBefore) ||
        evidence?.childRestartsAfter !== evidence.childRestartsBefore + 1 ||
        evidence?.nodeProcessRestarted !== false ||
        evidence?.readinessRestored !== true
      ) {
        reasons.push("native owner child restart evidence is incomplete");
      }
      break;
    }
    case "post-submit-recovery": {
      const hashes = [
        evidence?.headerHash,
        evidence?.submissionTxHash,
        evidence?.baseRoot,
        evidence?.candidateRoot,
        evidence?.eventLogDigest,
        evidence?.ownerBinarySha256,
      ];
      if (
        hashes.some((hash) => !TX_HASH.test(hash ?? "")) ||
        evidence.ownerBinarySha256 !== context.identity?.ownerBinary?.sha256 ||
        !positiveInteger(evidence?.replayEventCount) ||
        evidence?.killedAfterSubmission !== true ||
        evidence?.killedBeforePromotion !== true ||
        !OWNER_EPOCH.test(evidence?.ownerEpochBefore ?? "") ||
        !OWNER_EPOCH.test(evidence?.ownerEpochAfter ?? "") ||
        evidence.ownerEpochAfter === evidence.ownerEpochBefore ||
        evidence?.authoritativeMarkerAfter !== evidence?.candidateRoot ||
        evidence?.replayedCandidateRoot !== evidence?.candidateRoot ||
        evidence?.journalStatus !== "locally_applied" ||
        evidence?.l2Status !== "committed" ||
        evidence?.auditDivergence !== 0 ||
        evidence?.recoveryLogMarker !==
          "Architecture G recovered post-submit promotion after native child restart"
      ) {
        reasons.push(
          "post-submit replay/promotion recovery evidence is incomplete",
        );
      }
      break;
    }
    case "final-readiness": {
      if (
        !ready(evidence?.node) ||
        !ready(evidence?.da) ||
        evidence?.allL2Committed !== true ||
        !zeroInteger(evidence?.stateQueueDepth) ||
        !zeroInteger(evidence?.unfinishedMutationJobs) ||
        evidence?.unexpectedErrorCount !== 0
      ) {
        reasons.push("final node/DA readiness or residue gate failed");
      }
      break;
    }
    default:
      reasons.push(`unexpected live step ${stepId}`);
  }
};
