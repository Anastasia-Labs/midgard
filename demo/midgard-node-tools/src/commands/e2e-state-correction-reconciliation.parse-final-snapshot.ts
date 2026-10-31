import {
  canonicalString,
  E2E_STATE_CORRECTION_FINAL_SNAPSHOT_SCHEMA_VERSION,
  exactKeys,
  exactString,
  type FinalSnapshot,
  lowerHex,
  nonNegativeInteger,
  record,
} from "./e2e-state-correction-reconciliation.final-snapshot.js";
import {
  canonicalAssetUnit,
  canonicalLovelace,
  canonicalOutRef,
  parseObservedChainPoint,
  sha256Hex,
  stringArray,
} from "./e2e-state-correction-reconciliation.parse-kupo-matches.js";

export const parseFinalSnapshot = (value: unknown): FinalSnapshot => {
  const field = "final snapshot";
  const candidate = record(value, field);
  exactKeys(
    candidate,
    [
      "schemaVersion",
      "runId",
      "network",
      "manifestId",
      "observedAt",
      "authentication",
      "stateQueue",
      "jobs",
      "watcher",
      "economics",
      "withdrawalReservePayout",
      "forcedClassifications",
    ],
    field,
  );
  const stateQueue = record(candidate.stateQueue, `${field}.stateQueue`);
  exactKeys(
    stateQueue,
    ["depth", "fraudulentHeaderHashes"],
    `${field}.stateQueue`,
  );
  const jobs = record(candidate.jobs, `${field}.jobs`);
  exactKeys(
    jobs,
    ["unfinishedMutationJobs", "pendingFinalizations"],
    `${field}.jobs`,
  );
  const watcher = record(candidate.watcher, `${field}.watcher`);
  exactKeys(watcher, ["readiness", "verification"], `${field}.watcher`);
  const authentication = record(
    candidate.authentication,
    `${field}.authentication`,
  );
  exactKeys(
    authentication,
    [
      "source",
      "kupoStateQueueResponsePath",
      "kupoStateQueueResponseSha256",
      "kupoProofTokenResponses",
      "ogmiosTipResponsePath",
      "ogmiosTipResponseSha256",
      "nodeDatabaseExportPath",
      "nodeDatabaseExportSha256",
    ],
    `${field}.authentication`,
  );
  if (!Array.isArray(authentication.kupoProofTokenResponses)) {
    throw new Error(
      `${field}.authentication.kupoProofTokenResponses must be an array`,
    );
  }
  const kupoProofTokenResponses = authentication.kupoProofTokenResponses.map(
    (value, index) => {
      const itemField = `${field}.authentication.kupoProofTokenResponses[${index.toString()}]`;
      const item = record(value, itemField);
      exactKeys(
        item,
        ["unit", "outRef", "responsePath", "responseSha256"],
        itemField,
      );
      return {
        unit: canonicalAssetUnit(item.unit, `${itemField}.unit`),
        outRef: canonicalOutRef(item.outRef, `${itemField}.outRef`),
        responsePath: canonicalString(
          item.responsePath,
          `${itemField}.responsePath`,
        ),
        responseSha256: sha256Hex(
          item.responseSha256,
          `${itemField}.responseSha256`,
        ),
      };
    },
  );
  if (!Array.isArray(candidate.economics)) {
    throw new Error(`${field}.economics must be an array`);
  }
  const economics = candidate.economics.map((value, index) => {
    const itemField = `${field}.economics[${index.toString()}]`;
    const item = record(value, itemField);
    exactKeys(
      item,
      [
        "familyId",
        "removalTxHash",
        "proofTokenUnit",
        "proofTokenOutRef",
        "removalReferencedProofTokenOutRef",
        "proofTokenFinalState",
        "operatorCredential",
        "proverCredential",
        "operatorBondInputOutRef",
        "operatorBondInputLovelace",
        "proverRewardOutputOutRef",
        "removalFeeLovelace",
        "slashedLovelace",
        "proverRewardLovelace",
        "duplicateRewardCount",
      ],
      itemField,
    );
    return {
      familyId: canonicalString(item.familyId, `${itemField}.familyId`),
      removalTxHash: sha256Hex(
        item.removalTxHash,
        `${itemField}.removalTxHash`,
      ),
      proofTokenUnit: canonicalAssetUnit(
        item.proofTokenUnit,
        `${itemField}.proofTokenUnit`,
      ),
      proofTokenOutRef: canonicalOutRef(
        item.proofTokenOutRef,
        `${itemField}.proofTokenOutRef`,
      ),
      removalReferencedProofTokenOutRef: canonicalOutRef(
        item.removalReferencedProofTokenOutRef,
        `${itemField}.removalReferencedProofTokenOutRef`,
      ),
      proofTokenFinalState: exactString(
        item.proofTokenFinalState,
        "retained",
        `${itemField}.proofTokenFinalState`,
      ),
      operatorCredential: canonicalString(
        item.operatorCredential,
        `${itemField}.operatorCredential`,
      ),
      proverCredential: canonicalString(
        item.proverCredential,
        `${itemField}.proverCredential`,
      ),
      operatorBondInputOutRef:
        item.operatorBondInputOutRef === null
          ? null
          : canonicalOutRef(
              item.operatorBondInputOutRef,
              `${itemField}.operatorBondInputOutRef`,
            ),
      operatorBondInputLovelace: canonicalLovelace(
        item.operatorBondInputLovelace,
        `${itemField}.operatorBondInputLovelace`,
      ),
      proverRewardOutputOutRef:
        item.proverRewardOutputOutRef === null
          ? null
          : canonicalOutRef(
              item.proverRewardOutputOutRef,
              `${itemField}.proverRewardOutputOutRef`,
            ),
      removalFeeLovelace: canonicalLovelace(
        item.removalFeeLovelace,
        `${itemField}.removalFeeLovelace`,
      ),
      slashedLovelace: canonicalLovelace(
        item.slashedLovelace,
        `${itemField}.slashedLovelace`,
      ),
      proverRewardLovelace: canonicalLovelace(
        item.proverRewardLovelace,
        `${itemField}.proverRewardLovelace`,
      ),
      duplicateRewardCount: nonNegativeInteger(
        item.duplicateRewardCount,
        `${itemField}.duplicateRewardCount`,
      ),
    };
  });
  const withdrawal = record(
    candidate.withdrawalReservePayout,
    `${field}.withdrawalReservePayout`,
  );
  exactKeys(
    withdrawal,
    [
      "withdrawalOrderTxHash",
      "reserveTxHash",
      "payoutInitTxHash",
      "payoutAddTxHashes",
      "payoutConcludeTxHash",
      "destination",
      "payoutValueSha256",
      "reserveValueSha256",
      "status",
    ],
    `${field}.withdrawalReservePayout`,
  );
  if (!Array.isArray(candidate.forcedClassifications)) {
    throw new Error(`${field}.forcedClassifications must be an array`);
  }
  const forcedClassifications = candidate.forcedClassifications.map(
    (value, index) => {
      const itemField = `${field}.forcedClassifications[${index.toString()}]`;
      const item = record(value, itemField);
      exactKeys(
        item,
        [
          "direction",
          "evidenceTxHash",
          "correctionTxHash",
          "canonicalClassification",
          "finalClassification",
        ],
        itemField,
      );
      const rawDirection = canonicalString(
        item.direction,
        `${itemField}.direction`,
      );
      if (
        rawDirection !== "valid-marked-invalid" &&
        rawDirection !== "invalid-marked-valid"
      ) {
        throw new Error(`${itemField}.direction is unsupported`);
      }
      const direction: "valid-marked-invalid" | "invalid-marked-valid" =
        rawDirection;
      const rawCanonicalClassification = canonicalString(
        item.canonicalClassification,
        `${itemField}.canonicalClassification`,
      );
      const rawFinalClassification = canonicalString(
        item.finalClassification,
        `${itemField}.finalClassification`,
      );
      if (
        (rawCanonicalClassification !== "valid" &&
          rawCanonicalClassification !== "invalid") ||
        (rawFinalClassification !== "valid" &&
          rawFinalClassification !== "invalid")
      ) {
        throw new Error(
          `${itemField} classifications must be valid or invalid`,
        );
      }
      const canonicalClassification: "valid" | "invalid" =
        rawCanonicalClassification;
      const finalClassification: "valid" | "invalid" = rawFinalClassification;
      return {
        direction,
        evidenceTxHash: sha256Hex(
          item.evidenceTxHash,
          `${itemField}.evidenceTxHash`,
        ),
        correctionTxHash: sha256Hex(
          item.correctionTxHash,
          `${itemField}.correctionTxHash`,
        ),
        canonicalClassification,
        finalClassification,
      };
    },
  );
  return {
    schemaVersion: exactString(
      candidate.schemaVersion,
      E2E_STATE_CORRECTION_FINAL_SNAPSHOT_SCHEMA_VERSION,
      `${field}.schemaVersion`,
    ),
    runId: canonicalString(candidate.runId, `${field}.runId`),
    network: exactString(candidate.network, "Preprod", `${field}.network`),
    manifestId: sha256Hex(candidate.manifestId, `${field}.manifestId`),
    observedAt: parseObservedChainPoint(
      candidate.observedAt,
      `${field}.observedAt`,
    ),
    authentication: {
      source: exactString(
        authentication.source,
        "local-kupmios-ogmios-and-node-db",
        `${field}.authentication.source`,
      ),
      kupoStateQueueResponsePath: canonicalString(
        authentication.kupoStateQueueResponsePath,
        `${field}.authentication.kupoStateQueueResponsePath`,
      ),
      kupoStateQueueResponseSha256: sha256Hex(
        authentication.kupoStateQueueResponseSha256,
        `${field}.authentication.kupoStateQueueResponseSha256`,
      ),
      kupoProofTokenResponses,
      ogmiosTipResponsePath: canonicalString(
        authentication.ogmiosTipResponsePath,
        `${field}.authentication.ogmiosTipResponsePath`,
      ),
      ogmiosTipResponseSha256: sha256Hex(
        authentication.ogmiosTipResponseSha256,
        `${field}.authentication.ogmiosTipResponseSha256`,
      ),
      nodeDatabaseExportPath: canonicalString(
        authentication.nodeDatabaseExportPath,
        `${field}.authentication.nodeDatabaseExportPath`,
      ),
      nodeDatabaseExportSha256: sha256Hex(
        authentication.nodeDatabaseExportSha256,
        `${field}.authentication.nodeDatabaseExportSha256`,
      ),
    },
    stateQueue: {
      depth: nonNegativeInteger(stateQueue.depth, `${field}.stateQueue.depth`),
      fraudulentHeaderHashes: stringArray(
        stateQueue.fraudulentHeaderHashes,
        `${field}.stateQueue.fraudulentHeaderHashes`,
        (entry, entryField) => lowerHex(entry, 28, entryField),
      ),
    },
    jobs: {
      unfinishedMutationJobs: nonNegativeInteger(
        jobs.unfinishedMutationJobs,
        `${field}.jobs.unfinishedMutationJobs`,
      ),
      pendingFinalizations: nonNegativeInteger(
        jobs.pendingFinalizations,
        `${field}.jobs.pendingFinalizations`,
      ),
    },
    watcher: {
      readiness: exactString(
        watcher.readiness,
        "ready",
        `${field}.watcher.readiness`,
      ),
      verification: exactString(
        watcher.verification,
        "resumed_after_reconciliation",
        `${field}.watcher.verification`,
      ),
    },
    economics,
    withdrawalReservePayout: {
      withdrawalOrderTxHash: sha256Hex(
        withdrawal.withdrawalOrderTxHash,
        `${field}.withdrawalReservePayout.withdrawalOrderTxHash`,
      ),
      reserveTxHash: sha256Hex(
        withdrawal.reserveTxHash,
        `${field}.withdrawalReservePayout.reserveTxHash`,
      ),
      payoutInitTxHash: sha256Hex(
        withdrawal.payoutInitTxHash,
        `${field}.withdrawalReservePayout.payoutInitTxHash`,
      ),
      payoutAddTxHashes: stringArray(
        withdrawal.payoutAddTxHashes,
        `${field}.withdrawalReservePayout.payoutAddTxHashes`,
        sha256Hex,
      ),
      payoutConcludeTxHash: sha256Hex(
        withdrawal.payoutConcludeTxHash,
        `${field}.withdrawalReservePayout.payoutConcludeTxHash`,
      ),
      destination: canonicalString(
        withdrawal.destination,
        `${field}.withdrawalReservePayout.destination`,
      ),
      payoutValueSha256: sha256Hex(
        withdrawal.payoutValueSha256,
        `${field}.withdrawalReservePayout.payoutValueSha256`,
      ),
      reserveValueSha256: sha256Hex(
        withdrawal.reserveValueSha256,
        `${field}.withdrawalReservePayout.reserveValueSha256`,
      ),
      status: exactString(
        withdrawal.status,
        "paid",
        `${field}.withdrawalReservePayout.status`,
      ),
    },
    forcedClassifications,
  };
};
