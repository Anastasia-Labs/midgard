import {
  type AuthenticatedL1TxObservation,
  canonicalString,
  E2E_AUTHENTICATED_L1_TX_OBSERVATION_SCHEMA_VERSION,
  E2E_STATE_CORRECTION_RECOVERY_OBSERVATION_SCHEMA_VERSION,
  exactKeys,
  exactString,
  nonNegativeInteger,
  record,
  type RecoveryObservation,
  type StateCorrectionIndependentAuthority,
} from "./e2e-state-correction-reconciliation.final-snapshot.js";
import {
  assertEqual,
  type DerivedL1Observation,
  parseChainPoint,
  parseKupoMatches,
  parseObservedChainPoint,
  parseOgmiosBlock,
  parseOgmiosTip,
  readDigestCheckedJson,
  sha256Hex,
} from "./e2e-state-correction-reconciliation.parse-kupo-matches.js";

export const deriveAuthenticatedL1Observation = async ({
  observation,
  observationPath,
  authority,
}: {
  readonly observation: AuthenticatedL1TxObservation;
  readonly observationPath: string;
  readonly authority: StateCorrectionIndependentAuthority;
}): Promise<DerivedL1Observation> => {
  const [kupoRaw, ogmiosBlockRaw, ogmiosTipRaw] = await Promise.all([
    readDigestCheckedJson({
      parentPath: observationPath,
      childPath: observation.authentication.kupoResponsePath,
      expectedSha256: observation.authentication.kupoResponseSha256,
      field: `${observation.txHash} raw Kupo response`,
    }),
    readDigestCheckedJson({
      parentPath: observationPath,
      childPath: observation.authentication.ogmiosBlockResponsePath,
      expectedSha256: observation.authentication.ogmiosBlockResponseSha256,
      field: `${observation.txHash} raw Ogmios block response`,
    }),
    readDigestCheckedJson({
      parentPath: observationPath,
      childPath: observation.authentication.ogmiosTipResponsePath,
      expectedSha256: observation.authentication.ogmiosTipResponseSha256,
      field: `${observation.txHash} raw Ogmios tip response`,
    }),
  ]);
  const kupoMatches = parseKupoMatches(
    kupoRaw.value,
    `${observation.txHash} raw Kupo response`,
  );
  if (kupoMatches.length === 0) {
    throw new Error(`${observation.txHash} raw Kupo response has no matches`);
  }
  const matchingKupo = kupoMatches.filter(
    (match) => match.transactionId === observation.txHash,
  );
  if (matchingKupo.length !== kupoMatches.length) {
    throw new Error(
      `${observation.txHash} raw Kupo response contains another transaction`,
    );
  }
  const kupoPoint = matchingKupo[0]!.createdAt;
  for (const match of matchingKupo) {
    assertEqual(
      match.createdAt,
      kupoPoint,
      `${observation.txHash} Kupo creation point agreement`,
    );
  }
  const ogmiosBlock = parseOgmiosBlock(
    ogmiosBlockRaw.value,
    `${observation.txHash} raw Ogmios block response`,
  );
  assertEqual(
    ogmiosBlock.point,
    kupoPoint,
    `${observation.txHash} Kupo/Ogmios inclusion point`,
  );
  if (!ogmiosBlock.transactionIds.has(observation.txHash)) {
    throw new Error(
      `${observation.txHash} is absent from its raw Ogmios block response`,
    );
  }
  const ogmiosTip = parseOgmiosTip(
    ogmiosTipRaw.value,
    `${observation.txHash} raw Ogmios tip response`,
  );
  if (ogmiosTip.height < ogmiosBlock.height) {
    throw new Error(`${observation.txHash} Ogmios tip precedes inclusion`);
  }
  const includedAt = kupoPoint;
  const observedAtTip = {
    slot: ogmiosTip.slot,
    blockHash: ogmiosTip.blockHash,
    confirmationDepth: ogmiosTip.height - ogmiosBlock.height + 1,
  };
  assertEqual(
    observation.includedAt,
    includedAt,
    `${observation.txHash} claimed/raw inclusion point`,
  );
  assertEqual(
    observation.observedAtTip,
    observedAtTip,
    `${observation.txHash} claimed/raw tip observation`,
  );
  const rawSourceDigests = {
    kupoResponseSha256: observation.authentication.kupoResponseSha256,
    ogmiosBlockResponseSha256:
      observation.authentication.ogmiosBlockResponseSha256,
    ogmiosTipResponseSha256: observation.authentication.ogmiosTipResponseSha256,
  };
  await authority.authenticateTransaction({
    txHash: observation.txHash,
    kupoOutputIndex: matchingKupo[0]!.outputIndex,
    includedAt,
    observedAtTip,
    rawSourceDigests,
  });
  return {
    observation: { ...observation, includedAt, observedAtTip },
    kupoOutputIndex: matchingKupo[0]!.outputIndex,
    inclusionHeight: ogmiosBlock.height,
    rawPaths: [kupoRaw.path, ogmiosBlockRaw.path, ogmiosTipRaw.path],
  };
};

export const parseAuthenticatedL1Observation = (
  value: unknown,
  field: string,
): AuthenticatedL1TxObservation => {
  const candidate = record(value, field);
  exactKeys(
    candidate,
    [
      "schemaVersion",
      "runId",
      "network",
      "manifestId",
      "txHash",
      "includedAt",
      "observedAtTip",
      "authentication",
    ],
    field,
  );
  const authentication = record(
    candidate.authentication,
    `${field}.authentication`,
  );
  exactKeys(
    authentication,
    [
      "source",
      "kupoResponsePath",
      "kupoResponseSha256",
      "ogmiosBlockResponsePath",
      "ogmiosBlockResponseSha256",
      "ogmiosTipResponsePath",
      "ogmiosTipResponseSha256",
    ],
    `${field}.authentication`,
  );
  return {
    schemaVersion: exactString(
      candidate.schemaVersion,
      E2E_AUTHENTICATED_L1_TX_OBSERVATION_SCHEMA_VERSION,
      `${field}.schemaVersion`,
    ),
    runId: canonicalString(candidate.runId, `${field}.runId`),
    network: exactString(candidate.network, "Preprod", `${field}.network`),
    manifestId: sha256Hex(candidate.manifestId, `${field}.manifestId`),
    txHash: sha256Hex(candidate.txHash, `${field}.txHash`),
    includedAt: parseChainPoint(candidate.includedAt, `${field}.includedAt`),
    observedAtTip: parseObservedChainPoint(
      candidate.observedAtTip,
      `${field}.observedAtTip`,
    ),
    authentication: {
      source: exactString(
        authentication.source,
        "local-kupmios-ogmios",
        `${field}.authentication.source`,
      ),
      kupoResponsePath: canonicalString(
        authentication.kupoResponsePath,
        `${field}.authentication.kupoResponsePath`,
      ),
      kupoResponseSha256: sha256Hex(
        authentication.kupoResponseSha256,
        `${field}.authentication.kupoResponseSha256`,
      ),
      ogmiosBlockResponsePath: canonicalString(
        authentication.ogmiosBlockResponsePath,
        `${field}.authentication.ogmiosBlockResponsePath`,
      ),
      ogmiosBlockResponseSha256: sha256Hex(
        authentication.ogmiosBlockResponseSha256,
        `${field}.authentication.ogmiosBlockResponseSha256`,
      ),
      ogmiosTipResponsePath: canonicalString(
        authentication.ogmiosTipResponsePath,
        `${field}.authentication.ogmiosTipResponsePath`,
      ),
      ogmiosTipResponseSha256: sha256Hex(
        authentication.ogmiosTipResponseSha256,
        `${field}.authentication.ogmiosTipResponseSha256`,
      ),
    },
  };
};

export const parseRecoveryObservation = (
  value: unknown,
  field: string,
): RecoveryObservation => {
  const candidate = record(value, field);
  exactKeys(
    candidate,
    [
      "schemaVersion",
      "runId",
      "manifestId",
      "id",
      "beforeJournalSha256",
      "afterJournalSha256",
      "duplicateSubmissionCount",
      "lostEvidenceCount",
      "verifiedBeforeReconciliationCount",
      "unrecoverableWorkflowCount",
      "manualRepairCount",
      "terminalState",
      "watcherState",
    ],
    field,
  );
  return {
    schemaVersion: exactString(
      candidate.schemaVersion,
      E2E_STATE_CORRECTION_RECOVERY_OBSERVATION_SCHEMA_VERSION,
      `${field}.schemaVersion`,
    ),
    runId: canonicalString(candidate.runId, `${field}.runId`),
    manifestId: sha256Hex(candidate.manifestId, `${field}.manifestId`),
    id: canonicalString(candidate.id, `${field}.id`),
    beforeJournalSha256: sha256Hex(
      candidate.beforeJournalSha256,
      `${field}.beforeJournalSha256`,
    ),
    afterJournalSha256: sha256Hex(
      candidate.afterJournalSha256,
      `${field}.afterJournalSha256`,
    ),
    duplicateSubmissionCount: nonNegativeInteger(
      candidate.duplicateSubmissionCount,
      `${field}.duplicateSubmissionCount`,
    ),
    lostEvidenceCount: nonNegativeInteger(
      candidate.lostEvidenceCount,
      `${field}.lostEvidenceCount`,
    ),
    verifiedBeforeReconciliationCount: nonNegativeInteger(
      candidate.verifiedBeforeReconciliationCount,
      `${field}.verifiedBeforeReconciliationCount`,
    ),
    unrecoverableWorkflowCount: nonNegativeInteger(
      candidate.unrecoverableWorkflowCount,
      `${field}.unrecoverableWorkflowCount`,
    ),
    manualRepairCount: nonNegativeInteger(
      candidate.manualRepairCount,
      `${field}.manualRepairCount`,
    ),
    terminalState: exactString(
      candidate.terminalState,
      "recovered",
      `${field}.terminalState`,
    ),
    watcherState: exactString(
      candidate.watcherState,
      "ready_after_reconciliation",
      `${field}.watcherState`,
    ),
  };
};
