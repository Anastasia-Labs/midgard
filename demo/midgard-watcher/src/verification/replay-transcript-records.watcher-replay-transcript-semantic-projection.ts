import { type WatcherReplayTranscriptRecords } from "./replay-transcript-records.parse-authority.js";

/** Explicit equality surface: source freshness stays with its own capture. */
export const watcherReplayTranscriptSemanticProjection = (
  records: WatcherReplayTranscriptRecords,
): unknown => {
  const { transcript: t, blockReplay: w25 } = records;
  return {
    schemaVersion: t.schemaVersion,
    deploymentFingerprint: t.deploymentFingerprint,
    headerHash: t.headerHash,
    headerCborHex: records.headerCborHex,
    inclusionPoint: {
      transactionHash: t.inclusionPoint.transactionHash,
      blockHash: t.inclusionPoint.blockHash,
      blockNo: t.inclusionPoint.blockNo,
      slot: t.inclusionPoint.slot,
      chainPointId: t.inclusionPoint.chainPointId,
    },
    coordinate: t.coordinate,
    payloadEnvelopeCborHex: t.payloadEnvelopeCborHex,
    payloadEnvelopeSha256: t.payloadEnvelopeSha256,
    payloadSha256: t.payloadSha256,
    priorState: t.priorState,
    ruleBundleCborHex: t.ruleBundleCborHex,
    ruleBundleCommitment: t.ruleBundleCommitment,
    reconstruction: records.reconstruction,
    phaseA: records.phaseA,
    blockReplay: {
      schemaVersion: w25.schemaVersion,
      action: w25.action,
      reasonCodes: w25.reasonCodes,
      verifiedRequires: w25.verifiedRequires,
      rejectionSelection: w25.rejectionSelection,
      consensusProfileId: w25.consensusProfileId,
      headerHash: w25.headerHash,
      payloadEnvelopeSha256: w25.payloadEnvelopeSha256,
      payloadSha256: w25.payloadSha256,
      reconstructionDigest: w25.reconstructionDigest,
      phaseAResultDigest: w25.phaseAResultDigest,
      ruleBundleCommitment: w25.ruleBundleCommitment,
      sourceManifestDigest: w25.sourceManifestDigest,
      effectManifestDigest: w25.effectManifestDigest,
      priorStateRoot: w25.priorStateRoot,
      expectedPriorStateRoot: w25.expectedPriorStateRoot,
      postStateRoot: w25.postStateRoot,
      expectedPostStateRoot: w25.expectedPostStateRoot,
      transactionCount: w25.transactionCount,
      acceptedCount: w25.acceptedCount,
      acceptedTxIds: w25.acceptedTxIds,
      intermediateRoots: w25.intermediateRoots,
      transactionRoots: w25.transactionRoots,
      eventRoots: w25.eventRoots,
      forcedValidationFacts: w25.forcedValidationFacts,
      stageMismatches: w25.stageMismatches,
      rejections: w25.rejections,
      selectedRejection: w25.selectedRejection,
      downstreamPrerequisite: {
        schemaVersion: w25.downstreamPrerequisite.schemaVersion,
        requiredVerifier: w25.downstreamPrerequisite.requiredVerifier,
        w29Eligibility: w25.downstreamPrerequisite.w29Eligibility,
      },
    },
    events: records.events.map((event) => ({
      phase: event.phase,
      eventKey: event.eventKey,
      event: {
        kind: event.event.kind,
        eventId: event.event.eventId,
        outRef: event.event.outRef,
        transactionHash: event.event.transactionHash,
        outputIndex: event.event.outputIndex,
        nonceOutRef: event.event.nonceOutRef,
        policyId: event.event.policyId,
        spendScriptHash: event.event.spendScriptHash,
        addressHex: event.event.addressHex,
        assetNameHex: event.event.assetNameHex,
        ...(event.event.kind === "forced_order"
          ? { witnessScriptHash: event.event.witnessScriptHash }
          : { historyPayloadCborHex: event.event.historyPayloadCborHex }),
        inclusionTime: event.event.inclusionTime,
        eventCborHex: event.event.eventCborHex,
        datumCborHex: event.event.datumCborHex,
        outputCborHex: event.event.outputCborHex,
        eventContentDigest: event.event.eventContentDigest,
        datumDigest: event.event.datumDigest,
        outputDigest: event.event.outputDigest,
        originBlockHash: event.event.originBlockHash,
        originSlot: event.event.originSlot,
        originBlockNo: event.event.originBlockNo,
        finalityStatus: event.event.finalityStatus,
        // Capture-specific point digests are verified within each record's
        // authority manifest. A fresh local owner regenerates those digests.
        ...("terminalStatus" in event.event
          ? {
              terminalStatus: event.event.terminalStatus,
              terminalTransactionHash: event.event.terminalTransactionHash,
              terminalBlockHash: event.event.terminalBlockHash,
              terminalSlot: event.event.terminalSlot,
              terminalBlockNo: event.event.terminalBlockNo,
              terminalFinalityStatus: event.event.terminalFinalityStatus,
              ...(event.event.terminalClassification === undefined
                ? {}
                : {
                    terminalClassification: {
                      schemaVersion:
                        event.event.terminalClassification.schemaVersion,
                      operatorValidity:
                        event.event.terminalClassification.operatorValidity,
                      terminalTransactionHash:
                        event.event.terminalClassification
                          .terminalTransactionHash,
                    },
                  }),
            }
          : {}),
      },
      network: event.network,
      committedClaim: event.committedClaim,
      canonicalNativeTxCborHex: event.canonicalNativeTxCborHex,
      programMaterialSidecarCborHex: event.programMaterialSidecarCborHex,
      transitionEffect: event.transitionEffect,
      deploymentManifestId: event.origin.deploymentManifestId,
      throughHeader:
        event.origin.throughHeader !== null
          ? {
              headerHash: event.origin.throughHeader.headerHash,
              headerCborHex: event.origin.throughHeader.headerCborHex,
              queueOutRef: event.origin.throughHeader.queueOutRef,
              observedTransactionHash:
                event.origin.throughHeader.observedTransactionHash,
              observedBlockHash: event.origin.throughHeader.observedBlockHash,
              observedSlot: event.origin.throughHeader.observedSlot,
              observedBlockNo: event.origin.throughHeader.observedBlockNo,
              transactionIndex: event.origin.throughHeader.transactionIndex,
            }
          : null,
    })),
  };
};
