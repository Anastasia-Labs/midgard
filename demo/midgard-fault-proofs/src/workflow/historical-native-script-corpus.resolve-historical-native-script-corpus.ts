import { createHash } from "node:crypto";

import {
  assertSecurityGradeEvidence,
  GENESIS_HEADER_HASH,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  reconstructDaPayload,
  type TransitionTraceReconstruction,
} from "../transition-trace/reconstruct.js";
import {
  buildCorpusEntries,
  entriesThroughHeaders,
  fetchHistoricalPayload,
  historicalCorpusEvidenceDigest,
  mergeCorpusEntries,
} from "./historical-native-script-corpus.build-corpus-entries.js";
import {
  HISTORICAL_NATIVE_SCRIPT_CHECKPOINT,
  HISTORICAL_NATIVE_SCRIPT_CORPUS,
  HISTORICAL_NATIVE_SCRIPT_CORPUS_PREIMAGE,
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptCorpus,
  type HistoricalNativeScriptHistorySource,
  type HistoricalNativeScriptOccurrence,
} from "./historical-native-script-corpus.create-historical-native-script-provider-roster.js";
import {
  admittedCorpusInternals,
  type AdmittedHistoricalNativeScriptCorpus,
  requireHistoricalNativeScriptHistoryAuthority,
} from "./historical-native-script-corpus.create-sqlite-historical-native-script-checkpoint-store.js";
import {
  requireCheckpoint,
  sha256,
} from "./historical-native-script-corpus.require-checkpoint.js";

/**
 * Extends a deployment-bound complete checkpoint with the exact contiguous
 * retained-DA segment that is still available. Bootstrap may walk to genesis,
 * but ordinary derivation never depends on permanent retained DA.
 */
export const resolveHistoricalNativeScriptCorpus = async ({
  deploymentFingerprint,
  checkpointStore,
  historySource,
  currentEvidence,
  sources,
  retries,
}: {
  readonly deploymentFingerprint: string;
  readonly checkpointStore: HistoricalNativeScriptCheckpointStore;
  readonly historySource: HistoricalNativeScriptHistorySource;
  readonly currentEvidence: CanonicalBlockEvidence;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly retries?: number;
}): Promise<HistoricalNativeScriptCorpus> => {
  assertSecurityGradeEvidence(currentEvidence.provenance.l1);
  assertSecurityGradeEvidence(currentEvidence.provenance.da);
  requireHistoricalNativeScriptHistoryAuthority({
    deploymentFingerprint,
    checkpointStore,
    historySource,
  });
  const checkpoint = await requireCheckpoint({
    value: await checkpointStore.load({ deploymentFingerprint }),
    deploymentFingerprint,
  });
  if (
    checkpoint !== null &&
    checkpoint.providerRosterDigest !== historySource.providerRosterDigest
  ) {
    throw new Error(
      "historical native-script checkpoint belongs to a different provider roster",
    );
  }
  const currentPayloadEnvelopeCbor = await fetchHistoricalPayload({
    headerHash: currentEvidence.headerHash,
    sources,
    historySource,
    ...(retries === undefined ? {} : { retries }),
  });
  if (
    sha256(currentPayloadEnvelopeCbor) !== currentEvidence.payloadEnvelopeSha256
  ) {
    throw new Error("historical current retained-DA payload changed");
  }
  const storedCurrentIndex =
    checkpoint?.headerHashes.indexOf(currentEvidence.headerHash) ?? -1;
  if (storedCurrentIndex >= 0) {
    if (
      checkpoint!.payloadEnvelopeSha256s[storedCurrentIndex] !==
      currentEvidence.payloadEnvelopeSha256
    ) {
      throw new Error("historical checkpoint disagrees on challenged payload");
    }
    const headers = Object.freeze(
      checkpoint!.headerHashes.slice(0, storedCurrentIndex + 1),
    );
    const payloadEnvelopeSha256s = Object.freeze(
      checkpoint!.payloadEnvelopeSha256s.slice(0, storedCurrentIndex + 1),
    );
    const entries = entriesThroughHeaders(
      checkpoint!.entries,
      new Set(headers),
    );
    const newestFirst = [currentEvidence.reconstruction];
    if (currentEvidence.header.prevHeaderHash !== GENESIS_HEADER_HASH) {
      const payloadEnvelopeCbor = await fetchHistoricalPayload({
        headerHash: currentEvidence.header.prevHeaderHash,
        sources,
        historySource,
        ...(retries === undefined ? {} : { retries }),
      });
      const predecessor = await reconstructDaPayload({
        payloadEnvelopeCbor,
        expectedHeaderHash: currentEvidence.header.prevHeaderHash,
      });
      if (
        predecessor.header.utxosRoot !== currentEvidence.header.prevUtxosRoot
      ) {
        throw new Error("historical challenged predecessor root changed");
      }
      newestFirst.push(predecessor);
    }
    const reconstructions = Object.freeze([...newestFirst].reverse());
    const withoutDigest = {
      schemaVersion: HISTORICAL_NATIVE_SCRIPT_CORPUS,
      throughHeaderHash: currentEvidence.headerHash,
      headerHashes: headers,
      payloadEnvelopeSha256s,
      entries,
      providerRosterDigest: historySource.providerRosterDigest,
      checkpointDigest: checkpoint!.checkpointDigest,
    } as const;
    const corpus = Object.freeze({
      ...withoutDigest,
      evidenceDigest: historicalCorpusEvidenceDigest(
        deploymentFingerprint,
        withoutDigest,
      ),
      corpusDigest: createHash("sha256")
        .update(JSON.stringify(withoutDigest))
        .digest("hex"),
    });
    admittedCorpusInternals.set(corpus, { currentEvidence, reconstructions });
    return corpus;
  }
  const newestFirst: TransitionTraceReconstruction[] = [
    currentEvidence.reconstruction,
  ];
  const envelopeShaByHeader = new Map<string, string>([
    [currentEvidence.headerHash, currentEvidence.payloadEnvelopeSha256],
  ]);
  const seen = new Set<string>([currentEvidence.headerHash]);
  const checkpointIndices = new Map(
    (checkpoint?.headerHashes ?? []).map((headerHash, index) => [
      headerHash,
      index,
    ]),
  );
  let expectedHeaderHash = currentEvidence.header.prevHeaderHash;
  // A corrected queue can replace the cached suffix. Join the exact common
  // ancestor named by the new hash/root chain, retaining only its prefix facts.
  while (
    expectedHeaderHash !== GENESIS_HEADER_HASH &&
    !checkpointIndices.has(expectedHeaderHash)
  ) {
    if (seen.has(expectedHeaderHash)) {
      throw new Error("historical retained-DA header chain contains a cycle");
    }
    seen.add(expectedHeaderHash);
    const payloadEnvelopeCbor = await fetchHistoricalPayload({
      headerHash: expectedHeaderHash,
      sources,
      historySource,
      ...(retries === undefined ? {} : { retries }),
    });
    const reconstruction = await reconstructDaPayload({
      payloadEnvelopeCbor,
      expectedHeaderHash,
    });
    const child = newestFirst[newestFirst.length - 1]!;
    if (
      child.header.prevHeaderHash !== reconstruction.headerHash ||
      child.header.prevUtxosRoot !== reconstruction.header.utxosRoot
    ) {
      throw new Error(
        "historical retained-DA predecessor does not match the child's committed hash/root",
      );
    }
    newestFirst.push(reconstruction);
    envelopeShaByHeader.set(
      reconstruction.headerHash,
      sha256(payloadEnvelopeCbor),
    );
    expectedHeaderHash = reconstruction.header.prevHeaderHash;
  }
  const prefixLength =
    expectedHeaderHash === GENESIS_HEADER_HASH
      ? 0
      : checkpointIndices.get(expectedHeaderHash)! + 1;
  const prefixHeaders = checkpoint?.headerHashes.slice(0, prefixLength) ?? [];
  if (prefixLength > 0) {
    const checkpointPayload =
      expectedHeaderHash === checkpoint!.throughHeaderHash
        ? Buffer.from(checkpoint!.throughPayloadEnvelopeCborHex, "hex")
        : await fetchHistoricalPayload({
            headerHash: expectedHeaderHash,
            sources,
            historySource,
            ...(retries === undefined ? {} : { retries }),
          });
    if (
      sha256(checkpointPayload) !==
      checkpoint!.payloadEnvelopeSha256s[prefixLength - 1]
    )
      throw new Error("historical checkpoint ancestor payload changed");
    const checkpointReconstruction = await reconstructDaPayload({
      payloadEnvelopeCbor: checkpointPayload,
      expectedHeaderHash,
    });
    const child = newestFirst[newestFirst.length - 1]!;
    if (
      child.header.prevHeaderHash !== checkpointReconstruction.headerHash ||
      child.header.prevUtxosRoot !== checkpointReconstruction.header.utxosRoot
    ) {
      throw new Error(
        "historical checkpoint does not join the retained segment",
      );
    }
    newestFirst.push(checkpointReconstruction);
    envelopeShaByHeader.set(
      expectedHeaderHash,
      checkpoint!.payloadEnvelopeSha256s[prefixLength - 1]!,
    );
  }
  const reconstructions = Object.freeze([...newestFirst].reverse());
  const appended = reconstructions.filter(
    (reconstruction) => reconstruction.headerHash !== expectedHeaderHash,
  );
  const headerHashes = Object.freeze([
    ...prefixHeaders,
    ...appended.map((reconstruction) => reconstruction.headerHash),
  ]);
  const payloadEnvelopeSha256s = Object.freeze([
    ...(checkpoint?.payloadEnvelopeSha256s.slice(0, prefixLength) ?? []),
    ...appended.map(
      (reconstruction) => envelopeShaByHeader.get(reconstruction.headerHash)!,
    ),
  ]);
  const entries = mergeCorpusEntries(
    entriesThroughHeaders(checkpoint?.entries ?? [], new Set(prefixHeaders)),
    buildCorpusEntries(appended),
  );
  const nextWithoutDigest = {
    schemaVersion: HISTORICAL_NATIVE_SCRIPT_CHECKPOINT,
    deploymentFingerprint,
    throughHeaderHash: currentEvidence.headerHash,
    throughUtxosRoot: currentEvidence.header.utxosRoot,
    throughPayloadEnvelopeCborHex: currentPayloadEnvelopeCbor.toString("hex"),
    throughPayloadEnvelopeSha256: currentEvidence.payloadEnvelopeSha256,
    headerHashes,
    payloadEnvelopeSha256s,
    entries,
    providerRosterDigest: historySource.providerRosterDigest,
    predecessorCheckpointDigest: checkpoint?.checkpointDigest ?? null,
  } as const;
  const next = Object.freeze({
    ...nextWithoutDigest,
    checkpointDigest: createHash("sha256")
      .update(JSON.stringify(nextWithoutDigest))
      .digest("hex"),
  });
  if (
    (await checkpointStore.compareAndSwap({
      deploymentFingerprint,
      expectedCheckpointDigest: checkpoint?.checkpointDigest ?? null,
      next,
    })) !== "stored"
  ) {
    throw new Error(
      "historical native-script checkpoint advanced concurrently; refetch required",
    );
  }
  const withoutDigest = {
    schemaVersion: HISTORICAL_NATIVE_SCRIPT_CORPUS,
    throughHeaderHash: currentEvidence.headerHash,
    headerHashes,
    payloadEnvelopeSha256s,
    entries,
    providerRosterDigest: historySource.providerRosterDigest,
    checkpointDigest: next.checkpointDigest,
  } as const;
  const corpus = Object.freeze({
    ...withoutDigest,
    evidenceDigest: historicalCorpusEvidenceDigest(
      deploymentFingerprint,
      withoutDigest,
    ),
    corpusDigest: createHash("sha256")
      .update(JSON.stringify(withoutDigest))
      .digest("hex"),
  });
  admittedCorpusInternals.set(corpus, {
    currentEvidence,
    reconstructions,
  });
  return corpus;
};

export const requireHistoricalNativeScriptCorpus = (
  corpus: HistoricalNativeScriptCorpus,
): AdmittedHistoricalNativeScriptCorpus => {
  const internals = admittedCorpusInternals.get(corpus);
  if (internals === undefined) {
    throw new Error(
      "historical native-script corpus was not derived from authenticated complete history",
    );
  }
  return internals;
};

export type HistoricalNativeScriptCorpusPreimage = Readonly<{
  schemaVersion: typeof HISTORICAL_NATIVE_SCRIPT_CORPUS_PREIMAGE;
  throughHeaderHash: string;
  scriptHash: string;
  scriptBytesHex: string;
  occurrences: readonly HistoricalNativeScriptOccurrence[];
  providerRosterDigest: string;
  corpusDigest: string;
  checkpointDigest: string;
  preimageDigest: string;
}>;

export const admittedCorpusPreimages = new WeakSet<object>();
