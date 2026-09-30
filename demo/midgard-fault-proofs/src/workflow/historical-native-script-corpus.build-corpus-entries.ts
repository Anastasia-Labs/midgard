import { createHash } from "node:crypto";

import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScriptListPreimage,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { assertSecurityGradeEvidence } from "@al-ft/midgard-sdk";

import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { type TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import {
  type HistoricalNativeScriptCorpus,
  type HistoricalNativeScriptCorpusEntry,
  type HistoricalNativeScriptHistorySource,
  type HistoricalNativeScriptOccurrence,
} from "./historical-native-script-corpus.create-historical-native-script-provider-roster.js";
import { occurrenceOrder } from "./historical-native-script-corpus.create-sqlite-historical-native-script-checkpoint-store.js";

export const buildCorpusEntries = (
  reconstructions: readonly TransitionTraceReconstruction[],
): readonly HistoricalNativeScriptCorpusEntry[] => {
  const scripts = new Map<
    string,
    {
      scriptBytesHex: string;
      occurrences: HistoricalNativeScriptOccurrence[];
    }
  >();
  for (const reconstruction of reconstructions) {
    for (const transaction of reconstruction.transactions) {
      const native = decodeMidgardNativeTxFullFromCanonicalCbor(
        transaction.fullTransactionCbor,
      );
      const record = (
        script: ReturnType<
          typeof decodeMidgardVersionedScriptListPreimage
        >[number],
        occurrence: HistoricalNativeScriptOccurrence,
      ) => {
        if (script.language !== "NativeCardano") return;
        const scriptHash = hashMidgardVersionedScript(script);
        const scriptBytesHex = Buffer.from(script.scriptBytes).toString("hex");
        const existing = scripts.get(scriptHash);
        if (
          existing !== undefined &&
          existing.scriptBytesHex !== scriptBytesHex
        ) {
          throw new Error(
            `historical native-script corpus found conflicting preimages for ${scriptHash}`,
          );
        }
        if (existing === undefined) {
          scripts.set(scriptHash, {
            scriptBytesHex,
            occurrences: [occurrence],
          });
        } else {
          existing.occurrences.push(occurrence);
        }
      };
      decodeMidgardVersionedScriptListPreimage(
        native.witnessSet.scriptTxWitsPreimageCbor,
        `historical transaction ${transaction.txId} script witnesses`,
      ).forEach((script, itemIndex) =>
        record(script, {
          headerHash: reconstruction.headerHash,
          txId: transaction.txId,
          source: "transaction_witness",
          itemIndex,
        }),
      );
      const outputItems = decodeMidgardFieldPreimage(
        native.body.outputsPreimageCbor,
      );
      outputItems.forEach((outputBytes, itemIndex) => {
        const script = decodeMidgardTxOutput(outputBytes).script_ref;
        if (script !== undefined) {
          record(script, {
            headerHash: reconstruction.headerHash,
            txId: transaction.txId,
            source: "reference_script",
            itemIndex,
          });
        }
      });
    }
  }
  return Object.freeze(
    [...scripts.entries()]
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([scriptHash, entry]) =>
        Object.freeze({
          scriptHash,
          scriptBytesHex: entry.scriptBytesHex,
          occurrences: Object.freeze(
            [...entry.occurrences]
              .sort(occurrenceOrder)
              .map((occurrence) => Object.freeze({ ...occurrence })),
          ),
        }),
      ),
  );
};

export const mergeCorpusEntries = (
  earlier: readonly HistoricalNativeScriptCorpusEntry[],
  later: readonly HistoricalNativeScriptCorpusEntry[],
): readonly HistoricalNativeScriptCorpusEntry[] => {
  const merged = new Map<
    string,
    {
      scriptBytesHex: string;
      occurrences: HistoricalNativeScriptOccurrence[];
    }
  >();
  for (const entry of [...earlier, ...later]) {
    const current = merged.get(entry.scriptHash);
    if (
      current !== undefined &&
      current.scriptBytesHex !== entry.scriptBytesHex
    ) {
      throw new Error(
        `historical native-script checkpoint conflicts at ${entry.scriptHash}`,
      );
    }
    const target = current ?? {
      scriptBytesHex: entry.scriptBytesHex,
      occurrences: [],
    };
    if (current === undefined) merged.set(entry.scriptHash, target);
    for (const occurrence of entry.occurrences) {
      const identity = JSON.stringify(occurrence);
      if (
        !target.occurrences.some(
          (candidate) => JSON.stringify(candidate) === identity,
        )
      ) {
        target.occurrences.push(occurrence);
      }
    }
  }
  return Object.freeze(
    [...merged.entries()]
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([scriptHash, entry]) =>
        Object.freeze({
          scriptHash,
          scriptBytesHex: entry.scriptBytesHex,
          occurrences: Object.freeze(
            [...entry.occurrences]
              .sort(occurrenceOrder)
              .map((occurrence) => Object.freeze({ ...occurrence })),
          ),
        }),
      ),
  );
};

export const entriesThroughHeaders = (
  entries: readonly HistoricalNativeScriptCorpusEntry[],
  headers: ReadonlySet<string>,
): readonly HistoricalNativeScriptCorpusEntry[] =>
  Object.freeze(
    entries.flatMap((entry) => {
      const occurrences = entry.occurrences.filter((occurrence) =>
        headers.has(occurrence.headerHash),
      );
      return occurrences.length === 0
        ? []
        : [
            Object.freeze({
              ...entry,
              occurrences: Object.freeze(occurrences),
            }),
          ];
    }),
  );

export const historicalCorpusEvidenceDigest = (
  deploymentFingerprint: string,
  corpus: Pick<
    HistoricalNativeScriptCorpus,
    | "throughHeaderHash"
    | "headerHashes"
    | "payloadEnvelopeSha256s"
    | "entries"
    | "providerRosterDigest"
  >,
): string =>
  createHash("sha256")
    .update(
      JSON.stringify({
        schemaVersion: "midgard-historical-native-script-evidence-v1",
        deploymentFingerprint,
        throughHeaderHash: corpus.throughHeaderHash,
        headerHashes: corpus.headerHashes,
        payloadEnvelopeSha256s: corpus.payloadEnvelopeSha256s,
        entries: corpus.entries,
        providerRosterDigest: corpus.providerRosterDigest,
      }),
    )
    .digest("hex");

export const fetchHistoricalPayload = async ({
  headerHash,
  sources,
  historySource,
  retries,
}: {
  readonly headerHash: string;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly historySource: HistoricalNativeScriptHistorySource;
  readonly retries?: number;
}): Promise<Buffer> => {
  void retries;
  if (sources.length > 0) {
    const results = await Promise.all(
      sources.map(
        async (source) => await source.fetchPayloadByHeaderHash(headerHash),
      ),
    );
    const successes = results.filter(
      (result): result is Extract<typeof result, { readonly ok: true }> =>
        result.ok,
    );
    if (successes.length > 0) {
      successes.forEach((success) =>
        assertSecurityGradeEvidence(success.provenance),
      );
      const first = successes[0]!.payloadEnvelopeCbor;
      if (
        successes.some((success) => !success.payloadEnvelopeCbor.equals(first))
      ) {
        throw new Error(
          "public retained-DA sources disagree on historical bytes",
        );
      }
      return first;
    }
    const attempts = results.flatMap((result) => result.attempts);
    if (
      attempts.length === 0 ||
      attempts.some((attempt) => attempt.status !== "not_found")
    ) {
      throw new Error(
        `public retained-DA history failed without authenticated retention absence for header ${headerHash}; sources: ${JSON.stringify(results.map((result) => result.sourceId))}; attempts: ${JSON.stringify(attempts)}`,
      );
    }
  }
  const archived = await historySource.fetchPayloadByHeaderHash({ headerHash });
  return archived.payloadEnvelopeCbor;
};
