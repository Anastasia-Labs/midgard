import { missingNativeScriptTxVersionedScriptHash } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

const authority = vi.hoisted(() => ({
  corpora: new WeakSet<object>(),
  preimages: new WeakSet<object>(),
  evidence: new WeakSet<object>(),
}));

vi.mock(
  "../src/workflow/production-historical-native-script-corpus-v1.js",
  () => ({
    HISTORICAL_NATIVE_SCRIPT_PREIMAGE:
      "midgard-production-historical-native-script-preimage-v1",
    requireHistoricalNativeScriptCorpus: (value: object) => {
      if (!authority.corpora.has(value)) {
        throw new Error("corpus was not admitted");
      }
      return {};
    },
    historicalNativeScriptPreimageFromCorpus: ({
      corpus,
      scriptHash,
    }: {
      corpus: Readonly<Record<string, unknown>>;
      scriptHash: string;
    }) => {
      if (!authority.corpora.has(corpus)) {
        throw new Error("corpus was not admitted");
      }
      const entries = corpus.entries as readonly Readonly<
        Record<string, unknown>
      >[];
      const entry = entries.find(
        (candidate) => candidate.scriptHash === scriptHash,
      );
      if (entry === undefined) return null;
      const preimage = Object.freeze({
        schemaVersion:
          "midgard-production-historical-native-script-corpus-preimage-v1",
        throughHeaderHash: corpus.throughHeaderHash,
        scriptHash,
        scriptBytesHex: entry.scriptBytesHex,
        occurrences: entry.occurrences,
        providerRosterDigest: corpus.providerRosterDigest,
        corpusDigest: corpus.corpusDigest,
        checkpointDigest: corpus.checkpointDigest,
        preimageDigest: "99".repeat(32),
      });
      authority.preimages.add(preimage);
      return preimage;
    },
    requireHistoricalNativeScriptCorpusPreimage: (value: object) => {
      if (!authority.preimages.has(value)) {
        throw new Error("corpus preimage was not admitted");
      }
      return value;
    },
  }),
);

vi.mock("../src/missing-native-script-tx/historical-script-v1.js", () => ({
  historicalNativeScriptBytes: (
    value: Readonly<{ scriptBytesHex: string }>,
  ) => {
    if (!authority.evidence.has(value)) {
      throw new Error("L1 evidence was not admitted");
    }
    return Buffer.from(value.scriptBytesHex, "hex");
  },
  admitHistoricalNativeScriptEvidence: ({ value }: { value: unknown }) => {
    if (
      typeof value !== "object" ||
      value === null ||
      Array.isArray(value) ||
      typeof (value as Readonly<Record<string, unknown>>).evidenceDigest !==
        "string"
    ) {
      throw new Error("persisted L1 evidence is invalid");
    }
    const admitted = Object.freeze({
      ...(value as Readonly<Record<string, unknown>>),
    });
    authority.evidence.add(admitted);
    return admitted;
  },
}));

import {
  admitHistoricalNativeScriptPreimage,
  prepareHistoricalNativeScriptPreimage,
} from "../src/missing-native-script-tx/production-historical-preimage-v1.js";
import { computeFraudProofRawL1PointId } from "../src/workflow/raw-l1-snapshot-v1.js";

const throughPoint = (() => {
  const point = {
    slot: "100",
    blockNo: "10",
    blockHash: "88".repeat(32),
  };
  return Object.freeze({
    ...point,
    pointId: computeFraudProofRawL1PointId(point),
  });
})();

const fixture = () => {
  const native = CML.NativeScript.new_script_all(CML.NativeScriptList.new());
  const scriptBytesHex = native.to_canonical_cbor_hex();
  const expectedScriptHash = missingNativeScriptTxVersionedScriptHash(
    Buffer.from(scriptBytesHex, "hex"),
  );
  const occurrence = Object.freeze({
    headerHash: "11".repeat(28),
    txId: "22".repeat(32),
    source: "transaction_witness" as const,
    itemIndex: 0,
  });
  const corpus = Object.freeze({
    schemaVersion: "midgard-production-historical-native-script-corpus-v1",
    throughHeaderHash: "33".repeat(28),
    headerHashes: ["33".repeat(28)],
    payloadEnvelopeSha256s: ["44".repeat(32)],
    entries: [
      Object.freeze({
        scriptHash: expectedScriptHash,
        scriptBytesHex,
        occurrences: Object.freeze([occurrence]),
      }),
    ],
    providerRosterDigest: "54".repeat(32),
    corpusDigest: "55".repeat(32),
    checkpointDigest: "66".repeat(32),
  });
  const corroboration = Object.freeze({
    expectedScriptHash,
    scriptBytesHex,
    applicationOverlayDigest: corpus.providerRosterDigest,
    evidenceDigest: "77".repeat(32),
  });
  authority.corpora.add(corpus);
  authority.evidence.add(corroboration);
  return { corpus, corroboration, expectedScriptHash, scriptBytesHex };
};

describe("production historical native-script preimage V1", () => {
  it("persists and re-admits the exact corpus occurrence plus full L1 evidence", async () => {
    const { corpus, corroboration, expectedScriptHash, scriptBytesHex } =
      fixture();
    const artifact = prepareHistoricalNativeScriptPreimage({
      corpus: corpus as never,
      expectedHeaderHash: corpus.throughHeaderHash,
      expectedScriptHash,
      corroboration: corroboration as never,
    });
    expect(artifact).toMatchObject({
      throughHeaderHash: corpus.throughHeaderHash,
      scriptHash: expectedScriptHash,
      scriptBytesHex,
      corpusDigest: corpus.corpusDigest,
      checkpointDigest: corpus.checkpointDigest,
      preimageDigest: "99".repeat(32),
      historicalL1Corroboration: corroboration,
    });
    const persisted: unknown = JSON.parse(JSON.stringify(artifact));
    const admitted = await admitHistoricalNativeScriptPreimage({
      value: persisted,
      corpus: corpus as never,
      expectedHeaderHash: corpus.throughHeaderHash,
      expectedScriptHash,
      roster: {} as never,
      throughPoint,
      releaseFinality: {} as never,
    });
    expect(Buffer.from(admitted.scriptBytes).toString("hex")).toBe(
      scriptBytesHex,
    );
    expect(admitted.artifact.artifactDigest).toBe(artifact.artifactDigest);
  });

  it("rejects structural authority clones and persisted substitutions", async () => {
    const { corpus, corroboration, expectedScriptHash } = fixture();
    expect(() =>
      prepareHistoricalNativeScriptPreimage({
        corpus: { ...corpus } as never,
        expectedHeaderHash: corpus.throughHeaderHash,
        expectedScriptHash,
        corroboration: corroboration as never,
      }),
    ).toThrow("corpus was not admitted");
    expect(() =>
      prepareHistoricalNativeScriptPreimage({
        corpus: corpus as never,
        expectedHeaderHash: corpus.throughHeaderHash,
        expectedScriptHash,
        corroboration: { ...corroboration } as never,
      }),
    ).toThrow("L1 evidence was not admitted");

    const artifact = prepareHistoricalNativeScriptPreimage({
      corpus: corpus as never,
      expectedHeaderHash: corpus.throughHeaderHash,
      expectedScriptHash,
      corroboration: corroboration as never,
    });
    await expect(
      admitHistoricalNativeScriptPreimage({
        value: { ...artifact, corpusDigest: "ff".repeat(32) },
        corpus: corpus as never,
        expectedHeaderHash: corpus.throughHeaderHash,
        expectedScriptHash,
        roster: {} as never,
        throughPoint,
        releaseFinality: {} as never,
      }),
    ).rejects.toThrow("changed its corpus/L1 authority binding");
  });
});
