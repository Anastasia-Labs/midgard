import { readFileSync } from "node:fs";

import {
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  deriveMidgardV1TxFieldChunks,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  reconstructMidgardTransactionV1FromChunks,
  verifyMidgardV1TxFieldChunk,
} from "@al-ft/midgard-core";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  eventKeyFingerprint,
  reconstructDaPayloadV1,
} from "../src/transition-trace/index.js";
import { buildStrictRetainedDaPairFixtureV1 } from "./helpers/cardano-capability-retained-da-v1.js";

type BoundaryCorpusEntryV1 = {
  readonly label: string;
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
  readonly canonicalCborHex: string;
};

const corpus = JSON.parse(
  readFileSync(
    new URL(
      "./fixtures/cardano-capability-p2-boundary-corpus-v1.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as {
  readonly schema: string;
  readonly entries: readonly BoundaryCorpusEntryV1[];
};

const expectedLabels = [
  "balanced-nested-datum",
  "balanced-nested-redeemer",
  "maximum-inline-datum-blob",
  "maximum-mint-and-native-policies",
  "maximum-nested-value",
  "maximum-observers-and-native-scripts",
  "maximum-outputs",
  "maximum-redeemers",
  "maximum-reference-inputs",
  "maximum-signers-and-witnesses",
  "maximum-spend-inputs",
  "mixed-size-balanced",
] as const;

const recomputeCorpusIdentityV1 = (
  canonicalCbor: Uint8Array,
): {
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
} => {
  const exactCanonicalCbor = Buffer.from(canonicalCbor);
  const transaction =
    decodeMidgardNativeTxFullV1FromCanonicalCbor(exactCanonicalCbor);
  const source =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(exactCanonicalCbor);
  return {
    transactionIdHex:
      computeMidgardNativeTxIdV1(transaction).toString("hex"),
    transactionCommitmentHex:
      computeMidgardNativeTxProofCommitmentV1(source).toString("hex"),
  };
};

const reconstructAuthenticatedCanonicalTransactionFromFieldChunksV1 = (
  canonicalCbor: Uint8Array,
): {
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
  readonly revealStepCount: number;
  readonly maximumChunkBytes: number;
  readonly reconstructed: Buffer;
} => {
  const exactCanonicalCbor = Buffer.from(canonicalCbor);
  const transaction =
    decodeMidgardNativeTxFullV1FromCanonicalCbor(exactCanonicalCbor);
  const transactionId = computeMidgardNativeTxIdV1(transaction);
  const source =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(exactCanonicalCbor);
  const transactionCommitment = computeMidgardNativeTxProofCommitmentV1(source);
  const chunks = deriveMidgardV1TxFieldChunks(exactCanonicalCbor);
  for (const chunk of chunks) {
    verifyMidgardV1TxFieldChunk({
      transactionId,
      transactionCommitment,
      source,
      collectionProof: chunk.collectionProof,
      proof: chunk.proof,
    });
  }
  return {
    transactionIdHex: transactionId.toString("hex"),
    transactionCommitmentHex: transactionCommitment.toString("hex"),
    revealStepCount: chunks.length,
    maximumChunkBytes: Math.max(
      ...chunks.map(({ proof }) => proof.chunk.length),
    ),
    reconstructed: reconstructMidgardTransactionV1FromChunks({
      transactionId,
      transactionCommitment,
      source,
      chunkProofs: chunks,
    }),
  };
};

describe("Cardano capability P2 production retained-DA boundary", () => {
  it("strictly authenticates every established maximum before bounded field/item chunk reconstruction", async () => {
    expect(corpus.schema).toBe(
      "midgard-cardano-capability-p2-boundary-corpus-v1",
    );
    expect(corpus.entries.map(({ label }) => label)).toEqual(expectedLabels);

    for (const boundary of corpus.entries) {
      const canonicalCbor = Buffer.from(boundary.canonicalCborHex, "hex");
      expect(recomputeCorpusIdentityV1(canonicalCbor)).toEqual({
        transactionIdHex: boundary.transactionIdHex,
        transactionCommitmentHex: boundary.transactionCommitmentHex,
      });

      const fixture = await buildStrictRetainedDaPairFixtureV1(canonicalCbor);
      expect({
        transactionIdHex: fixture.transactionIdHex,
        transactionCommitmentHex: fixture.transactionCommitmentHex,
      }).toEqual({
        transactionIdHex: boundary.transactionIdHex,
        transactionCommitmentHex: boundary.transactionCommitmentHex,
      });

      const reconstruction = await reconstructDaPayloadV1({
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        expectedHeaderHash: fixture.headerHash,
        committedHeader: fixture.header,
      });
      expect(reconstruction.counts).toEqual({
        withdrawalCount: 0n,
        forcedTransactionCount: 1n,
        l2TransactionCount: 1n,
        depositCount: 0n,
        totalEventCount: 2n,
        transitionStepCount: 2n,
        validationTraceCount: 2n,
      });
      expect(reconstruction.roots).toEqual({
        utxosRoot: fixture.header.utxosRoot,
        withdrawalsRoot: fixture.header.withdrawalsRoot,
        forcedTransactionsRoot: fixture.header.forcedTransactionsRoot,
        transactionsRoot: fixture.header.transactionsRoot,
        depositsRoot: fixture.header.depositsRoot,
        transitionTraceRoot: fixture.header.transitionTraceRoot,
        eventToStepRoot: fixture.header.eventToStepRoot,
        validationTracesRoot: fixture.header.validationTracesRoot,
      });
      expect({
        sourceEventCount: reconstruction.sourceEvents.length,
        sourceEventMapSize: reconstruction.sourceEventsByFingerprint.size,
        transitionTraceCount: reconstruction.transitionTrace.length,
        transitionTraceMapSize: reconstruction.traceByStepIndex.size,
        eventToStepCount: reconstruction.eventToStep.length,
        eventToStepMapSize: reconstruction.eventToStepByFingerprint.size,
        validationTraceCount:
          reconstruction.payload.block_body.validation_traces.length,
        validationTraceRootCount:
          reconstruction.rootData.validationTraces.count,
      }).toEqual({
        sourceEventCount: 2,
        sourceEventMapSize: 2,
        transitionTraceCount: 2,
        transitionTraceMapSize: 2,
        eventToStepCount: 2,
        eventToStepMapSize: 2,
        validationTraceCount: 2,
        validationTraceRootCount: 2n,
      });
      for (const sourceEvent of reconstruction.sourceEvents) {
        const mapped = reconstruction.eventToStepByFingerprint.get(
          sourceEvent.fingerprint,
        );
        expect(mapped).toBeDefined();
        expect(mapped!.value.phase).toBe(sourceEvent.phase);
        const trace = reconstruction.traceByStepIndex.get(
          mapped!.value.step_index,
        );
        expect(trace).toBeDefined();
        expect(trace!.value.phase).toBe(sourceEvent.phase);
        expect(eventKeyFingerprint(trace!.value.event_key)).toBe(
          sourceEvent.fingerprint,
        );
      }

      const normal = reconstruction.transactions[0];
      const forced = reconstruction.forcedTransactions[0];
      expect(normal).toBeDefined();
      expect(forced).toBeDefined();

      const authenticated = [
        normal!.fullTransactionCbor,
        forced!.fullTransactionCbor,
      ];
      for (const sourceCanonicalCbor of authenticated) {
        const folded =
          reconstructAuthenticatedCanonicalTransactionFromFieldChunksV1(
            sourceCanonicalCbor,
          );
        expect({
          transactionIdHex: folded.transactionIdHex,
          transactionCommitmentHex: folded.transactionCommitmentHex,
        }).toEqual({
          transactionIdHex: boundary.transactionIdHex,
          transactionCommitmentHex: boundary.transactionCommitmentHex,
        });
        expect(folded.revealStepCount).toBeGreaterThan(0);
        expect(folded.maximumChunkBytes).toBeLessThanOrEqual(
          MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
        );
        expect(folded.reconstructed).toEqual(canonicalCbor);
      }
    }
  }, 60_000);

  it("keeps roots, counts, and forced-preimage authentication fail closed", async () => {
    const canonicalCbor = Buffer.from(
      corpus.entries.find(({ label }) => label === "maximum-redeemers")!
        .canonicalCborHex,
      "hex",
    );
    const fixture = await buildStrictRetainedDaPairFixtureV1(canonicalCbor);
    const badRootHeader: SDK.HeaderV1 = {
      ...fixture.header,
      transactionsRoot: "ff".repeat(32),
    };
    const badRootHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeaderV1(badRootHeader),
    );
    const badRootPayload: SDK.DaPayloadV1 = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        header_hash: badRootHeaderHash,
        header: badRootHeader,
      },
    };
    await expect(
      reconstructDaPayloadV1({
        payloadEnvelopeCbor: await wrapDaPayloadV1(
          SDK.encodeDaPayloadV1(badRootPayload),
          { mode: "identity" },
        ),
        expectedHeaderHash: badRootHeaderHash,
        committedHeader: badRootHeader,
      }),
    ).rejects.toMatchObject({ code: "rootMismatch" });

    const badTraceCountsPayload: SDK.DaPayloadV1 = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        counts: {
          ...fixture.payload.block_body.counts,
          validationTraceCount: 1n,
        },
      },
    };
    await expect(
      reconstructDaPayloadV1({
        payloadEnvelopeCbor: await wrapDaPayloadV1(
          SDK.encodeDaPayloadV1(badTraceCountsPayload),
          { mode: "identity" },
        ),
      }),
    ).rejects.toMatchObject({ code: "countMismatch" });

    const alternateCanonicalCborHex = corpus.entries.find(
      ({ label }) => label === "mixed-size-balanced",
    )!.canonicalCborHex;
    expect(alternateCanonicalCborHex).not.toBe(canonicalCbor.toString("hex"));
    const tamperedForcedPreimagePayload: SDK.DaPayloadV1 = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        forced_transaction_preimages:
          fixture.payload.block_body.forced_transaction_preimages.map(
            ([key]) => [key, alternateCanonicalCborHex],
          ),
      },
    };
    await expect(
      reconstructDaPayloadV1({
        payloadEnvelopeCbor: await wrapDaPayloadV1(
          SDK.encodeDaPayloadV1(tamperedForcedPreimagePayload),
          { mode: "identity" },
        ),
        expectedHeaderHash: fixture.headerHash,
        committedHeader: fixture.header,
      }),
    ).rejects.toMatchObject({ code: "malformedPayload" });
  });
});
