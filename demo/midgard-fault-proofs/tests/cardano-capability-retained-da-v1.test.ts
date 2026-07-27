import { readFileSync } from "node:fs";

import {
  collectMidgardV1AttachedProgramEnvelopes,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardCekProgramMaterialDaEntryV1,
  decodeMidgardCekProgramMaterialSidecarV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScriptListPreimage,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  deriveMidgardV1TxFieldChunks,
  encodeMidgardCekProgramMaterialDaValueV1,
  encodeMidgardTxOutput,
  hashMidgardVersionedScript,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  reconstructMidgardTransactionV1FromChunks,
  verifyMidgardCekProgramMaterialBundleV1,
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
  readonly canonicalMaterialSidecarCborHex?: string;
  readonly sourceRawScriptAuditHash?: string;
  readonly productionAdmission:
    | "required"
    | "diagnostic-synthetic-script-witnesses";
  readonly resolvedReferenceUtxos?: readonly SDK.DaPayloadEntry[];
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
  "maximum-constructor-datum-breadth",
  "maximum-constructor-redeemer-breadth",
  "maximum-inline-datum-blob",
  "maximum-list-datum-breadth",
  "maximum-list-redeemer-breadth",
  "maximum-map-datum-breadth",
  "maximum-map-redeemer-breadth",
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
    transactionIdHex: computeMidgardNativeTxIdV1(transaction).toString("hex"),
    transactionCommitmentHex:
      computeMidgardNativeTxProofCommitmentV1(source).toString("hex"),
  };
};

const verifyFixtureProgramMaterialV1 = ({
  canonicalCbor,
  payload,
}: {
  readonly canonicalCbor: Uint8Array;
  readonly payload: SDK.DaPayloadV1;
}) => {
  const transaction =
    decodeMidgardNativeTxFullV1FromCanonicalCbor(canonicalCbor);
  const envelopes = collectMidgardV1AttachedProgramEnvelopes(transaction);
  const material = payload.block_body.cek_program_material.map(
    ([rootHex, valueHex]) =>
      decodeMidgardCekProgramMaterialDaEntryV1(
        Buffer.from(rootHex, "hex"),
        Buffer.from(valueHex, "hex"),
      ),
  );
  return verifyMidgardCekProgramMaterialBundleV1(envelopes, material);
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

      const materialSidecar =
        boundary.canonicalMaterialSidecarCborHex === undefined
          ? undefined
          : Buffer.from(boundary.canonicalMaterialSidecarCborHex, "hex");
      const fixture = await buildStrictRetainedDaPairFixtureV1({
        canonicalTransactionCbor: canonicalCbor,
        canonicalMaterialSidecarCbor: materialSidecar,
        resolvedReferenceUtxos: boundary.resolvedReferenceUtxos,
      });
      expect({
        transactionIdHex: fixture.transactionIdHex,
        transactionCommitmentHex: fixture.transactionCommitmentHex,
      }).toEqual({
        transactionIdHex: boundary.transactionIdHex,
        transactionCommitmentHex: boundary.transactionCommitmentHex,
      });
      const transaction =
        decodeMidgardNativeTxFullV1FromCanonicalCbor(canonicalCbor);
      expect(fixture.payload.block_body.utxos).toEqual(
        boundary.resolvedReferenceUtxos ?? [],
      );
      const diagnosticSyntheticScripts =
        boundary.productionAdmission ===
        "diagnostic-synthetic-script-witnesses";
      expect(diagnosticSyntheticScripts).toBe(
        boundary.label === "mixed-size-balanced",
      );
      if (diagnosticSyntheticScripts) {
        expect(materialSidecar).toBeUndefined();
        expect(boundary.sourceRawScriptAuditHash).toBeUndefined();
        expect(fixture.payload.block_body.cek_program_material).toEqual([]);
      } else if (materialSidecar === undefined) {
        const attachedPrograms =
          collectMidgardV1AttachedProgramEnvelopes(transaction);
        expect(boundary.sourceRawScriptAuditHash).toBeUndefined();
        expect(attachedPrograms).toHaveLength(0);
        expect(fixture.payload.block_body.cek_program_material).toEqual([]);
      } else {
        const attachedPrograms =
          collectMidgardV1AttachedProgramEnvelopes(transaction);
        const sidecarEntries =
          decodeMidgardCekProgramMaterialSidecarV1(materialSidecar);
        expect(sidecarEntries.length).toBeGreaterThan(0);
        expect(fixture.payload.block_body.cek_program_material).toEqual(
          sidecarEntries
            .map(
              (entry): SDK.DaPayloadEntry => [
                Buffer.from(entry.root).toString("hex"),
                encodeMidgardCekProgramMaterialDaValueV1(entry).toString("hex"),
              ],
            )
            .sort(([left], [right]) => left.localeCompare(right)),
        );
        expect(attachedPrograms).toHaveLength(1);
        expect(
          verifyFixtureProgramMaterialV1({
            canonicalCbor,
            payload: fixture.payload,
          }),
        ).toHaveLength(1);
        const scripts = decodeMidgardVersionedScriptListPreimage(
          transaction.witnessSet.scriptTxWitsPreimageCbor,
        );
        expect(scripts).toHaveLength(1);
        expect(boundary.sourceRawScriptAuditHash).toMatch(/^[0-9a-f]{56}$/u);
        expect(boundary.sourceRawScriptAuditHash).not.toBe(
          hashMidgardVersionedScript(scripts[0]!),
        );
      }

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
  }, 120_000);

  it("keeps material, roots, counts, and forced-preimage authentication fail closed", async () => {
    const canonicalCbor = Buffer.from(
      corpus.entries.find(
        ({ label }) => label === "maximum-list-redeemer-breadth",
      )!.canonicalCborHex,
      "hex",
    );
    const materialSidecar = Buffer.from(
      corpus.entries.find(
        ({ label }) => label === "maximum-list-redeemer-breadth",
      )!.canonicalMaterialSidecarCborHex!,
      "hex",
    );
    const fixture = await buildStrictRetainedDaPairFixtureV1({
      canonicalTransactionCbor: canonicalCbor,
      canonicalMaterialSidecarCbor: materialSidecar,
    });
    const missingMaterialPayload: SDK.DaPayloadV1 = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        cek_program_material: [],
      },
    };
    expect(() =>
      verifyFixtureProgramMaterialV1({
        canonicalCbor,
        payload: missingMaterialPayload,
      }),
    ).toThrow();

    const [materialRootHex, materialValueHex] =
      fixture.payload.block_body.cek_program_material[0]!;
    const [materialEntry] =
      decodeMidgardCekProgramMaterialSidecarV1(materialSidecar);
    const mutatedPreimage = Buffer.from(materialEntry!.preimage);
    mutatedPreimage[mutatedPreimage.length - 1] ^= 0x01;
    const mutatedMaterialPayload: SDK.DaPayloadV1 = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        cek_program_material: [
          [
            materialRootHex,
            encodeMidgardCekProgramMaterialDaValueV1({
              kind: materialEntry!.kind,
              preimage: mutatedPreimage,
            }).toString("hex"),
          ],
          ...fixture.payload.block_body.cek_program_material.slice(1),
        ],
      },
    };
    expect(() =>
      verifyFixtureProgramMaterialV1({
        canonicalCbor,
        payload: mutatedMaterialPayload,
      }),
    ).toThrow();

    const wrongMaterialRootPayload: SDK.DaPayloadV1 = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        cek_program_material: [
          ["ff".repeat(32), materialValueHex] satisfies SDK.DaPayloadEntry,
          ...fixture.payload.block_body.cek_program_material.slice(1),
        ].sort(([left], [right]) => left.localeCompare(right)),
      },
    };
    expect(() =>
      verifyFixtureProgramMaterialV1({
        canonicalCbor,
        payload: wrongMaterialRootPayload,
      }),
    ).toThrow();

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

  it("keeps resolved reference UTxO coverage and descriptor roots fail closed", async () => {
    const boundary = corpus.entries.find(
      ({ label }) => label === "maximum-reference-inputs",
    )!;
    const canonicalCbor = Buffer.from(boundary.canonicalCborHex, "hex");
    const fixture = await buildStrictRetainedDaPairFixtureV1({
      canonicalTransactionCbor: canonicalCbor,
      resolvedReferenceUtxos: boundary.resolvedReferenceUtxos,
    });
    expect(fixture.payload.block_body.utxos.length).toBeGreaterThan(0);

    const missing: SDK.DaPayloadV1 = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        utxos: [],
      },
    };
    await expect(
      reconstructDaPayloadV1({
        payloadEnvelopeCbor: await wrapDaPayloadV1(
          SDK.encodeDaPayloadV1(missing),
          { mode: "identity" },
        ),
        expectedHeaderHash: fixture.headerHash,
        committedHeader: fixture.header,
      }),
    ).rejects.toMatchObject({ code: "rootMismatch" });

    const [first, ...remaining] = fixture.payload.block_body.utxos;
    const decodedOutput = decodeMidgardTxOutput(Buffer.from(first![1], "hex"));
    const substitutedOutput = encodeMidgardTxOutput({
      ...decodedOutput,
      value: {
        ...decodedOutput.value,
        lovelace: decodedOutput.value.lovelace + 1n,
      },
    });
    const substituted: SDK.DaPayloadV1 = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        utxos: [[first![0], substitutedOutput.toString("hex")], ...remaining],
      },
    };
    await expect(
      reconstructDaPayloadV1({
        payloadEnvelopeCbor: await wrapDaPayloadV1(
          SDK.encodeDaPayloadV1(substituted),
          { mode: "identity" },
        ),
        expectedHeaderHash: fixture.headerHash,
        committedHeader: fixture.header,
      }),
    ).rejects.toMatchObject({ code: "rootMismatch" });
  });
});
