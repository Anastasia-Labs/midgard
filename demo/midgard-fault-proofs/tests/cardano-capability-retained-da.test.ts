import "node:fs";
import "node:path";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-sdk";
import "@al-ft/midgard-validation";
import "effect";
import "vitest";
import "../src/transition-trace/index.js";
import "./helpers/cardano-capability-retained-da.js";
import "./cardano-capability-retained-da.reconstruct-authenticated-canonical-transaction-from-field-chunks.js";

import {
  collectMidgardAttachedProgramEnvelopes,
  decodeMidgardCekProgramMaterialSidecar,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScriptListPreimage,
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardTxOutput,
  hashMidgardVersionedScript,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
} from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildCountedRoot,
  eventKeyFingerprint,
  reconstructDaPayload,
} from "../src/transition-trace/index.js";
import {
  corpus,
  expectedLabels,
  recomputeCorpusIdentity,
  reconstructAuthenticatedCanonicalTransactionFromFieldChunks,
  verifyFixtureProgramMaterial,
} from "./cardano-capability-retained-da.reconstruct-authenticated-canonical-transaction-from-field-chunks.js";
import { buildStrictRetainedDaPairFixture } from "./helpers/cardano-capability-retained-da.js";

describe("Cardano capability P2 production retained-DA boundary", () => {
  it("strictly authenticates every established maximum before bounded field/item chunk reconstruction", async () => {
    expect(corpus.schema).toBe(
      "midgard-cardano-capability-p2-boundary-corpus-v1",
    );
    expect(corpus.entries.map(({ label }) => label)).toEqual(expectedLabels);

    for (const boundary of corpus.entries) {
      const canonicalCbor = Buffer.from(boundary.canonicalCborHex, "hex");
      expect(recomputeCorpusIdentity(canonicalCbor)).toEqual({
        transactionIdHex: boundary.transactionIdHex,
        transactionCommitmentHex: boundary.transactionCommitmentHex,
      });

      const materialSidecar =
        boundary.canonicalMaterialSidecarCborHex === undefined
          ? undefined
          : Buffer.from(boundary.canonicalMaterialSidecarCborHex, "hex");
      const fixture = await buildStrictRetainedDaPairFixture({
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
        decodeMidgardNativeTxFullFromCanonicalCbor(canonicalCbor);
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
          collectMidgardAttachedProgramEnvelopes(transaction);
        expect(boundary.sourceRawScriptAuditHash).toBeUndefined();
        expect(attachedPrograms).toHaveLength(0);
        expect(fixture.payload.block_body.cek_program_material).toEqual([]);
      } else {
        const attachedPrograms =
          collectMidgardAttachedProgramEnvelopes(transaction);
        const sidecarEntries =
          decodeMidgardCekProgramMaterialSidecar(materialSidecar);
        expect(sidecarEntries.length).toBeGreaterThan(0);
        expect(fixture.payload.block_body.cek_program_material).toEqual(
          sidecarEntries
            .map(
              (entry): SDK.DaPayloadEntry => [
                Buffer.from(entry.root).toString("hex"),
                encodeMidgardCekProgramMaterialDaValue(entry).toString("hex"),
              ],
            )
            .sort(([left], [right]) => left.localeCompare(right)),
        );
        expect(attachedPrograms).toHaveLength(1);
        expect(
          verifyFixtureProgramMaterial({
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

      const reconstruction = await reconstructDaPayload({
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
        [
          "normal",
          normal!.fullTransactionCbor,
          fixture.transactionCommitmentHex,
        ],
        [
          "forced",
          forced!.fullTransactionCbor,
          fixture.forcedTransactionCommitmentHex,
        ],
      ] as const;
      for (const [
        sourceKind,
        sourceCanonicalCbor,
        expectedCommitment,
      ] of authenticated) {
        const folded =
          reconstructAuthenticatedCanonicalTransactionFromFieldChunks(
            sourceCanonicalCbor,
            sourceKind,
          );
        expect({
          transactionIdHex: folded.transactionIdHex,
          transactionCommitmentHex: folded.transactionCommitmentHex,
        }).toEqual({
          transactionIdHex: boundary.transactionIdHex,
          transactionCommitmentHex: expectedCommitment,
        });
        expect(folded.revealStepCount).toBeGreaterThan(0);
        expect(folded.maximumChunkBytes).toBeLessThanOrEqual(
          MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
        );
        expect(folded.reconstructed).toEqual(sourceCanonicalCbor);
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
    const fixture = await buildStrictRetainedDaPairFixture({
      canonicalTransactionCbor: canonicalCbor,
      canonicalMaterialSidecarCbor: materialSidecar,
    });
    const missingMaterialPayload: SDK.DaPayload = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        cek_program_material: [],
      },
    };
    expect(() =>
      verifyFixtureProgramMaterial({
        canonicalCbor,
        payload: missingMaterialPayload,
      }),
    ).toThrow();

    const [materialRootHex, materialValueHex] =
      fixture.payload.block_body.cek_program_material[0]!;
    const [materialEntry] =
      decodeMidgardCekProgramMaterialSidecar(materialSidecar);
    const mutatedPreimage = Buffer.from(materialEntry!.preimage);
    mutatedPreimage[mutatedPreimage.length - 1] ^= 0x01;
    const mutatedMaterialPayload: SDK.DaPayload = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        cek_program_material: [
          [
            materialRootHex,
            encodeMidgardCekProgramMaterialDaValue({
              kind: materialEntry!.kind,
              preimage: mutatedPreimage,
            }).toString("hex"),
          ],
          ...fixture.payload.block_body.cek_program_material.slice(1),
        ],
      },
    };
    expect(() =>
      verifyFixtureProgramMaterial({
        canonicalCbor,
        payload: mutatedMaterialPayload,
      }),
    ).toThrow();

    const wrongMaterialRootPayload: SDK.DaPayload = {
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
      verifyFixtureProgramMaterial({
        canonicalCbor,
        payload: wrongMaterialRootPayload,
      }),
    ).toThrow();

    const badRootHeader: SDK.Header = {
      ...fixture.header,
      transactionsRoot: "ff".repeat(32),
    };
    const badRootHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeader(badRootHeader),
    );
    const badRootPayload: SDK.DaPayload = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        header_hash: badRootHeaderHash,
        header: badRootHeader,
      },
    };
    await expect(
      reconstructDaPayload({
        payloadEnvelopeCbor: await wrapDaPayload(
          SDK.encodeDaPayload(badRootPayload),
          { mode: "identity" },
        ),
        expectedHeaderHash: badRootHeaderHash,
        committedHeader: badRootHeader,
      }),
    ).rejects.toMatchObject({ code: "rootMismatch" });

    // Declared counts are not committed by the header, so a tamper of them
    // alone still reconstructs, against the header's counts.
    const declaredCounts = fixture.payload.block_body.counts;
    const declaredOnlyPayload: SDK.DaPayload = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        counts: {
          ...declaredCounts,
          validationTraceCount: declaredCounts.validationTraceCount + 1n,
        },
      },
    };
    const declaredOnly = await reconstructDaPayload({
      payloadEnvelopeCbor: await wrapDaPayload(
        SDK.encodeDaPayload(declaredOnlyPayload),
        { mode: "identity" },
      ),
      expectedHeaderHash: fixture.headerHash,
      committedHeader: fixture.header,
    });
    expect(declaredOnly.counts.validationTraceCount).toBe(
      fixture.header.validationTraceCount,
    );

    // No arm proves a validation_traces count, so an authenticated member
    // list that disagrees with the header count still aborts.
    const fewerTraces = fixture.payload.block_body.validation_traces.slice(1);
    const fewerTracesHeader: SDK.Header = {
      ...fixture.header,
      validationTracesRoot: (
        await buildCountedRoot(
          SDK.ROOT_DOMAINS.validationTraces,
          fewerTraces.map(([key, value]) => ({
            key: Buffer.from(key, "hex"),
            value: Buffer.from(value, "hex"),
          })),
        )
      ).root,
    };
    const fewerTracesHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeader(fewerTracesHeader),
    );
    const fewerTracesPayload: SDK.DaPayload = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        header_hash: fewerTracesHeaderHash,
        header: fewerTracesHeader,
        validation_traces: fewerTraces,
      },
    };
    await expect(
      reconstructDaPayload({
        payloadEnvelopeCbor: await wrapDaPayload(
          SDK.encodeDaPayload(fewerTracesPayload),
          { mode: "identity" },
        ),
        expectedHeaderHash: fewerTracesHeaderHash,
        committedHeader: fewerTracesHeader,
      }),
    ).rejects.toMatchObject({
      code: "countMismatch",
      message: expect.stringContaining("validation_traces member count"),
    });

    const alternateCanonicalCborHex = corpus.entries.find(
      ({ label }) => label === "mixed-size-balanced",
    )!.canonicalCborHex;
    expect(alternateCanonicalCborHex).not.toBe(canonicalCbor.toString("hex"));
    const tamperedForcedPreimagePayload: SDK.DaPayload = {
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
      reconstructDaPayload({
        payloadEnvelopeCbor: await wrapDaPayload(
          SDK.encodeDaPayload(tamperedForcedPreimagePayload),
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
    const fixture = await buildStrictRetainedDaPairFixture({
      canonicalTransactionCbor: canonicalCbor,
      resolvedReferenceUtxos: boundary.resolvedReferenceUtxos,
    });
    expect(fixture.payload.block_body.utxos.length).toBeGreaterThan(0);

    const missing: SDK.DaPayload = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        utxos: [],
      },
    };
    await expect(
      reconstructDaPayload({
        payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(missing), {
          mode: "identity",
        }),
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
    const substituted: SDK.DaPayload = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        utxos: [[first![0], substitutedOutput.toString("hex")], ...remaining],
      },
    };
    await expect(
      reconstructDaPayload({
        payloadEnvelopeCbor: await wrapDaPayload(
          SDK.encodeDaPayload(substituted),
          { mode: "identity" },
        ),
        expectedHeaderHash: fixture.headerHash,
        committedHeader: fixture.header,
      }),
    ).rejects.toMatchObject({ code: "rootMismatch" });
  }, 30_000);
});
