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
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScriptListPreimage,
  encodeMidgardTxOutput,
  hashMidgardVersionedScript,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
} from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  checkLedgerAgainstRoot,
  LedgerRootMismatchError,
} from "../src/ledger-at.js";
import {
  eventKeyFingerprint,
  reconstructDaPayload,
} from "../src/transition-trace/index.js";
import {
  corpus,
  expectedLabels,
  recomputeCorpusIdentity,
  reconstructAuthenticatedCanonicalTransactionFromFieldChunks,
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

      const fixture = await buildStrictRetainedDaPairFixture({
        canonicalTransactionCbor: canonicalCbor,
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
      expect(fixture.ledger).toEqual(boundary.resolvedReferenceUtxos ?? []);
      const diagnosticSyntheticScripts =
        boundary.productionAdmission ===
        "diagnostic-synthetic-script-witnesses";
      expect(diagnosticSyntheticScripts).toBe(
        boundary.label === "mixed-size-balanced",
      );
      const scripts = decodeMidgardVersionedScriptListPreimage(
        transaction.witnessSet.scriptTxWitsPreimageCbor,
      );
      if (diagnosticSyntheticScripts) {
        expect(boundary.sourceScriptHash).toBeUndefined();
      } else if (boundary.sourceScriptHash === undefined) {
        expect(
          scripts.filter(({ language }) => language !== "NativeCardano"),
        ).toEqual([]);
      } else {
        expect(scripts).toHaveLength(1);
        expect(boundary.sourceScriptHash).toMatch(/^[0-9a-f]{56}$/u);
        expect(hashMidgardVersionedScript(scripts[0]!)).toBe(
          boundary.sourceScriptHash,
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
        validationTraceCount: 0n,
      });
      expect(reconstruction.roots).toEqual({
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

  it("keeps roots, counts, and forced-preimage authentication fail closed", async () => {
    const canonicalCbor = Buffer.from(
      corpus.entries.find(
        ({ label }) => label === "maximum-list-redeemer-breadth",
      )!.canonicalCborHex,
      "hex",
    );
    const fixture = await buildStrictRetainedDaPairFixture({
      canonicalTransactionCbor: canonicalCbor,
    });
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
    // alone still reconstructs, against the header's counts. The declared
    // validation_trace_count is the exception: it must match the body's trie.
    const declaredCounts = fixture.payload.block_body.counts;
    const declaredOnlyPayload: SDK.DaPayload = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        counts: {
          ...declaredCounts,
          transitionStepCount: declaredCounts.transitionStepCount + 1n,
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
    expect(declaredOnly.counts.transitionStepCount).toBe(
      fixture.header.transitionStepCount,
    );

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

  it("refuses a resolved reference ledger that misses or substitutes an entry at the root check", async () => {
    const boundary = corpus.entries.find(
      ({ label }) => label === "maximum-reference-inputs",
    )!;
    const canonicalCbor = Buffer.from(boundary.canonicalCborHex, "hex");
    const fixture = await buildStrictRetainedDaPairFixture({
      canonicalTransactionCbor: canonicalCbor,
      resolvedReferenceUtxos: boundary.resolvedReferenceUtxos,
    });
    expect(fixture.ledger.length).toBeGreaterThan(0);
    const check = (entries: readonly SDK.DaPayloadEntry[]) =>
      checkLedgerAgainstRoot({
        entries: entries.map(([outRef, output]) => ({
          outRef: Buffer.from(outRef, "hex"),
          output: Buffer.from(output, "hex"),
        })),
        utxosRoot: fixture.header.utxosRoot,
        label: "reference ledger",
      });
    await expect(check(fixture.ledger)).resolves.toMatchObject({
      utxosRoot: fixture.header.utxosRoot,
    });
    await expect(check([])).rejects.toBeInstanceOf(LedgerRootMismatchError);

    const [first, ...remaining] = fixture.ledger;
    const decodedOutput = decodeMidgardTxOutput(Buffer.from(first![1], "hex"));
    const substitutedOutput = encodeMidgardTxOutput({
      ...decodedOutput,
      value: {
        ...decodedOutput.value,
        lovelace: decodedOutput.value.lovelace + 1n,
      },
    });
    await expect(
      check([[first![0], substitutedOutput.toString("hex")], ...remaining]),
    ).rejects.toBeInstanceOf(LedgerRootMismatchError);
  }, 30_000);
});
