import "./header-root-reconstruction.w22-malformed-payload-bytes.js";

import { reconstructDaPayload } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it } from "vitest";

import {
  makeWatcherHeaderRootReconstructedState,
  WatcherHeaderRootReconstructionError,
} from "../../src/verification/header-root-reconstruction.js";
import {
  buildFixture,
  CHAIN_POINT,
  commitMutatedHeader,
  evaluateFixture,
  L1_PROVENANCE,
  observationFor,
} from "./header-root-reconstruction.build-fixture.js";
import {
  corpus,
  corpusTransaction,
} from "./header-root-reconstruction.watcher-header-record.js";

describe("W22 fail-closed and non-circularity", () => {
  /**
   * PROVENANCE: the observation is the state-queue header record for block A; the payload is a
   * complete, internally consistent public payload for block B (its embedded
   * header hashes to its own header_hash, and its own roots/counts agree with
   * it). The only correct outcome is rejection, and the reported expected root
   * set must remain block A's.
   */
  it("rejects a self-consistent payload describing a different block, and never adopts its header", async () => {
    const blockA = await buildFixture({ transactions: [corpusTransaction(0)] });
    const blockB = await buildFixture({
      transactions: [corpusTransaction(0), corpusTransaction(1)],
      depositBytes: [11],
    });
    const standalone = await reconstructDaPayload({
      payloadEnvelopeCbor: blockB.envelope,
    });
    expect(standalone.headerHash).toBe(blockB.headerHash);

    const result = await evaluateFixture(blockA, { envelope: blockB.envelope });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["payload_header_mismatch"]);
    expect(result.headerHash).toBe(blockA.headerHash);
    expect(result.headerRoots.transactions_root).toBe(
      blockA.header.transactionsRoot,
    );
    expect(result.headerRoots.transactions_root).not.toBe(
      blockB.header.transactionsRoot,
    );
    expect(result.reconstructedRoots).toBeNull();
    expect(result.reconstructedCounts).toBeNull();
  });

  /**
   * PROVENANCE: the header struct is taken from the payload (operator-supplied),
   * while the header hash is the real L1-observed one. Admission re-derives the
   * hash, so the pairing is refused.
   */
  it("rejects a caller header that is not the one the state-queue record committed", async () => {
    const blockA = await buildFixture({ transactions: [corpusTransaction(0)] });
    const blockB = await buildFixture({ depositBytes: [11] });
    const forged: SDK.AuthenticatedStateQueueHeaderObservation = {
      schemaVersion: SDK.CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
      sourceMode: "local_node",
      provenance: L1_PROVENANCE,
      chainPoint: CHAIN_POINT,
      confirmationDepth: 12,
      headerHash: blockA.headerHash,
      header: blockB.header,
    };
    const result = await evaluateFixture(blockA, {
      observation: forged,
      envelope: blockB.envelope,
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["header_hash_mismatch"]);
    expect(result.reconstructedRoots).toBeNull();
  });

  it("rejects an insufficient confirmation depth at evaluation time", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const observation = await observationFor(fixture, {
      confirmationDepth: 3,
    });
    const result = await evaluateFixture(fixture, {
      observation,
      minimumConfirmationDepth: 20,
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual([
      "insufficient_confirmation_depth",
    ]);
  });

  it.each([
    ["authenticated_cardano_l1", "da_evidence_wrong_trust_class"],
    ["signed_deployment_identity", "da_evidence_wrong_trust_class"],
    ["deterministic_local_computation", "da_evidence_wrong_trust_class"],
  ])(
    "rejects DA bytes carrying trust class %s",
    async (trustClass, expected) => {
      const fixture = await buildFixture({
        transactions: [corpusTransaction(0)],
      });
      const result = await evaluateFixture(fixture, {
        daProvenance: {
          trustClass: trustClass as SDK.EvidenceProvenance["trustClass"],
          sourceId: "some-source",
          grade: "security",
        },
      });
      expect(result.action).toBe("reject");
      expect(result.reasonCodes).toStrictEqual([expected]);
    },
  );

  it("rejects operator-private DA provenance", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const result = await evaluateFixture(fixture, {
      daProvenance: {
        trustClass: "operator_private_database",
        sourceId: "operator-db",
        grade: "security",
      },
    });
    expect(result.action).toBe("reject");
    expect(result.reasonCodes).toStrictEqual(["prohibited_trust_class"]);
  });

  it("never sources the expected root set from the payload", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    // The header the payload embeds is byte-identical to the L1 one here, so
    // the only way to see whose values were used is to change the L1 record and
    // observe the expected set follow it.
    const mutated = await commitMutatedHeader(fixture, (header) => ({
      ...header,
      transactionsRoot: h32(0x3c),
    }));
    const result = await evaluateFixture(mutated);
    expect(result.headerRoots.transactions_root).toBe(h32(0x3c));
    expect(result.headerRoots.transactions_root).not.toBe(
      fixture.header.transactionsRoot,
    );
    expect(result.rootMismatches).toStrictEqual(["transactions_root"]);
  });
});

describe("W22 reconstructed-state durable record", () => {
  it("builds the reserved record from an accepted result and the W21 input ids", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const result = await evaluateFixture(fixture);
    const record = makeWatcherHeaderRootReconstructedState({
      result,
      chainPointId: "chain-point-1",
      inputIds: ["da-payload-1", "proof-bundle-1"],
    });
    expect(record.blockHash).toBe(fixture.headerHash);
    expect(record.chainPointId).toBe("chain-point-1");
    expect(record.priorStateRoot).toBe(fixture.header.prevUtxosRoot);
    expect(record.postStateRoot).toBe(result.reconstructedRoots?.utxos_root);
    expect(record.inputIds).toStrictEqual(["da-payload-1", "proof-bundle-1"]);
    expect(record.state.sha256).toMatch(/^[0-9a-f]{64}$/u);
  });

  it("produces the same state bytes for the same reconstruction", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const build = async () =>
      makeWatcherHeaderRootReconstructedState({
        result: await evaluateFixture(fixture),
        chainPointId: "chain-point-1",
        inputIds: ["da-payload-1"],
      });
    expect(await build()).toStrictEqual(await build());
  });

  it("refuses to build a record from a rejected result", async () => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const mutated = await commitMutatedHeader(fixture, (header) => ({
      ...header,
      depositsRoot: h32(0x11),
    }));
    const result = await evaluateFixture(mutated);
    expect(() =>
      makeWatcherHeaderRootReconstructedState({
        result,
        chainPointId: "chain-point-1",
        inputIds: ["da-payload-1"],
      }),
    ).toThrowError(WatcherHeaderRootReconstructionError);
  });

  it.each([
    ["empty", []],
    ["duplicated", ["da-payload-1", "da-payload-1"]],
    ["blank", [""]],
  ])("refuses %s input ids", async (_label, inputIds) => {
    const fixture = await buildFixture({
      transactions: [corpusTransaction(0)],
    });
    const result = await evaluateFixture(fixture);
    expect(() =>
      makeWatcherHeaderRootReconstructedState({
        result,
        chainPointId: "chain-point-1",
        inputIds: inputIds as readonly string[],
      }),
    ).toThrowError(WatcherHeaderRootReconstructionError);
  });
});

describe("W22 canonical producer agreement", () => {
  /**
   * The watcher imports the canonical reconstruction rather than reimplementing
   * it, so agreement is an identity. This asserts it for a payload whose
   * transactions are the exact cross-language corpus entries at
   * demo/midgard-fault-proofs/tests/fixtures/cardano-capability-p2-boundary-corpus-v1.json.
   */
  it("equals reconstructDaPayload for a cross-language corpus fixture", async () => {
    expect(corpus.schema).toBe(
      "midgard-cardano-capability-p2-boundary-corpus-v1",
    );
    const transactions = [corpusTransaction(0), corpusTransaction(1)];
    expect(transactions[0]!.txId).toBe(corpus.entries[0]!.transactionIdHex);
    const fixture = await buildFixture({ transactions });
    const producer = await reconstructDaPayload({
      payloadEnvelopeCbor: fixture.envelope,
      expectedHeaderHash: fixture.headerHash,
      committedHeader: fixture.header,
    });
    const result = await evaluateFixture(fixture);
    expect(result.action).toBe("accept");
    expect(result.reconstructedRoots).toStrictEqual({
      utxos_root: producer.roots.utxosRoot,
      withdrawals_root: producer.roots.withdrawalsRoot,
      forced_transactions_root: producer.roots.forcedTransactionsRoot,
      transactions_root: producer.roots.transactionsRoot,
      deposits_root: producer.roots.depositsRoot,
      transition_trace_root: producer.roots.transitionTraceRoot,
      event_to_step_root: producer.roots.eventToStepRoot,
      validation_traces_root: producer.roots.validationTracesRoot,
    });
    expect(result.reconstructedCounts).toStrictEqual({
      withdrawal_count: producer.counts.withdrawalCount.toString(),
      forced_transaction_count:
        producer.counts.forcedTransactionCount.toString(),
      l2_transaction_count: producer.counts.l2TransactionCount.toString(),
      deposit_count: producer.counts.depositCount.toString(),
      total_event_count: producer.counts.totalEventCount.toString(),
      transition_step_count: producer.counts.transitionStepCount.toString(),
      validation_trace_count: producer.counts.validationTraceCount.toString(),
    });
  });
});
