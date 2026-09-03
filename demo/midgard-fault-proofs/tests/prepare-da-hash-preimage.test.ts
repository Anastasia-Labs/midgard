/**
 * `da-hash-preimage` DA-first evidence builder (Goal task `Q44`).
 *
 * Every case here goes through the same two security inputs the family is
 * allowed to consume: an authenticated L1 header observation and public
 * retained-DA payload bytes. There is no operator REST/DB/file input.
 */
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { fetchFraudProofEvidence } from "../src/evidence/production-fraud-proof-evidence-v1.js";
import {
  classifyCommittedTransactionsLeaves,
  daHashPreimageBlockEvidenceFromVerifiedPayload,
  DaHashPreimageRejection,
  prepareDaHashPreimageFromCommittedLeaves,
} from "../src/prepare-da-hash-preimage.js";
import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  type CanonicalBlockFixture,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const FOREIGN_COMMITTED_KEY =
  "9999999999999999999999999999999999999999999999999999999999999999";

const DA_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "retained-da-peer",
  grade: "security",
};

const buildBlock = async (): Promise<CanonicalBlockFixture> =>
  await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x21, 0n)],
        fee: 1_000_000n,
      }),
    ],
    transactionsRootMode: "payloadSource",
  });

const retainedSource = (
  payloadEnvelopeCbor: Buffer,
): RetainedDaPayloadSource => ({
  sourceId: "retained-da-peer",
  fetchPayloadByHeaderHash: async () => ({
    ok: true,
    provenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "retained-da-peer/peer-a",
      grade: "security",
    },
    sourceId: "retained-da-peer",
    sourcePeerId: "peer-a",
    payloadEnvelopeCbor,
    attempts: [],
  }),
});

/**
 * Re-commits a fixture so its only `transactions` leaf is
 * `(committedKey, leafValue)`, then re-derives the counted root, the header and
 * the header hash — the exact shape a faulty operator publishes.
 */
const recommitLeaf = async ({
  fixture,
  committedKey,
  leafValue,
}: {
  readonly fixture: CanonicalBlockFixture;
  readonly committedKey: string;
  readonly leafValue: Buffer;
}): Promise<{
  readonly payloadEnvelopeCbor: Buffer;
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly transactionsRoot: string;
}> => {
  const counted = await buildCountedRoot(SDK.ROOT_DOMAINS.transactionsV1, [
    { key: Buffer.from(committedKey, "hex"), value: leafValue },
  ]);
  const header: SDK.Header = {
    ...fixture.header,
    transactionsRoot: counted.root,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    ...fixture.payload,
    block_body: {
      ...fixture.payload.block_body,
      header,
      header_hash: headerHash,
      transactions: [[committedKey, leafValue.toString("hex")]],
    },
  };
  return {
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
    observation: authenticatedHeaderObservation({
      ...fixture,
      header,
      headerHash,
    }),
    transactionsRoot: counted.root,
  };
};

describe("Q44 da-hash-preimage leaf classification", () => {
  it("classifies an honestly keyed source leaf as compliant", async () => {
    const fixture = await buildBlock();
    const [tx] = fixture.transactions;
    const leaves = classifyCommittedTransactionsLeaves([
      [tx.txId, tx.sourceValueBytes.toString("hex")],
    ]);
    expect(leaves).toHaveLength(1);
    expect(leaves[0].derivedTxId).toBe(tx.txId);
    expect(leaves[0].isViolation).toBe(false);
  });

  it("classifies a foreign-keyed leaf as a violation", async () => {
    const fixture = await buildBlock();
    const [tx] = fixture.transactions;
    const leaves = classifyCommittedTransactionsLeaves([
      [FOREIGN_COMMITTED_KEY, tx.sourceValueBytes.toString("hex")],
    ]);
    expect(leaves[0].committedTxId).toBe(FOREIGN_COMMITTED_KEY);
    expect(leaves[0].verdict).toBe("KeyMismatch");
    expect(leaves[0].embeddedTxId).toBe(tx.txId);
    expect(leaves[0].derivedTxId).toBeNull();
    expect(leaves[0].isViolation).toBe(true);
  });

  it("preserves the exact total verdict order for hostile source leaves", async () => {
    const fixture = await buildBlock();
    const [tx] = fixture.transactions;
    const malformedProofSource = Data.to(
      {
        ...tx.source,
        source: { ...tx.source.source, witness_set_compact_cbor: "80" },
      },
      SDK.L2TransactionSource,
    );
    const derivedIdMismatch = Data.to(
      { ...tx.source, tx_id: FOREIGN_COMMITTED_KEY },
      SDK.L2TransactionSource,
    );
    expect(
      classifyCommittedTransactionsLeaves([
        [FOREIGN_COMMITTED_KEY, "deadbeef"],
      ])[0]?.verdict,
    ).toBe("MalformedSource");
    expect(
      classifyCommittedTransactionsLeaves([[tx.txId, malformedProofSource]])[0]
        ?.verdict,
    ).toBe("MalformedProofSource");
    expect(
      classifyCommittedTransactionsLeaves([
        [FOREIGN_COMMITTED_KEY, derivedIdMismatch],
      ])[0]?.verdict,
    ).toBe("DerivedIdMismatch");
  });
});

describe("Q44 da-hash-preimage proof plan", () => {
  it("builds a submittable plan for a miskeyed committed leaf", async () => {
    const fixture = await buildBlock();
    const [tx] = fixture.transactions;
    const recommitted = await recommitLeaf({
      fixture,
      committedKey: FOREIGN_COMMITTED_KEY,
      leafValue: tx.sourceValueBytes,
    });
    const evidence = await daHashPreimageBlockEvidenceFromVerifiedPayload({
      observation: recommitted.observation,
      payloadEnvelopeCbor: recommitted.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    expect(evidence.grade).toBe("security");
    expect(evidence.committedTransactionsRoot).toBe(
      recommitted.transactionsRoot,
    );

    const plan = await prepareDaHashPreimageFromCommittedLeaves({
      headerHash: evidence.headerHash,
      committedTransactionsRoot: evidence.committedTransactionsRoot,
      l2TransactionCount: evidence.l2TransactionCount,
      entries: evidence.entries,
    });
    expect(plan.violationId).toBe("da-hash-preimage");
    expect(plan.violation.committedTxId).toBe(FOREIGN_COMMITTED_KEY);
    expect(plan.violation.verdict).toBe("KeyMismatch");
    expect(plan.violation.embeddedTxId).toBe(tx.txId);
    expect(plan.violation.derivedTxId).toBeNull();
    expect(plan.violation.isViolation).toBe(true);
    expect(plan.step02State).toEqual({ verdict: "KeyMismatch" });
    expect(plan.txInclusion.committedLeafValueCbor).toBe(
      tx.sourceValueBytes.toString("hex"),
    );
    expect(plan.txInclusion.txMembershipProofCbor.length).toBeGreaterThan(0);
    expect(plan.transactionsPhasRoot).toMatch(/^[0-9a-f]{64}$/u);
  });

  it("convicts a committed leaf that is not a transaction at all", async () => {
    const fixture = await buildBlock();
    const garbage = Buffer.from("deadbeef", "hex");
    const recommitted = await recommitLeaf({
      fixture,
      committedKey: FOREIGN_COMMITTED_KEY,
      leafValue: garbage,
    });
    const evidence = await daHashPreimageBlockEvidenceFromVerifiedPayload({
      observation: recommitted.observation,
      payloadEnvelopeCbor: recommitted.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    const plan = await prepareDaHashPreimageFromCommittedLeaves({
      headerHash: evidence.headerHash,
      committedTransactionsRoot: evidence.committedTransactionsRoot,
      l2TransactionCount: evidence.l2TransactionCount,
      entries: evidence.entries,
    });
    expect(plan.violation.committedLeafByteCount).toBe(4);
    expect(plan.violation.verdict).toBe("MalformedSource");
    expect(plan.violation.isViolation).toBe(true);
  });

  it("refuses to challenge a valid block", async () => {
    const fixture = await buildBlock();
    const [tx] = fixture.transactions;
    const recommitted = await recommitLeaf({
      fixture,
      committedKey: tx.txId,
      leafValue: tx.sourceValueBytes,
    });
    const evidence = await daHashPreimageBlockEvidenceFromVerifiedPayload({
      observation: recommitted.observation,
      payloadEnvelopeCbor: recommitted.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    await expect(
      prepareDaHashPreimageFromCommittedLeaves({
        headerHash: evidence.headerHash,
        committedTransactionsRoot: evidence.committedTransactionsRoot,
        l2TransactionCount: evidence.l2TransactionCount,
        entries: evidence.entries,
      }),
    ).rejects.toMatchObject({ code: "no_violating_leaf" });
  });

  it("refuses evidence whose leaves do not open the committed root", async () => {
    const fixture = await buildBlock();
    const [tx] = fixture.transactions;
    await expect(
      prepareDaHashPreimageFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedTransactionsRoot: fixture.payloadSourceTransactionsRoot,
        l2TransactionCount: 1n,
        entries: [[FOREIGN_COMMITTED_KEY, tx.sourceValueBytes.toString("hex")]],
      }),
    ).rejects.toMatchObject({ code: "transactions_root_mismatch" });
  });

  it("accepts the canonical source-leaf convention as the valid negative", async () => {
    const fixture = await buildBlock();
    const [tx] = fixture.transactions;
    const evidence = await daHashPreimageBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    expect(evidence.entries[0][1]).toBe(tx.sourceValueBytes.toString("hex"));
    await expect(
      prepareDaHashPreimageFromCommittedLeaves({
        headerHash: evidence.headerHash,
        committedTransactionsRoot: evidence.committedTransactionsRoot,
        l2TransactionCount: evidence.l2TransactionCount,
        entries: evidence.entries,
      }),
    ).rejects.toMatchObject({ code: "no_violating_leaf" });
  });
});

describe("Q44 da-hash-preimage evidence admission", () => {
  it("rejects operator-private DA provenance", async () => {
    const fixture = await buildBlock();
    await expect(
      daHashPreimageBlockEvidenceFromVerifiedPayload({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        daProvenance: {
          trustClass: "operator_admin_api",
          sourceId: "node-admin",
          grade: "diagnostic",
          diagnosticLabel: "operator diagnostic",
        },
      }),
    ).rejects.toThrow();
  });

  it("rejects a payload whose embedded header is not the observed one", async () => {
    const fixture = await buildBlock();
    const other = await buildCanonicalBlockFixture({
      transactions: [
        buildFixtureTransaction({
          spendInputs: [outRefCbor(0x22, 1n)],
          fee: 2_000_000n,
        }),
      ],
      transactionsRootMode: "payloadSource",
    });
    await expect(
      daHashPreimageBlockEvidenceFromVerifiedPayload({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: other.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
      }),
    ).rejects.toBeInstanceOf(DaHashPreimageRejection);
  });
});

describe("Q44 exact production evidence route", () => {
  it("uses canonical reconstruction for an honest source leaf", async () => {
    const fixture = await buildBlock();
    await expect(
      fetchFraudProofEvidence({
        observation: authenticatedHeaderObservation(fixture),
        sources: [retainedSource(fixture.payloadEnvelopeCbor)],
        retries: 0,
      }),
    ).resolves.toMatchObject({ kind: "canonical_block" });
  });

  it("routes only an L1-root-authenticated source-leaf violation to Q44", async () => {
    const fixture = await buildBlock();
    const [tx] = fixture.transactions;
    const recommitted = await recommitLeaf({
      fixture,
      committedKey: FOREIGN_COMMITTED_KEY,
      leafValue: tx.sourceValueBytes,
    });
    const routed = await fetchFraudProofEvidence({
      observation: recommitted.observation,
      sources: [retainedSource(recommitted.payloadEnvelopeCbor)],
      retries: 0,
    });
    expect(routed.kind).toBe("da_hash_preimage");
    if (routed.kind !== "da_hash_preimage") throw new Error("wrong route");
    expect(routed.plan.violation).toMatchObject({
      committedTxId: FOREIGN_COMMITTED_KEY,
      verdict: "KeyMismatch",
      isViolation: true,
    });
    expect(
      routed.plan.txInclusion.txMembershipProofCbor.length,
    ).toBeGreaterThan(0);
  });

  it("does not turn a forged raw root/key/value substitution into Q44", async () => {
    const fixture = await buildBlock();
    const [tx] = fixture.transactions;
    const forgedPayload: SDK.DaPayload = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        transactions: [
          [FOREIGN_COMMITTED_KEY, tx.sourceValueBytes.toString("hex")],
        ],
      },
    };
    const forgedEnvelope = await wrapDaPayload(
      SDK.encodeDaPayload(forgedPayload),
      { mode: "identity" },
    );
    await expect(
      fetchFraudProofEvidence({
        observation: authenticatedHeaderObservation(fixture),
        sources: [retainedSource(forgedEnvelope)],
        retries: 0,
      }),
    ).rejects.toMatchObject({ code: "rootMismatch" });
  });

  it("does not fall through on arbitrary transport/provider failures", async () => {
    const fixture = await buildBlock();
    const failed: RetainedDaPayloadSource = {
      sourceId: "hostile-provider",
      fetchPayloadByHeaderHash: async () => {
        throw new Error("arbitrary fetch failure");
      },
    };
    await expect(
      fetchFraudProofEvidence({
        observation: authenticatedHeaderObservation(fixture),
        sources: [failed],
        retries: 0,
      }),
    ).rejects.toThrow("arbitrary fetch failure");
  });

  it("does not fall through on L1 finality or DA provenance errors", async () => {
    const fixture = await buildBlock();
    await expect(
      fetchFraudProofEvidence({
        observation: authenticatedHeaderObservation(fixture, {
          confirmationDepth: 1,
        }),
        sources: [retainedSource(fixture.payloadEnvelopeCbor)],
        retries: 0,
        minimumConfirmationDepth: 30,
      }),
    ).rejects.toThrow("insufficient_confirmation_depth");

    const privateSource: RetainedDaPayloadSource = {
      sourceId: "private",
      fetchPayloadByHeaderHash: async () => ({
        ok: true,
        provenance: {
          trustClass: "operator_private_file",
          sourceId: "private/peer-a",
          grade: "diagnostic",
          diagnosticLabel: "hostile private input",
        },
        sourceId: "private",
        sourcePeerId: "peer-a",
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        attempts: [],
      }),
    };
    await expect(
      fetchFraudProofEvidence({
        observation: authenticatedHeaderObservation(fixture),
        sources: [privateSource],
        retries: 0,
      }),
    ).rejects.toThrow("prohibited_trust_class");
  });
});
