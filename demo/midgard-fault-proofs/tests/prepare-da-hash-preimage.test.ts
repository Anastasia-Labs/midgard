/**
 * `da-hash-preimage` DA-first evidence builder (Goal task `Q44`).
 *
 * Every case here goes through the same two security inputs the family is
 * allowed to consume: an authenticated L1 header observation and public
 * retained-DA payload bytes. There is no operator REST/DB/file input.
 */
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  classifyCommittedTransactionsLeavesV1,
  daHashPreimageBlockEvidenceFromVerifiedPayloadV1,
  DaHashPreimageRejectionV1,
  prepareDaHashPreimageFromCommittedLeavesV1,
} from "../src/prepare-da-hash-preimage.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  type CanonicalBlockFixtureV1,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const FOREIGN_COMMITTED_KEY =
  "9999999999999999999999999999999999999999999999999999999999999999";

const DA_PROVENANCE: SDK.EvidenceProvenanceV1 = {
  trustClass: "public_or_permissionless_da",
  sourceId: "retained-da-peer",
  grade: "security",
};

const buildBlock = async (
  mode: "payloadSource" | "nativeCompact",
): Promise<CanonicalBlockFixtureV1> =>
  await buildCanonicalBlockFixtureV1({
    transactions: [
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(0x21, 0n)],
        fee: 1_000_000n,
      }),
    ],
    transactionsRootMode: mode,
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
  readonly fixture: CanonicalBlockFixtureV1;
  readonly committedKey: string;
  readonly leafValue: Buffer;
}): Promise<{
  readonly payloadEnvelopeCbor: Buffer;
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservationV1;
  readonly transactionsRoot: string;
}> => {
  const counted = await buildCountedRoot(SDK.ROOT_DOMAINS.transactionsV1, [
    { key: Buffer.from(committedKey, "hex"), value: leafValue },
  ]);
  const header: SDK.HeaderV1 = {
    ...fixture.header,
    transactionsRoot: counted.root,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  const payload: SDK.DaPayloadV1 = {
    ...fixture.payload,
    block_body: {
      ...fixture.payload.block_body,
      header,
      header_hash: headerHash,
      transactions: [[committedKey, leafValue.toString("hex")]],
    },
  };
  return {
    payloadEnvelopeCbor: await wrapDaPayloadV1(SDK.encodeDaPayloadV1(payload), {
      mode: "identity",
    }),
    observation: authenticatedHeaderObservationV1({
      ...fixture,
      header,
      headerHash,
    }),
    transactionsRoot: counted.root,
  };
};

describe("Q44 da-hash-preimage leaf classification", () => {
  it("classifies an honestly keyed native-compact leaf as compliant", async () => {
    const fixture = await buildBlock("nativeCompact");
    const [tx] = fixture.transactions;
    const leaves = classifyCommittedTransactionsLeavesV1([
      [tx.txId, tx.compactCbor.toString("hex")],
    ]);
    expect(leaves).toHaveLength(1);
    expect(leaves[0].derivedTxId).toBe(tx.txId);
    expect(leaves[0].isViolation).toBe(false);
  });

  it("classifies a foreign-keyed leaf as a violation", async () => {
    const fixture = await buildBlock("nativeCompact");
    const [tx] = fixture.transactions;
    const leaves = classifyCommittedTransactionsLeavesV1([
      [FOREIGN_COMMITTED_KEY, tx.compactCbor.toString("hex")],
    ]);
    expect(leaves[0].committedTxId).toBe(FOREIGN_COMMITTED_KEY);
    expect(leaves[0].derivedTxId).toBe(tx.txId);
    expect(leaves[0].isViolation).toBe(true);
  });
});

describe("Q44 da-hash-preimage proof plan", () => {
  it("builds a submittable plan for a miskeyed committed leaf", async () => {
    const fixture = await buildBlock("nativeCompact");
    const [tx] = fixture.transactions;
    const recommitted = await recommitLeaf({
      fixture,
      committedKey: FOREIGN_COMMITTED_KEY,
      leafValue: tx.compactCbor,
    });
    const evidence = await daHashPreimageBlockEvidenceFromVerifiedPayloadV1({
      observation: recommitted.observation,
      payloadEnvelopeCbor: recommitted.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    expect(evidence.grade).toBe("security");
    expect(evidence.committedTransactionsRoot).toBe(
      recommitted.transactionsRoot,
    );

    const plan = await prepareDaHashPreimageFromCommittedLeavesV1({
      headerHash: evidence.headerHash,
      committedTransactionsRoot: evidence.committedTransactionsRoot,
      l2TransactionCount: evidence.l2TransactionCount,
      entries: evidence.entries,
    });
    expect(plan.violationId).toBe("da-hash-preimage");
    expect(plan.violation.committedTxId).toBe(FOREIGN_COMMITTED_KEY);
    expect(plan.violation.derivedTxId).toBe(tx.txId);
    expect(plan.violation.isViolation).toBe(true);
    expect(plan.step02State).toEqual({
      committedTxId: FOREIGN_COMMITTED_KEY,
      derivedTxId: tx.txId,
      committedLeafByteCount: tx.compactCbor.length,
    });
    expect(plan.txInclusion.committedLeafValueCbor).toBe(
      tx.compactCbor.toString("hex"),
    );
    expect(plan.txInclusion.txMembershipProofCbor.length).toBeGreaterThan(0);
    expect(plan.transactionsPhasRoot).toMatch(/^[0-9a-f]{64}$/u);
  });

  it("convicts a committed leaf that is not a transaction at all", async () => {
    const fixture = await buildBlock("nativeCompact");
    const garbage = Buffer.from("deadbeef", "hex");
    const recommitted = await recommitLeaf({
      fixture,
      committedKey: FOREIGN_COMMITTED_KEY,
      leafValue: garbage,
    });
    const evidence = await daHashPreimageBlockEvidenceFromVerifiedPayloadV1({
      observation: recommitted.observation,
      payloadEnvelopeCbor: recommitted.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    const plan = await prepareDaHashPreimageFromCommittedLeavesV1({
      headerHash: evidence.headerHash,
      committedTransactionsRoot: evidence.committedTransactionsRoot,
      l2TransactionCount: evidence.l2TransactionCount,
      entries: evidence.entries,
    });
    expect(plan.violation.committedLeafByteCount).toBe(4);
    expect(plan.violation.isViolation).toBe(true);
  });

  it("refuses to challenge a valid block", async () => {
    const fixture = await buildBlock("nativeCompact");
    const [tx] = fixture.transactions;
    const recommitted = await recommitLeaf({
      fixture,
      committedKey: tx.txId,
      leafValue: tx.compactCbor,
    });
    const evidence = await daHashPreimageBlockEvidenceFromVerifiedPayloadV1({
      observation: recommitted.observation,
      payloadEnvelopeCbor: recommitted.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    await expect(
      prepareDaHashPreimageFromCommittedLeavesV1({
        headerHash: evidence.headerHash,
        committedTransactionsRoot: evidence.committedTransactionsRoot,
        l2TransactionCount: evidence.l2TransactionCount,
        entries: evidence.entries,
      }),
    ).rejects.toMatchObject({ code: "no_violating_leaf" });
  });

  it("refuses evidence whose leaves do not open the committed root", async () => {
    const fixture = await buildBlock("nativeCompact");
    const [tx] = fixture.transactions;
    await expect(
      prepareDaHashPreimageFromCommittedLeavesV1({
        headerHash: fixture.headerHash,
        committedTransactionsRoot: fixture.nativeCompactTransactionsRoot,
        l2TransactionCount: 1n,
        entries: [[FOREIGN_COMMITTED_KEY, tx.compactCbor.toString("hex")]],
      }),
    ).rejects.toMatchObject({ code: "transactions_root_mismatch" });
  });

  it("escalates the payload-source leaf convention instead of convicting", async () => {
    const fixture = await buildBlock("payloadSource");
    const [tx] = fixture.transactions;
    const evidence = await daHashPreimageBlockEvidenceFromVerifiedPayloadV1({
      observation: authenticatedHeaderObservationV1(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    expect(evidence.entries[0][1]).toBe(tx.sourceValueBytes.toString("hex"));
    await expect(
      prepareDaHashPreimageFromCommittedLeavesV1({
        headerHash: evidence.headerHash,
        committedTransactionsRoot: evidence.committedTransactionsRoot,
        l2TransactionCount: evidence.l2TransactionCount,
        entries: evidence.entries,
      }),
    ).rejects.toMatchObject({ code: "payload_source_convention_block" });
  });
});

describe("Q44 da-hash-preimage evidence admission", () => {
  it("rejects operator-private DA provenance", async () => {
    const fixture = await buildBlock("nativeCompact");
    await expect(
      daHashPreimageBlockEvidenceFromVerifiedPayloadV1({
        observation: authenticatedHeaderObservationV1(fixture),
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
    const fixture = await buildBlock("nativeCompact");
    const other = await buildCanonicalBlockFixtureV1({
      transactions: [
        buildFixtureTransactionV1({
          spendInputs: [outRefCbor(0x22, 1n)],
          fee: 2_000_000n,
        }),
      ],
      transactionsRootMode: "nativeCompact",
    });
    await expect(
      daHashPreimageBlockEvidenceFromVerifiedPayloadV1({
        observation: authenticatedHeaderObservationV1(fixture),
        payloadEnvelopeCbor: other.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
      }),
    ).rejects.toBeInstanceOf(DaHashPreimageRejectionV1);
  });
});
