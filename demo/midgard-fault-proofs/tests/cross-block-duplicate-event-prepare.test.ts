import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  type AuthenticatedSettlementEvidence,
  prepareCrossBlockDuplicateEvent,
} from "../src/cross-block-duplicate-event/index.js";
import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../src/evidence/canonical-block-evidence.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { encodeData } from "../src/transition-trace/reconstruct.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
  reencodeFixturePayload,
} from "./helpers/canonical-block-evidence-fixture.js";

type EventKind = "deposit" | "withdrawal" | "forced-transaction";

const EVENT_KEY: SDK.OutputReference = {
  transactionId: "71".repeat(32),
  outputIndex: 2n,
};
const DEPOSIT_VALUE: SDK.DepositInfo = {
  l2_address: {
    paymentCredential: { PublicKeyCredential: ["72".repeat(28)] },
    stakeCredential: null,
  },
  l2_network_id: 0n,
  l2_datum: null,
};
const WITHDRAWAL_VALUE: SDK.WithdrawalInfo = {
  body: {
    l2_outref: { transactionId: "73".repeat(32), outputIndex: 1n },
    l2_owner: "74".repeat(28),
    l2_value: new Map(),
    l1_address: {
      paymentCredential: { PublicKeyCredential: ["75".repeat(28)] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  },
  signature: ["76".repeat(32), "77".repeat(64)],
  validity: "WithdrawalIsValid",
};

const DOMAIN: Record<EventKind, SDK.RootDomain> = {
  deposit: SDK.ROOT_DOMAINS.deposits,
  withdrawal: SDK.ROOT_DOMAINS.withdrawals,
  "forced-transaction": SDK.ROOT_DOMAINS.forcedTransactionsV1,
};
const EVENT_KIND: Record<EventKind, SDK.CrossBlockDuplicateEventKind> = {
  deposit: "DuplicateDepositV1",
  withdrawal: "DuplicateWithdrawalV1",
  "forced-transaction": "DuplicateForcedTransactionV1",
};

const SETTLEMENT_POLICY_ID = "82".repeat(28);

const sortEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const bufferEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): readonly { readonly key: Buffer; readonly value: Buffer }[] =>
  entries.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));

export type EventBlock = {
  readonly evidence: CanonicalBlockEvidence;
  /** The exact counted root the L1 header commits for this event domain. */
  readonly committedRoot: string;
  /** The exact leaf value bytes committed under `EVENT_KEY`. */
  readonly valueBytes: Buffer;
};

/**
 * Builds a block that really commits one L1 event under `EVENT_KEY`, publishes
 * it as retained-DA bytes, and turns it into evidence through the production
 * entry point `canonicalBlockEvidenceFromVerifiedPayload`. Nothing here is cast
 * into the evidence type: the header hash, every counted root and every count
 * are re-derived and cross-checked by `reconstructDaPayload`, so a block whose
 * event set does not open the committed roots cannot reach the builder at all.
 */
const buildEventBlock = async ({
  kind,
  prevHeaderHash,
  eventKey = EVENT_KEY,
}: {
  readonly kind: EventKind;
  readonly prevHeaderHash: string;
  readonly eventKey?: SDK.OutputReference;
}): Promise<EventBlock> => {
  const base = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x21, 0n)],
        fee: 1_000_000n,
      }),
    ],
    prevHeaderHash,
  });
  const forcedTx = buildFixtureTransaction({
    spendInputs: [outRefCbor(0x31, 0n)],
    fee: 2_000_000n,
  });
  const forcedValue: SDK.ForcedInclusionTxV1 = {
    tx_id: forcedTx.txId,
    source: forcedTx.source.source,
    verdict: "ForcedTxValid",
  };
  const keyBytes = encodeData(eventKey, SDK.OutputReference as never);
  const valueBytes =
    kind === "deposit"
      ? encodeData(DEPOSIT_VALUE, SDK.DepositInfoSchema)
      : kind === "withdrawal"
        ? Buffer.from(
            SDK.committedWithdrawalValueBytes(WITHDRAWAL_VALUE),
            "hex",
          )
        : encodeData(forcedValue, SDK.ForcedInclusionTxV1Schema);
  const counted = await buildCountedRoot(DOMAIN[kind], [
    { key: keyBytes, value: valueBytes },
  ]);
  const eventKeyValue: SDK.EventKey =
    kind === "deposit"
      ? { DepositEventKey: { deposit_id: eventKey } }
      : kind === "withdrawal"
        ? { WithdrawalEventKey: { withdrawal_id: eventKey } }
        : { ForcedTransactionEventKey: { tx_order_id: eventKey } };
  const phase: SDK.TransitionPhase =
    kind === "deposit"
      ? "Deposit"
      : kind === "withdrawal"
        ? "Withdrawal"
        : "ForcedTransaction";
  const eventToStep = sortEntries([
    ...base.payload.block_body.event_to_step,
    [
      encodeData(eventKeyValue, SDK.EventKeySchema).toString("hex"),
      encodeData(
        { step_index: 1n, phase } satisfies SDK.EventToStepValue,
        SDK.EventToStepValueSchema,
      ).toString("hex"),
    ],
  ]);
  const eventToStepRoot = await buildCountedRoot(
    SDK.ROOT_DOMAINS.eventToStep,
    bufferEntries(eventToStep),
  );
  const validationTraces =
    kind === "forced-transaction"
      ? sortEntries([
          ...base.payload.block_body.validation_traces,
          [
            encodeData(eventKeyValue, SDK.EventKeySchema).toString("hex"),
            encodeData(
              {
                schema_version: 1n,
                machine_version: 1n,
                trace_root: "c1".repeat(32),
                step_count: 1n,
                initial_state_hash: "c2".repeat(32),
                terminal_state_hash: "c3".repeat(32),
                verdict: "Accepted",
                rejection_code_hash: "c4".repeat(32),
              } satisfies SDK.ValidationTraceDescriptor,
              SDK.ValidationTraceDescriptorSchema,
            ).toString("hex"),
          ],
        ])
      : base.payload.block_body.validation_traces;
  const validationTracesRoot = await buildCountedRoot(
    SDK.ROOT_DOMAINS.validationTraces,
    bufferEntries(validationTraces),
  );
  const counts: typeof base.payload.block_body.counts = {
    ...base.payload.block_body.counts,
    depositCount: kind === "deposit" ? 1n : 0n,
    withdrawalCount: kind === "withdrawal" ? 1n : 0n,
    forcedTransactionCount: kind === "forced-transaction" ? 1n : 0n,
    totalEventCount: base.payload.block_body.counts.totalEventCount + 1n,
    validationTraceCount: BigInt(validationTraces.length),
  };
  const header: SDK.Header = {
    ...base.header,
    depositsRoot: kind === "deposit" ? counted.root : base.header.depositsRoot,
    withdrawalsRoot:
      kind === "withdrawal" ? counted.root : base.header.withdrawalsRoot,
    forcedTransactionsRoot:
      kind === "forced-transaction"
        ? counted.root
        : base.header.forcedTransactionsRoot,
    eventToStepRoot: eventToStepRoot.root,
    validationTracesRoot: validationTracesRoot.root,
    ...counts,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const eventEntries: SDK.DaPayloadEntry[] = [
    [keyBytes.toString("hex"), valueBytes.toString("hex")],
  ];
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header,
      header_hash: headerHash,
      deposits: kind === "deposit" ? eventEntries : [],
      withdrawals: kind === "withdrawal" ? eventEntries : [],
      forced_transactions: kind === "forced-transaction" ? eventEntries : [],
      forced_transaction_preimages:
        kind === "forced-transaction"
          ? [[keyBytes.toString("hex"), forcedTx.canonicalCbor.toString("hex")]]
          : [],
      event_to_step: eventToStep,
      validation_traces: validationTraces,
      counts,
    },
  };
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({ header, headerHash }),
    payloadEnvelopeCbor: await reencodeFixturePayload(payload),
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "cross-block-duplicate-event-test",
      grade: "security",
    },
  });
  return { evidence, committedRoot: counted.root, valueBytes };
};

const settlementEvidence = ({
  headerHash,
  root,
  kind,
}: {
  readonly headerHash: string;
  readonly root: string;
  readonly kind: EventKind;
}): AuthenticatedSettlementEvidence => ({
  observation: {
    schemaVersion: SDK.CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
    sourceMode: "local_node",
    provenance: {
      trustClass: "authenticated_cardano_l1",
      sourceId: "local-cardano-node",
      grade: "security",
    },
    chainPoint: { slot: 1n, blockHash: "81".repeat(32) },
    confirmationDepth: 1,
  },
  policyId: SETTLEMENT_POLICY_ID,
  assetName: headerHash,
  live: true,
  datum: {
    deposits_root: kind === "deposit" ? root : SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawals_root: kind === "withdrawal" ? root : SDK.EMPTY_MERKLE_TREE_ROOT,
    forced_transactions_root:
      kind === "forced-transaction" ? root : SDK.EMPTY_MERKLE_TREE_ROOT,
    transactions_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    resolution_claim: null,
  },
});

/** The digest the compacted proof must carry: hash of the committed leaf bytes. */
const expectedValueDigest = (valueBytes: Buffer): string =>
  computeHash32(
    Buffer.from(
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        valueBytes.toString("hex"),
      ),
      "hex",
    ),
  ).toString("hex");

describe.each(["deposit", "withdrawal", "forced-transaction"] as const)(
  "cross-block duplicate %s preparation",
  (kind) => {
    it("opens the same committed event in both authenticated blocks", async () => {
      const challenged = await buildEventBlock({
        kind,
        prevHeaderHash: "83".repeat(28),
      });
      const settled = await buildEventBlock({
        kind,
        prevHeaderHash: "84".repeat(28),
      });
      expect(challenged.evidence.headerHash).not.toBe(
        settled.evidence.headerHash,
      );

      const prepared = await prepareCrossBlockDuplicateEvent({
        challenged: challenged.evidence,
        settled: settled.evidence,
        settlement: settlementEvidence({
          headerHash: settled.evidence.headerHash,
          root: settled.committedRoot,
          kind,
        }),
        kind,
        eventKey: EVENT_KEY,
      });

      expect(prepared.challengedHeaderHash).toBe(
        challenged.evidence.headerHash,
      );
      expect(prepared.settledHeaderHash).toBe(settled.evidence.headerHash);
      expect(prepared.step02State).toEqual({
        challenged_header_hash: challenged.evidence.headerHash,
        settlement_policy_id: SETTLEMENT_POLICY_ID,
        event_kind: EVENT_KIND[kind],
        event_key: EVENT_KEY,
      });

      // Each opening must name its own block's L1-committed counted root, the
      // right domain and cardinality, the challenged event key, and the digest
      // of the exact leaf bytes that block committed.
      const digest = expectedValueDigest(challenged.valueBytes);
      expect(settled.valueBytes.equals(challenged.valueBytes)).toBe(true);
      for (const [label, proof, block] of [
        ["challenged", prepared.challengedEvent, challenged],
        ["settled", prepared.settledEvent, settled],
      ] as const) {
        expect(proof, label).toHaveProperty("CommittedDuplicateEventDigestV1");
        const digestProof = proof as Extract<
          SDK.CommittedDuplicateEventProof,
          { CommittedDuplicateEventDigestV1: unknown }
        >;
        expect(
          digestProof.CommittedDuplicateEventDigestV1.event_kind,
          label,
        ).toBe(EVENT_KIND[kind]);
        const membership =
          digestProof.CommittedDuplicateEventDigestV1.membership;
        expect(membership.domain, label).toBe(DOMAIN[kind]);
        expect(membership.root, label).toBe(block.committedRoot);
        expect(membership.count, label).toBe(1n);
        expect(membership.key, label).toEqual(EVENT_KEY);
        expect(membership.value, label).toBe(digest);
      }
    });

    it("refuses a block that does not commit the challenged event key", async () => {
      const challenged = await buildEventBlock({
        kind,
        prevHeaderHash: "83".repeat(28),
      });
      const settled = await buildEventBlock({
        kind,
        prevHeaderHash: "84".repeat(28),
        eventKey: { transactionId: "7a".repeat(32), outputIndex: 5n },
      });
      await expect(
        prepareCrossBlockDuplicateEvent({
          challenged: challenged.evidence,
          settled: settled.evidence,
          settlement: settlementEvidence({
            headerHash: settled.evidence.headerHash,
            root: settled.committedRoot,
            kind,
          }),
          kind,
          eventKey: EVENT_KEY,
        }),
      ).rejects.toThrow(/is absent from one canonical block/u);
    });
  },
);

describe("cross-block duplicate settlement admission", () => {
  it("rejects same-header, burned, misbound, and root-forged history", async () => {
    const challenged = await buildEventBlock({
      kind: "deposit",
      prevHeaderHash: "85".repeat(28),
    });
    const settled = await buildEventBlock({
      kind: "deposit",
      prevHeaderHash: "86".repeat(28),
    });
    const authentic = settlementEvidence({
      headerHash: settled.evidence.headerHash,
      root: settled.committedRoot,
      kind: "deposit",
    });
    const base = {
      challenged: challenged.evidence,
      settled: settled.evidence,
      settlement: authentic,
      kind: "deposit" as const,
      eventKey: EVENT_KEY,
    };
    // The base scenario is a demonstrated accept, so each refusal below is
    // caused by the one condition it introduces.
    await expect(prepareCrossBlockDuplicateEvent(base)).resolves.toHaveProperty(
      "step02State.event_kind",
      "DuplicateDepositV1",
    );
    await expect(
      prepareCrossBlockDuplicateEvent({
        ...base,
        settled: challenged.evidence,
        settlement: {
          ...authentic,
          assetName: challenged.evidence.headerHash,
        },
      }),
    ).rejects.toThrow(/same header/u);
    await expect(
      prepareCrossBlockDuplicateEvent({
        ...base,
        settlement: { ...authentic, live: false },
      }),
    ).rejects.toThrow(/no longer live/u);
    await expect(
      prepareCrossBlockDuplicateEvent({
        ...base,
        settlement: { ...authentic, assetName: "87".repeat(28) },
      }),
    ).rejects.toThrow(/does not bind/u);
    await expect(
      prepareCrossBlockDuplicateEvent({
        ...base,
        settlement: {
          ...authentic,
          datum: { ...authentic.datum, deposits_root: "88".repeat(32) },
        },
      }),
    ).rejects.toThrow(/does not preserve/u);
  });
});
