import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type AuthenticatedSettlementEvidence,
  prepareCrossBlockDuplicateEvent,
} from "../src/cross-block-duplicate-event/index.js";
import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  buildCountedRoot,
  type CountedRoot,
} from "../src/transition-trace/phas.js";

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
const FORCED_TRANSACTION_VALUE: SDK.ForcedInclusionTxV1 = {
  tx_id: "78".repeat(32),
  source: {
    compact_cbor: "80",
    witness_set_compact_cbor: "80",
    field_preimage_lengths_cbor: "80",
  },
  verdict: "ForcedTxValid",
};

const L1_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-cardano-node",
  grade: "security",
};

const emptyRoot = (domain: SDK.RootDomain): CountedRoot => ({
  domain,
  root: SDK.EMPTY_MERKLE_TREE_ROOT,
  phasRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  count: 0n,
  entries: [],
});

const evidence = async (
  kind: "deposit" | "withdrawal" | "forced-transaction",
  headerHash: string,
): Promise<{
  readonly block: CanonicalBlockEvidence;
  readonly root: string;
}> => {
  const keyBytes = Buffer.from(Data.to(EVENT_KEY, SDK.OutputReference), "hex");
  const value =
    kind === "deposit"
      ? DEPOSIT_VALUE
      : kind === "withdrawal"
        ? WITHDRAWAL_VALUE
        : FORCED_TRANSACTION_VALUE;
  const valueSchema =
    kind === "deposit"
      ? SDK.DepositInfo
      : kind === "withdrawal"
        ? SDK.WithdrawalInfo
        : SDK.ForcedInclusionTxV1;
  const valueBytes = Buffer.from(
    Data.to(value as never, valueSchema as never),
    "hex",
  );
  const domain =
    kind === "deposit"
      ? SDK.ROOT_DOMAINS.deposits
      : kind === "withdrawal"
        ? SDK.ROOT_DOMAINS.withdrawals
        : SDK.ROOT_DOMAINS.forcedTransactionsV1;
  const counted = await buildCountedRoot(domain, [
    { key: keyBytes, value: valueBytes },
  ]);
  const entry = { key: EVENT_KEY, value, keyBytes, valueBytes };
  const deposits = kind === "deposit" ? [entry] : [];
  const withdrawals = kind === "withdrawal" ? [entry] : [];
  const forcedTransactions = kind === "forced-transaction" ? [entry] : [];
  const block = {
    provenance: { l1: L1_PROVENANCE },
    headerHash,
    header: {
      depositsRoot:
        kind === "deposit" ? counted.root : SDK.EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot:
        kind === "withdrawal" ? counted.root : SDK.EMPTY_MERKLE_TREE_ROOT,
      forcedTransactionsRoot:
        kind === "forced-transaction"
          ? counted.root
          : SDK.EMPTY_MERKLE_TREE_ROOT,
    },
    reconstruction: {
      deposits,
      withdrawals,
      forcedTransactions,
      rootData: {
        deposits:
          kind === "deposit" ? counted : emptyRoot(SDK.ROOT_DOMAINS.deposits),
        withdrawals:
          kind === "withdrawal"
            ? counted
            : emptyRoot(SDK.ROOT_DOMAINS.withdrawals),
        forcedTransactions:
          kind === "forced-transaction"
            ? counted
            : emptyRoot(SDK.ROOT_DOMAINS.forcedTransactionsV1),
      },
    },
  } as unknown as CanonicalBlockEvidence;
  return { block, root: counted.root };
};

const settlementEvidence = ({
  headerHash,
  root,
  kind,
}: {
  readonly headerHash: string;
  readonly root: string;
  readonly kind: "deposit" | "withdrawal" | "forced-transaction";
}): AuthenticatedSettlementEvidence => ({
  observation: {
    schemaVersion: SDK.CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
    sourceMode: "local_node",
    provenance: L1_PROVENANCE,
    chainPoint: { slot: 1n, blockHash: "81".repeat(32) },
    confirmationDepth: 1,
  },
  policyId: "82".repeat(28),
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

describe.each(["deposit", "withdrawal", "forced-transaction"] as const)(
  "cross-block duplicate %s preparation",
  (kind) => {
    it("builds two canonical counted-root openings", async () => {
      const challenged = await evidence(kind, "83".repeat(28));
      const settled = await evidence(kind, "84".repeat(28));
      const prepared = await prepareCrossBlockDuplicateEvent({
        challenged: challenged.block,
        settled: settled.block,
        settlement: settlementEvidence({
          headerHash: settled.block.headerHash,
          root: settled.root,
          kind,
        }),
        kind,
        eventKey: EVENT_KEY,
      });
      expect(prepared.challengedHeaderHash).toBe(challenged.block.headerHash);
      expect(prepared.settledHeaderHash).toBe(settled.block.headerHash);
      expect(prepared.step02State.event_key).toEqual(EVENT_KEY);
    });
  },
);

describe("cross-block duplicate settlement admission", () => {
  it("rejects same-header, burned, misbound, and root-forged history", async () => {
    const challenged = await evidence("deposit", "85".repeat(28));
    const settled = await evidence("deposit", "86".repeat(28));
    const authentic = settlementEvidence({
      headerHash: settled.block.headerHash,
      root: settled.root,
      kind: "deposit",
    });
    const base = {
      challenged: challenged.block,
      settled: settled.block,
      settlement: authentic,
      kind: "deposit" as const,
      eventKey: EVENT_KEY,
    };
    await expect(
      prepareCrossBlockDuplicateEvent({
        ...base,
        settled: challenged.block,
        settlement: { ...authentic, assetName: challenged.block.headerHash },
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
