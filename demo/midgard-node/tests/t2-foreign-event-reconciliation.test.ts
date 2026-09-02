import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { computeDaPayloadRoots } from "@/workers/commit-block-header/da-payload.js";
import {
  resolveT2ForeignEventEvidence,
  type T2CandidateEventIds,
} from "@/workers/t2-foreign-event-reconciliation.js";

const emptyIds = (): T2CandidateEventIds => ({
  deposits: [],
  forcedTransactions: [],
  withdrawals: [],
});

const headerFor = (overrides: Partial<SDK.HeaderV1> = {}): SDK.HeaderV1 => ({
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
  validationTraceCount: 0n,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "11".repeat(28),
  operatorVkey: "22".repeat(28),
  protocolVersion: 1n,
  ...overrides,
});

const resolve = async ({
  header,
  candidateIds,
  payload,
}: {
  readonly header: SDK.HeaderV1;
  readonly candidateIds: T2CandidateEventIds;
  readonly payload?: SDK.DaPayloadV1;
}) =>
  Effect.runPromise(
    SDK.hashBlockHeaderV1(header).pipe(
      Effect.flatMap((foreignHeaderHash) =>
        resolveT2ForeignEventEvidence({
          foreignHeaderHash,
          header,
          candidateIds,
          payload,
        }),
      ),
    ),
  );

const oneDepositPayload = async (depositId: string) => {
  const counts: SDK.DaPayloadCountsV1 = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  };
  const draft: SDK.DaPayloadV1 = {
    version: SDK.DA_PAYLOAD_V1_VERSION,
    block_body: {
      header_hash: "00".repeat(28),
      header: headerFor(),
      utxos: [],
      withdrawals: [],
      forced_transactions: [],
      transactions: [],
      deposits: [[depositId, "01"]],
      transition_trace: [],
      event_to_step: [],
      transaction_preimages: [],
      forced_transaction_preimages: [],
      cek_program_material: [],
      validation_traces: [],
      validation_trace_witnesses: [],
      counts,
    },
  };
  const roots = await Effect.runPromise(computeDaPayloadRoots(draft));
  const header = headerFor({
    utxosRoot: roots.utxosRoot,
    withdrawalsRoot: roots.withdrawalsRoot,
    forcedTransactionsRoot: roots.forcedTransactionsRoot,
    transactionsRoot: roots.transactionsRoot,
    depositsRoot: roots.depositsRoot,
    transitionTraceRoot: roots.transitionTraceRoot,
    eventToStepRoot: roots.eventToStepRoot,
    validationTracesRoot: roots.validationTracesRoot,
    depositCount: 1n,
    totalEventCount: 1n,
    validationTraceCount: 0n,
  });
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  return {
    header,
    payload: {
      ...draft,
      block_body: {
        ...draft.block_body,
        header_hash: headerHash,
        header,
      },
    } satisfies SDK.DaPayloadV1,
  };
};

describe("T2 foreign event reconciliation evidence", () => {
  it("proves candidate absence from empty category roots without DA", async () => {
    const header = headerFor();
    const candidateIds = {
      deposits: ["aa".repeat(32)],
      forcedTransactions: ["bb".repeat(32)],
      withdrawals: ["cc".repeat(32)],
    };
    await expect(
      resolve({
        header,
        candidateIds,
      }),
    ).resolves.toEqual({
      type: "Ready",
      absent: candidateIds,
    });
  });

  it("awaits DA for a non-empty category root", async () => {
    const header = headerFor({
      depositsRoot: "33".repeat(32),
      depositCount: 1n,
      totalEventCount: 1n,
    });
    const result = await resolve({
      header,
      candidateIds: { ...emptyIds(), deposits: ["aa".repeat(32)] },
    });
    expect(result.type).toBe("AwaitingForeignDa");
    if (result.type === "AwaitingForeignDa")
      expect(result.reason).toBe("missing");
  });

  it("rejects an empty category root with a nonzero count before any local event is visible", async () => {
    const header = headerFor({
      depositCount: 1n,
      totalEventCount: 1n,
    });
    const result = await resolve({
      header,
      candidateIds: emptyIds(),
    });
    expect(result.type).toBe("AwaitingForeignDa");
    if (result.type === "AwaitingForeignDa") {
      expect(result.reason).toBe("invalid");
      expect(result.detail).toBe(
        "foreign header event root/count evidence is inconsistent",
      );
    }
  });

  it("rejects DA whose header binding or roots are invalid", async () => {
    const { header, payload } = await oneDepositPayload("aa".repeat(32));
    const result = await resolve({
      header,
      candidateIds: { ...emptyIds(), deposits: ["bb".repeat(32)] },
      payload: {
        ...payload,
        block_body: { ...payload.block_body, header_hash: "ff".repeat(28) },
      },
    });
    expect(result.type).toBe("AwaitingForeignDa");
    if (result.type === "AwaitingForeignDa")
      expect(result.reason).toBe("invalid");
  });

  it("defers foreign-present events and releases proven-absent events", async () => {
    const presentId = "aa".repeat(32);
    const absentId = "bb".repeat(32);
    const { header, payload } = await oneDepositPayload(presentId);
    const present = await resolve({
      header,
      candidateIds: { ...emptyIds(), deposits: [presentId] },
      payload,
    });
    expect(present.type).toBe("AwaitingForeignDa");
    if (present.type === "AwaitingForeignDa") {
      expect(present.reason).toBe(
        "foreign_event_present_requires_finalization",
      );
      expect(present.present.deposits).toEqual([presentId]);
    }
    await expect(
      resolve({
        header,
        candidateIds: { ...emptyIds(), deposits: [absentId] },
        payload,
      }),
    ).resolves.toEqual({
      type: "Ready",
      absent: { ...emptyIds(), deposits: [absentId] },
    });
  });
});
