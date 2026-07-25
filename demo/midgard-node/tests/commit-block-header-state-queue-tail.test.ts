import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { Lucid as RuntimeLucid } from "@/services/index.js";
import { resolveLiveTailCommitBase } from "@/workers/commit-block-header/pending-journal.js";
import { fetchExpectedStateQueueTailLocal } from "@/workers/commit-block-header/state-queue.js";

const policyId = "aa".repeat(28);
const stateQueueAddress =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

const headerFixture = (overrides: Partial<SDK.HeaderV1> = {}): SDK.HeaderV1 => ({
  prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  utxosRoot: "11".repeat(32),
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
  startTime: 1_000n,
  endTime: 2_000n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: "22".repeat(28),
  operatorVkey: "33".repeat(28),
  protocolVersion: 1n,
  ...overrides,
});

const makeTail = async ({
  txHash = "44".repeat(32),
  outputIndex = 0,
  header = headerFixture(),
  next = "Empty",
}: {
  readonly txHash?: string;
  readonly outputIndex?: number;
  readonly header?: SDK.HeaderV1;
  readonly next?: SDK.LinkedListNodeView["next"];
} = {}): Promise<SDK.StateQueueUTxO> => {
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  const assetName = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash;
  const datum: SDK.LinkedListNodeView = {
    key: { Key: { key: headerHash } },
    next,
    data: SDK.castStateQueueNodeV1ToData({
      header,
      da_attestation: SDK.NO_DA_ATTESTATION,
    }) as SDK.LinkedListNodeView["data"],
  };
  const utxo: UTxO = {
    txHash,
    outputIndex,
    address: stateQueueAddress,
    assets: {
      lovelace: 3_000_000n,
      [toUnit(policyId, assetName)]: 1n,
    },
    datum: SDK.encodeLinkedListNodeView(datum),
  };
  return { utxo, datum, assetName };
};

const config: SDK.StateQueueFetchConfig = {
  stateQueueAddress,
  stateQueuePolicyId: policyId,
};

const contracts = {
  stateQueue: {
    spendingScriptAddress: stateQueueAddress,
    policyId,
  },
} as unknown as SDK.MidgardValidators;

const fakeLucid = (candidates: readonly UTxO[]) => {
  const utxosAt = vi.fn(() =>
    Promise.reject(new Error("address-wide lookup must not be called")),
  );
  const utxosAtWithUnit = vi.fn().mockResolvedValue(candidates);
  return {
    api: { utxosAt, utxosAtWithUnit } as unknown as LucidEvolution,
    utxosAt,
    utxosAtWithUnit,
  };
};

describe("commit-block expected state-queue tail lookup", () => {
  it("uses only the exact expected NFT unit for an unchanged tail", async () => {
    const expected = await makeTail();
    const lucid = fakeLucid([expected.utxo]);

    const actual = await Effect.runPromise(
      fetchExpectedStateQueueTailLocal(lucid.api, config, expected),
    );

    expect(actual).toBe(expected);
    expect(lucid.utxosAtWithUnit).toHaveBeenCalledWith(
      stateQueueAddress,
      toUnit(policyId, expected.assetName),
    );
    expect(lucid.utxosAt).not.toHaveBeenCalled();
  });

  it("accepts an out-ref replacement that preserves the logical tail header", async () => {
    const expected = await makeTail();
    const replacement = await makeTail({
      txHash: "55".repeat(32),
      outputIndex: 1,
    });
    const lucid = fakeLucid([replacement.utxo]);

    const actual = await Effect.runPromise(
      resolveLiveTailCommitBase(
        contracts,
        expected,
        MIDGARD_CONSENSUS_PROFILE_V1,
      ).pipe(
        Effect.provideService(RuntimeLucid, lucid as unknown as RuntimeLucid),
      ),
    );

    expect(actual.utxo.txHash).toBe(replacement.utxo.txHash);
    expect(actual.utxo.outputIndex).toBe(replacement.utxo.outputIndex);
    expect(lucid.utxosAt).not.toHaveBeenCalled();
  });

  it("classifies the expected NFT becoming a non-tail as a stale commit base", async () => {
    const expected = await makeTail();
    const advanced = await makeTail({
      txHash: "66".repeat(32),
      next: { Key: { key: "77".repeat(28) } },
    });
    const lucid = fakeLucid([advanced.utxo]);

    const outcome = await Effect.runPromise(
      Effect.either(
        resolveLiveTailCommitBase(
          contracts,
          expected,
          MIDGARD_CONSENSUS_PROFILE_V1,
        ).pipe(
          Effect.provideService(RuntimeLucid, lucid as unknown as RuntimeLucid),
        ),
      ),
    );
    expect(outcome).toMatchObject({
      _tag: "Left",
      left: {
        _tag: "StateQueueError",
        message:
          "Commit base is stale; aborting block build before creating a pending journal",
      },
    });
    expect(lucid.utxosAt).not.toHaveBeenCalled();
  });

  it("fails closed when the exact expected unit has zero or multiple matches", async () => {
    const expected = await makeTail();
    const duplicate = await makeTail({ txHash: "88".repeat(32) });

    const missingLucid = fakeLucid([]);
    const missingOutcome = await Effect.runPromise(
      Effect.either(
        resolveLiveTailCommitBase(
          contracts,
          expected,
          MIDGARD_CONSENSUS_PROFILE_V1,
        ).pipe(
          Effect.provideService(
            RuntimeLucid,
            missingLucid as unknown as RuntimeLucid,
          ),
        ),
      ),
    );
    expect(missingOutcome).toMatchObject({
      _tag: "Left",
      left: {
        _tag: "StateQueueError",
        message:
          "Commit base is stale; aborting block build before creating a pending journal",
      },
    });

    const duplicateLucid = fakeLucid([expected.utxo, duplicate.utxo]);
    const duplicateOutcome = await Effect.runPromise(
      Effect.either(
        fetchExpectedStateQueueTailLocal(duplicateLucid.api, config, expected),
      ),
    );
    expect(duplicateOutcome).toMatchObject({
      _tag: "Left",
      left: {
        _tag: "StateQueueError",
        message: "Expected state-queue tail unit is not unique",
        cause: expect.stringContaining("matches=2"),
      },
    });

    expect(missingLucid.utxosAt).not.toHaveBeenCalled();
    expect(duplicateLucid.utxosAt).not.toHaveBeenCalled();
  });
});
