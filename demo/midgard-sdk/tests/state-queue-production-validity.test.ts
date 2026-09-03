import type { LucidEvolution, TxBuilder, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { castActiveOperatorDatumToData } from "../src/active-operators.js";
import {
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
} from "../src/ledger-constants.js";
import {
  castConfirmedStateToData,
  EMPTY_HEADER_TRANSITION_COMMITMENTS,
  type Header,
  makeGenesisConfirmedState,
} from "../src/ledger-state.js";
import {
  encodeLinkedListNodeView,
  type LinkedListNodeView,
} from "../src/linked-list.js";
import {
  STATE_QUEUE_ROOT_ASSET_NAME,
  type StateQueueUTxO,
} from "../src/state-queue.js";
import {
  buildCommitBlockHeaderTxProgram,
  buildMergeToConfirmedStateTxProgram,
  COMMIT_MAX_VALIDITY_RANGE_MS,
  type CommitBlockHeaderParams,
  commitHeaderMatchesValidityUpperBound,
  isCommitValidityInterval,
  type MergeToConfirmedStateParams,
} from "../src/state-queue-production.js";

const DUMMY_ADDRESS =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";
const DUMMY_POLICY_ID = "11".repeat(28);
const OPERATOR_KEY_HASH = "22".repeat(28);
const DUMMY_SCRIPT = { type: "PlutusV3", script: "00" } as const;

const makeUtxo = (
  txHash: string,
  outputIndex: number,
  assets: UTxO["assets"],
): UTxO => ({
  txHash,
  outputIndex,
  address: DUMMY_ADDRESS,
  assets,
  datum: undefined,
  datumHash: undefined,
  scriptRef: undefined,
});

const makeCommitParams = (
  lucid: LucidEvolution,
  validFrom: number,
  validTo: number,
): CommitBlockHeaderParams => {
  const updatedNodeDatum: LinkedListNodeView = {
    key: "Empty",
    next: { Key: { key: "aa".repeat(28) } },
    data: castConfirmedStateToData(
      makeGenesisConfirmedState(0n),
    ) as LinkedListNodeView["data"],
  };
  const activeOperatorInput = {
    ...makeUtxo("33".repeat(32), 0, { lovelace: 10_000_000n }),
    datum: encodeLinkedListNodeView({
      key: "Empty",
      next: "Empty",
      data: castActiveOperatorDatumToData({
        bond_unlock_time: null,
        inactivity_strikes: 0n,
      }) as LinkedListNodeView["data"],
    }),
  } as UTxO & { readonly datum: string };
  const latestBlockUtxo = makeUtxo("44".repeat(32), 0, {
    lovelace: 10_000_000n,
    [`${DUMMY_POLICY_ID}${STATE_QUEUE_ROOT_ASSET_NAME}`]: 1n,
  });
  const latestBlock: StateQueueUTxO = {
    utxo: latestBlockUtxo,
    datum: updatedNodeDatum,
    assetName: STATE_QUEUE_ROOT_ASSET_NAME,
  };
  const newHeader: Header = {
    prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
    ...EMPTY_HEADER_TRANSITION_COMMITMENTS,
    transactionsRoot: EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: EMPTY_MERKLE_TREE_ROOT,
    startTime: 0n,
    endTime: BigInt(validTo - 1),
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: GENESIS_HEADER_HASH,
    operatorVkey: OPERATOR_KEY_HASH,
    protocolVersion: 1n,
  };
  const stateQueueValidator = {
    mintingScriptCBOR: "00",
    mintingScript: DUMMY_SCRIPT,
    policyId: DUMMY_POLICY_ID,
    spendingScriptCBOR: "00",
    spendingScript: DUMMY_SCRIPT,
    spendingScriptHash: OPERATOR_KEY_HASH,
    spendingScriptAddress: DUMMY_ADDRESS,
    yields: {
      commit: {
        withdrawalScriptCBOR: "00",
        withdrawalScript: DUMMY_SCRIPT,
        withdrawalScriptHash: OPERATOR_KEY_HASH,
      },
    },
  };
  const activeOperatorsValidator = {
    ...stateQueueValidator,
    spendingScriptAddress: `${DUMMY_ADDRESS}active`,
  };

  return {
    lucid,
    contracts: {
      stateQueue: stateQueueValidator,
      activeOperators: activeOperatorsValidator,
    } as unknown as CommitBlockHeaderParams["contracts"],
    latestBlock,
    updatedNodeDatum,
    newHeader,
    validFrom,
    validTo,
    witness: {
      operatorKeyHash: OPERATOR_KEY_HASH,
      schedulerRefInput: makeUtxo("55".repeat(32), 0, {
        lovelace: 1_000_000n,
      }),
      hubOracleRefInput: makeUtxo("66".repeat(32), 0, {
        lovelace: 1_000_000n,
      }),
      correctionLockRefInput: {
        utxo: makeUtxo("68".repeat(32), 0, { lovelace: 1_000_000n }),
        datum: "Idle",
        assetName: "MIDGARD_CORRECTION_LOCK",
      },
      stateQueueCommitYieldScriptRef: makeUtxo("69".repeat(32), 0, {
        lovelace: 1_000_000n,
      }),
      activeOperatorInput,
      activeOperatorsSpendingScript: DUMMY_SCRIPT,
      operatorWalletView: {
        knownUtxos: [makeUtxo("77".repeat(32), 0, { lovelace: 10_000_000n })],
        consumedOutRefs: [],
      },
    },
  };
};

const makeValidityRecordingLucid = () => {
  // Assigned after the recursive spies are created so they can return it.
  // eslint-disable-next-line prefer-const
  let txBuilder: TxBuilder;
  const validFrom = vi.fn((_: number) => txBuilder);
  const validTo = vi.fn((_: number) => {
    throw new Error("stop after recording the validity interval");
  });
  txBuilder = { validFrom, validTo } as unknown as TxBuilder;
  const newTx = vi.fn(() => txBuilder);
  return {
    lucid: {
      newTx,
      config: () => ({ network: "Preprod" }),
    } as unknown as LucidEvolution,
    newTx,
    validFrom,
    validTo,
  };
};

describe("production commit validity binding", () => {
  it("passes the exact bounded interval to the Lucid transaction builder", async () => {
    const validFrom = 1_000_000;
    const validTo = validFrom + COMMIT_MAX_VALIDITY_RANGE_MS;
    const recordingLucid = makeValidityRecordingLucid();

    const result = await Effect.runPromiseExit(
      buildCommitBlockHeaderTxProgram(
        makeCommitParams(recordingLucid.lucid, validFrom, validTo),
      ),
    );

    expect(result._tag).toBe("Failure");
    expect(recordingLucid.newTx).toHaveBeenCalledTimes(1);
    expect(recordingLucid.validFrom).toHaveBeenCalledWith(validFrom);
    expect(recordingLucid.validTo).toHaveBeenCalledWith(validTo);
  });

  it("rejects an equal validity bound before opening a transaction builder", async () => {
    const validTo = 1_480_000;
    const newTx = vi.fn(() => {
      throw new Error("newTx should not be called for an equal bound");
    });

    const result = await Effect.runPromiseExit(
      buildCommitBlockHeaderTxProgram(
        makeCommitParams(
          { newTx } as unknown as LucidEvolution,
          validTo,
          validTo,
        ),
      ),
    );

    expect(result._tag).toBe("Failure");
    expect(newTx).not.toHaveBeenCalled();
  });

  it("rejects append while the authenticated correction lock is held", async () => {
    const validFrom = 1_000_000;
    const validTo = validFrom + COMMIT_MAX_VALIDITY_RANGE_MS;
    const newTx = vi.fn(() => {
      throw new Error("newTx should not be called while correction is locked");
    });
    const params = makeCommitParams(
      { newTx } as unknown as LucidEvolution,
      validFrom,
      validTo,
    );

    const result = await Effect.runPromiseExit(
      buildCommitBlockHeaderTxProgram({
        ...params,
        witness: {
          ...params.witness,
          correctionLockRefInput: {
            ...params.witness.correctionLockRefInput,
            datum: {
              Locked: {
                target_header_hash: "99".repeat(28),
                correction_identity: "AttestationTimeout",
              },
            },
          },
        },
      }),
    );

    expect(result._tag).toBe("Failure");
    expect(newTx).not.toHaveBeenCalled();
  });

  it("rejects merge while the authenticated correction lock is held", async () => {
    const newTx = vi.fn(() => {
      throw new Error("newTx should not be called while correction is locked");
    });
    const lockedDatum = {
      Locked: {
        target_header_hash: "99".repeat(28),
        correction_identity: "AttestationTimeout" as const,
      },
    };
    const params = {
      lucid: { newTx } as unknown as LucidEvolution,
      correctionLockRefInput: {
        utxo: makeUtxo("98".repeat(32), 0, { lovelace: 2_000_000n }),
        datum: lockedDatum,
        assetName: "MIDGARD_CORRECTION_LOCK",
      },
    } as unknown as MergeToConfirmedStateParams;

    const result = await Effect.runPromiseExit(
      buildMergeToConfirmedStateTxProgram(params),
    );

    expect(result._tag).toBe("Failure");
    expect(newTx).not.toHaveBeenCalled();
  });

  it("accepts a bounded interval and binds the header to its inclusive upper bound", () => {
    const validFrom = 1_000_000;
    const validTo = validFrom + COMMIT_MAX_VALIDITY_RANGE_MS;

    expect(isCommitValidityInterval({ validFrom, validTo })).toBe(true);
    expect(
      commitHeaderMatchesValidityUpperBound({
        headerEndTime: BigInt(validTo - 1),
        validTo,
      }),
    ).toBe(true);
  });

  it("rejects absent, empty, unsafe, and overlong lower-bound intervals", () => {
    const validFrom = 1_000_000;

    expect(
      isCommitValidityInterval({
        validFrom: Number.NaN,
        validTo: validFrom + 1,
      }),
    ).toBe(false);
    expect(
      isCommitValidityInterval({
        validFrom,
        validTo: validFrom,
      }),
    ).toBe(false);
    expect(
      isCommitValidityInterval({
        validFrom,
        validTo: validFrom + COMMIT_MAX_VALIDITY_RANGE_MS + 1,
      }),
    ).toBe(false);
  });

  it("rejects a header end-time that differs from validTo minus one", () => {
    const validTo = 1_480_000;

    expect(
      commitHeaderMatchesValidityUpperBound({
        headerEndTime: BigInt(validTo),
        validTo,
      }),
    ).toBe(false);
  });
});
