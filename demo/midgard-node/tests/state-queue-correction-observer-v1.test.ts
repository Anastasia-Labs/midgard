import { createHash } from "node:crypto";

import {
  CORRECTION_LOCK_ASSET_NAME,
  CorrectionLockDatum,
  deriveStateQueueAuthenticatedReplayCheckpointV1,
  deriveStateQueueAuthenticatedTransitionV1,
  LinkedListDatum,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  type StateQueueAuthenticatedTransitionV1,
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
  type StateQueueTransitionNodeV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  makeLocalKupmiosStateQueueCorrectionSourceV1,
  parseStateQueueCorrectionObserverStateV1,
  reconcileStateQueueCorrectionObserverV1,
  type StateQueueCorrectionObserverSourceV1,
  type StateQueueCorrectionObserverStoreV1,
} from "@/services/state-queue-correction-observer-v1.js";

const h28 = (byte: string): string => byte.repeat(56);
const h32 = (byte: string): string => byte.repeat(64);
const outRef = (byte: string, index = 0): string =>
  `${h32(byte)}#${index.toString()}`;
const deployment = h32("a");
const policy = h28("b");
const target = h28("1");
const descendant = h28("2");
const transactionHash = h32("c");
const correctionLockOutRef = outRef("f");
const hubPolicy = h28("a");
const fraudPolicy = h28("e");
const correctionLockAddress = "addr_test_correction_lock";
const fraudProofAddress = "addr_test_fraud_proof";

const canonicalJson = (value: unknown): string => {
  if (value === null || typeof value !== "object") return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(canonicalJson).join(",")}]`;
  return `{${Object.entries(value as Record<string, unknown>)
    .sort(([left], [right]) => left.localeCompare(right))
    .map(([key, member]) => `${JSON.stringify(key)}:${canonicalJson(member)}`)
    .join(",")}}`;
};
const sha256 = (value: unknown): string =>
  createHash("sha256").update(canonicalJson(value)).digest("hex");

const before: readonly StateQueueTransitionNodeV1[] = [
  { headerHash: null, outRef: outRef("0") },
  { headerHash: target, outRef: outRef("1") },
  { headerHash: descendant, outRef: outRef("2") },
];
const after: readonly StateQueueTransitionNodeV1[] = [
  { headerHash: null, outRef: outRef("0") },
  { headerHash: target, outRef: `${transactionHash}#0` },
];

const authenticatedTransition = (txHash = transactionHash) =>
  deriveStateQueueAuthenticatedTransitionV1({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    transactionHash: txHash,
    blockHash: h32("7"),
    slot: "100",
    blockNo: "90",
    transactionIndex: "0",
    chainPointId: h32("5"),
    finalityDepth: "1",
    mintPolicyIds: [policy],
    referenceInputOutRefs: [],
    correctionLockWitness: {
      kind: "correction_transition",
      consumedOutRef: correctionLockOutRef,
      continuedOutRef: `${txHash}#9`,
      targetHeaderHash: target,
      correctionIdentity: "AttestationTimeout",
      previousDatum: "Idle",
      nextDatum: {
        Locked: {
          target_header_hash: target,
          correction_identity: "AttestationTimeout",
        },
      },
    },
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(
          {
            RemoveUnattestedBlockAfterTimeout: {
              yield_to_ref_input_index: 0n,
              timed_out_header_hash: target,
              removal_approach: {
                PruneTimedOutBlockDescendant: {
                  confirmed_state_ref_input_index: 0n,
                  timed_out_node_input_outref: {
                    transactionId: h32("1"),
                    outputIndex: 0n,
                  },
                  timed_out_node_output_index: 0n,
                },
              },
            },
          } satisfies StateQueueRedeemerType,
          StateQueueRedeemer,
        ),
      },
    ],
    spentInputOutRefs: [outRef("1"), outRef("2"), correctionLockOutRef],
    previousQueue: before,
    nextQueue: [
      { headerHash: null, outRef: outRef("0") },
      { headerHash: target, outRef: `${txHash}#0` },
    ],
  })!;

const authenticatedFraudTransition = () => {
  const txHash = h32("d");
  return deriveStateQueueAuthenticatedTransitionV1({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    transactionHash: txHash,
    blockHash: h32("8"),
    slot: "101",
    blockNo: "91",
    transactionIndex: "0",
    chainPointId: h32("6"),
    finalityDepth: "1",
    mintPolicyIds: [policy],
    referenceInputOutRefs: [],
    correctionLockWitness: {
      kind: "correction_transition",
      consumedOutRef: correctionLockOutRef,
      continuedOutRef: `${txHash}#9`,
      targetHeaderHash: descendant,
      correctionIdentity: {
        FraudProof: { fraud_proof_asset_name: `00000001${descendant}` },
      },
      previousDatum: "Idle",
      nextDatum: "Idle",
    },
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(
          {
            RemoveFraudulentBlockHeader: {
              yield_to_ref_input_index: 0n,
              fraudulent_operator: h28("f"),
              fraudulent_blocks_header_hash: descendant,
              slashing_approach: {
                OperatorAlreadySlashed: {
                  active_operators_element_ref_input_index: 0n,
                  retired_operators_element_ref_input_index: 1n,
                },
              },
              fraud_proof_ref_input_index: 0n,
              block_removal_approach: {
                RemoveLastFraudulentBlock: {
                  anchor_element_input_outref: {
                    transactionId: h32("1"),
                    outputIndex: 0n,
                  },
                  anchor_element_output_index: 0n,
                },
              },
            },
          } satisfies StateQueueRedeemerType,
          StateQueueRedeemer,
        ),
      },
    ],
    spentInputOutRefs: [outRef("1"), outRef("2"), correctionLockOutRef],
    previousQueue: before,
    nextQueue: [
      { headerHash: null, outRef: outRef("0") },
      { headerHash: target, outRef: `${txHash}#0` },
    ],
  })!;
};

const checkpointFromTerminal = (transition = authenticatedTransition()) =>
  deriveStateQueueAuthenticatedReplayCheckpointV1({
    deploymentIdentityDigest: transition.deploymentIdentityDigest,
    stateQueuePolicyId: transition.stateQueuePolicyId,
    transactionHash: transition.transactionHash,
    blockHash: transition.blockHash,
    slot: transition.slot,
    blockNo: transition.blockNo,
    transactionIndex: transition.transactionIndex,
    chainPointId: transition.chainPointId,
    finalityDepth: transition.finalityDepth,
    mintPolicyIds: [transition.stateQueuePolicyId],
    redeemers: [transition.stateQueueMintRedeemer],
    spentInputOutRefs:
      transition.correctionLockWitness.kind === "correction_transition"
        ? [
            ...transition.consumedQueueOutRefs,
            transition.correctionLockWitness.consumedOutRef,
          ]
        : transition.consumedQueueOutRefs,
    referenceInputOutRefs: [],
    correctionLockWitness: transition.correctionLockWitness,
    previousQueue: transition.previousQueue,
    nextQueue: transition.nextQueue,
  })!;

const appendCheckpoint = ({
  transactionByte,
  headerHash,
  previousQueue,
  blockNo,
}: {
  transactionByte: string;
  headerHash: string;
  previousQueue: readonly StateQueueTransitionNodeV1[];
  blockNo: number;
}) => {
  const transaction = h32(transactionByte);
  const priorTail = previousQueue.at(-1)!;
  const nextQueue = [
    ...previousQueue.slice(0, -1),
    { headerHash: priorTail.headerHash, outRef: `${transaction}#0` },
    { headerHash, outRef: `${transaction}#1` },
  ];
  return deriveStateQueueAuthenticatedReplayCheckpointV1({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    transactionHash: transaction,
    blockHash: h32(transactionByte),
    slot: (blockNo + 10).toString(),
    blockNo: blockNo.toString(),
    transactionIndex: "0",
    chainPointId: h32(transactionByte),
    finalityDepth: "30",
    mintPolicyIds: [policy],
    referenceInputOutRefs: [correctionLockOutRef],
    correctionLockWitness: {
      kind: "idle_reference",
      referenceOutRef: correctionLockOutRef,
      datum: "Idle",
    },
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(
          {
            CommitBlockHeader: {
              yield_to_ref_input_index: 0n,
              new_block_output_index: 1n,
              continued_latest_block_output_index: 0n,
              operator: h28("9"),
              scheduler_ref_input_index: 0n,
              active_operators_input_index: 0n,
              active_operators_redeemer_index: 0n,
              m_confirmed_state_ref_input_index: null,
              m_head_state_queue_node_ref_input_index: null,
            },
          } satisfies StateQueueRedeemerType,
          StateQueueRedeemer,
        ),
      },
    ],
    spentInputOutRefs: [priorTail.outRef],
    previousQueue,
    nextQueue,
  })!;
};

const memoryStore = (): StateQueueCorrectionObserverStoreV1 & {
  current: () => unknown | null;
} => {
  let value: unknown | null = null;
  return {
    load: async () => structuredClone(value),
    save: async (next) => {
      value = structuredClone(next);
    },
    current: () => structuredClone(value),
  };
};

const harness = () => {
  let queue = before;
  type Observation = Awaited<
    ReturnType<StateQueueCorrectionObserverSourceV1["observeTransitions"]>
  >[number];
  let observations: readonly Observation[] | "gap" = [checkpointFromTerminal()];
  let depth: bigint | null = 1n;
  const depthByTransaction = new Map<string, bigint | null>();
  const observeTransitions = vi.fn(async () => {
    if (observations === "gap") {
      throw new Error("ordered-transition gap; durable cursor retained");
    }
    return observations;
  });
  const source: StateQueueCorrectionObserverSourceV1 = {
    readQueue: async () => queue,
    observeTransitions,
    canonicalDepth: async (transition) =>
      depthByTransaction.has(transition.transactionHash)
        ? depthByTransaction.get(transition.transactionHash)!
        : depth,
  };
  return {
    source,
    setQueue: (next: readonly StateQueueTransitionNodeV1[]) => {
      queue = next;
    },
    setDepth: (next: bigint | null) => {
      depth = next;
    },
    setTransactionDepth: (txHash: string, next: bigint | null) => {
      depthByTransaction.set(txHash, next);
    },
    setObservation: (next: Observation | "gap") => {
      observations = next === "gap" ? "gap" : [next];
    },
    setObservations: (next: readonly Observation[]) => {
      observations = next;
    },
    observeTransitions,
  };
};

const run = async ({
  source,
  store,
  reinclude,
  restore,
  persistTerminal,
  revokeTerminal,
}: {
  source: StateQueueCorrectionObserverSourceV1;
  store: StateQueueCorrectionObserverStoreV1;
  reinclude: (transition: StateQueueAuthenticatedTransitionV1) => Promise<void>;
  restore: (transition: StateQueueAuthenticatedTransitionV1) => Promise<void>;
  persistTerminal?: Parameters<
    typeof reconcileStateQueueCorrectionObserverV1
  >[0]["persistTerminal"];
  revokeTerminal?: Parameters<
    typeof reconcileStateQueueCorrectionObserverV1
  >[0]["revokeTerminal"];
}) =>
  await reconcileStateQueueCorrectionObserverV1({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    requiredFinalityDepth: 30n,
    source,
    store,
    reinclude,
    restoreAfterRollback: restore,
    persistTerminal,
    revokeTerminal,
  });

describe("node-owned state-queue correction observer", () => {
  it("admits an external winner at 30 confirmations across restart exactly once", async () => {
    const h = harness();
    const store = memoryStore();
    const reinclude = vi.fn(async () => undefined);
    const restore = vi.fn(async () => undefined);
    await expect(
      run({ source: h.source, store, reinclude, restore }),
    ).resolves.toMatchObject({ status: "bootstrapped" });
    h.setQueue(after);
    h.setDepth(29n);
    await run({ source: h.source, store, reinclude, restore });
    expect(reinclude).not.toHaveBeenCalled();
    expect(
      parseStateQueueCorrectionObserverStateV1(store.current())?.pending,
    ).toHaveLength(1);

    h.setDepth(30n);
    await run({ source: h.source, store, reinclude, restore });
    await run({ source: h.source, store, reinclude, restore });
    expect(reinclude).toHaveBeenCalledTimes(1);
    expect(reinclude).toHaveBeenCalledWith(
      expect.objectContaining({ finalityDepth: "30" }),
    );
  });

  it("reincludes a finalized fraud removal once and restores it after rollback", async () => {
    const h = harness();
    const store = memoryStore();
    const reinclude = vi.fn(async () => undefined);
    const restore = vi.fn(async () => undefined);
    const fraud = authenticatedFraudTransition();
    await run({ source: h.source, store, reinclude, restore });
    h.setObservation(checkpointFromTerminal(fraud));
    h.setQueue(fraud.nextQueue);
    h.setDepth(30n);
    await run({ source: h.source, store, reinclude, restore });
    await run({ source: h.source, store, reinclude, restore });
    expect(reinclude).toHaveBeenCalledTimes(1);
    expect(reinclude).toHaveBeenCalledWith(
      expect.objectContaining({
        transactionHash: fraud.transactionHash,
        transitionKind: "fraud_removal",
        removedHeaderHashes: [descendant],
      }),
    );

    h.setQueue(before);
    h.setDepth(null);
    await run({ source: h.source, store, reinclude, restore });
    expect(restore).toHaveBeenCalledTimes(1);
    expect(restore).toHaveBeenCalledWith(
      expect.objectContaining({ transactionHash: fraud.transactionHash }),
    );
  });

  it("retracts before finality and permits the exact transaction to reappear canonically", async () => {
    const h = harness();
    const store = memoryStore();
    const reinclude = vi.fn(async () => undefined);
    const restore = vi.fn(async () => undefined);
    await run({ source: h.source, store, reinclude, restore });
    h.setQueue(after);
    h.setDepth(10n);
    await run({ source: h.source, store, reinclude, restore });
    h.setQueue(before);
    h.setDepth(null);
    const rolledBack = await run({
      source: h.source,
      store,
      reinclude,
      restore,
    });
    expect(rolledBack.retractedTransactionHashes).toEqual([transactionHash]);
    expect(reinclude).not.toHaveBeenCalled();
    expect(restore).not.toHaveBeenCalled();

    h.setQueue(after);
    h.setDepth(30n);
    await run({ source: h.source, store, reinclude, restore });
    expect(reinclude).toHaveBeenCalledTimes(1);
    expect(
      parseStateQueueCorrectionObserverStateV1(store.current())
        ?.retractedTransactionHashes,
    ).not.toContain(transactionHash);
  });

  it("records and atomically restores a correction rolled back after admission", async () => {
    const h = harness();
    const store = memoryStore();
    const reinclude = vi.fn(async () => undefined);
    const restore = vi.fn(async () => undefined);
    const persistTerminal = vi.fn(async () => undefined);
    const revokeTerminal = vi.fn(async () => undefined);
    await run({
      source: h.source,
      store,
      reinclude,
      restore,
      persistTerminal,
      revokeTerminal,
    });
    h.setQueue(after);
    h.setDepth(30n);
    await run({
      source: h.source,
      store,
      reinclude,
      restore,
      persistTerminal,
      revokeTerminal,
    });
    expect(persistTerminal).toHaveBeenCalledTimes(1);
    h.setQueue(before);
    h.setDepth(null);
    const rollback = await run({
      source: h.source,
      store,
      reinclude,
      restore,
      persistTerminal,
      revokeTerminal,
    });
    await run({
      source: h.source,
      store,
      reinclude,
      restore,
      persistTerminal,
      revokeTerminal,
    });
    expect(revokeTerminal).toHaveBeenCalledTimes(1);
    expect(revokeTerminal.mock.invocationCallOrder[0]).toBeLessThan(
      restore.mock.invocationCallOrder[0]!,
    );
    expect(restore).toHaveBeenCalledTimes(1);
    expect(rollback.postFinalityRollbackTransactionHashes).toEqual([
      transactionHash,
    ]);
  });

  it("revokes an orphaned terminal tx and admits a same-topology replacement between scans", async () => {
    const h = harness();
    const store = memoryStore();
    const reinclude = vi.fn(async () => undefined);
    const restore = vi.fn(async () => undefined);
    const persistTerminal = vi.fn(async () => undefined);
    const revokeTerminal = vi.fn(async () => undefined);
    await run({
      source: h.source,
      store,
      reinclude,
      restore,
      persistTerminal,
      revokeTerminal,
    });
    h.setQueue(after);
    h.setDepth(30n);
    await run({
      source: h.source,
      store,
      reinclude,
      restore,
      persistTerminal,
      revokeTerminal,
    });

    const replacementHash = h32("d");
    const replacement = authenticatedTransition(replacementHash);
    const replacementQueue = replacement.nextQueue;
    h.setQueue(replacementQueue);
    h.setTransactionDepth(transactionHash, null);
    h.setTransactionDepth(replacementHash, 30n);
    h.setObservation(checkpointFromTerminal(replacement));
    await run({
      source: h.source,
      store,
      reinclude,
      restore,
      persistTerminal,
      revokeTerminal,
    });

    expect(revokeTerminal).toHaveBeenCalledWith(
      expect.objectContaining({ transactionHash }),
    );
    expect(persistTerminal).toHaveBeenLastCalledWith(
      expect.objectContaining({ transactionHash: replacementHash }),
    );
    expect(
      parseStateQueueCorrectionObserverStateV1(store.current())?.admitted.map(
        ({ transactionHash: hash }) => hash,
      ),
    ).toEqual([replacementHash]);
    expect(
      parseStateQueueCorrectionObserverStateV1(store.current())?.cursorQueue,
    ).toEqual(replacementQueue);
  });

  it("replays multiple replacement-branch transitions from the exact pre-correction anchor", async () => {
    const h = harness();
    const store = memoryStore();
    const reinclude = vi.fn(async () => undefined);
    const restore = vi.fn(async () => undefined);
    await run({ source: h.source, store, reinclude, restore });
    h.setQueue(after);
    h.setDepth(30n);
    await run({ source: h.source, store, reinclude, restore });

    const firstAppend = appendCheckpoint({
      transactionByte: "d",
      headerHash: h28("3"),
      previousQueue: before,
      blockNo: 91,
    });
    const secondAppend = appendCheckpoint({
      transactionByte: "e",
      headerHash: h28("4"),
      previousQueue: firstAppend.nextQueue,
      blockNo: 92,
    });
    const replacement = secondAppend.nextQueue;
    h.setQueue(replacement);
    h.setDepth(null);
    h.setObservations([firstAppend, secondAppend]);
    await run({ source: h.source, store, reinclude, restore });

    expect(restore).toHaveBeenCalledTimes(1);
    expect(h.observeTransitions).toHaveBeenLastCalledWith(before, replacement);
    expect(
      parseStateQueueCorrectionObserverStateV1(store.current())?.cursorQueue,
    ).toEqual(replacement);
  });

  it("rejects terminal-kind drift and retains its cursor on an unknown gap", async () => {
    const h = harness();
    const store = memoryStore();
    const reinclude = vi.fn(async () => undefined);
    const restore = vi.fn(async () => undefined);
    await run({ source: h.source, store, reinclude, restore });
    h.setQueue(after);
    h.setObservation({
      ...checkpointFromTerminal(),
      checkpointKind: "merge",
    } as unknown as Awaited<
      ReturnType<StateQueueCorrectionObserverSourceV1["observeTransitions"]>
    >[number]);
    await expect(
      run({ source: h.source, store, reinclude, restore }),
    ).rejects.toThrow(/checkpoint replay/u);
    expect(
      parseStateQueueCorrectionObserverStateV1(store.current())?.cursorQueue,
    ).toEqual(before);
    expect(reinclude).not.toHaveBeenCalled();

    h.setObservation("gap");
    await expect(
      run({ source: h.source, store, reinclude, restore }),
    ).rejects.toThrow(/cursor retained/);
    expect(
      parseStateQueueCorrectionObserverStateV1(store.current())?.cursorQueue,
    ).toEqual(before);
  });

  it("rejects injected authority fields and altered durable state", async () => {
    const h = harness();
    const store = memoryStore();
    await run({
      source: h.source,
      store,
      reinclude: async () => undefined,
      restore: async () => undefined,
    });
    expect(
      parseStateQueueCorrectionObserverStateV1({
        ...(store.current() as object),
        completedAuthority: true,
      }),
    ).toBeNull();
    expect(
      parseStateQueueCorrectionObserverStateV1({
        ...(store.current() as object),
        stateDigest: h32("f"),
      }),
    ).toBeNull();

    h.setQueue(after);
    await run({
      source: h.source,
      store,
      reinclude: async () => undefined,
      restore: async () => undefined,
    });
    const durable = store.current() as {
      pending: readonly Record<string, unknown>[];
      stateDigest: string;
      [key: string]: unknown;
    };
    const forgedTransition: Record<string, unknown> = {
      ...durable.pending[0],
      transitionKind: "fraud_removal",
    };
    const {
      transitionDigest: _transitionDigest,
      ...forgedTransitionWithoutDigest
    } = forgedTransition;
    const forgedState = {
      ...durable,
      pending: [
        {
          ...forgedTransitionWithoutDigest,
          transitionDigest: sha256(forgedTransitionWithoutDigest),
        },
      ],
    };
    const { stateDigest: _stateDigest, ...forgedWithoutStateDigest } =
      forgedState;
    expect(
      parseStateQueueCorrectionObserverStateV1({
        ...forgedWithoutStateDigest,
        stateDigest: sha256(forgedWithoutStateDigest),
      }),
    ).toBeNull();
  });

  it("classifies a timeout from Kupo history plus the exact Ogmios mint arm", async () => {
    const mintRedeemer = Data.to(
      {
        RemoveUnattestedBlockAfterTimeout: {
          yield_to_ref_input_index: 0n,
          timed_out_header_hash: target,
          removal_approach: {
            PruneTimedOutBlockDescendant: {
              confirmed_state_ref_input_index: 0n,
              timed_out_node_input_outref: {
                transactionId: h32("1"),
                outputIndex: 0n,
              },
              timed_out_node_output_index: 0n,
            },
          },
        },
      } satisfies StateQueueRedeemerType,
      StateQueueRedeemer,
    );
    const block = () => ({
      id: h32("7"),
      slot: 100,
      height: 90,
      transactions: [
        {
          id: transactionHash,
          inputs: [
            { transaction: { id: h32("1") }, index: 0 },
            { transaction: { id: h32("2") }, index: 0 },
            { transaction: { id: h32("f") }, index: 0 },
          ],
          references: [],
          mint: { [policy]: { "": -1 } },
          redeemers: [
            {
              redeemer: mintRedeemer,
              validator: { purpose: "mint", index: 0 },
            },
          ],
        },
      ],
    });
    const fetchImpl = vi.fn(async (url: string, init?: RequestInit) => {
      if (url.includes("/matches/")) {
        if (url.includes("/matches/*@")) {
          return new Response(
            JSON.stringify([
              {
                transaction_id: transactionHash,
                output_index: 0,
                address: "addr_test_state_queue",
                datum_type: "inline",
                datum: Data.to(
                  { data: { Node: { data: 0n } }, link: null },
                  LinkedListDatum,
                ),
                value: {
                  coins: 2_000_000,
                  assets: {
                    [`${policy}.${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${target}`]: 1,
                  },
                },
              },
              {
                transaction_id: transactionHash,
                output_index: 9,
                address: correctionLockAddress,
                datum_type: "inline",
                datum: Data.to(
                  {
                    Locked: {
                      target_header_hash: target,
                      correction_identity: "AttestationTimeout",
                    },
                  },
                  CorrectionLockDatum,
                ),
                value: {
                  coins: 2_000_000,
                  assets: {
                    [`${hubPolicy}.${CORRECTION_LOCK_ASSET_NAME}`]: 1,
                  },
                },
              },
            ]),
          );
        }
        const match = /matches\/(\d+)@([0-9a-f]{64})/u.exec(url)!;
        const isQueueInput = match[2] === h32("1") || match[2] === h32("2");
        const isLockInput = match[2] === h32("f");
        return new Response(
          JSON.stringify([
            {
              transaction_id: match[2],
              output_index: Number(match[1]),
              address: isLockInput ? correctionLockAddress : undefined,
              datum_type: isLockInput ? "inline" : undefined,
              datum: isLockInput ? Data.to("Idle", CorrectionLockDatum) : null,
              value: isLockInput
                ? {
                    coins: 2_000_000,
                    assets: {
                      [`${hubPolicy}.${CORRECTION_LOCK_ASSET_NAME}`]: 1,
                    },
                  }
                : undefined,
              spent_at:
                isQueueInput || isLockInput
                  ? {
                      slot_no: 100,
                      header_hash: h32("7"),
                      transaction_id: transactionHash,
                      input_index:
                        match[2] === h32("1")
                          ? 0
                          : match[2] === h32("2")
                            ? 1
                            : 2,
                      redeemer: "d87980",
                    }
                  : null,
            },
          ]),
        );
      }
      if (url.includes("/checkpoints/")) {
        return new Response(
          JSON.stringify({ slot_no: 99, header_hash: h32("6") }),
        );
      }
      expect(init?.method).toBe("POST");
      return new Response(
        JSON.stringify({
          result: { id: h32("9"), slot: 130, height: 119 },
        }),
      );
    });
    const webSocketFactory = () => {
      const listeners = new Map<string, ((event: never) => void)[]>();
      let nextBlockCount = 0;
      const emit = (type: string, event: unknown) => {
        for (const listener of listeners.get(type) ?? []) {
          listener(event as never);
        }
      };
      const socket = {
        send: (payload: string) => {
          const request = JSON.parse(payload) as {
            id: number;
            method: string;
          };
          queueMicrotask(() => {
            if (request.method === "findIntersection") {
              emit("message", {
                data: JSON.stringify({
                  id: request.id,
                  result: {
                    intersection: { slot: 99, id: h32("6") },
                  },
                }),
              });
              return;
            }
            nextBlockCount += 1;
            emit("message", {
              data: JSON.stringify({
                id: request.id,
                result:
                  nextBlockCount === 1
                    ? { direction: "backward" }
                    : { direction: "forward", block: block() },
              }),
            });
          });
        },
        close: () => undefined,
        addEventListener: (type: string, listener: (event: never) => void) => {
          listeners.set(type, [...(listeners.get(type) ?? []), listener]);
          if (type === "open") queueMicrotask(() => listener({} as never));
        },
      };
      return socket;
    };
    const source = makeLocalKupmiosStateQueueCorrectionSourceV1({
      deploymentIdentityDigest: deployment,
      stateQueuePolicyId: policy,
      stateQueueAddress: "addr_test_state_queue",
      hubOraclePolicyId: hubPolicy,
      correctionLockAddress,
      fraudProofPolicyId: fraudPolicy,
      fraudProofAddress,
      kupoUrl: "http://kupo.test",
      ogmiosUrl: "ws://ogmios.test",
      readQueue: async () => after,
      fetchImpl,
      webSocketFactory,
    });
    await expect(
      source.observeTransitions(before, after),
    ).resolves.toMatchObject([
      {
        checkpointKind: "timeout_correction",
        terminalTransition: { finalityDepth: "30" },
      },
    ]);
  });

  it.each([0, 1, 2])(
    "replays three transitions observed offline with timeout at ordered position %i",
    async (timeoutIndex) => {
      const stateQueueAddress = "addr_test_state_queue";
      const transactions: {
        txHash: string;
        slot: number;
        blockNo: number;
        blockHash: string;
        inputs: readonly string[];
        output: { headerHash: string; next: string | null };
        timeout: boolean;
      }[] = [];
      const spendByOutRef = new Map<string, (typeof transactions)[number]>();
      const offlineLockInput = outRef("f", 0);
      let current: readonly StateQueueTransitionNodeV1[] = before;
      for (let index = 0; index < 3; index += 1) {
        const isTimeout = index === timeoutIndex;
        const txHash = h32(["c", "d", "e"][index]!);
        const currentTarget = current.find(
          ({ headerHash }) => headerHash === target,
        )!;
        const currentDescendant = current.find(
          ({ headerHash }) => headerHash === descendant,
        );
        const inputs = isTimeout
          ? [currentTarget.outRef, currentDescendant!.outRef, offlineLockInput]
          : [currentTarget.outRef];
        const item = {
          txHash,
          slot: 100 + index,
          blockNo: 90 + index,
          blockHash: h32(["7", "8", "9"][index]!),
          inputs,
          output: {
            headerHash: target,
            next: isTimeout ? null : (currentDescendant?.headerHash ?? null),
          },
          timeout: isTimeout,
        } as const;
        transactions.push(item);
        for (const input of inputs) spendByOutRef.set(input, item);
        current = [
          current[0]!,
          { headerHash: target, outRef: `${txHash}#0` },
          ...(isTimeout || currentDescendant === undefined
            ? []
            : [currentDescendant]),
        ];
      }
      const latest = current;
      const transactionByHash = new Map(
        transactions.map((item) => [item.txHash, item]),
      );
      const transactionBySlot = new Map(
        transactions.map((item) => [item.slot, item]),
      );
      const fetchImpl = vi.fn(async (url: string, init?: RequestInit) => {
        if (url.includes("/matches/*@")) {
          const txHash = /\*@([0-9a-f]{64})/u.exec(url)![1]!;
          const item = transactionByHash.get(txHash)!;
          return new Response(
            JSON.stringify([
              {
                transaction_id: txHash,
                output_index: 0,
                address: stateQueueAddress,
                datum_type: "inline",
                datum: Data.to(
                  {
                    data: { Node: { data: 0n } },
                    link: item.output.next,
                  },
                  LinkedListDatum,
                ),
                value: {
                  coins: 2_000_000,
                  assets: {
                    [`${policy}.${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${target}`]: 1,
                  },
                },
              },
              ...(item.timeout
                ? [
                    {
                      transaction_id: txHash,
                      output_index: 9,
                      address: correctionLockAddress,
                      datum_type: "inline",
                      datum: Data.to(
                        {
                          Locked: {
                            target_header_hash: target,
                            correction_identity: "AttestationTimeout",
                          },
                        },
                        CorrectionLockDatum,
                      ),
                      value: {
                        coins: 2_000_000,
                        assets: {
                          [`${hubPolicy}.${CORRECTION_LOCK_ASSET_NAME}`]: 1,
                        },
                      },
                    },
                  ]
                : []),
            ]),
          );
        }
        if (url.includes("/matches/")) {
          const match = /matches\/(\d+)@([0-9a-f]{64})/u.exec(url)!;
          const reference = `${match[2]}#${match[1]}`;
          const spend = spendByOutRef.get(reference);
          const isLockInput = reference === offlineLockInput;
          return new Response(
            JSON.stringify([
              {
                transaction_id: match[2],
                output_index: Number(match[1]),
                address: isLockInput ? correctionLockAddress : undefined,
                datum_type: isLockInput ? "inline" : undefined,
                datum: isLockInput
                  ? Data.to("Idle", CorrectionLockDatum)
                  : null,
                value: isLockInput
                  ? {
                      coins: 2_000_000,
                      assets: {
                        [`${hubPolicy}.${CORRECTION_LOCK_ASSET_NAME}`]: 1,
                      },
                    }
                  : undefined,
                spent_at:
                  spend === undefined
                    ? null
                    : {
                        slot_no: spend.slot,
                        header_hash: spend.blockHash,
                        transaction_id: spend.txHash,
                        input_index: spend.inputs.indexOf(reference),
                        redeemer: "d87980",
                      },
              },
            ]),
          );
        }
        if (url.includes("/checkpoints/")) {
          const slot = Number(/checkpoints\/(\d+)/u.exec(url)![1]!);
          return new Response(
            JSON.stringify({ slot_no: slot, header_hash: h32("6") }),
          );
        }
        expect(init?.method).toBe("POST");
        return new Response(
          JSON.stringify({ result: { id: h32("f"), slot: 140, height: 130 } }),
        );
      });
      const webSocketFactory = () => {
        const listeners = new Map<string, ((event: never) => void)[]>();
        let blockSlot = 0;
        let nextBlockCount = 0;
        const emit = (type: string, event: unknown) => {
          for (const listener of listeners.get(type) ?? []) {
            listener(event as never);
          }
        };
        return {
          send: (payload: string) => {
            const request = JSON.parse(payload) as {
              id: number;
              method: string;
              params?: { points?: readonly { slot: number }[] };
            };
            queueMicrotask(() => {
              if (request.method === "findIntersection") {
                blockSlot = request.params!.points![0]!.slot + 1;
                emit("message", {
                  data: JSON.stringify({
                    id: request.id,
                    result: {
                      intersection: {
                        slot: blockSlot - 1,
                        id: h32("6"),
                      },
                    },
                  }),
                });
                return;
              }
              nextBlockCount += 1;
              const item = transactionBySlot.get(blockSlot)!;
              const redeemer: StateQueueRedeemerType = {
                RemoveUnattestedBlockAfterTimeout: {
                  yield_to_ref_input_index: 0n,
                  timed_out_header_hash: target,
                  removal_approach: {
                    PruneTimedOutBlockDescendant: {
                      confirmed_state_ref_input_index: 0n,
                      timed_out_node_input_outref: {
                        transactionId: item.inputs[0]!.slice(0, 64),
                        outputIndex: BigInt(item.inputs[0]!.split("#")[1]!),
                      },
                      timed_out_node_output_index: 0n,
                    },
                  },
                },
              };
              emit("message", {
                data: JSON.stringify({
                  id: request.id,
                  result:
                    nextBlockCount === 1
                      ? { direction: "backward" }
                      : {
                          direction: "forward",
                          block: {
                            id: item.blockHash,
                            slot: item.slot,
                            height: item.blockNo,
                            transactions: [
                              {
                                id: item.txHash,
                                inputs: item.inputs.map((reference) => ({
                                  transaction: { id: reference.slice(0, 64) },
                                  index: Number(reference.split("#")[1]),
                                })),
                                references: [],
                                mint: item.timeout
                                  ? { [policy]: { "": -1 } }
                                  : {},
                                redeemers: item.timeout
                                  ? [
                                      {
                                        redeemer: Data.to(
                                          redeemer,
                                          StateQueueRedeemer,
                                        ),
                                        validator: {
                                          purpose: "mint",
                                          index: 0,
                                        },
                                      },
                                    ]
                                  : [],
                              },
                            ],
                          },
                        },
                }),
              });
            });
          },
          close: () => undefined,
          addEventListener: (
            type: string,
            listener: (event: never) => void,
          ) => {
            listeners.set(type, [...(listeners.get(type) ?? []), listener]);
            if (type === "open") queueMicrotask(() => listener({} as never));
          },
        };
      };
      const source = makeLocalKupmiosStateQueueCorrectionSourceV1({
        deploymentIdentityDigest: deployment,
        stateQueuePolicyId: policy,
        stateQueueAddress,
        hubOraclePolicyId: hubPolicy,
        correctionLockAddress,
        fraudProofPolicyId: fraudPolicy,
        fraudProofAddress,
        kupoUrl: "http://kupo.test",
        ogmiosUrl: "ws://ogmios.test",
        readQueue: async () => latest,
        fetchImpl,
        webSocketFactory,
      });
      const observations = await source.observeTransitions(before, latest);
      expect(observations).toHaveLength(3);
      expect(observations.map(({ checkpointKind }) => checkpointKind)).toEqual(
        ["datum_update", "datum_update", "datum_update"].map((kind, index) =>
          index === timeoutIndex ? "timeout_correction" : kind,
        ),
      );
      expect(
        observations.filter(
          ({ checkpointKind }) => checkpointKind === "timeout_correction",
        ),
      ).toHaveLength(1);
    },
  );
});
