import { createHash } from "node:crypto";

import {
  CORRECTION_LOCK_ASSET_NAME,
  CorrectionLockDatum,
  deriveStateQueueAuthenticatedReplayCheckpoint,
  deriveStateQueueAuthenticatedTransition,
  LinkedListDatum,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  type StateQueueAuthenticatedTransition,
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerType,
  type StateQueueTransitionNode,
} from "@al-ft/midgard-sdk";
import {
  kupoExchange,
  kupoMatch,
  type L1Recording,
  loadL1Recording,
  ogmiosExchanges,
  recordedFetch,
  recordedOgmiosWebSocket,
  recordedTransaction,
} from "@al-ft/midgard-test-support/l1-recordings";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  makeLocalKupmiosStateQueueCorrectionSource,
  parseStateQueueCorrectionObserverState,
  reconcileStateQueueCorrectionObserver,
  type StateQueueCorrectionObserverSource,
  type StateQueueCorrectionObserverStore,
} from "../src/services/state-queue-correction-observer.js";

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

/** Ogmios v6: queryNetwork/tip carries no height; blockHeight carries it. */
const ogmiosTipResponse = (
  init: RequestInit | undefined,
  point: { readonly id: string; readonly slot: number },
  height: number,
): Response => {
  const { method } = JSON.parse(String(init?.body)) as { method: string };
  if (method === "queryNetwork/tip")
    return new Response(JSON.stringify({ result: point }));
  expect(method).toBe("queryNetwork/blockHeight");
  return new Response(JSON.stringify({ result: height }));
};

const tipOnlySource = (
  answer: (method: string, call: number) => unknown,
): {
  readonly source: StateQueueCorrectionObserverSource;
  readonly methods: string[];
} => {
  const methods: string[] = [];
  const source = makeLocalKupmiosStateQueueCorrectionSource({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    stateQueueAddress: "addr_test_state_queue",
    hubOraclePolicyId: hubPolicy,
    correctionLockAddress,
    fraudProofPolicyId: fraudPolicy,
    fraudProofAddress,
    kupoUrl: "http://kupo.test",
    ogmiosUrl: "ws://ogmios.test",
    readQueue: async () => before,
    fetchImpl: async (_url: string, init?: RequestInit) => {
      const { method } = JSON.parse(String(init?.body)) as { method: string };
      methods.push(method);
      return new Response(
        JSON.stringify({ result: answer(method, methods.length) }),
      );
    },
  });
  return { source, methods };
};

const before: readonly StateQueueTransitionNode[] = [
  { headerHash: null, outRef: outRef("0") },
  { headerHash: target, outRef: outRef("1") },
  { headerHash: descendant, outRef: outRef("2") },
];
const after: readonly StateQueueTransitionNode[] = [
  { headerHash: null, outRef: outRef("0") },
  { headerHash: target, outRef: `${transactionHash}#0` },
];

const authenticatedTransition = (txHash = transactionHash) =>
  deriveStateQueueAuthenticatedTransition({
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
                PruneUnattestedBlockDescendant: {
                  predecessor_ref_input_index: 0n,
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
  return deriveStateQueueAuthenticatedTransition({
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
  deriveStateQueueAuthenticatedReplayCheckpoint({
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
  previousQueue: readonly StateQueueTransitionNode[];
  blockNo: number;
}) => {
  const transaction = h32(transactionByte);
  const priorTail = previousQueue.at(-1)!;
  const nextQueue = [
    ...previousQueue.slice(0, -1),
    { headerHash: priorTail.headerHash, outRef: `${transaction}#0` },
    { headerHash, outRef: `${transaction}#1` },
  ];
  return deriveStateQueueAuthenticatedReplayCheckpoint({
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

const memoryStore = (): StateQueueCorrectionObserverStore & {
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
    ReturnType<StateQueueCorrectionObserverSource["observeTransitions"]>
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
  const source: StateQueueCorrectionObserverSource = {
    readQueue: async () => queue,
    observeTransitions,
    canonicalDepth: async (transition) =>
      depthByTransaction.has(transition.transactionHash)
        ? depthByTransaction.get(transition.transactionHash)!
        : depth,
  };
  return {
    source,
    setQueue: (next: readonly StateQueueTransitionNode[]) => {
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
  assertRollbackPermitted,
}: {
  source: StateQueueCorrectionObserverSource;
  store: StateQueueCorrectionObserverStore;
  reinclude: (transition: StateQueueAuthenticatedTransition) => Promise<void>;
  restore: (transition: StateQueueAuthenticatedTransition) => Promise<void>;
  persistTerminal?: Parameters<
    typeof reconcileStateQueueCorrectionObserver
  >[0]["persistTerminal"];
  revokeTerminal?: Parameters<
    typeof reconcileStateQueueCorrectionObserver
  >[0]["revokeTerminal"];
  assertRollbackPermitted?: Parameters<
    typeof reconcileStateQueueCorrectionObserver
  >[0]["assertRollbackPermitted"];
}) =>
  await reconcileStateQueueCorrectionObserver({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    requiredFinalityDepth: 30n,
    source,
    store,
    reinclude,
    restoreAfterRollback: restore,
    persistTerminal,
    revokeTerminal,
    assertRollbackPermitted,
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
      parseStateQueueCorrectionObserverState(store.current())?.pending,
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
      parseStateQueueCorrectionObserverState(store.current())
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

  it("refuses a removal's rollback before revoking its terminal outcome, persists nothing, and revokes it only once the rollback is permitted", async () => {
    const h = harness();
    const store = memoryStore();
    const reinclude = vi.fn(async () => undefined);
    const restore = vi.fn(async () => undefined);
    const persistTerminal = vi.fn(async () => undefined);
    const revokeTerminal = vi.fn(async () => undefined);
    const refusal = new Error("rewound removal rolled back");
    let permitted = false;
    const assertRollbackPermitted = vi.fn(async () => {
      if (!permitted) throw refusal;
    });
    const scan = () =>
      run({
        source: h.source,
        store,
        reinclude,
        restore,
        persistTerminal,
        revokeTerminal,
        assertRollbackPermitted,
      });
    await scan();
    h.setQueue(after);
    h.setDepth(30n);
    await scan();
    expect(persistTerminal).toHaveBeenCalledTimes(1);
    expect(assertRollbackPermitted).not.toHaveBeenCalled();
    const admitted = store.current();

    h.setQueue(before);
    h.setDepth(null);
    for (let attempt = 0; attempt < 2; attempt += 1) {
      await expect(scan()).rejects.toBe(refusal);
      expect(revokeTerminal).not.toHaveBeenCalled();
      expect(restore).not.toHaveBeenCalled();
      expect(store.current()).toEqual(admitted);
    }
    expect(assertRollbackPermitted).toHaveBeenCalledWith(
      expect.objectContaining({ transactionHash }),
    );

    permitted = true;
    const rollback = await scan();
    expect(revokeTerminal).toHaveBeenCalledTimes(1);
    expect(restore).toHaveBeenCalledTimes(1);
    expect(
      assertRollbackPermitted.mock.invocationCallOrder.at(-1)!,
    ).toBeLessThan(revokeTerminal.mock.invocationCallOrder[0]!);
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
      parseStateQueueCorrectionObserverState(store.current())?.admitted.map(
        ({ transactionHash: hash }) => hash,
      ),
    ).toEqual([replacementHash]);
    expect(
      parseStateQueueCorrectionObserverState(store.current())?.cursorQueue,
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
      parseStateQueueCorrectionObserverState(store.current())?.cursorQueue,
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
      ReturnType<StateQueueCorrectionObserverSource["observeTransitions"]>
    >[number]);
    await expect(
      run({ source: h.source, store, reinclude, restore }),
    ).rejects.toThrow(/checkpoint replay/u);
    expect(
      parseStateQueueCorrectionObserverState(store.current())?.cursorQueue,
    ).toEqual(before);
    expect(reinclude).not.toHaveBeenCalled();

    h.setObservation("gap");
    await expect(
      run({ source: h.source, store, reinclude, restore }),
    ).rejects.toThrow(/cursor retained/);
    expect(
      parseStateQueueCorrectionObserverState(store.current())?.cursorQueue,
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
      parseStateQueueCorrectionObserverState({
        ...(store.current() as object),
        completedAuthority: true,
      }),
    ).toBeNull();
    expect(
      parseStateQueueCorrectionObserverState({
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
      parseStateQueueCorrectionObserverState({
        ...forgedWithoutStateDigest,
        stateDigest: sha256(forgedWithoutStateDigest),
      }),
    ).toBeNull();
  });

  /**
   * The spent inputs, ascending as the ledger's `Set TxIn` orders them. The
   * Kupo double here attributes them as a *fixed* Kupo would — the ledger's own
   * pointer, and a redeemer — which is the one attribution no recording can
   * show: every live Kupo v2.11.0 mirrors it (CardanoSolutions/kupo#210). The
   * mirrored attribution is not doubled at all; the recorded preprod removal
   * below serves the real one.
   */
  const timeoutSpentInputs = [h32("1"), h32("2"), h32("f")] as const;
  type KupoAttribution = (ledgerIndex: number) => {
    input_index: number;
    redeemer: string | null;
  };
  const classifiesTimeoutFromKupoHistory = async (
    kupoAttribution: KupoAttribution,
  ) => {
    const mintRedeemer = Data.to(
      {
        RemoveUnattestedBlockAfterTimeout: {
          yield_to_ref_input_index: 0n,
          timed_out_header_hash: target,
          removal_approach: {
            PruneUnattestedBlockDescendant: {
              predecessor_ref_input_index: 0n,
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
          inputs: timeoutSpentInputs.map((id) => ({
            transaction: { id },
            index: 0,
          })),
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
                      ...kupoAttribution(
                        timeoutSpentInputs.indexOf(
                          match[2] as (typeof timeoutSpentInputs)[number],
                        ),
                      ),
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
      return ogmiosTipResponse(init, { id: h32("9"), slot: 130 }, 119);
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
    const source = makeLocalKupmiosStateQueueCorrectionSource({
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
        // The redeemer the classification rests on is the transaction's own,
        // read through Ogmios — never Kupo's `spent_at.redeemer`.
        stateQueueMintRedeemer: {
          purpose: "mint",
          index: "0",
          cborHex: mintRedeemer,
        },
        terminalTransition: { finalityDepth: "30" },
      },
    ]);
  };

  it("classifies a timeout from Kupo history plus the exact Ogmios mint arm", () =>
    classifiesTimeoutFromKupoHistory((ledgerIndex) => ({
      input_index: ledgerIndex,
      redeemer: "d87980",
    })));

  describe("a recorded preprod timeout removal", () => {
    /**
     * `a2a47d2e` removed block `2150a18c…` from a two-entry queue after its
     * attestation timeout, recorded off preprod Kupo v2.11.0 and Ogmios v7.0.0.
     * Every Kupo and Ogmios answer the observer reads is the live one, including
     * Kupo's mirrored `spent_at` on all four inputs: `null` for the root's Plutus
     * spend, another script's redeemer for the node and the CorrectionLock, a
     * script's redeemer for the key input. The deployment's identifiers are the
     * preprod deployment's; its identity digest is not on chain and the
     * fraud-proof pair plays no part in a timeout, so those stay placeholders.
     */
    const recordedPolicy =
      "0aa62a61a5c1e74f340514cc872df261e5e79ff2e0299618d5042aed";
    const recordedStateQueueAddress =
      "addr_test1wr405mwutmgnt0mntdl5jumg4rdkx6qglmjvarranq85hgcts4ka2";
    const recordedHubPolicy =
      "f705e1cf306f21d935ad9a155e219cc07ed0d3313b87d0f593efbf1c";
    const removedHeaderHash =
      "2150a18c7e67f90418c0b0f8d2625ff9267fd2d40ed860cce75c86a5";
    const priorQueueTransaction =
      "2f279544d5984c45510d5411e7dfa47ff613fb95c022a0726ecfed8d2c4f13a8";
    const lockInput = {
      txHash:
        "cbeddfdf94390715ce39f0c79798faed1cfcf5bf31aa6a4b309edde3e7962927",
      outputIndex: 8,
    };
    const recordedBefore: readonly StateQueueTransitionNode[] = [
      { headerHash: null, outRef: `${priorQueueTransaction}#1` },
      { headerHash: removedHeaderHash, outRef: `${priorQueueTransaction}#0` },
    ];

    const observe = (recording: L1Recording) => {
      const recordedAfter: readonly StateQueueTransitionNode[] = [
        { headerHash: null, outRef: `${recording.transaction!.id}#0` },
      ];
      const ogmios = recordedOgmiosWebSocket(recording);
      return makeLocalKupmiosStateQueueCorrectionSource({
        deploymentIdentityDigest: deployment,
        stateQueuePolicyId: recordedPolicy,
        stateQueueAddress: recordedStateQueueAddress,
        hubOraclePolicyId: recordedHubPolicy,
        correctionLockAddress: kupoMatch(recording, lockInput)
          .address as string,
        fraudProofPolicyId: fraudPolicy,
        fraudProofAddress,
        kupoUrl: "http://kupo.recorded",
        ogmiosUrl: "ws://ogmios.recorded",
        readQueue: async () => recordedAfter,
        fetchImpl: recordedFetch(recording),
        webSocketFactory: (url) => new ogmios.WebSocket(url),
      }).observeTransitions(recordedBefore, recordedAfter);
    };

    /** The state-queue mint redeemer the removal ran, off its own transaction. */
    const recordedMintRedeemer = (recording: L1Recording) =>
      (
        recordedTransaction(recording).redeemers as {
          redeemer: string;
          validator: { purpose: string; index: number };
        }[]
      ).find(({ validator }) => validator.purpose === "mint")!;

    it("classifies it as a timeout correction from Kupo v2.11.0's mirrored spends", async () => {
      const recording = loadL1Recording("preprod-state-queue-removal-a2a47d2e");
      const mintRedeemer = recordedMintRedeemer(recording).redeemer;
      expect(Data.from(mintRedeemer, StateQueueRedeemer)).toMatchObject({
        RemoveUnattestedBlockAfterTimeout: {
          timed_out_header_hash: removedHeaderHash,
        },
      });
      await expect(observe(recording)).resolves.toMatchObject([
        {
          checkpointKind: "timeout_correction",
          // The redeemer the classification rests on is the transaction's own,
          // read through Ogmios — never Kupo's `spent_at.redeemer`, which for
          // this removal names no input's real redeemer.
          stateQueueMintRedeemer: {
            purpose: "mint",
            index: "0",
            cborHex: mintRedeemer,
          },
          terminalTransition: {
            transactionHash: recording.transaction!.id,
            blockHash: recording.transaction!.block.id,
            slot: recording.transaction!.block.slot.toString(),
            blockNo: "5220548",
            // The recorded Ogmios tip is block 5220935.
            finalityDepth: "388",
          },
        },
      ]);
    });

    it("ignores what Kupo reports as the input index and redeemer", async () => {
      const honest = await observe(
        loadL1Recording("preprod-state-queue-removal-a2a47d2e"),
      );
      const recording = loadL1Recording("preprod-state-queue-removal-a2a47d2e");
      const inputs = recordedTransaction(recording).inputs as {
        transaction: { id: string };
        index: number;
      }[];
      // Kupo's answers rewritten to what a fixed Kupo would say for each input
      // — the ledger's pointer — and a redeemer none of them ran.
      inputs.forEach((input, ledgerIndex) => {
        const [match] = kupoExchange(
          recording,
          `/matches/${input.index.toString()}@${input.transaction.id}?resolve_hashes`,
        ).response.body as { spent_at: Record<string, unknown> }[];
        match!.spent_at = {
          ...match!.spent_at,
          input_index: ledgerIndex,
          redeemer: "d87b80",
        };
      });
      await expect(observe(recording)).resolves.toStrictEqual(honest);
    });

    it("refuses the removal once its Ogmios mint redeemer names another block", async () => {
      const recording = loadL1Recording("preprod-state-queue-removal-a2a47d2e");
      // Rewritten in the block chain-sync will serve, not in a parsed copy.
      const forward = ogmiosExchanges(recording, "nextBlock")
        .map(
          ({ response }) =>
            (
              response.body as {
                result: {
                  direction: string;
                  block?: { transactions: Record<string, unknown>[] };
                };
              }
            ).result,
        )
        .find(({ direction }) => direction === "forward")!;
      const mint = (
        forward.block!.transactions.find(
          ({ id }) => id === recording.transaction!.id,
        )!.redeemers as { redeemer: string; validator: { purpose: string } }[]
      ).find(({ validator }) => validator.purpose === "mint")!;
      const decoded = Data.from(mint.redeemer, StateQueueRedeemer) as Extract<
        StateQueueRedeemerType,
        { RemoveUnattestedBlockAfterTimeout: unknown }
      >;
      mint.redeemer = Data.to(
        {
          RemoveUnattestedBlockAfterTimeout: {
            ...decoded.RemoveUnattestedBlockAfterTimeout,
            timed_out_header_hash: h28("3"),
          },
        } satisfies StateQueueRedeemerType,
        StateQueueRedeemer,
      );
      await expect(observe(recording)).rejects.toThrow(
        "State-queue transaction failed exact authenticated checkpoint derivation",
      );
    });
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
      let current: readonly StateQueueTransitionNode[] = before;
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
        return ogmiosTipResponse(init, { id: h32("f"), slot: 140 }, 130);
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
                    PruneUnattestedBlockDescendant: {
                      predecessor_ref_input_index: 0n,
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
      const source = makeLocalKupmiosStateQueueCorrectionSource({
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

  it("binds the Ogmios block height to a tip read on both sides of it", async () => {
    // The first bracket straddles a tip change; the second agrees.
    const tips = [h32("8"), h32("9"), h32("9"), h32("9")];
    let tipReads = 0;
    const { source, methods } = tipOnlySource((method) =>
      method === "queryNetwork/tip" ? { id: tips[tipReads++], slot: 130 } : 119,
    );
    await expect(source.observeTransitions(before, before)).resolves.toEqual(
      [],
    );
    expect(methods).toEqual([
      "queryNetwork/tip",
      "queryNetwork/blockHeight",
      "queryNetwork/tip",
      "queryNetwork/tip",
      "queryNetwork/blockHeight",
      "queryNetwork/tip",
    ]);
  });

  it("refuses a tip that keeps moving across a bounded number of reads", async () => {
    const { source, methods } = tipOnlySource((method, call) =>
      method === "queryNetwork/tip"
        ? { id: h32(call.toString(16).slice(-1)), slot: 100 + call }
        : 119,
    );
    await expect(source.observeTransitions(before, before)).rejects.toThrow(
      "Ogmios tip moved during each of 5 block height reads",
    );
    expect(methods).toHaveLength(15);
  });

  it.each([["origin"], [undefined], [-1], ["119"], [1.5]])(
    "fails closed on an invalid Ogmios block height %j",
    async (height) => {
      const { source } = tipOnlySource((method) =>
        method === "queryNetwork/tip" ? { id: h32("9"), slot: 130 } : height,
      );
      await expect(source.observeTransitions(before, before)).rejects.toThrow(
        "Ogmios block height query returned no block height",
      );
    },
  );

  it("fails closed on an origin Ogmios tip", async () => {
    const { source } = tipOnlySource(() => "origin");
    await expect(source.observeTransitions(before, before)).rejects.toThrow(
      "Ogmios tip query returned no canonical point",
    );
  });
});
