import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { makeLocalKupmiosStateQueueCorrectionSource } from "../src/services/state-queue-correction-observer.js";
import {
  makeState,
  parseStateQueueCorrectionObserverState,
  STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
  type StateQueueCorrectionObserverSource,
} from "../src/services/state-queue-correction-observer.parse-state-queue-correction-observer-state.js";
import { type CorrectionObserverJournalDependency } from "../src/services/state-queue-correction-observer.prune-admitted.js";
import {
  CORRECTION_OBSERVER_ROLLBACK_SAMPLE_LIMIT,
  reconcileStateQueueCorrectionObserver,
} from "../src/services/state-queue-correction-observer.reconcile-state-queue-correction-observer.js";
import { intersectionSocket } from "./helpers/ogmios-intersection-socket.js";
import {
  authenticatedTransition,
  deployment,
  h28,
  h32,
  ogmiosTipResponse,
  policy,
} from "./state-queue-correction-observer.authenticated-fraud-transition.js";
import { memoryStore } from "./state-queue-correction-observer.harness.js";

const rootOf = (sequence: number): string =>
  sequence === 0 ? `${h32("0")}#0` : `${mergeHash(sequence)}#0`;

const mergeHash = (sequence: number): string =>
  `${"5".repeat(62)}${sequence.toString(16).padStart(2, "0")}`;

/** Merge `sequence` (block 90 + sequence) of the head `headerByte`: it spends
 * the previous merge's root output and the head, and re-outputs the root. */
const merge = (sequence: number, headerByte: string) => {
  const previousRoot = rootOf(sequence - 1);
  const headerOutRef = `${"6".repeat(62)}${sequence.toString(16).padStart(2, "0")}#1`;
  const transition = SDK.deriveStateQueueAuthenticatedTransition({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    transactionHash: mergeHash(sequence),
    blockHash: h32("9"),
    slot: (100 + sequence).toString(),
    blockNo: (90 + sequence).toString(),
    transactionIndex: "0",
    chainPointId: h32("8"),
    finalityDepth: "30",
    mintPolicyIds: [policy],
    referenceInputOutRefs: [`${h32("f")}#0`],
    correctionLockWitness: {
      kind: "idle_reference",
      referenceOutRef: `${h32("f")}#0`,
      datum: "Idle",
    },
    redeemers: [
      {
        purpose: "mint",
        index: "0",
        cborHex: Data.to(
          {
            MergeToConfirmedStateV1: {
              yield_to_ref_input_index: 0n,
              header_node_key: h28(headerByte),
              confirmed_state_input_outref: {
                transactionId: previousRoot.slice(0, 64),
                outputIndex: 0n,
              },
              confirmed_state_output_index: 0n,
              m_settlement_redeemer_index: null,
              merged_block_withdrawals_root: h32("1"),
              merged_block_forced_transactions_root: h32("2"),
              merged_block_transactions_root: h32("3"),
              merged_block_deposits_root: h32("4"),
              merged_block_transition_trace_root: h32("5"),
              merged_block_event_to_step_root: h32("6"),
              merged_block_validation_traces_root: h32("7"),
              merged_block_withdrawal_count: 0n,
              merged_block_forced_transaction_count: 0n,
              merged_block_l2_transaction_count: 0n,
              merged_block_deposit_count: 0n,
              merged_block_total_event_count: 0n,
              merged_block_transition_step_count: 0n,
              merged_block_validation_trace_count: 0n,
            },
          } as const,
          SDK.StateQueueRedeemer,
        ),
      },
    ],
    spentInputOutRefs: [previousRoot, headerOutRef],
    previousQueue: [
      { headerHash: null, outRef: previousRoot },
      { headerHash: h28(headerByte), outRef: headerOutRef },
    ],
    nextQueue: [{ headerHash: null, outRef: rootOf(sequence) }],
  });
  if (transition === null) throw new Error("invalid merge fixture");
  return transition;
};

const CURRENT_QUEUE = [{ headerHash: null, outRef: rootOf(3) }] as const;

const fixture = ({
  admitted,
  depth,
  diagnosticHashes = [],
}: {
  admitted: readonly SDK.StateQueueAuthenticatedTransition[];
  depth: (
    transition: SDK.StateQueueAuthenticatedTransition,
  ) => bigint | null | Promise<bigint | null>;
  diagnosticHashes?: readonly string[];
}) => {
  const store = memoryStore();
  void store.save(
    makeState({
      schemaVersion: STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
      deploymentIdentityDigest: deployment,
      stateQueuePolicyId: policy,
      cursorQueue: CURRENT_QUEUE,
      pending: [],
      admitted,
      retractedTransactionHashes: diagnosticHashes,
      postFinalityRollbackIncidents: diagnosticHashes.map(
        (transactionHash) => ({ transactionHash, transitionDigest: h32("a") }),
      ),
    }),
  );
  const canonicalDepth = vi.fn(async (transition) => depth(transition));
  const source: StateQueueCorrectionObserverSource = {
    readQueue: async () => CURRENT_QUEUE,
    observeTransitions: async () => {
      throw new Error("the cursor is current");
    },
    canonicalDepth,
  };
  const persistTerminal = vi.fn(async () => undefined);
  const revokeTerminal = vi.fn(async () => undefined);
  const provenFinal = new Set<string>();
  const reconcile = (
    dependencies?: readonly CorrectionObserverJournalDependency[],
  ) =>
    reconcileStateQueueCorrectionObserver({
      deploymentIdentityDigest: deployment,
      stateQueuePolicyId: policy,
      requiredFinalityDepth: 30n,
      source,
      store,
      reinclude: async () => undefined,
      restoreAfterRollback: async () => undefined,
      persistTerminal,
      revokeTerminal,
      provenFinal,
      ...(dependencies !== undefined && {
        journalDependencies: async () => dependencies,
      }),
    });
  const admittedHashes = () =>
    parseStateQueueCorrectionObserverState(store.current())!.admitted.map(
      ({ transactionHash }) => transactionHash,
    );
  return {
    store,
    canonicalDepth,
    persistTerminal,
    revokeTerminal,
    reconcile,
    admittedHashes,
  };
};

const M1 = merge(1, "1");
const M2 = merge(2, "2");
const M3 = merge(3, "3");
const ALL = [M1, M2, M3].map(({ transactionHash }) => transactionHash);

describe("state-queue correction observer: transitions deeper than k", () => {
  it("reads a transition proven deeper than 2160 once, and one inside the horizon on every reconcile", async () => {
    const correction = authenticatedTransition();
    let depth: bigint | null = 2162n;
    const deep = fixture({ admitted: [correction], depth: () => depth });
    await deep.reconcile();
    await deep.reconcile();
    // Its absence now would be a rollback past k: it is never read again.
    depth = null;
    await deep.reconcile();
    expect(deep.canonicalDepth).toHaveBeenCalledTimes(1);
    expect(deep.admittedHashes()).toEqual([correction.transactionHash]);

    const shallow = fixture({ admitted: [correction], depth: () => 2161n });
    await shallow.reconcile();
    await shallow.reconcile();
    await shallow.reconcile();
    expect(shallow.canonicalDepth).toHaveBeenCalledTimes(3);
  });

  it.each([2160, 2161])(
    "uses the production inclusive depth reader at %i blocks AFTER inclusion",
    async (afterDepth) => {
      const transitions = [M2, M3];
      const canonical = makeLocalKupmiosStateQueueCorrectionSource({
        deploymentIdentityDigest: deployment,
        stateQueuePolicyId: policy,
        stateQueueAddress: "fixture-queue",
        hubOraclePolicyId: h28("a"),
        correctionLockAddress: "fixture-lock",
        fraudProofPolicyId: h28("e"),
        fraudProofAddress: "fixture-proof",
        kupoUrl: "http://kupo.test",
        ogmiosUrl: "http://ogmios.test",
        readQueue: async () => CURRENT_QUEUE,
        webSocketFactory: intersectionSocket((request) => ({
          result: {
            intersection: request.params.points[0],
            tip: {
              id: h32("f"),
              slot: 9999,
              height: Number(M2.blockNo) + afterDepth,
            },
          },
        })).factory,
        fetchImpl: async (url, init) => {
          if (url.includes("/matches/")) {
            const match = /matches\/(\d+)@([0-9a-f]{64})/u.exec(url)!;
            const outref = `${match[2]}#${match[1]}`;
            const transition = transitions.find((entry) =>
              entry.consumedQueueOutRefs.includes(outref),
            )!;
            return new Response(
              JSON.stringify([
                {
                  transaction_id: match[2],
                  output_index: Number(match[1]),
                  datum: null,
                  spent_at: {
                    transaction_id: transition.transactionHash,
                    input_index: 0,
                    redeemer: null,
                    slot_no: Number(transition.slot),
                    header_hash: transition.blockHash,
                  },
                },
              ]),
            );
          }
          return ogmiosTipResponse(
            init,
            { id: h32("f"), slot: 9999 },
            Number(M2.blockNo) + afterDepth,
          );
        },
      });
      const f = fixture({
        admitted: transitions,
        depth: canonical.canonicalDepth,
      });
      await f.reconcile([]);
      expect(f.admittedHashes()).toEqual(
        afterDepth === 2160
          ? transitions.map((entry) => entry.transactionHash)
          : [M3.transactionHash],
      );
    },
  );

  it("drops resolved merges deeper than k exactly once, keeping the newest merge and every correction", async () => {
    const correction = authenticatedTransition();
    const f = fixture({
      admitted: [M1, M2, M3, correction],
      depth: () => 3000n,
    });
    await f.reconcile([]);
    expect(f.admittedHashes()).toEqual([
      M3.transactionHash,
      correction.transactionHash,
    ]);
    await f.reconcile([]);
    expect(f.admittedHashes()).toEqual([
      M3.transactionHash,
      correction.transactionHash,
    ]);
    // The pruned merges were never revoked as terminals, and the kept ones
    // were proven once.
    expect(f.revokeTerminal).not.toHaveBeenCalled();
    expect(f.persistTerminal).not.toHaveBeenCalled();
    expect(f.canonicalDepth).toHaveBeenCalledTimes(4);
  });

  it("keeps every merge inside the horizon, and drops nothing without the journal read", async () => {
    const inside = fixture({ admitted: [M1, M2, M3], depth: () => 2161n });
    await inside.reconcile([]);
    expect(inside.admittedHashes()).toEqual(ALL);

    const unwired = fixture({ admitted: [M1, M2, M3], depth: () => 3000n });
    await unwired.reconcile();
    expect(unwired.admittedHashes()).toEqual(ALL);
  });

  it("keeps a merge an active journal's base depends on, and the transition after it", async () => {
    const f = fixture({ admitted: [M1, M2, M3], depth: () => 3000n });
    await f.reconcile([
      {
        headerHash: h28("e"),
        baseTailHeaderHash: h28("1"),
        baseTailOutRef: `${h32("e")}#0`,
        abandoned: false,
      },
    ]);
    expect(f.admittedHashes()).toEqual(ALL);
  });

  it("keeps retained abandoned journal evidence even when its base is spent deeper than k", async () => {
    const named = {
      // Its own header is on a recorded queue: it may have landed.
      headerHash: h28("2"),
      baseTailHeaderHash: h28("1"),
      baseTailOutRef: `${h32("e")}#0`,
      abandoned: true,
    };
    const kept = fixture({ admitted: [M1, M2, M3], depth: () => 3000n });
    await kept.reconcile([named]);
    expect(kept.admittedHashes()).toEqual(ALL);

    const retired = fixture({ admitted: [M1, M2, M3], depth: () => 3000n });
    await retired.reconcile([{ ...named, headerHash: h28("d") }]);
    expect(retired.admittedHashes()).toEqual(ALL);
  });
  it("bounds diagnostic rollback samples without changing admitted recovery evidence", async () => {
    expect(CORRECTION_OBSERVER_ROLLBACK_SAMPLE_LIMIT).toBe(256);
    const hashes = Array.from({ length: 300 }, (_, index) =>
      index.toString(16).padStart(64, "0"),
    );
    const correction = authenticatedTransition();
    const f = fixture({
      admitted: [M1, M2, M3, correction],
      depth: () => 3000n,
      diagnosticHashes: hashes,
    });
    await f.reconcile([
      {
        headerHash: h28("e"),
        baseTailHeaderHash: h28("1"),
        baseTailOutRef: `${h32("e")}#0`,
        abandoned: false,
      },
    ]);
    const saved = parseStateQueueCorrectionObserverState(f.store.current())!;
    expect(saved.retractedTransactionHashes).toEqual(
      hashes.slice(-CORRECTION_OBSERVER_ROLLBACK_SAMPLE_LIMIT),
    );
    expect(
      saved.postFinalityRollbackIncidents.map(
        ({ transactionHash }) => transactionHash,
      ),
    ).toEqual(saved.retractedTransactionHashes);
    expect(
      saved.admitted.map(({ transactionHash }) => transactionHash),
    ).toEqual([...ALL, correction.transactionHash]);
  });
});
