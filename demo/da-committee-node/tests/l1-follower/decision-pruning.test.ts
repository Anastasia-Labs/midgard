import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { depth, type FactStore, isFinal } from "@al-ft/midgard-l1-follower";
import { afterAll, describe, expect, it } from "vitest";

import {
  DECISION_PRUNE_BATCH,
  type DecisionPruneInputs,
  decisionsToPrune,
} from "../../src/committee-service.decision-pruning.js";
import type { CommitteeConfig } from "../../src/config.js";
import type {
  Obligation,
  SlotTime,
} from "../../src/l1/follower/obligations.js";
import { loadDaSigner, signDaAttestation } from "../../src/signer.js";
import type { PostgresCommitteeStore } from "../../src/store/postgres.js";
import {
  retentionCycleOptions,
  runRetentionCycle,
} from "../../src/store/retention.js";
import {
  commitmentFor,
  signatureRecord,
} from "../peer-coordinator.signature-record.js";
import { payloadRecord } from "../store-retention.header-record.js";
import {
  committeeOnQueueChain,
  countingCoordinator,
  factStoreDialects,
  reconcilerFor,
} from "./committee-harness.js";
import type { QueueChainNode } from "./queue-chain.js";
import { SIM_DEPTHS, SIM_SLOT_TIME } from "./queue-sim.js";

/**
 * Plan §11, class B: the member's signed decision for a header, and every
 * row keyed by the header's hash, is deleted once `obligations()` reads the
 * decision deletable and the header has left every queue the committee
 * reads: its exit is final (depth > k), or it never landed and a final
 * block is past its end time. Never before. Without promise adoption this
 * is the only deleter of those rows; under it, retirement is, and decision
 * pruning deletes nothing.
 */
const { databases, dialects } = factStoreDialects("midgard_test_c3fix_prune");
afterAll(async () => {
  await databases.dropAll();
}, 120_000);

/** One slot spans a challengeability horizon, so a payload can be released. */
const SLOT_TIME: SlotTime = {
  ...SIM_SLOT_TIME,
  slotLength: MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
};

const ADOPTION: NonNullable<CommitteeConfig["availabilityPromiseAdoption"]> = {
  policyArtifactPath: "/unused/policy.json",
  trustedPolicyDigest: "11".repeat(32),
  resourceProfilePath: "/unused/profile.json",
  trustedResourceProfileDigest: "22".repeat(32),
  calibrationEvidencePath: "/unused/calibration.json",
  trustedCalibrationEvidenceDigest: "33".repeat(32),
  faultModelPath: "/unused/fault-model.json",
  trustedFaultModelDigest: "44".repeat(32),
};

/** The member's signature (a signed decision) and a peer's, for `node`. */
const signatures = async (node: QueueChainNode, fingerprint: string) =>
  Promise.all(
    [0, 1].map(async (signerIndex) => {
      const signer = await loadDaSigner(
        `hex:${"00".repeat(31)}${(signerIndex + 1).toString(16).padStart(2, "0")}`,
      );
      const commitment = commitmentFor(node.hash);
      const record = signatureRecord({
        deploymentFingerprint: fingerprint,
        headerHash: node.hash,
        signerIndex,
        committeeSignersHash: "77".repeat(32),
        commitment,
        signatureWitness: signDaAttestation({
          signer,
          signerIndex,
          availabilityCommitment: commitment.commitment,
        }),
      });
      const local = signerIndex === 0;
      return {
        ...record,
        source: local ? "local" : "peer",
        broadcastStatus: local ? "local" : "posted",
        validation: {
          ...record.validation,
          l1Header: {
            ...record.validation.l1Header,
            endTime: node.header.endTime.toString(),
          },
        },
      } as const;
    }),
  );

/** Stores a signed decision for `node` and the rows keyed by its hash. */
const storeDecisionRows = async (
  store: PostgresCommitteeStore,
  node: QueueChainNode,
  fingerprint: string,
): Promise<void> => {
  const [own, peer] = await signatures(node, fingerprint);
  await store.saveDaSignature(own!);
  await store.saveDaSignature(peer!);
  await store.savePeerBroadcast({
    deploymentFingerprint: fingerprint,
    peerId: "peer-1",
    headerHash: node.hash,
    availabilityCommitmentDigest: own!.availabilityCommitmentDigest,
    signerIndex: 0,
    status: "posted",
    attempts: 1,
    updatedAt: "2026-10-08T00:00:00.000Z",
  });
  await store.saveL1Submission({
    deploymentFingerprint: fingerprint,
    headerHash: node.hash,
    txKind: "apply",
    txHash: "ab".repeat(32),
    inputsUsed: [],
    submittedAt: "2026-10-08T00:00:00.000Z",
    resultStatus: "confirmed",
  });
};

/** Which of `headerHash`'s decision rows the store still holds. */
const decisionRows = async (
  store: PostgresCommitteeStore,
  headerHash: string,
) => ({
  signed: (await store.listSignedDecisions()).some(
    (signed) => signed.headerHash === headerHash,
  ),
  signatures: (await store.listDaSignatures(headerHash)).length,
  outbox: (await store.listDecisionOutbox(headerHash)).length,
  broadcasts: (await store.listPeerBroadcasts(headerHash)).length,
  submissions: (await store.listL1Submissions()).filter(
    (submission) => submission.headerHash === headerHash,
  ).length,
  observed:
    (await store.getL1SourceState())?.observations.some(
      (observation) => observation.headerHash === headerHash,
    ) ?? false,
});

const KEPT = {
  signed: true,
  signatures: 2,
  outbox: 1,
  broadcasts: 1,
  submissions: 1,
  observed: true,
};
const DELETED = {
  signed: false,
  signatures: 0,
  outbox: 0,
  broadcasts: 0,
  submissions: 0,
  observed: false,
};

/**
 * Header A is committed with a verified payload, reconciled at a safe depth
 * (one outbox row), signed (the member's and a peer's signatures, a peer
 * broadcast, an L1 submission), attested and merged; B merges after it as
 * the confirmed head. Then the chain extends, one tick per block, until B's
 * merge is final with blocks to spare. Returns each tick's rows of A with
 * the depth of A's merge at that tick.
 */
const mergeAndExtend = async (
  factStore: FactStore,
  config: Partial<CommitteeConfig>,
) => {
  const harness = await committeeOnQueueChain(factStore, {
    slotTime: SLOT_TIME,
    config,
  });
  const { queue, apply, store } = harness;
  const fingerprint = harness.config.deploymentFingerprint;
  const service = await harness.service({
    submitterReconciler: reconcilerFor(
      harness.config,
      store,
      countingCoordinator(),
    ),
  });
  const step = async (event: Parameters<typeof apply>[0]) => {
    await apply(event);
    await expect(service.tick()).resolves.toMatchObject({ errors: [] });
  };
  await step(queue.init());
  await apply(queue.append());
  const a = queue.nodes[0]!;
  await store.saveDaPayload(payloadRecord(a.hash, fingerprint));
  await step(queue.empty());
  await storeDecisionRows(store, a, fingerprint);
  await step(queue.attest());
  await step(queue.append());
  await store.saveDaPayload(payloadRecord(queue.nodes[1]!.hash, fingerprint));
  await step(queue.merge());
  await step(queue.attest());
  await step(queue.merge());
  expect(await decisionRows(store, a.hash)).toEqual(KEPT);
  const ticks: { aMerge: number; bMerge: number; rows: typeof KEPT }[] = [];
  const mergeDepth = (index: number): number =>
    depth(queue.chain.tip.height, queue.merged[index]!.mergedAt);
  while (!isFinal(mergeDepth(1) - 3, SIM_DEPTHS)) {
    await step(queue.empty());
    ticks.push({
      aMerge: mergeDepth(0),
      bMerge: mergeDepth(1),
      rows: await decisionRows(store, a.hash),
    });
  }
  return { ...harness, service, a, ticks };
};

describe.each(dialects)("signed-decision pruning (%s)", (_, open) => {
  it(
    "deletes the signed decision and the rows keyed by its header once its exit is final and it has left every queue read, never before",
    { timeout: 180_000 },
    async () => {
      const factStore = await open();
      try {
        const { ticks, store, a, service, config } = await mergeAndExtend(
          factStore,
          {},
        );
        // A is the confirmed head until B's merge is final: deleted at the
        // first tick that reads B's merge final, when A's own exit (merged
        // before B) is final too. Never earlier.
        const first = ticks.findIndex(({ rows }) => !rows.signed);
        expect(first).toBeGreaterThan(0);
        expect(
          ticks.findIndex(({ bMerge }) => isFinal(bMerge, SIM_DEPTHS)),
        ).toBe(first);
        expect(isFinal(ticks[first]!.aMerge, SIM_DEPTHS)).toBe(true);
        for (const tick of ticks.slice(0, first))
          expect(tick.rows).toEqual(KEPT);
        for (const tick of ticks.slice(first))
          expect(tick.rows).toEqual(DELETED);

        // The payload is still retained, so the header row stays with it,
        // settled; once the payload is released, the header row goes too.
        expect(await store.getDaPayload(a.hash)).toBeDefined();
        expect(await store.getStateQueueHeader(a.hash)).toMatchObject({
          status: "merged",
        });
        const { prune } = await runRetentionCycle(
          store,
          retentionCycleOptions(config, service.latestL1View()!, Date.now()),
        );
        expect(prune.prunedHeaderHashes).toContain(a.hash);
        expect(await store.getStateQueueHeader(a.hash)).toBeUndefined();
      } finally {
        await factStore.close();
      }
    },
  );

  it(
    "deletes nothing under promise adoption, where retirement is the only deleter",
    { timeout: 180_000 },
    async () => {
      const factStore = await open();
      try {
        const { ticks, store, a, service, config } = await mergeAndExtend(
          factStore,
          { availabilityPromiseAdoption: ADOPTION },
        );
        for (const tick of ticks) expect(tick.rows).toEqual(KEPT);
        // The released payload leaves its header row to retirement too.
        const { prune } = await runRetentionCycle(
          store,
          retentionCycleOptions(config, service.latestL1View()!, Date.now()),
        );
        expect(prune.prunedHeaderHashes).toContain(a.hash);
        expect(await store.getStateQueueHeader(a.hash)).toMatchObject({
          status: "merged",
        });
        expect(await decisionRows(store, a.hash)).toEqual(KEPT);
      } finally {
        await factStore.close();
      }
    },
  );

  it(
    "deletes the signed decision of a header that never landed once a final block is past its end time, never before",
    { timeout: 180_000 },
    async () => {
      const factStore = await open();
      try {
        const harness = await committeeOnQueueChain(factStore);
        const { queue, apply, store } = harness;
        const fingerprint = harness.config.deploymentFingerprint;
        const service = await harness.service();
        await apply(queue.init());
        await apply(queue.append());
        const a = queue.nodes[0]!;
        await apply(queue.empty());
        await service.tick();
        const [own] = await signatures(a, fingerprint);
        await store.saveDaSignature(own!);
        // The commit is rolled back: A is on no chain the follower holds.
        await apply(queue.rollBack(2));
        await service.tick();
        const endTimeMs = Number(a.header.endTime);
        let deletedAt: number | null = null;
        // Eighteen blocks: three times the simulator's k.
        for (let block = 0; block < 18; block++) {
          await apply(queue.empty());
          await expect(service.tick()).resolves.toMatchObject({ errors: [] });
          const finalBlockTimeMs = service.latestL1View()!.finalBlockTimeMs;
          const signed = (await store.listSignedDecisions()).some(
            ({ headerHash }) => headerHash === a.hash,
          );
          if (finalBlockTimeMs === null || finalBlockTimeMs <= endTimeMs)
            expect(signed).toBe(true);
          else {
            deletedAt ??= block;
            expect(signed).toBe(false);
          }
        }
        expect(deletedAt).not.toBeNull();
        expect(await store.listDaSignatures(a.hash)).toEqual([]);
        // No payload is retained for A: its header row goes with the decision.
        expect(await store.getStateQueueHeader(a.hash)).toBeUndefined();
      } finally {
        await factStore.close();
      }
    },
  );
});

describe("pruneSignedDecisions", () => {
  it(
    "skips a header with a decision effect in flight in this store instance",
    { timeout: 120_000 },
    async () => {
      const factStore = await dialects[0][1]();
      try {
        const harness = await committeeOnQueueChain(factStore);
        const { queue, apply, store, config } = harness;
        await apply(queue.init());
        await apply(queue.append());
        const a = queue.nodes[0]!;
        await store.saveDaPayload(
          payloadRecord(a.hash, config.deploymentFingerprint),
        );
        await apply(queue.empty());
        const coordinator = countingCoordinator();
        const service = await harness.service({
          submitterReconciler: reconcilerFor(config, store, coordinator),
        });
        coordinator.hold();
        const tick = service.tick();
        while (coordinator.posts.length === 0)
          await new Promise((resolve) => setImmediate(resolve));
        await storeDecisionRows(store, a, config.deploymentFingerprint);
        expect(await store.pruneSignedDecisions([a.hash])).toEqual([]);
        expect(await decisionRows(store, a.hash)).toMatchObject({
          signed: true,
          signatures: 2,
          outbox: 1,
        });
        coordinator.release();
        await tick;
        expect(await store.pruneSignedDecisions([a.hash])).toEqual([a.hash]);
        expect(await decisionRows(store, a.hash)).toEqual(DELETED);
        // A payload is retained for A: its header row stays with it.
        expect(await store.getStateQueueHeader(a.hash)).toBeDefined();
      } finally {
        await factStore.close();
      }
    },
  );
});

describe("decisionsToPrune", () => {
  const hash = (n: number): string => n.toString(16).padStart(56, "0");
  const obligation = (
    n: number,
    state: Obligation["state"],
    decisionDeletable = true,
  ): Obligation => ({
    headerHash: hash(n),
    state,
    depth: null,
    level: null,
    liveStatus: null,
    retainCommitRecord: !decisionDeletable,
    decisionDeletable,
  });
  const inputs = (
    obligations: readonly Obligation[],
    overrides: Partial<DecisionPruneInputs> = {},
  ): DecisionPruneInputs => ({
    obligations,
    signed: obligations.map(({ headerHash }) => ({
      headerHash,
      endTimeMs: 1_000n,
    })),
    live: new Set(),
    finalQueue: [],
    unsettled: new Set(),
    finalBlockTimeMs: 1_001,
    ...overrides,
  });

  it("prunes only decisions obligations() reads deletable", () => {
    expect(
      decisionsToPrune(
        inputs([
          obligation(1, "final"),
          obligation(2, "cannot_land"),
          obligation(3, "beyond_retention"),
          obligation(4, "landed", false),
          obligation(5, "pending", false),
        ]),
      ),
    ).toEqual([hash(1), hash(2), hash(3)]);
  });

  it("keeps a final commit whose stored record is unsettled, or that a queue still reads", () => {
    const final = [obligation(1, "final")];
    expect(
      decisionsToPrune(inputs(final, { unsettled: new Set([hash(1)]) })),
    ).toEqual([]);
    expect(decisionsToPrune(inputs(final, { finalQueue: [hash(1)] }))).toEqual(
      [],
    );
    // An unsettled record of a header that never landed does not keep it.
    expect(
      decisionsToPrune(
        inputs([obligation(2, "cannot_land")], {
          unsettled: new Set([hash(2)]),
        }),
      ),
    ).toEqual([hash(2)]);
  });

  it("keeps a header live at the tip whose commit is final, past its end time, with no unsettled record", () => {
    // The tip's queue holds it on its own, whatever the final block's queue
    // and the stored records read.
    const final = [obligation(1, "final")];
    expect(decisionsToPrune(inputs(final))).toEqual([hash(1)]);
    expect(
      decisionsToPrune(inputs(final, { live: new Set([hash(1)]) })),
    ).toEqual([]);
  });

  it("keeps a final commit until the release clock is past its end time", () => {
    const final = [obligation(1, "final")];
    expect(
      decisionsToPrune(inputs(final, { finalBlockTimeMs: 1_000 })),
    ).toEqual([]);
    expect(decisionsToPrune(inputs(final, { finalBlockTimeMs: null }))).toEqual(
      [],
    );
    expect(decisionsToPrune(inputs(final, { signed: [] }))).toEqual([]);
  });

  it("prunes at most one batch per tick", () => {
    const many = Array.from({ length: DECISION_PRUNE_BATCH + 5 }, (_, n) =>
      obligation(n, "final"),
    );
    expect(decisionsToPrune(inputs(many))).toHaveLength(DECISION_PRUNE_BATCH);
  });
});
