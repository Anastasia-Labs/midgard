/**
 * The `validation_trace_replay_stuck` and `da_fetch_failure` alerts of a
 * header clear once the header leaves the observed queue, as the
 * `proof_family_coverage_gap` and `fault_detector_failed` alerts do: a
 * rollback that drops the header records no outcome for it, so nothing else
 * would clear them before a restart. A header still queued keeps its alerts
 * until it is decided, and a `da_fetch_failure` stands while some DA fetcher
 * may still fetch its header.
 */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type { RetainedDaPayloadSource } from "@al-ft/midgard-fault-proofs";
import { buildCanonicalBlockFixture } from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import type { PerHeaderAlert } from "../../src/fault-proofs/per-header-alert.js";
import { createWatcherReplayedLedger } from "../../src/fault-proofs/replayed-ledger.js";
import { createWatcherValidationTraceReplayStuckAlert } from "../../src/fault-proofs/validation-trace-replay-stuck-alert.js";
import {
  watcherDaFetchAlertSubject,
  watcherValidationTraceReplayStuckAlertSubject,
} from "../../src/runtime/operations-observability.alert-book.js";
import type { WatcherAlertCode } from "../../src/runtime/operations-observability.js";
import {
  bindWatcherRetainedDaOperations,
  createWatcherRetainedDaRuntime,
  registerWatcherDaFetcher,
} from "../../src/storage/retained-da-runtime.js";
import {
  admittedConfig,
  transportFactory,
} from "../storage/retained-da-runtime.fixtures.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import { harness } from "./fault-decision-bridge.harness.js";
import {
  headerFixture,
  observation,
} from "./fault-decision-bridge.observation.js";
import { operations, proof } from "./fault-decision-bridge.released-fixture.js";

const AUTHORITY = makeWatcherDeploymentAuthorityFixture();

const active = (
  observability: ReturnType<typeof operations>,
  code: WatcherAlertCode,
) =>
  observability.api
    .status()
    .activeAlerts.filter((alert) => alert.code === code)
    .map(({ subjectDigest }) => subjectDigest);

/** A queue of two headers, and the same queue after a rollback dropped the
 * second. Neither is decided: replaying the first fails, which postpones
 * the second. */
const rolledBackQueue = (
  observability: ReturnType<typeof operations>,
  alert: Pick<PerHeaderAlert, "reconcile">,
) => {
  const before = observation([headerFixture("01"), headerFixture("02")]);
  const after = observation([headerFixture("01")]);
  const [kept, dropped] = before.finalizedHeaders.map(
    ({ headerHash }) => headerHash,
  );
  const failure = new Error("replay of the block threw");
  const h = harness({
    current: before,
    categoryByHeader: { [kept!]: "doubleSpend", [dropped!]: "doubleSpend" },
    operationsSink: observability.sink,
    perHeaderAlerts: [alert],
    classifyOverride: () => {
      throw failure;
    },
  });
  h.admitted.add(after);
  return { before, after, kept: kept!, dropped: dropped!, failure, h };
};

type Block = Awaited<ReturnType<typeof buildCanonicalBlockFixture>>;

const emptyBlock = async (prevHeaderHash: string) =>
  await buildCanonicalBlockFixture({
    transactions: [],
    utxos: [],
    prevHeaderHash,
  });

/** The watcher's replayed ledger, its checkpoint moving to `confirmed`. */
const replayKeeper = (confirmed: () => Block) =>
  createWatcherReplayedLedger({
    follower: {
      epoch: () => 0,
      confirmed: async () => ({
        state: {
          ...SDK.makeGenesisConfirmedState(1000n),
          headerHash: confirmed().headerHash,
          utxoRoot: confirmed().header.utxosRoot,
        },
        current: () => true,
      }),
      materialFor: async (payload) => ({
        blockTimeMs: Number(payload.block_body.header.endTime),
        replayUserEvent: () => Effect.die("the block has no user event"),
        verifyForcedSource: () => Effect.die("the block has no forced order"),
      }),
      availabilityStatus: async () => "Unattested",
      checkpointDurable: () => undefined,
      close: () => undefined,
    },
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    availabilityAuthority: () => ({
      deploymentIdentity: "c7".repeat(28),
      responseGeometry: SDK.availabilityResponseGeometry({
        chunkByteLength: 4096,
        trancheByteLength: 64 * 1024,
        maxTrancheCount: 1024,
      }),
    }),
    checkpointPath: "unused",
    unsafeCheckpointStoreForTest: {
      read: async () => undefined,
      write: async () => undefined,
    },
  });

/** A source serving the payloads of `blocks` and no other. */
const serving = (blocks: readonly Block[]): RetainedDaPayloadSource => ({
  sourceId: "serving",
  fetchPayloadByHeaderHash: async (headerHash) => {
    const block = blocks.find((block) => block.headerHash === headerHash);
    if (block === undefined)
      return { ok: false, sourceId: "serving", attempts: [] };
    return {
      ok: true,
      sourceId: "serving",
      sourcePeerId: "peer",
      payloadEnvelopeCbor: block.payloadEnvelopeCbor,
      provenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "serving/peer",
        grade: "security",
      },
      attempts: [],
    };
  },
});

/** A bound diagnostics sink, and a retained-DA runtime every public DA peer
 * of which fails each fetch. */
const failingFetches = async (
  observability: ReturnType<typeof operations>,
  mayStillFetch?: () => Iterable<string>,
) => {
  const binding = bindWatcherRetainedDaOperations({
    deploymentIdentity: AUTHORITY.result,
    sink: observability.sink,
  });
  const fake = transportFactory();
  fake.request.mockRejectedValue(new Error("public DA peer did not answer"));
  const runtime = await createWatcherRetainedDaRuntime({
    watcherConfig: admittedConfig(),
    deploymentIdentity: AUTHORITY.result,
    unsafeTransportFactoryForTest: fake.factory,
    ...(mayStillFetch === undefined ? {} : { mayStillFetch }),
  });
  return {
    binding,
    runtime,
    close: async () => {
      await runtime.close();
      binding.close();
    },
  };
};

describe("fault decision bridge on the per-header alerts raised outside it", () => {
  it("clears the stuck-replay alert of a header a rollback drops and keeps the alert of a header still queued", async () => {
    const observability = operations();
    const alert = createWatcherValidationTraceReplayStuckAlert(
      observability.sink,
    );
    const { before, after, kept, dropped, failure, h } = rolledBackQueue(
      observability,
      alert,
    );
    alert.stuck({ headerHash: kept });
    alert.stuck({ headerHash: dropped });

    await expect(h.bridge.reconcileAndDispatch(before)).rejects.toBe(failure);
    expect(active(observability, "validation_trace_replay_stuck")).toEqual(
      [
        watcherValidationTraceReplayStuckAlertSubject(kept),
        watcherValidationTraceReplayStuckAlertSubject(dropped),
      ].sort(),
    );

    await expect(h.bridge.reconcileAndDispatch(after)).rejects.toBe(failure);
    expect(active(observability, "validation_trace_replay_stuck")).toEqual([
      watcherValidationTraceReplayStuckAlertSubject(kept),
    ]);
  });

  it("clears the failed DA fetch alert of a header a rollback drops and keeps the alert of a header still queued", async () => {
    const observability = operations();
    const binding = bindWatcherRetainedDaOperations({
      deploymentIdentity: AUTHORITY.result,
      sink: observability.sink,
    });
    const fake = transportFactory();
    fake.request.mockRejectedValue(new Error("public DA peer did not answer"));
    const runtime = await createWatcherRetainedDaRuntime({
      watcherConfig: admittedConfig(),
      deploymentIdentity: AUTHORITY.result,
      unsafeTransportFactoryForTest: fake.factory,
    });
    try {
      const { before, after, kept, dropped, failure, h } = rolledBackQueue(
        observability,
        binding,
      );
      for (const headerHash of [kept, dropped])
        expect(
          (await runtime.sources[0]!.fetchPayloadByHeaderHash(headerHash)).ok,
        ).toBe(false);

      await expect(h.bridge.reconcileAndDispatch(before)).rejects.toBe(failure);
      expect(active(observability, "da_fetch_failure")).toEqual(
        [
          watcherDaFetchAlertSubject(kept),
          watcherDaFetchAlertSubject(dropped),
        ].sort(),
      );

      await expect(h.bridge.reconcileAndDispatch(after)).rejects.toBe(failure);
      expect(active(observability, "da_fetch_failure")).toEqual([
        watcherDaFetchAlertSubject(kept),
      ]);
    } finally {
      await runtime.close();
      binding.close();
    }
  });

  it("keeps the failed DA fetch alert of a header merged at the tip while availability still holds it pending, and clears it once availability drops it", async () => {
    const observability = operations();
    // Availability's pending set, read at release depth: the merge has not
    // reached it, so the merged header is still pending there.
    const pending = new Set<string>();
    const binding = bindWatcherRetainedDaOperations({
      deploymentIdentity: AUTHORITY.result,
      sink: observability.sink,
    });
    const fake = transportFactory();
    fake.request.mockRejectedValue(new Error("public DA peer did not answer"));
    // Availability fetches through its own runtime, and names its pending
    // headers as those it may still fetch.
    const runtime = await createWatcherRetainedDaRuntime({
      watcherConfig: admittedConfig(),
      deploymentIdentity: AUTHORITY.result,
      unsafeTransportFactoryForTest: fake.factory,
      mayStillFetch: () => pending,
    });
    try {
      const {
        before,
        after,
        kept,
        dropped: merged,
        failure,
        h,
      } = rolledBackQueue(observability, binding);
      pending.add(kept).add(merged);
      const fetchFails = async () =>
        expect(
          (await runtime.sources[0]!.fetchPayloadByHeaderHash(merged)).ok,
        ).toBe(false);
      await fetchFails();
      await expect(h.bridge.reconcileAndDispatch(before)).rejects.toBe(failure);

      // Gone from the tip queue, still pending: each pass keeps the alert.
      for (let pass = 0; pass < 2; pass += 1) {
        await fetchFails();
        await expect(h.bridge.reconcileAndDispatch(after)).rejects.toBe(
          failure,
        );
        expect(active(observability, "da_fetch_failure")).toEqual([
          watcherDaFetchAlertSubject(merged),
        ]);
      }

      // The merge reached release depth: availability no longer holds it.
      pending.delete(merged);
      await expect(h.bridge.reconcileAndDispatch(after)).rejects.toBe(failure);
      expect(active(observability, "da_fetch_failure")).toEqual([]);
    } finally {
      await runtime.close();
      binding.close();
    }
  });

  it("keeps the failed DA fetch alert of a released header while a fetcher still names it", async () => {
    const observability = operations();
    const headerHash = "ab".repeat(28);
    const named = new Set([headerHash]);
    const { binding, runtime, close } = await failingFetches(
      observability,
      () => named,
    );
    try {
      expect(
        (await runtime.sources[0]!.fetchPayloadByHeaderHash(headerHash)).ok,
      ).toBe(false);
      const released = new Map([[headerHash, proof(headerHash)]]);
      binding.reconcile(new Set([headerHash]), released);
      expect(active(observability, "da_fetch_failure")).toEqual([
        watcherDaFetchAlertSubject(headerHash),
      ]);

      named.clear();
      binding.reconcile(new Set([headerHash]), released);
      expect(active(observability, "da_fetch_failure")).toEqual([]);
    } finally {
      await close();
    }
  });

  it("keeps the failed DA fetch alert of a merged header the replayed ledger still fetches, and clears it once the checkpoint passes it", async () => {
    const observability = operations();
    const { binding, runtime, close } = await failingFetches(observability);
    // Two merged blocks, in no observed queue: the checkpoint moves to the
    // later one, whose replay starts from the earlier one's ledger.
    const parent = await emptyBlock(SDK.GENESIS_HEADER_HASH);
    const head = await emptyBlock(parent.headerHash);
    const keeper = replayKeeper(() => head);
    const unregister = registerWatcherDaFetcher(
      AUTHORITY.result,
      keeper.mayStillFetch,
    );
    try {
      const { after, failure, h } = rolledBackQueue(observability, binding);
      // Each pass the checkpoint advance asks for the earlier block again,
      // and every public DA peer fails it.
      for (let pass = 0; pass < 2; pass += 1) {
        expect(
          await keeper.advance(observation([]), [
            serving([head]),
            ...runtime.sources,
          ]),
        ).toMatchObject([{ reason: "replayed_ledger_source_pending" }]);
        await expect(h.bridge.reconcileAndDispatch(after)).rejects.toBe(
          failure,
        );
        expect(active(observability, "da_fetch_failure")).toEqual([
          watcherDaFetchAlertSubject(parent.headerHash),
        ]);
      }

      // Another source serves it: the checkpoint passes both blocks.
      expect(
        await keeper.advance(observation([]), [serving([head, parent])]),
      ).toEqual([]);
      await expect(h.bridge.reconcileAndDispatch(after)).rejects.toBe(failure);
      expect(active(observability, "da_fetch_failure")).toEqual([]);
    } finally {
      unregister();
      await close();
    }
  });

  it("drops the failed DA fetch alert of a header no replay asks for again once the checkpoint moved twice", async () => {
    const observability = operations();
    const { binding, runtime, close } = await failingFetches(observability);
    const first = await emptyBlock(SDK.GENESIS_HEADER_HASH);
    const second = await emptyBlock(first.headerHash);
    let confirmed = first;
    const keeper = replayKeeper(() => confirmed);
    const unregister = registerWatcherDaFetcher(
      AUTHORITY.result,
      keeper.mayStillFetch,
    );
    // A classification's ledger walk asked for a header that later left
    // the chain: it is never merged, and no replay asks for it again.
    const removed = "0d".repeat(28);
    try {
      const { after, failure, h } = rolledBackQueue(observability, binding);
      await expect(keeper.ledgerAt(runtime.sources)(removed)).rejects.toThrow();
      const sources = [serving([first, second])];

      // A checkpoint advance that finds it at the confirmed head already
      // does not move it.
      for (let pass = 0; pass < 2; pass += 1) {
        expect(await keeper.advance(observation([]), sources)).toEqual([]);
        await expect(h.bridge.reconcileAndDispatch(after)).rejects.toBe(
          failure,
        );
        expect(active(observability, "da_fetch_failure")).toEqual([
          watcherDaFetchAlertSubject(removed),
        ]);
      }

      confirmed = second;
      expect(await keeper.advance(observation([]), sources)).toEqual([]);
      await expect(h.bridge.reconcileAndDispatch(after)).rejects.toBe(failure);
      expect(active(observability, "da_fetch_failure")).toEqual([]);
    } finally {
      unregister();
      await close();
    }
  });
});
