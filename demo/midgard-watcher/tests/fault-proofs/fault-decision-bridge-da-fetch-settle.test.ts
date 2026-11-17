/**
 * A header's `da_fetch_failure` settles through the DA fetchers' binding:
 * the outcome of the header, or the deferral of a later header on its
 * payload, clears it at once unless a DA fetcher still names the header,
 * and once none does otherwise.
 */
import {
  FraudProofL1UnavailableError,
  RetainedDaPayloadUnavailableError,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { watcherDaFetchAlertSubject } from "../../src/runtime/operations-observability.alert-book.js";
import { registerWatcherDaFetcher } from "../../src/storage/retained-da-runtime.js";
import {
  active,
  AUTHORITY,
  emptyBlock,
  failingFetches,
  replayKeeper,
  serving,
} from "./fault-decision-bridge.da-fetchers-fixture.js";
import { harness } from "./fault-decision-bridge.harness.js";
import {
  headerFixture,
  observation,
} from "./fault-decision-bridge.observation.js";
import {
  ATTESTED,
  operations,
  records,
} from "./fault-decision-bridge.released-fixture.js";

describe("fault decision bridge settling a failed DA fetch", () => {
  it("clears the failed DA fetch alert of a decided header still queued once no fetcher names it, and keeps a later failure", async () => {
    const observability = operations();
    const named = new Set<string>();
    const { binding, runtime, close } = await failingFetches(
      observability,
      () => named,
    );
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [queued] = current.finalizedHeaders;
    const h = harness({
      current,
      categoryByHeader: { [queued!.headerHash]: "transitionTrace" },
      operationsSink: observability.sink,
      nowMs: () => 3n,
      perHeaderAlerts: [binding],
      daFetchAlert: binding,
    });
    try {
      named.add(queued!.headerHash);
      expect(
        (await runtime.sources[0]!.fetchPayloadByHeaderHash(queued!.headerHash))
          .ok,
      ).toBe(false);

      // Decided while a fetcher still names it: the alert stands.
      await h.bridge.reconcileAndDispatch(current);
      expect(active(observability, "da_fetch_failure")).toEqual([
        watcherDaFetchAlertSubject(queued!.headerHash),
      ]);

      // Still queued, no longer named: its outcome settled it.
      named.clear();
      await h.bridge.reconcileAndDispatch(current);
      expect(active(observability, "da_fetch_failure")).toEqual([]);

      // A later failure is not the one its outcome accounted for.
      expect(
        (await runtime.sources[0]!.fetchPayloadByHeaderHash(queued!.headerHash))
          .ok,
      ).toBe(false);
      binding.reconcile(new Set([queued!.headerHash]), new Map());
      expect(active(observability, "da_fetch_failure")).toEqual([
        watcherDaFetchAlertSubject(queued!.headerHash),
      ]);
    } finally {
      await close();
    }
  });

  it("keeps the failed DA fetch alert of a header whose classification is only deferred", async () => {
    const observability = operations();
    const { binding, runtime, close } = await failingFetches(observability);
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [queued] = current.finalizedHeaders;
    const h = harness({
      current,
      categoryByHeader: { [queued!.headerHash]: "transitionTrace" },
      operationsSink: observability.sink,
      nowMs: () => 3n,
      perHeaderAlerts: [binding],
      daFetchAlert: binding,
      classifyOverride: () => {
        throw new FraudProofL1UnavailableError("the node did not answer");
      },
    });
    try {
      expect(
        (await runtime.sources[0]!.fetchPayloadByHeaderHash(queued!.headerHash))
          .ok,
      ).toBe(false);
      await h.bridge.reconcileAndDispatch(current);
      expect(records(observability).map(({ outcome }) => outcome)).toEqual([
        "pending_l1",
      ]);
      expect(active(observability, "da_fetch_failure")).toEqual([
        watcherDaFetchAlertSubject(queued!.headerHash),
      ]);
    } finally {
      await close();
    }
  });

  it("keeps the failed DA fetch alert of a merged header the replayed ledger still fetches while a queued header defers on it, and clears it once it is served", async () => {
    const observability = operations();
    const { binding, runtime, close } = await failingFetches(observability);
    const merged = await emptyBlock(SDK.GENESIS_HEADER_HASH);
    const head = await emptyBlock(merged.headerHash);
    const keeper = replayKeeper(() => head);
    const unregister = registerWatcherDaFetcher(
      AUTHORITY.result,
      keeper.mayStillFetch,
    );
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [queued] = current.finalizedHeaders;
    let missing = true;
    const h = harness({
      current,
      categoryByHeader: { [queued!.headerHash]: "transitionTrace" },
      operationsSink: observability.sink,
      mergedHeaders: async () => new Map(),
      // Inside the queued header's challengeability window.
      nowMs: () => 3n,
      perHeaderAlerts: [binding],
      daFetchAlert: binding,
      // The queued header's classification needs the merged block too.
      classifyOverride: (fresh) => {
        if (missing)
          throw new RetainedDaPayloadUnavailableError(
            merged.headerHash,
            "every public DA peer failed the merged block",
            "unreachable",
          );
        return fresh;
      },
    });
    try {
      for (let pass = 0; pass < 2; pass += 1) {
        expect(
          await keeper.advance(observation([]), [
            serving([head]),
            ...runtime.sources,
          ]),
        ).toMatchObject([{ reason: "replayed_ledger_source_pending" }]);
        expect(
          (await h.bridge.reconcileAndDispatch(current)).target,
        ).toBeNull();
        expect(active(observability, "da_fetch_failure")).toEqual([
          watcherDaFetchAlertSubject(merged.headerHash),
        ]);
      }

      // Another source serves it: the checkpoint passes both blocks.
      expect(
        await keeper.advance(observation([]), [serving([head, merged])]),
      ).toEqual([]);
      missing = false;
      await h.bridge.reconcileAndDispatch(current);
      expect(active(observability, "da_fetch_failure")).toEqual([]);
    } finally {
      unregister();
      await close();
    }
  });
});
