import { describe, expect, it, vi } from "vitest";

import { canonicalOgmiosBlockDepth } from "../src/services/state-queue-correction-observer.canonical-block-depth.js";
import { makeLocalKupmiosStateQueueCorrectionSource } from "../src/services/state-queue-correction-observer.make-local-kupmios-state-queue-correction-source.js";
import {
  makeState,
  STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
} from "../src/services/state-queue-correction-observer.parse-state-queue-correction-observer-state.js";
import { reconcileStateQueueCorrectionObserver } from "../src/services/state-queue-correction-observer.reconcile-state-queue-correction-observer.js";
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

const block = { blockHash: h32("9"), slot: 100, blockNo: 90n };
const tip = { id: h32("f"), slot: 9999, height: 2251 };
const read = (factory: ReturnType<typeof intersectionSocket>["factory"]) =>
  canonicalOgmiosBlockDepth({
    ogmiosUrl: "ws://ogmios.test",
    ...block,
    timeoutMs: 100,
    webSocketFactory: factory,
  });

describe("selected-chain canonical depth", () => {
  it("uses the exact intersection and its same-response tip; repeated reads see rollback", async () => {
    let canonical = true;
    const socket = intersectionSocket((request) =>
      canonical
        ? { result: { intersection: request.params.points[0], tip } }
        : {
            error: {
              code: 1000,
              message: "Intersection not found",
              data: { tip },
            },
          },
    );
    expect(await read(socket.factory)).toBe(2162n);
    canonical = false;
    expect(await read(socket.factory)).toBeNull();
    expect(socket.requests).toHaveLength(2);
    expect(socket.close).toHaveBeenCalledTimes(2);
    expect(socket.requests[0]).toMatchObject({
      method: "findIntersection",
      params: { points: [{ slot: block.slot, id: block.blockHash }] },
    });
  });

  it.each([
    { intersection: { slot: 100, id: h32("8") }, tip },
    {
      intersection: { slot: 100, id: block.blockHash },
      tip: { id: tip.id, slot: tip.slot },
    },
    {
      intersection: { slot: 100, id: block.blockHash },
      tip: { ...tip, height: 89 },
    },
    { intersection: "origin", tip },
    {
      intersection: { slot: 100, id: block.blockHash },
      tip: { id: block.blockHash, slot: 100, height: 2251 },
    },
  ])(
    "rejects mismatch, missing height, older tip and fabricated intersection %#",
    async (result) => {
      const socket = intersectionSocket(() => ({ result }));
      await expect(read(socket.factory)).rejects.toThrow("exact intersection");
      expect(socket.close).toHaveBeenCalledOnce();
    },
  );

  it("treats a transport/protocol error as unavailable rather than rollback evidence", async () => {
    const socket = intersectionSocket(() => ({
      error: { code: 1001, message: "Interleaved request" },
    }));
    await expect(read(socket.factory)).rejects.toThrow();
    expect(socket.close).toHaveBeenCalledOnce();
  });

  it("bounds a socket that never answers", async () => {
    const socket = intersectionSocket(() => ({ result: {} }));
    const silent = (url: string) => ({
      ...socket.factory(url),
      send: () => undefined,
    });
    await expect(read(silent)).rejects.toThrow();
    expect(socket.close).toHaveBeenCalledOnce();
  });

  it("does not freeze a shallow orphan when Kupo is stale and Ogmios has grown on the winning branch", async () => {
    const transition = authenticatedTransition();
    let caughtUp = false;
    const socket = intersectionSocket(() => ({
      error: {
        code: 1000,
        message: "Intersection not found",
        data: { tip: { ...tip, height: 3100 } },
      },
    }));
    const http = vi.fn(async (url: string, init?: RequestInit) => {
      // The old implementation could obtain a coherent newer tip here while
      // Kupo still named C on the losing branch. The fixed reader never uses it.
      if (!url.includes("/matches/"))
        return ogmiosTipResponse(init, { id: tip.id, slot: tip.slot }, 3100);
      const match = /matches\/(\d+)@([0-9a-f]{64})/u.exec(url)!;
      return new Response(
        JSON.stringify([
          {
            transaction_id: match[2],
            output_index: Number(match[1]),
            datum: null,
            spent_at: caughtUp
              ? null
              : {
                  transaction_id: transition.transactionHash,
                  input_index: 0,
                  redeemer: null,
                  slot_no: Number(transition.slot),
                  header_hash: transition.blockHash,
                },
          },
        ]),
      );
    });
    const source = makeLocalKupmiosStateQueueCorrectionSource({
      deploymentIdentityDigest: deployment,
      stateQueuePolicyId: policy,
      stateQueueAddress: "fixture-queue",
      hubOraclePolicyId: h28("a"),
      correctionLockAddress: "fixture-lock",
      fraudProofPolicyId: h28("e"),
      fraudProofAddress: "fixture-proof",
      kupoUrl: "http://kupo.test",
      ogmiosUrl: "ws://ogmios.test",
      fetchImpl: http,
      webSocketFactory: socket.factory,
      readQueue: async () => transition.previousQueue,
    });
    expect(await source.canonicalDepth(transition)).toBeNull();
    const store = memoryStore();
    await store.save(
      makeState({
        schemaVersion: STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
        deploymentIdentityDigest: deployment,
        stateQueuePolicyId: policy,
        cursorQueue: transition.nextQueue,
        pending: [],
        admitted: [transition],
        retractedTransactionHashes: [],
        postFinalityRollbackIncidents: [],
      }),
    );
    const provenFinal = new Set<string>();
    const revoke = vi.fn(async () => undefined);
    const restore = vi.fn(async () => undefined);
    caughtUp = true;
    const result = await reconcileStateQueueCorrectionObserver({
      source,
      store,
      deploymentIdentityDigest: deployment,
      stateQueuePolicyId: policy,
      requiredFinalityDepth: 30n,
      provenFinal,
      reinclude: async () => undefined,
      revokeTerminal: revoke,
      restoreAfterRollback: restore,
    });
    expect(provenFinal.size).toBe(0);
    expect(result.retractedTransactionHashes).toEqual([
      transition.transactionHash,
    ]);
    expect(revoke).toHaveBeenCalledOnce();
    expect(restore).toHaveBeenCalledOnce();
    expect(http.mock.calls.every(([url]) => url.includes("/matches/"))).toBe(
      true,
    );
  });
});
