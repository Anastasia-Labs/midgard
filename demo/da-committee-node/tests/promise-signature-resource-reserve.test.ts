import { describe, expect, it, vi } from "vitest";

import { promiseSignatureResourceReserve } from "../src/availability/promise-signature-resource-reserve.js";
import { CommitteeService } from "../src/committee-service.js";
import type { AttestationCoordinator } from "../src/coordinator/coordinator.js";
import type { DaSignatureRecord } from "../src/domain.js";
import { loadDaSigner } from "../src/signer.js";
import { promiseAdmissionFixture } from "./helpers/promise-admission.js";

describe("prospective signature storage reserve", () => {
  it("covers the actual durable signature plus publication outbox before the external callback", async () => {
    const f = await promiseAdmissionFixture();
    const captures = new Map<
      string,
      {
        reserve: Awaited<ReturnType<typeof promiseSignatureResourceReserve>>;
        before: Awaited<ReturnType<typeof f.store.promiseStoreResourceUsage>>;
      }
    >();
    const projectionFailures: unknown[] = [];
    Object.assign(f.source, {
      projectResources: async (
        candidate: Parameters<
          typeof promiseSignatureResourceReserve
        >[0]["candidate"],
      ) => {
        let projected: Awaited<
          ReturnType<typeof promiseSignatureResourceReserve>
        >;
        try {
          projected = await promiseSignatureResourceReserve({
            candidate,
            config: { ...f.config, cardanoL1Source: { networkMagic: 1 } },
            store: f.store,
          });
        } catch (error) {
          projectionFailures.push(String(error));
          throw error;
        }
        captures.set(candidate.record.headerHash, {
          before: await f.store.promiseStoreResourceUsage(),
          reserve: projected,
        });
        return projected;
      },
      readResourceWorkload: async () => ({
        ...(await f.store.promiseStoreResourceUsage()),
        journalEntries: 0,
        walletInputs: 1,
      }),
    });
    const publishSignature = vi.fn(async (signature: DaSignatureRecord) => {
      const capture = captures.get(signature.headerHash)!;
      const actual = await f.store.promiseStoreResourceUsage();
      expect(await f.store.listDaSignatures()).toHaveLength(1);
      expect(await f.store.listDecisionOutbox()).toHaveLength(1);
      expect(capture.reserve.storeRecords).toBe(2);
      expect(actual.storeRecords - capture.before.storeRecords).toBe(2);
      expect(
        actual.storeEncodedBytes - capture.before.storeEncodedBytes,
      ).toBeLessThanOrEqual(capture.reserve.storeEncodedBytes);
      return "posted" as const;
    });
    const service = new CommitteeService({
      config: f.config,
      store: f.store,
      l1: f.provider,
      payloadSource: f.payloadSource,
      signer: await loadDaSigner(`hex:${"00".repeat(31)}01`),
      signerValidation: f.signerValidation,
      promiseAdmission: f.admission(),
      coordinator: { publishSignature } as unknown as AttestationCoordinator,
      writeEvent: () => {},
    });
    await service.initialize();
    // Discovery first persists the new header's complete source observation;
    // a projection cannot borrow an observation that is not durable yet.
    expect(await service.tick()).toMatchObject({
      signedHeaders: 0,
      errors: [],
    });
    expect(projectionFailures).toEqual([
      "Error: Prospective source observation is unavailable",
      "Error: Prospective source observation is unavailable",
    ]);
    projectionFailures.length = 0;
    const tick = await service.tick();
    expect(projectionFailures).toEqual([]);
    expect(tick).toMatchObject({ signedHeaders: 1, errors: [] });
    expect(publishSignature).toHaveBeenCalledTimes(1);
    expect(await f.store.listDecisionOutbox()).toMatchObject([
      { status: "published" },
    ]);
  });

  it("refuses record growth after capture before invoking the signer", async () => {
    const f = await promiseAdmissionFixture();
    const cap = 10000;
    let reads = 0;
    Object.assign(f.source, {
      projectResources: async () => ({
        storeRecords: 2,
        storeEncodedBytes: 100,
      }),
      readResourceWorkload: async () => ({
        walletInputs: 1,
        journalEntries: 0,
        storeRecords: ++reads === 1 ? cap : cap + 1,
        storeEncodedBytes: 0,
      }),
    });
    const service = f.service();
    await service.initialize();
    expect(await service.tick()).toMatchObject({
      signedHeaders: 0,
      errors: [],
    });
    expect(reads).toBeGreaterThan(0);
    expect(f.sign).not.toHaveBeenCalled();
    expect(await f.store.listDaSignatures()).toEqual([]);
  });
});
