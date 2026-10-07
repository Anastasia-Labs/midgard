import { describe, expect, it, vi } from "vitest";

import { assertCommitteePromiseEnrollment } from "../src/availability/promise-profile-selection.js";
import { CommitteeService } from "../src/committee-service.js";
import { signVerifiedCommitteePayload } from "../src/committee-service.sign-verified-payload.js";
import { committeePromiseAdoptionConfig } from "../src/config.promise-admission.js";
import { retentionFixture } from "./helpers/committee-retirement.js";
import { promiseAdmissionFixture } from "./helpers/promise-admission.js";

describe("explicit committee promise profile selection", () => {
  it("preserves verified ordinary signing without numerical adoption", async () => {
    const f = await promiseAdmissionFixture();
    const store = f.store;
    await assertCommitteePromiseEnrollment(f.config, store);
    const service = new CommitteeService({
      config: f.config,
      store,
      stateQueueProvider: f.provider,
      payloadSource: f.payloadSource,
      signer: {
        publicKeyHex: f.signerValidation.signerPublicKeyHex,
        sign: f.sign,
      },
      signerValidation: f.signerValidation,
      writeEvent: () => {},
    });
    await service.initialize();
    expect(await service.tick()).toMatchObject({
      signedHeaders: 2,
      errors: [],
    });
    expect(f.sign).toHaveBeenCalledTimes(2);
    const readiness = await service.readinessSnapshot();
    expect(readiness.promiseAdmissionPolicy).toBeUndefined();
    expect(readiness.reasons.some((r) => r.includes("promise_policy"))).toBe(
      false,
    );
    expect(await store.listDaSignatures()).toHaveLength(2);
    expect(await store.getRetirementFloor()).toBeUndefined();
  });

  it("retains durable enrollment through restart and refuses missing authority before signing", async () => {
    const f = await promiseAdmissionFixture();
    const retirement = await retentionFixture(f.store);
    try {
      await retirement.compact();
      const seeded = await retirement.seed(1);
      await f.store.close();
      const reopened = await f.open();
      expect(await reopened.getRetirementFloor()).toBeDefined();
      const startupRejected = await assertCommitteePromiseEnrollment(
        f.config,
        reopened,
      ).then(
        () => false,
        (error: unknown) =>
          error instanceof Error &&
          error.message.includes("missing on restart"),
      );
      const observe = vi.fn();
      const signature = await signVerifiedCommitteePayload(
        {
          config: f.config,
          store: reopened,
          stateQueueProvider: f.provider,
          payloadSource: f.payloadSource,
          signer: {
            publicKeyHex: f.signerValidation.signerPublicKeyHex,
            sign: f.sign,
          },
          signerValidation: f.signerValidation,
        },
        seeded.header,
        seeded.verified,
        observe,
      );
      expect({ startupRejected, signature }).toEqual({
        startupRejected: true,
        signature: undefined,
      });
      expect(f.sign).not.toHaveBeenCalled();
      expect(observe).toHaveBeenCalledWith(
        expect.objectContaining({
          status: "incomplete_evidence",
          reason: "finite_runtime_policy_unavailable",
        }),
      );
    } finally {
      retirement.journal.close();
    }
  });

  it("preserves the final synchronous generation fence in ordinary mode", async () => {
    const f = await promiseAdmissionFixture();
    const retirement = await retentionFixture(f.store);
    try {
      const seeded = await retirement.seed(1);
      const original = f.store.getRetirementFloor.bind(f.store);
      vi.spyOn(f.store, "getRetirementFloor").mockImplementationOnce(
        async () => {
          await retirement.compact();
          expect(await original()).toBeDefined();
          return undefined;
        },
      );
      await expect(
        signVerifiedCommitteePayload(
          {
            config: f.config,
            store: f.store,
            stateQueueProvider: f.provider,
            payloadSource: f.payloadSource,
            signer: {
              publicKeyHex: f.signerValidation.signerPublicKeyHex,
              sign: f.sign,
            },
            signerValidation: f.signerValidation,
          },
          seeded.header,
          seeded.verified,
          vi.fn(),
        ),
      ).rejects.toThrow("generation changed");
      expect(f.sign).not.toHaveBeenCalled();
    } finally {
      retirement.journal.close();
    }
  });

  it("does not clear ordinary source quarantine or claim readiness without adoption", async () => {
    const f = await promiseAdmissionFixture();
    const service = f.service(f.store, false);
    await service.initialize();
    const source = (await f.store.getL1SourceState())!;
    await f.store.saveL1SourceState({
      ...source,
      status: "quarantined",
      quarantineReason: "controlled existing source hold",
      quarantinedAt: new Date().toISOString(),
    });
    expect(await service.tick()).toMatchObject({ signedHeaders: 0 });
    expect(f.sign).not.toHaveBeenCalled();
    expect(await service.readinessSnapshot()).toMatchObject({ ready: false });
    expect((await f.store.getL1SourceState())?.status).toBe("quarantined");
  });

  it("refuses partial selection instead of treating it as ordinary mode", () => {
    expect(committeePromiseAdoptionConfig({})).toBeUndefined();
    expect(() =>
      committeePromiseAdoptionConfig({
        DA_PROMISE_POLICY_ARTIFACT_PATH: "/selected/policy",
      }),
    ).toThrow("Explicit promise policy adoption requires");
  });
});
