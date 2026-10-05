import "./workflow-kupmios-source.captured-reference-body-reader-bounds.js";

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  readAdmittedLocalKupmiosSignedTransactionRecovery,
  rebroadcastAdmittedLocalKupmiosSignedTransaction,
} from "../src/workflow/index.js";
import { signedRecoveryFixture } from "./workflow-kupmios-source.signed-recovery-fixture.js";

describe("production signed intent recovery through concrete Kupo/Ogmios transports", () => {
  it.each([
    [{ referenceSpent: "stable", scriptOrdinary: true }, "invalidated"],
    [
      { referenceSpent: "volatile", scriptOrdinary: true },
      "invalidated_at_tip",
    ],
    [
      { referenceSpent: "stable", scriptOrdinary: true, spent: true },
      "invalidated",
    ],
    [
      { referenceSpent: "stable", scriptOrdinary: true, missing: true },
      "unknown",
    ],
    [
      {
        referenceSpent: "stable",
        scriptOrdinary: true,
        scriptCollateral: true,
      },
      "invalidated",
    ],
    [
      { referenceSpent: "mixed_stable_first", scriptOrdinary: true },
      "invalidated",
    ],
    [
      { referenceSpent: "mixed_volatile_first", scriptOrdinary: true },
      "invalidated",
    ],
    [
      {
        referenceSpent: "mixed_stable_first",
        scriptOrdinary: true,
        spent: true,
      },
      "invalidated",
    ],
  ] as const)(
    "retires impossible signed attempts without treating input roles as objective failures: %j",
    async (options, status) => {
      const fixture = await signedRecoveryFixture(options);
      expect(fixture.reference!.outputIndex).toBeLessThan(
        fixture.funding.outputIndex,
      );
      expect(
        CML.Address.from_bech32(fixture.reference!.address)
          .payment_cred()
          ?.as_script(),
      ).toBeDefined();
      const result = await readAdmittedLocalKupmiosSignedTransactionRecovery(
        fixture.input,
      );
      expect(result.status).toBe(status);
      if (status === "invalidated")
        expect(result.inputs.map(({ outRef }) => outRef)).toContain(
          `${fixture.funding.txHash}#${fixture.funding.outputIndex}`,
        );
      expect(fixture.submissions).toEqual([]);
    },
  );

  it.each([
    [{ referenceSpent: "stable", keyCollateral: true }, "invalidated"],
    [{ referenceSpent: "volatile", keyCollateral: true }, "invalidated_at_tip"],
    [
      { referenceSpent: "stable", keyCollateral: true, missing: true },
      "unknown",
    ],
    [{ referenceSpent: "stable" }, "invalidated"],
    [{ referenceSpent: "stable", ttl: null }, "invalidated"],
    [{ referenceSpent: "volatile", spent: true }, "invalidated_at_tip"],
    [
      { referenceSpent: "mixed_volatile_first", keyCollateral: true },
      "invalidated",
    ],
    [{ referenceSpent: "volatile" }, "invalidated_at_tip"],
    [{ referenceSpent: "mixed_stable_first" }, "invalidated"],
    [{ referenceSpent: "mixed_volatile_first" }, "invalidated"],
    [{ referenceSpent: "mixed_stable_first", spent: true }, "invalidated"],
    [{ referenceSpent: "stable", spent: true }, "invalidated"],
    [{ referenceSpent: "stable", missing: true }, "unknown"],
  ] as const)(
    "authenticates mixed reference and funding spends before retiring an attempt: %j",
    async (options, status) => {
      const fixture = await signedRecoveryFixture(options);
      expect(fixture.reference!.outputIndex).toBeLessThan(
        fixture.funding.outputIndex,
      );
      const result = await readAdmittedLocalKupmiosSignedTransactionRecovery(
        fixture.input,
      );
      expect(result.status).toBe(status);
      if (status === "invalidated")
        expect(result.inputs).toHaveLength(
          options.referenceSpent.startsWith("mixed_") ? 3 : 2,
        );
      expect(fixture.submissions).toEqual([]);
    },
  );

  it("authenticates release-final expiry for a signed transaction never submitted", async () => {
    const fixture = await signedRecoveryFixture({ ttl: 399 });
    const observed = await readAdmittedLocalKupmiosSignedTransactionRecovery(
      fixture.input,
    );
    expect(observed.status).toBe("expired");
    expect(observed.releaseFinalPoint.slot).toBe("400");
    expect(fixture.submissions).toEqual([]);
    expect(await fixture.lucid.utxosByOutRef([fixture.funding])).toHaveLength(
      1,
    );
  });

  it.each([
    [{ ttl: 399, missing: true }, "expired"],
    [{ ttl: 399, missing: true, referenceSpent: "stable" }, "expired"],
    [{ ttl: 900, missing: true }, "expired_at_tip"],
    [{ ttl: 399, missing: true, included: true }, "included"],
  ] as const)(
    "recovers dependent attempts after a parent rollback only with stable expiry or inclusion: %j",
    async (options, status) => {
      const fixture = await signedRecoveryFixture(options);
      const observed = await readAdmittedLocalKupmiosSignedTransactionRecovery(
        fixture.input,
      );
      expect(observed.status).toBe(status);
      expect(fixture.submissions).toEqual([]);
    },
  );

  it("distinguishes canonical expiry from merely passing TTL at the current tip", async () => {
    const fixture = await signedRecoveryFixture({ ttl: 900 });
    expect(
      (await readAdmittedLocalKupmiosSignedTransactionRecovery(fixture.input))
        .status,
    ).toBe("expired_at_tip");
    expect(fixture.submissions).toEqual([]);
  });

  it("rejects stable expiry when its canonical boundary rolls back during observation", async () => {
    const fixture = await signedRecoveryFixture({
      ttl: 399,
      missing: true,
      rollbackDuringExpiry: true,
    });
    await expect(
      readAdmittedLocalKupmiosSignedTransactionRecovery(fixture.input),
    ).rejects.toThrow();
    expect(fixture.submissions).toEqual([]);
  });

  it.each([6000, null])(
    "rebroadcasts the exact signed bytes only after authorization and the emulator accepts them (TTL: %s)",
    async (ttl) => {
      const fixture = await signedRecoveryFixture({ ttl });
      expect(
        (await readAdmittedLocalKupmiosSignedTransactionRecovery(fixture.input))
          .status,
      ).toBe("rebroadcast");
      const authorize = vi.fn(async (input) => {
        expect(input.signedTransactionCborHex).toBe(
          fixture.input.signedTransactionCborHex,
        );
        expect(fixture.submissions).toEqual([]);
      });
      expect(
        await rebroadcastAdmittedLocalKupmiosSignedTransaction({
          ...fixture.input,
          authorizeResubmission: authorize,
        }),
      ).toBe(fixture.signed.toHash());
      expect(authorize).toHaveBeenCalledOnce();
      expect(fixture.submissions).toEqual([
        fixture.input.signedTransactionCborHex,
      ]);
      fixture.emulator.awaitBlock();
      expect(await fixture.lucid.utxosByOutRef([fixture.funding])).toEqual([]);
    },
  );

  it("never broadcasts when the live authorization check refuses", async () => {
    const fixture = await signedRecoveryFixture();
    await expect(
      rebroadcastAdmittedLocalKupmiosSignedTransaction({
        ...fixture.input,
        authorizeResubmission: async () => {
          throw new Error("read-only reconciliation");
        },
      }),
    ).rejects.toThrow("read-only reconciliation");
    expect(fixture.submissions).toEqual([]);
  });

  it.each([
    [{ spent: true }, "invalidated"],
    [{ missing: true }, "unknown"],
    [{ ttl: null }, "rebroadcast"],
    [{ ttl: null, mempoolPresent: true }, "pending"],
    [{ ttl: null, missing: true }, "unknown"],
    [{ ttl: null, spent: true }, "invalidated"],
    [{ ttl: null, included: true }, "included"],
    [{ mempoolPresent: true }, "pending"],
  ] as const)(
    "distinguishes safely retired attempts from unresolved attempts: %j",
    async (options, status) => {
      const fixture = await signedRecoveryFixture(options);
      expect(
        (await readAdmittedLocalKupmiosSignedTransactionRecovery(fixture.input))
          .status,
      ).toBe(status);
      expect(fixture.submissions).toEqual([]);
    },
  );
});

it("rechecks canonicality before returning an included signed transaction", async () => {
  const included = await signedRecoveryFixture({ included: true });
  expect(
    (await readAdmittedLocalKupmiosSignedTransactionRecovery(included.input))
      .status,
  ).toBe("included");
  const rolledBack = await signedRecoveryFixture({
    included: true,
    rollbackDuringInclusion: true,
  });
  await expect(
    readAdmittedLocalKupmiosSignedTransactionRecovery(rolledBack.input),
  ).rejects.toThrow();
});
