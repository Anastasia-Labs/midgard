import { rmSync } from "node:fs";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type DaAvailabilityOperationObservation,
  reconcileDaAvailabilityOperations,
  runDaAvailabilityOperation,
} from "@al-ft/midgard-sdk";
import { afterEach, describe, expect, it } from "vitest";

import {
  confirm,
  dirs,
  fixture,
  included,
  MIN_DEPTH,
} from "./availability-challenge-operation-rollback.fixture.js";

/**
 * Confirmation depth is not finality. A confirmed intent that a rollback
 * deeper than the confirmation depth contradicts is rewound and its exact
 * signed bytes land again; nothing is re-signed, nothing it reserved is
 * released early, and evidence about one intent never stops another. The
 * emulator cannot roll back, so the canonical observation is injected at the
 * provider seam while the journal and signed transactions are real.
 */
afterEach(() =>
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true })),
);

describe("availability reconciliation after a rollback deeper than the confirmation depth", () => {
  it("rewinds a confirmed intent within recovery depth and rebroadcasts its identical bytes, never a replacement", async () => {
    const f = await fixture();
    try {
      const record = await f.prepare("bb".repeat(28), f.coins[0]!);
      await confirm(f);
      expect(f.journal.get(record.intent.id)?.state).toBe("confirmed");
      // Confirmation depth releases no reservation.
      expect(f.journal.reservedOutRefs(f.context.actor)).toEqual(
        record.intent.spentOutRefs,
      );
      f.submitted.splice(0);
      // The block holding it is gone and its inputs are back, before validity.
      f.observe(() => ({ status: "unspent", currentSlot: 0 }));
      await expect(
        reconcileDaAvailabilityOperations(f.context),
      ).resolves.toMatchObject([
        { status: "submitted", txHash: record.intent.txHash },
      ]);
      expect(f.submitted).toEqual([record.intent.signedCbor]);
      expect(f.journal.get(record.intent.id)?.state).toBe("pending");
      expect(f.journal.reservedOutRefs(f.context.actor)).toEqual(
        record.intent.spentOutRefs,
      );
      // The next operation reconciles the original; nothing new is signed.
      f.observe(() => ({ status: "unknown", reason: "mempool" }));
      await expect(
        runDaAvailabilityOperation(f.context, {
          action: "prepare",
          headerHash: "bb".repeat(28),
          build: async () => {
            throw new Error("A replacement must never be built");
          },
        }),
      ).resolves.toMatchObject({
        status: "waiting",
        txHash: record.intent.txHash,
      });
      expect(f.build).toHaveBeenCalledTimes(1);
      // The same bytes land again and confirm again.
      f.observe((txHash) => included(txHash, 2));
      await reconcileDaAvailabilityOperations(f.context);
      expect(f.journal.get(record.intent.id)?.state).toBe("included");
      await confirm(f);
      expect(f.journal.get(record.intent.id)?.state).toBe("confirmed");
      // Rolled back once more, now past validity: it can never land, so it
      // expires and only then frees its inputs.
      f.submitted.splice(0);
      f.observe(() => ({
        status: "unspent",
        currentSlot: record.intent.validUntilSlot,
      }));
      await expect(
        reconcileDaAvailabilityOperations(f.context),
      ).resolves.toMatchObject([{ status: "expired" }]);
      expect(f.submitted).toEqual([]);
      expect(f.journal.reservedOutRefs(f.context.actor)).toEqual([]);
    } finally {
      f.journal.close();
    }
  });

  it.each([
    [
      "unknown evidence",
      () => ({ status: "unknown", reason: "source lagging" }),
      { status: "waiting" },
    ],
    [
      "missing-input evidence that is not canonical",
      () => ({
        status: "inputs_missing",
        currentSlot: 0,
        missingOutRefs: [`${"ff".repeat(32)}#0`],
      }),
      {
        status: "held",
        detail: expect.stringMatching(
          /Missing-input evidence .* not canonical/,
        ),
      },
    ],
    [
      "missing inputs that prove no expiry",
      (spent: readonly string[]) => ({
        status: "inputs_missing",
        currentSlot: 0,
        missingOutRefs: spent,
      }),
      { status: "waiting" },
    ],
  ] as const)(
    "never rewinds a confirmed intent on %s",
    async (_label, evidence, expected) => {
      const f = await fixture();
      try {
        const record = await f.prepare("f6".repeat(28), f.coins[0]!);
        await confirm(f);
        const confirmed = f.journal.get(record.intent.id);
        expect(confirmed?.state).toBe("confirmed");
        f.submitted.splice(0);
        f.observe(
          () =>
            evidence(
              record.intent.spentOutRefs,
            ) as DaAvailabilityOperationObservation,
        );
        await expect(
          reconcileDaAvailabilityOperations(f.context),
        ).resolves.toEqual([
          expect.objectContaining({
            ...expected,
            txHash: record.intent.txHash,
          }),
        ]);
        // Nothing changed: still confirmed, still reserved, nothing resent.
        expect(f.journal.get(record.intent.id)).toEqual(confirmed);
        expect(f.journal.reservedOutRefs(f.context.actor)).toEqual(
          record.intent.spentOutRefs,
        );
        expect(f.submitted).toEqual([]);
      } finally {
        f.journal.close();
      }
    },
  );

  it("surfaces a conflict when a rewound intent's released input was reserved by a newer intent", async () => {
    const f = await fixture();
    try {
      const a = await f.prepare("a7".repeat(28), f.coins[0]!);
      // Confirmed with the tip past its validity: its input is released.
      f.observe((txHash) =>
        included(txHash, MIN_DEPTH, a.intent.validUntilSlot),
      );
      await reconcileDaAvailabilityOperations(f.context);
      expect(f.journal.reservedOutRefs(f.context.actor)).toEqual([]);
      // The tip rolls back below that validity and the same coin funds B.
      f.observe((txHash) =>
        txHash === a.intent.txHash
          ? included(txHash, MIN_DEPTH, 0)
          : { status: "unknown", reason: "mempool" },
      );
      const b = await f.prepare("b8".repeat(28), f.coins[0]!);
      expect(b.intent.spentOutRefs).toEqual(a.intent.spentOutRefs);
      f.submitted.splice(0);
      // A itself left the chain: its input is back, before its validity.
      f.observe((txHash) =>
        txHash === a.intent.txHash
          ? { status: "unspent", currentSlot: 0 }
          : { status: "unknown", reason: "mempool" },
      );
      const results = await reconcileDaAvailabilityOperations(f.context);
      expect(results).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            status: "conflict",
            txHash: a.intent.txHash,
            detail: expect.stringContaining(
              `also reserved by ${a.intent.spentOutRefs[0]} (intent ${b.intent.id})`,
            ),
          }),
        ]),
      );
      expect(f.journal.get(a.intent.id)?.state).toBe("conflict");
      expect(f.submitted).toEqual([]);
      expect(f.journal.reservedOutRefs(f.context.actor)).toEqual(
        a.intent.spentOutRefs,
      );
    } finally {
      f.journal.close();
    }
  });

  it("retires an earlier deployment's confirmed intent after a redeploy, and never rewinds or resends it", async () => {
    const f = await fixture();
    try {
      const old = await f.prepare("c9".repeat(28), f.coins[0]!);
      await confirm(f);
      const redeployed = { ...f.context, deploymentIdentity: "dd".repeat(32) };
      f.submitted.splice(0);
      // Evidence that would rewind it, or that does not authenticate it,
      // changes nothing for the new deployment.
      for (const evidence of [
        (): DaAvailabilityOperationObservation => ({
          status: "unspent",
          currentSlot: 0,
        }),
        () =>
          included(
            "ee".repeat(32),
            DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth,
            old.intent.validUntilSlot,
          ),
        // Shallower than the confirmation depth.
        (txHash: string) =>
          included(txHash, MIN_DEPTH - 1, old.intent.validUntilSlot),
      ]) {
        f.observe(evidence);
        await expect(
          reconcileDaAvailabilityOperations(redeployed),
        ).resolves.toEqual([]);
        expect(f.journal.get(old.intent.id)?.state).toBe("confirmed");
        expect(f.journal.reservedOutRefs(f.context.actor)).toEqual(
          old.intent.spentOutRefs,
        );
      }
      expect(f.submitted).toEqual([]);
      // Authenticated inclusion past its validity frees its input.
      f.observe((txHash) =>
        included(txHash, MIN_DEPTH, old.intent.validUntilSlot),
      );
      await reconcileDaAvailabilityOperations(redeployed);
      expect(f.journal.reservedOutRefs(f.context.actor)).toEqual([]);
      expect(f.journal.get(old.intent.id)?.state).toBe("confirmed");
      // Past recovery depth it is pruned.
      f.observe((txHash) =>
        included(
          txHash,
          DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth + 1,
          old.intent.validUntilSlot,
        ),
      );
      await reconcileDaAvailabilityOperations(redeployed);
      expect(f.journal.get(old.intent.id)).toBeNull();
      expect(f.journal.finalizedAnchors(f.context.actor)).toEqual([]);
    } finally {
      f.journal.close();
    }
  });

  it("holds only the intent whose evidence does not authenticate, and the hold clears itself", async () => {
    const f = await fixture();
    try {
      const a = await f.prepare("a1".repeat(28), f.coins[0]!);
      await confirm(f);
      const b = await f.prepare("b2".repeat(28), f.coins[1]!);
      await confirm(f);
      expect(f.journal.get(a.intent.id)?.state).toBe("confirmed");
      expect(f.journal.get(b.intent.id)?.state).toBe("confirmed");
      const heldRecord = f.journal.get(a.intent.id);
      f.submitted.splice(0);
      // A's source names some other transaction; B was rolled back.
      f.observe((txHash) =>
        txHash === a.intent.txHash
          ? included("ee".repeat(32), MIN_DEPTH)
          : { status: "unspent", currentSlot: 0 },
      );
      const results = await reconcileDaAvailabilityOperations(f.context);
      expect(results).toHaveLength(2);
      expect(results).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            status: "held",
            txHash: a.intent.txHash,
            detail: expect.stringMatching(/does not authenticate/),
          }),
          expect.objectContaining({
            status: "submitted",
            txHash: b.intent.txHash,
          }),
        ]),
      );
      expect(f.submitted).toEqual([b.intent.signedCbor]);
      // The hold changed nothing about A.
      expect(f.journal.get(a.intent.id)).toEqual(heldRecord);
      expect(f.journal.reservedOutRefs(f.context.actor)).toEqual(
        [...a.intent.spentOutRefs, ...b.intent.spentOutRefs].sort(),
      );
      // Authenticated evidence clears it, and past recovery depth the record
      // and its reservations are pruned.
      f.observe((txHash) =>
        txHash === a.intent.txHash
          ? included(
              txHash,
              DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth + 1,
              a.intent.validUntilSlot,
            )
          : included(txHash, 2),
      );
      const cleared = await reconcileDaAvailabilityOperations(f.context);
      expect(cleared).toHaveLength(2);
      expect(cleared).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            status: "confirmed",
            txHash: a.intent.txHash,
          }),
          expect.objectContaining({
            status: "included",
            txHash: b.intent.txHash,
          }),
        ]),
      );
      expect(f.journal.get(a.intent.id)).toBeNull();
      expect(f.journal.reservedOutRefs(f.context.actor)).toEqual(
        b.intent.spentOutRefs,
      );
    } finally {
      f.journal.close();
    }
  });

  it("frees a confirmed intent's inputs only past its validity, and a shallow re-inclusion is only included", async () => {
    const f = await fixture();
    try {
      const a = await f.prepare("d4".repeat(28), f.coins[0]!);
      await confirm(f);
      expect(f.journal.reservedOutRefs(f.context.actor)).toEqual(
        a.intent.spentOutRefs,
      );
      // Still included past validity: the audit frees its inputs.
      f.observe((txHash) =>
        included(txHash, MIN_DEPTH, a.intent.validUntilSlot),
      );
      await expect(
        reconcileDaAvailabilityOperations(f.context),
      ).resolves.toMatchObject([{ status: "confirmed" }]);
      expect(f.journal.reservedOutRefs(f.context.actor)).toEqual([]);
      // A first confirmation already past validity frees them at once.
      f.observe((txHash) =>
        txHash === a.intent.txHash
          ? included(txHash, MIN_DEPTH, a.intent.validUntilSlot)
          : { status: "unknown", reason: "mempool" },
      );
      const b = await f.prepare("e5".repeat(28), f.coins[1]!);
      expect(f.journal.get(b.intent.id)?.state).toBe("pending");
      f.observe((txHash) =>
        txHash === b.intent.txHash
          ? included(txHash, MIN_DEPTH, b.intent.validUntilSlot)
          : included(txHash, MIN_DEPTH, a.intent.validUntilSlot),
      );
      await reconcileDaAvailabilityOperations(f.context);
      expect(f.journal.get(b.intent.id)?.state).toBe("confirmed");
      expect(f.journal.reservedOutRefs(f.context.actor)).toEqual([]);
      // Re-included shallower than the confirmation depth after a rollback:
      // it is included again, not still confirmed.
      f.observe((txHash) => included(txHash, 2));
      const shallow = await reconcileDaAvailabilityOperations(f.context);
      expect(shallow.map(({ status }) => status)).toEqual([
        "included",
        "included",
      ]);
      expect(f.journal.get(a.intent.id)?.state).toBe("included");
    } finally {
      f.journal.close();
    }
  });

  it("expires a confirmed chain rolled back past validity in one pass, child after parent", async () => {
    const f = await fixture();
    try {
      const header = "c3".repeat(28);
      const parent = await f.prepare(header, f.coins[0]!);
      await confirm(f);
      await f.lucid.config().provider!.submitTx(parent.intent.signedCbor);
      f.emulator.awaitBlock();
      const fundingInput = (await f.lucid.wallet().getUtxos()).find(
        (utxo) =>
          utxo.txHash === parent.intent.txHash && utxo.outputIndex === 0,
      )!;
      const child = await f.prepare(header, fundingInput, 25_000_000n);
      await confirm(f);
      expect(f.journal.get(child.intent.id)?.state).toBe("confirmed");
      // Rolled back past validity: the parent's inputs are back and the
      // child's input never existed. The child is visited first, before
      // anything proves its parent expired.
      const slot = child.intent.validUntilSlot + 1_000;
      f.observe((txHash) =>
        txHash === parent.intent.txHash
          ? { status: "unspent", currentSlot: slot }
          : {
              status: "inputs_missing",
              currentSlot: slot,
              missingOutRefs: child.intent.spentOutRefs,
            },
      );
      const results = await reconcileDaAvailabilityOperations(f.context);
      expect(results.map(({ status }) => status)).toEqual([
        "expired",
        "expired",
      ]);
      expect(f.submitted).not.toContain(child.intent.signedCbor);
      expect(f.journal.reservedOutRefs(f.context.actor)).toEqual([]);
      expect(f.build).toHaveBeenCalledTimes(2);
    } finally {
      f.journal.close();
    }
  });
});

describe("availability expiry history pruning in the production reconciliation path", () => {
  it("prunes an expired signed intent only beyond an authenticated recovery horizon", async () => {
    const f = await fixture();
    try {
      const record = await f.prepare("bb".repeat(28), f.coins[0]!);
      f.observe(() => ({
        status: "unspent",
        currentSlot: record.intent.validUntilSlot,
      }));
      await expect(
        reconcileDaAvailabilityOperations(f.context),
      ).resolves.toMatchObject([{ status: "expired" }]);
      expect(f.journal.get(record.intent.id)?.retentionBlockNo).toBe(100);
      f.boundary(
        100 + DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth,
      );
      await expect(
        reconcileDaAvailabilityOperations(f.context),
      ).resolves.toEqual([]);
      expect(f.journal.get(record.intent.id)?.state).toBe("expired");
      f.boundary(
        101 + DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth,
      );
      await expect(
        reconcileDaAvailabilityOperations(f.context),
      ).resolves.toEqual([]);
      expect(f.journal.get(record.intent.id)).toBeNull();
      expect(f.submitted).toEqual([]);
      expect(f.build).toHaveBeenCalledTimes(1);
    } finally {
      f.journal.close();
    }
  });
});
