import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import {
  buildDaAvailabilityFundingPreparationTx,
  type DaAvailabilityOperationContext,
  inspectDaAvailabilitySignedIntent,
  reconcileDaAvailabilityOperations,
  runDaAvailabilityOperation,
} from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
} from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

const dirs: string[] = [];
afterEach(() =>
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true })),
);
const fixture = async () => {
  const account = generateEmulatorAccount({ lovelace: 100_000_000n });
  const emulator = new Emulator([account]);
  emulator.awaitBlock(5);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const dir = mkdtempSync(join(tmpdir(), "availability-operation-"));
  dirs.push(dir);
  const path = join(dir, "journal.sqlite");
  const journal = openAvailabilityOperationJournal(path);
  const build = vi.fn(async () =>
    buildDaAvailabilityFundingPreparationTx(lucid, {
      fundingInput: (await lucid.wallet().getUtxos())[0]!,
      outputLovelace: 50_000_000n,
      feeLovelace: 1_000_000n,
      validFrom: BigInt(emulator.now() - 60_000),
      validTo: BigInt(emulator.now() + 60_000),
    }),
  );
  const context: DaAvailabilityOperationContext = {
    deploymentIdentity: "aa".repeat(32),
    actor: paymentCredentialOf(account.address).hash,
    journal,
    stateQueuePolicyId: "cc".repeat(28),
    minimumConfirmationDepth: 30,
    transactionLimits: {
      maxTxSize: 16384,
      maxTxExMem: 16500000n,
      maxTxExSteps: 10000000000n,
      coinsPerUtxoByte: 4310n,
      feeCeilings: { prepare: 1000000n },
    },
    assertActuationCurrent: () => {},
    observe: async () => ({ status: "unspent", currentSlot: 0 }),
    submit: async (bytes) =>
      inspectDaAvailabilitySignedIntent({
        deploymentIdentity: "aa".repeat(32),
        actor: paymentCredentialOf(account.address).hash,
        headerHash: "bb".repeat(28),
        action: "prepare",
        signedCbor: bytes,
      }).txHash,
  };
  return {
    context,
    path,
    build,
    lucid,
    emulator,
    operation: { action: "prepare", headerHash: "bb".repeat(28), build },
  };
};

describe("availability operation signed recovery", () => {
  it("recovers an ambiguous submission from disk without building or signing replacement bytes", async () => {
    const f = await fixture();
    const broadcasts: string[] = [];
    await expect(
      runDaAvailabilityOperation(
        {
          ...f.context,
          submit: async (bytes) => {
            broadcasts.push(bytes);
            throw new Error("connection lost after broadcast");
          },
        },
        f.operation,
      ),
    ).rejects.toThrow(/connection lost/);
    f.context.journal.close();
    const restarted = openAvailabilityOperationJournal(f.path);
    try {
      const recovered = await runDaAvailabilityOperation(
        {
          ...f.context,
          journal: restarted,
          submit: async (bytes) => {
            broadcasts.push(bytes);
            return f.context.submit(bytes);
          },
        },
        f.operation,
      );
      expect(recovered.status).toBe("submitted");
      expect(broadcasts[0]).toBe(broadcasts[1]);
      expect(f.build).toHaveBeenCalledTimes(1);
      const [confirmed] = await reconcileDaAvailabilityOperations({
        ...f.context,
        journal: restarted,
        observe: async (intent) => ({
          status: "included",
          txHash: intent.txHash,
          inclusionPoint: "block",
          confirmationDepth: 30,
        }),
      });
      expect(confirmed?.status).toBe("confirmed");
      expect(
        restarted.pending(f.context.deploymentIdentity, f.context.actor),
      ).toHaveLength(0);
    } finally {
      restarted.close();
    }
  });

  it("progresses canonical inclusion before finality and returns to pending on rollback uncertainty", async () => {
    const f = await fixture();
    try {
      await runDaAvailabilityOperation(f.context, f.operation);
      const included = await reconcileDaAvailabilityOperations({
        ...f.context,
        observe: async (intent) => ({
          status: "included",
          txHash: intent.txHash,
          inclusionPoint: "block",
          confirmationDepth: 0,
        }),
      });
      expect(included[0]?.status).toBe("included");
      expect(
        f.context.journal.pending(
          f.context.deploymentIdentity,
          f.context.actor,
        ),
      ).toHaveLength(0);
      expect(
        f.context.journal.unfinalized(
          f.context.deploymentIdentity,
          f.context.actor,
        ),
      ).toHaveLength(1);
      const uncertain = {
        ...f.context,
        observe: async () => ({
          status: "unknown" as const,
          reason: "rollback recovery",
        }),
      };
      await reconcileDaAvailabilityOperations(uncertain);
      expect(
        (await runDaAvailabilityOperation(uncertain, f.operation)).status,
      ).toBe("waiting");
      expect(f.build).toHaveBeenCalledTimes(1);
    } finally {
      f.context.journal.close();
    }
  });

  it("releases an expired intent only with positive unspent evidence and blocks stale-generation signing", async () => {
    const f = await fixture();
    try {
      let checks = 0;
      await expect(
        runDaAvailabilityOperation(
          {
            ...f.context,
            assertActuationCurrent: () => {
              if (++checks > 1) throw new Error("rollback generation changed");
            },
          },
          f.operation,
        ),
      ).rejects.toThrow(/rollback generation/);
      expect(
        f.context.journal.pending(
          f.context.deploymentIdentity,
          f.context.actor,
        ),
      ).toHaveLength(0);
      await runDaAvailabilityOperation(f.context, f.operation);
      expect(
        (
          await reconcileDaAvailabilityOperations({
            ...f.context,
            observe: async () => ({
              status: "unknown",
              reason: "no index evidence",
            }),
          })
        )[0]?.status,
      ).toBe("waiting");
      expect(
        (
          await reconcileDaAvailabilityOperations({
            ...f.context,
            observe: async (intent) => ({
              status: "unspent",
              currentSlot: intent.validUntilSlot,
            }),
          })
        )[0]?.status,
      ).toBe("expired");
      expect(
        f.context.journal.pending(
          f.context.deploymentIdentity,
          f.context.actor,
        ),
      ).toHaveLength(0);
    } finally {
      f.context.journal.close();
    }
  });

  it("expires a rolled-back child after its parent is proven expired, in dependency order", async () => {
    const f = await fixture();
    try {
      const parent = await runDaAvailabilityOperation(f.context, f.operation);
      const parentIntent = f.context.journal.pending(
        f.context.deploymentIdentity,
        f.context.actor,
      )[0]!.intent;
      await f.lucid.config().provider!.submitTx(parentIntent.signedCbor);
      f.emulator.awaitBlock();
      await reconcileDaAvailabilityOperations({
        ...f.context,
        observe: async (intent) => ({
          status: "included",
          txHash: intent.txHash,
          inclusionPoint: "parent-block",
          confirmationDepth: 0,
        }),
      });
      const fundingInput = (await f.lucid.wallet().getUtxos()).find(
        (utxo) => utxo.txHash === parent.txHash && utxo.outputIndex === 0,
      )!;
      expect(fundingInput.assets.lovelace).toBe(50_000_000n);
      await runDaAvailabilityOperation(f.context, {
        ...f.operation,
        build: async () =>
          buildDaAvailabilityFundingPreparationTx(f.lucid, {
            fundingInput,
            outputLovelace: 25_000_000n,
            feeLovelace: 1_000_000n,
            validFrom: BigInt(f.emulator.now() - 60_000),
            validTo: BigInt(f.emulator.now() + 60_000),
          }),
      });
      const recovered = await reconcileDaAvailabilityOperations({
        ...f.context,
        observe: async (intent) =>
          intent.txHash === parent.txHash
            ? { status: "unspent", currentSlot: intent.validUntilSlot + 1_000 }
            : {
                status: "inputs_missing",
                currentSlot: intent.validUntilSlot + 1_000,
                missingOutRefs: intent.spentOutRefs,
              },
      });
      expect(recovered.map(({ status }) => status)).toEqual([
        "expired",
        "expired",
      ]);
      expect(
        f.context.journal.pending(
          f.context.deploymentIdentity,
          f.context.actor,
        ),
      ).toHaveLength(0);
      expect(
        f.context.journal.unfinalized(
          f.context.deploymentIdentity,
          f.context.actor,
        ),
      ).toHaveLength(0);
    } finally {
      f.context.journal.close();
    }
  });

  it("refuses signed envelope overflow before persistence and rejects a foreign wallet witness", async () => {
    const f = await fixture();
    try {
      await expect(
        runDaAvailabilityOperation(
          {
            ...f.context,
            transactionLimits: {
              ...f.context.transactionLimits,
              maxTxSize: 100,
            },
          },
          f.operation,
        ),
      ).rejects.toThrow(/size or execution reserve/);
      expect(
        f.context.journal.pending(
          f.context.deploymentIdentity,
          f.context.actor,
        ),
      ).toHaveLength(0);
      const signed = await (await f.build()).sign.withWallet().complete();
      expect(() =>
        inspectDaAvailabilitySignedIntent({
          deploymentIdentity: f.context.deploymentIdentity,
          actor: "ff".repeat(28),
          headerHash: f.operation.headerHash,
          action: "prepare",
          signedCbor: signed.toCBOR(),
        }),
      ).toThrow(/reserved actor/);
    } finally {
      f.context.journal.close();
    }
  });

  it("recovers ancestors from canonical children and durably halts if a finalized frontier rolls back", async () => {
    const f = await fixture();
    try {
      const parent = await runDaAvailabilityOperation(f.context, f.operation);
      const parentRecord = f.context.journal.findTransaction(parent.txHash)!;
      await f.lucid.config().provider!.submitTx(parentRecord.intent.signedCbor);
      f.emulator.awaitBlock();
      await reconcileDaAvailabilityOperations({
        ...f.context,
        observe: async (intent) => ({
          status: "included",
          txHash: intent.txHash,
          inclusionPoint: "parent-block",
          confirmationDepth: 0,
        }),
      });
      const fundingInput = (await f.lucid.wallet().getUtxos()).find(
        (utxo) => utxo.txHash === parent.txHash && utxo.outputIndex === 0,
      )!;
      const child = await runDaAvailabilityOperation(f.context, {
        ...f.operation,
        build: async () =>
          buildDaAvailabilityFundingPreparationTx(f.lucid, {
            fundingInput,
            outputLovelace: 25_000_000n,
            feeLovelace: 1_000_000n,
            validFrom: BigInt(f.emulator.now() - 60_000),
            validTo: BigInt(f.emulator.now() + 60_000),
          }),
      });
      await reconcileDaAvailabilityOperations({
        ...f.context,
        observe: async () => ({ status: "unknown", reason: "source recovery" }),
      });
      expect(f.context.journal.findTransaction(parent.txHash)?.state).toBe(
        "pending",
      );
      let depth = 0;
      const observe = vi.fn(
        async (intent: Parameters<typeof f.context.observe>[0]) => {
          expect(intent.txHash).toBe(child.txHash);
          return {
            status: "included" as const,
            txHash: intent.txHash,
            inclusionPoint: "child-block",
            confirmationDepth: depth,
          };
        },
      );
      await reconcileDaAvailabilityOperations({ ...f.context, observe });
      expect(observe).toHaveBeenCalledTimes(1);
      expect(f.context.journal.findTransaction(parent.txHash)?.state).toBe(
        "included",
      );
      depth = 30;
      await reconcileDaAvailabilityOperations({ ...f.context, observe });
      expect(observe).toHaveBeenCalledTimes(2);
      expect(f.context.journal.findTransaction(parent.txHash)?.state).toBe(
        "confirmed",
      );
      expect(
        f.context.journal
          .finalizedAnchors(f.context.deploymentIdentity, f.context.actor)
          .map((record) => record.intent.txHash),
      ).toEqual([child.txHash]);
      await reconcileDaAvailabilityOperations({ ...f.context, observe });
      expect(observe).toHaveBeenCalledTimes(3);
      expect(
        (
          await runDaAvailabilityOperation(
            {
              ...f.context,
              observe: async () => ({
                status: "unknown",
                reason: "source unavailable",
              }),
            },
            f.operation,
          )
        ).status,
      ).toBe("waiting");
      expect(f.build).toHaveBeenCalledTimes(1);
      await expect(
        reconcileDaAvailabilityOperations({
          ...f.context,
          observe: async () => ({ status: "unspent", currentSlot: 0 }),
        }),
      ).rejects.toThrow(/Finalized availability transaction rolled back/);
      const restarted = openAvailabilityOperationJournal(f.path);
      try {
        await expect(
          runDaAvailabilityOperation(
            { ...f.context, journal: restarted },
            f.operation,
          ),
        ).rejects.toThrow(/halted/);
      } finally {
        restarted.close();
      }
    } finally {
      f.context.journal.close();
    }
  });
});
