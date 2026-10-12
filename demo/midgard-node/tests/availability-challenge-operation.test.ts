import "./helpers/follower-emulator-installed.js";

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
  CML,
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

  // Anyone may TopUp the shared DA bond pool, and every Timeout spends it, so a
  // signed intent can lose one input to a transaction not in this journal.
  it("expires an intent whose input was spent elsewhere while another stays unspent, and only past its validity", async () => {
    const f = await fixture();
    try {
      // Split the one wallet coin so the intent can spend two.
      const split = await f.lucid
        .newTx()
        .pay.ToAddress(await f.lucid.wallet().address(), {
          lovelace: 20_000_000n,
        })
        .complete();
      await (await split.sign.withWallet().complete()).submit();
      f.emulator.awaitBlock();
      const inputs = (await f.lucid.wallet().getUtxos()).slice(0, 2);
      expect(inputs).toHaveLength(2);
      await runDaAvailabilityOperation(f.context, {
        ...f.operation,
        build: async () =>
          f.lucid
            .newTx()
            .collectFrom(inputs)
            .pay.ToAddress(await f.lucid.wallet().address(), {
              lovelace: 10_000_000n,
            })
            .validFrom(f.emulator.now() - 60_000)
            .validTo(f.emulator.now() + 60_000)
            .complete({ coinSelection: false }),
      });
      const intent = f.context.journal.pending(
        f.context.deploymentIdentity,
        f.context.actor,
      )[0]!.intent;
      expect(intent.spentOutRefs).toHaveLength(2);
      const reconcileWith = async (
        missingOutRefs: readonly string[],
        slotPastExpiry: number,
      ) =>
        (
          await reconcileDaAvailabilityOperations({
            ...f.context,
            observe: async (observed) => ({
              status: "inputs_missing",
              currentSlot: observed.validUntilSlot + slotPastExpiry,
              missingOutRefs,
            }),
          })
        )[0]?.status;

      // Every normal input gone may be this very transaction, not yet indexed.
      expect(await reconcileWith(intent.spentOutRefs, 1_000)).toBe("waiting");
      // Still inside its validity it could yet land once the input returns.
      expect(await reconcileWith([intent.spentOutRefs[0]!], -1)).toBe(
        "waiting",
      );
      expect(await reconcileWith([intent.spentOutRefs[0]!], 0)).toBe("expired");
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

  // Another watcher's Timeout on the same header spends every normal input of
  // ours, so nothing stays unspent to prove this intent is out of the chain.
  it("expires an intent whose every normal input was finally spent by another transaction, and only then", async () => {
    const f = await fixture();
    try {
      // Split the one wallet coin so one can serve as collateral.
      const split = await f.lucid
        .newTx()
        .pay.ToAddress(await f.lucid.wallet().address(), {
          lovelace: 20_000_000n,
        })
        .complete();
      await (await split.sign.withWallet().complete()).submit();
      f.emulator.awaitBlock();
      const [spent, collateral] = await f.lucid.wallet().getUtxos();
      await runDaAvailabilityOperation(f.context, {
        ...f.operation,
        build: async () => {
          const built = (
            await f.lucid
              .newTx()
              .collectFrom([spent!])
              .pay.ToAddress(await f.lucid.wallet().address(), {
                lovelace: 10_000_000n,
              })
              .validFrom(f.emulator.now() - 60_000)
              .validTo(f.emulator.now() + 60_000)
              .complete({ coinSelection: false })
          ).toTransaction();
          const body = built.body();
          const collateralInputs = CML.TransactionInputList.new();
          collateralInputs.add(
            CML.TransactionInput.new(
              CML.TransactionHash.from_hex(collateral!.txHash),
              BigInt(collateral!.outputIndex),
            ),
          );
          body.set_collateral_inputs(collateralInputs);
          return f.lucid.fromTx(
            CML.Transaction.new(
              body,
              built.witness_set(),
              true,
              built.auxiliary_data(),
            ).to_cbor_hex(),
          );
        },
      });
      const intent = f.context.journal.pending(
        f.context.deploymentIdentity,
        f.context.actor,
      )[0]!.intent;
      const [normal] = intent.spentOutRefs;
      expect(intent.spentOutRefs).toHaveLength(1);
      expect(intent.collateralOutRefs).toHaveLength(1);
      const missingOutRefs = [
        ...intent.spentOutRefs,
        ...intent.collateralOutRefs,
      ];
      const foreign = (outRef: string, confirmationDepth = 30) => ({
        outRef,
        spendingTxHash: "ee".repeat(32),
        spendPoint: "900:" + "ff".repeat(32),
        confirmationDepth,
      });
      const reconcileWith = async (
        foreignSpends: unknown,
        slotPastExpiry = 0,
        missing: readonly string[] = missingOutRefs,
      ) =>
        (
          await reconcileDaAvailabilityOperations({
            ...f.context,
            observe: async (observed) =>
              ({
                status: "inputs_missing",
                currentSlot: observed.validUntilSlot + slotPastExpiry,
                missingOutRefs: missing,
                ...(foreignSpends === undefined ? {} : { foreignSpends }),
              }) as never,
          })
        )[0]?.status;

      // Absence alone is never proof.
      expect(await reconcileWith(undefined)).toBe("waiting");
      expect(await reconcileWith([])).toBe("waiting");
      // Inside its validity the spend could still roll back and ours land.
      expect(await reconcileWith([foreign(normal!)], -1)).toBe("waiting");
      // Short of finality the spend itself may roll back.
      expect(await reconcileWith([foreign(normal!, 29)])).toBe("waiting");
      // Collateral is consumed only by a failing script, never by our spend.
      expect(await reconcileWith([foreign(intent.collateralOutRefs[0]!)])).toBe(
        "waiting",
      );
      // Our own transaction spending the input is inclusion, not a foreign spend.
      await expect(
        reconcileWith([{ ...foreign(normal!), spendingTxHash: intent.txHash }]),
      ).rejects.toThrow("Invalid canonical missing-input observation");
      // Evidence must be about a ref the observation reports missing.
      await expect(
        reconcileWith([foreign(normal!)], 0, intent.collateralOutRefs),
      ).rejects.toThrow("Invalid canonical missing-input observation");
      await expect(
        reconcileWith([{ ...foreign(normal!), confirmationDepth: -1 }]),
      ).rejects.toThrow("Invalid canonical missing-input observation");
      await expect(
        reconcileWith([{ ...foreign(normal!), spendingTxHash: "ee" }]),
      ).rejects.toThrow("Invalid canonical missing-input observation");
      expect(f.context.journal.reservedOutRefs(f.context.actor)).toEqual(
        expect.arrayContaining(missingOutRefs),
      );

      expect(await reconcileWith([foreign(normal!)])).toBe("expired");
      expect(f.context.journal.get(intent.id)?.detail).toBe(
        "Expired with a normal input finally spent by another transaction",
      );
      expect(f.context.journal.reservedOutRefs(f.context.actor)).toEqual([]);
      // The wallet is free again: the next header's work builds.
      const next = vi.fn(async () =>
        buildDaAvailabilityFundingPreparationTx(f.lucid, {
          fundingInput: spent!,
          outputLovelace: 5_000_000n,
          feeLovelace: 1_000_000n,
          validFrom: BigInt(f.emulator.now() - 60_000),
          validTo: BigInt(f.emulator.now() + 60_000),
        }),
      );
      await runDaAvailabilityOperation(f.context, {
        ...f.operation,
        headerHash: "dd".repeat(28),
        build: next,
      });
      expect(next).toHaveBeenCalledTimes(1);
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

  it("recovers ancestors from canonical children and rebroadcasts the same bytes if a finalized frontier rolls back", async () => {
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
          .finalizedAnchors(f.context.actor)
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
      // Every input observed unspent: both left the chain, so both bytes land
      // again, child first; a restart reconciles them and signs nothing.
      const submit = vi.fn(f.context.submit);
      const chain = [child, parent].map(
        ({ txHash }) => f.context.journal.findTransaction(txHash)!.intent,
      );
      await expect(
        reconcileDaAvailabilityOperations({ ...f.context, submit }),
      ).resolves.toMatchObject(chain.map(({ txHash }) => ({ txHash })));
      expect(submit.mock.calls.flat()).toEqual(chain.map((i) => i.signedCbor));
      const restarted = openAvailabilityOperationJournal(f.path);
      const rerun = { ...f.context, journal: restarted };
      await expect(
        runDaAvailabilityOperation(rerun, f.operation),
      ).resolves.toMatchObject({ status: "submitted" });
      restarted.close();
      expect(f.build).toHaveBeenCalledTimes(1);
    } finally {
      f.context.journal.close();
    }
  });
});
