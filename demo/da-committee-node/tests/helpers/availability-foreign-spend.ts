import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  paymentCredentialOf,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect, vi } from "vitest";

import { AvailabilityResponderAwaitingScanError } from "../../src/availability/awaiting-scan-error.js";
import { availabilityResponderOperations } from "../../src/availability/factory.js";
import { AvailabilityResponder } from "../../src/availability/responder.js";
import { followerBoundary } from "./follower-boundary.js";

/**
 * The committee responder over fake follower reads, for its foreign-spend
 * paths: the committee's signed step, a rival spending its normal inputs,
 * and the spend evidence the follower serves.
 *
 * A committee Publish, Settle or Close spends only protocol UTxOs; the
 * responder wallet backs collateral alone. When another party's transaction
 * (another member, a watcher, a griefer copying the mempool bytes) consumes
 * those inputs first, every normal input of the committee's signed intent is
 * gone. The intent must expire once that rival spend is verified and final,
 * so the next tick discovers live challenges again; anything short of that
 * keeps it pending, and inconsistent evidence throws.
 *
 * The wallet's coins stand in for the protocol inputs: reconcile judges an
 * intent by its signed bytes and the spends of its inputs, not by who owns
 * them.
 */

export const FINALITY = 10;
export const TIP_BLOCK_NO = 1_000;
const TIP_HASH = "ab".repeat(32);
const SPEND_POINT = { slot: 500, blockHash: "cd".repeat(32) };
const DEPLOYMENT = "aa".repeat(32);
const HEADER = "bb".repeat(28);

const dirs: string[] = [];
/** Removes every fixture's journal directory; call after each test. */
export const removeFixtureDirs = () =>
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true }));

export const outRef = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

type Evidence = {
  /** The transaction the follower stored as the spender. */
  txHash: string;
  cbor?: string;
  blockNo: number;
};

export const fixture = async (
  action: "publish" | "settle" | "close",
  normalInputs: number,
) => {
  const account = generateEmulatorAccount({ lovelace: 500_000_000n });
  const emulator = new Emulator([account]);
  emulator.awaitBlock(5);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const address = await lucid.wallet().address();
  const actor = paymentCredentialOf(address).hash;
  const splitBuilder = lucid.newTx();
  for (let index = 0; index <= normalInputs; index += 1)
    splitBuilder.pay.ToAddress(address, { lovelace: 20_000_000n });
  const split = await (await splitBuilder.complete()).sign
    .withWallet()
    .complete();
  await split.submit();
  emulator.awaitBlock();
  const coins = (await lucid.wallet().getUtxos()).filter(
    (utxo) =>
      utxo.txHash === split.toHash() && utxo.assets.lovelace === 20_000_000n,
  );
  const normal = coins.slice(0, normalInputs);
  const collateral = coins[normalInputs]!;
  const spendNormal = async (lovelace: bigint, collateralInput?: UTxO) => {
    const builder = lucid
      .newTx()
      .collectFrom(normal)
      .pay.ToAddress(address, { lovelace });
    // Only the committee's own step carries the validity window reconcile
    // judges; the rival lands as an ordinary transaction.
    if (collateralInput !== undefined)
      builder
        .validFrom(emulator.now() - 60_000)
        .validTo(emulator.now() + 60_000);
    const built = (
      await builder.complete({ coinSelection: false })
    ).toTransaction();
    const body = built.body();
    if (collateralInput !== undefined) {
      const collateralInputs = CML.TransactionInputList.new();
      collateralInputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(collateralInput.txHash),
          BigInt(collateralInput.outputIndex),
        ),
      );
      body.set_collateral_inputs(collateralInputs);
    }
    return (
      await lucid
        .fromTx(
          CML.Transaction.new(
            body,
            built.witness_set(),
            true,
            built.auxiliary_data(),
          ).to_cbor_hex(),
        )
        .sign.withWallet()
        .complete()
    ).toCBOR();
  };
  // The committee signs and persists its step, as the operation executor does
  // before its first submission.
  const ours = SDK.inspectDaAvailabilitySignedIntent({
    deploymentIdentity: DEPLOYMENT,
    actor,
    headerHash: HEADER,
    action,
    signedCbor: await spendNormal(10_000_000n, collateral),
  });
  const dir = mkdtempSync(join(tmpdir(), "committee-foreign-spend-"));
  dirs.push(dir);
  const journal = openAvailabilityOperationJournal(join(dir, "journal.sqlite"));
  const lease = journal.acquire(actor, "setup", Date.now(), 60_000);
  journal.persist(lease, ours, Date.now());
  journal.release(lease);
  // The rival wins: another transaction consumes every normal input.
  const rivalCbor = await spendNormal(7_000_000n);
  const rival = CML.hash_transaction(
    CML.Transaction.from_cbor_hex(rivalCbor).body(),
  ).to_hex();
  await lucid.wallet().submitTx(rivalCbor);
  emulator.awaitBlock();
  expect(await lucid.utxosByOutRef(normal)).toEqual([]);
  expect(await lucid.utxosByOutRef([collateral])).toHaveLength(1);

  const chain = {
    slot: ours.validUntilSlot,
    /** The follower's view generation; a rollback bumps it. */
    generation: 0,
    /** The follower's readiness reasons, which hold every boundary read. */
    held: undefined as string | undefined,
    /** Runs on every stored-transaction read, mid-pass. */
    onReadTransaction: () => {},
    evidence: {
      txHash: rival,
      cbor: rivalCbor,
      blockNo: TIP_BLOCK_NO - FINALITY,
    } as Evidence | undefined,
    /**
     * The committee's own step landed and failed instead: its normal inputs
     * stay unspent, its collateral is consumed, and the follower names the
     * failed landing as its spender.
     */
    ownFailed: false,
  };
  const observed: LucidEvolution = {
    transactionStatus: async (txHash: string) => ({
      txHash,
      status: "not_found",
    }),
    utxosByOutRef: async (
      refs: Parameters<LucidEvolution["utxosByOutRef"]>[0],
    ) =>
      chain.ownFailed
        ? normal.filter((utxo) =>
            refs.some(
              (ref) =>
                ref.txHash === utxo.txHash &&
                ref.outputIndex === utxo.outputIndex,
            ),
          )
        : lucid.utxosByOutRef(refs),
  } as unknown as LucidEvolution;
  const operations = availabilityResponderOperations({
    lucid: observed,
    reads: {
      readBoundary: async () => {
        if (chain.held !== undefined)
          throw new AvailabilityResponderAwaitingScanError(chain.held);
        return followerBoundary(
          { slot: chain.slot, blockHash: TIP_HASH, blockNo: TIP_BLOCK_NO },
          chain.generation,
        );
      },
      viewValid: async (view) => view.generation === chain.generation,
      canonicalPoint: async () => null,
      submissionPoint: async () => null,
      landingPoint: async () => null,
      failedLanding: async (txHash) =>
        chain.ownFailed && txHash === ours.txHash
          ? { transactionId: ours.txHash, point: SPEND_POINT }
          : undefined,
      intentPins: { add: async () => {}, bind: () => {} },
      foreignSpend: {
        fetchSpend: async (ref) =>
          chain.evidence !== undefined &&
          normal.some(
            (utxo) =>
              utxo.txHash === ref.txHash &&
              utxo.outputIndex === ref.outputIndex,
          )
            ? { transactionId: chain.evidence.txHash, point: SPEND_POINT }
            : undefined,
        fetchAncestor: async (slot) => ({
          slot: slot - 1,
          blockHash: "00".repeat(32),
        }),
        readTransaction: async ({ point, txHash }) => {
          chain.onReadTransaction();
          return {
            txHash,
            point: { ...point, blockNo: chain.evidence!.blockNo },
            ...(chain.evidence!.cbor === undefined
              ? {}
              : { cbor: chain.evidence!.cbor }),
          };
        },
      },
    },
    assertSourceHealthy: async () => {},
    context: {
      deploymentIdentity: DEPLOYMENT,
      actor,
      journal,
      stateQueuePolicyId: "cc".repeat(28),
      minimumConfirmationDepth: FINALITY,
      transactionLimits: {
        maxTxSize: 16_384,
        maxTxExMem: 16_500_000n,
        maxTxExSteps: 10_000_000_000n,
        coinsPerUtxoByte: 4_310n,
        feeCeilings: {},
      },
      submit: async () => {
        throw new Error("reconcile must not resubmit a spent intent");
      },
    },
  });
  const discover = vi.fn(async () => []);
  const execute = vi.fn(async () => "pending" as const);
  const responder = new AvailabilityResponder({
    deploymentFingerprint: "ff".repeat(32),
    deploymentIdentity: "ee".repeat(28),
    store: { getDaPayload: async () => undefined } as never,
    discover,
    reconcile: operations.reconcile,
    execute,
  });
  const state = () => journal.get(ours.id)?.state;
  return {
    chain,
    ours,
    normal,
    collateral,
    split: { txHash: split.toHash(), cbor: split.toCBOR() },
    rival: { txHash: rival, cbor: rivalCbor },
    journal,
    discover,
    responder,
    state,
  };
};

export type Fixture = Awaited<ReturnType<typeof fixture>>;

export const expectStillPending = async (f: Fixture) => {
  await expect(f.responder.tick()).resolves.toStrictEqual({
    challenges: 0,
    status: "pending",
  });
  expect(f.discover).not.toHaveBeenCalled();
  expect(f.state()).toBe("pending");
  expect(f.journal.reservedOutRefs(f.ours.actor)).toEqual(
    expect.arrayContaining(f.normal.map(outRef)),
  );
};

export const expectRefused = async (f: Fixture, message: string | RegExp) => {
  await expect(f.responder.tick()).rejects.toThrow(message);
  expectNothingReleased(f);
};

/** Aborted like a refusal, but reported as the wait it is, not thrown. */
export const expectAwaitingScan = async (f: Fixture, detail: string) => {
  await expect(f.responder.tick()).resolves.toStrictEqual({
    challenges: 0,
    status: "awaiting_scan",
    detail: new AvailabilityResponderAwaitingScanError(detail).message,
  });
  expectNothingReleased(f);
};

export const expectNothingReleased = (f: Fixture) => {
  expect(f.discover).not.toHaveBeenCalled();
  expect(f.state()).toBe("pending");
  expect(f.journal.reservedOutRefs(f.ours.actor)).toEqual(
    expect.arrayContaining(f.normal.map(outRef)),
  );
};
