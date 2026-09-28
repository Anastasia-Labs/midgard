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
import { afterEach, describe, expect, it, vi } from "vitest";

import { availabilityResponderOperations } from "../src/availability/factory.js";
import { AvailabilityResponder } from "../src/availability/responder.js";
import type { CanonicalChainPoint } from "../src/l1/provider.js";

/**
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

const FINALITY = 10;
const TIP_BLOCK_NO = 1_000;
const TIP_HASH = "ab".repeat(32);
const SPEND_POINT = { slot: 500, blockHash: "cd".repeat(32) };
const DEPLOYMENT = "aa".repeat(32);
const HEADER = "bb".repeat(28);

const dirs: string[] = [];
afterEach(() =>
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true })),
);

const outRef = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex.toString()}`;

type Evidence = {
  /** The transaction Kupo names as the spender, as Ogmios serves it. */
  txHash: string;
  cbor?: string;
  blockNo: number;
};

const fixture = async (
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
    /** Tip points served by successive tip reads, before the steady tip. */
    tipOverrides: [] as (Partial<CanonicalChainPoint> | undefined)[],
    evidence: {
      txHash: rival,
      cbor: rivalCbor,
      blockNo: TIP_BLOCK_NO - FINALITY,
    } as Evidence | undefined,
  };
  const tip = (): CanonicalChainPoint => ({
    network: "Custom",
    slot: chain.slot,
    blockHash: TIP_HASH,
    providerSource: "test",
    observedAt: "test",
  });
  const observed: LucidEvolution = {
    transactionStatus: async (txHash: string) => ({
      txHash,
      status: "not_found",
    }),
    utxosByOutRef: (refs: Parameters<LucidEvolution["utxosByOutRef"]>[0]) =>
      lucid.utxosByOutRef(refs),
  } as unknown as LucidEvolution;
  const operations = availabilityResponderOperations({
    lucid: observed,
    readers: {
      currentPoint: async () => ({ ...tip(), ...chain.tipOverrides.shift() }),
      currentCursor: async () => ({
        sequence: 1,
        rollbackGeneration: 0,
        point: tip(),
      }),
      tipBlockNo: async () => TIP_BLOCK_NO,
      resolveInclusion: async () => ({}),
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
        readTransaction: async ({ point, txHash }) => ({
          txHash,
          point: { ...point, blockNo: chain.evidence!.blockNo },
          ...(chain.evidence!.cbor === undefined
            ? {}
            : { cbor: chain.evidence!.cbor }),
        }),
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

type Fixture = Awaited<ReturnType<typeof fixture>>;

const expectStillPending = async (f: Fixture) => {
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

const expectRefused = async (f: Fixture, message: string | RegExp) => {
  await expect(f.responder.tick()).rejects.toThrow(message);
  expect(f.discover).not.toHaveBeenCalled();
  expect(f.state()).toBe("pending");
  expect(f.journal.reservedOutRefs(f.ours.actor)).toEqual(
    expect.arrayContaining(f.normal.map(outRef)),
  );
};

const STEPS = [
  // A first-chunk Publish spends the tranche thread alone, so no other input
  // can stay unspent to prove it lost.
  ["a first-chunk Publish", "publish", 1],
  ["a Settle", "settle", 3],
  ["a Close", "close", 3],
] as const;

describe("committee responder after a rival spent its step's inputs", () => {
  it.each(STEPS)(
    "expires %s once the rival spend is final, and discovers again",
    async (_label, action, inputs) => {
      const f = await fixture(action, inputs);
      try {
        await expect(f.responder.tick()).resolves.toStrictEqual({
          challenges: 0,
          status: "idle",
        });
        expect(f.discover).toHaveBeenCalledTimes(1);
        expect(f.state()).toBe("expired");
        expect(f.journal.get(f.ours.id)?.detail).toBe(
          "Expired with a normal input finally spent by another transaction",
        );
        expect(f.journal.reservedOutRefs(f.ours.actor)).toEqual([]);
      } finally {
        f.journal.close();
      }
    },
  );

  it.each([
    [
      "the rival spend is one block short of finality",
      (f: Fixture) => {
        f.chain.evidence!.blockNo = TIP_BLOCK_NO - FINALITY + 1;
      },
    ],
    [
      "the intent is still inside its validity",
      (f: Fixture) => {
        f.chain.slot = f.ours.validUntilSlot - 1;
      },
    ],
    [
      "the transaction Kupo names does not list the input",
      (f: Fixture) => {
        f.chain.evidence = { ...f.split, blockNo: TIP_BLOCK_NO - FINALITY };
      },
    ],
    [
      "the rival transaction failed phase 2",
      (f: Fixture) => {
        const valid = CML.Transaction.from_cbor_hex(f.rival.cbor);
        f.chain.evidence!.cbor = CML.Transaction.new(
          valid.body(),
          valid.witness_set(),
          false,
          valid.auxiliary_data(),
        ).to_cbor_hex();
      },
    ],
    [
      "Kupo reports no spend",
      (f: Fixture) => {
        f.chain.evidence = undefined;
      },
    ],
  ])("keeps the intent pending when %s", async (_label, arrange) => {
    const f = await fixture("settle", 3);
    try {
      arrange(f);
      await expectStillPending(f);
    } finally {
      f.journal.close();
    }
  });

  it.each([
    [
      "the named spender is the committee's own transaction",
      (f: Fixture) => {
        f.chain.evidence = {
          txHash: f.ours.txHash,
          cbor: f.ours.signedCbor,
          blockNo: TIP_BLOCK_NO - FINALITY,
        };
      },
      "Invalid canonical missing-input observation",
    ],
    [
      "the spend block lies above the boundary",
      (f: Fixture) => {
        f.chain.evidence!.blockNo = TIP_BLOCK_NO + 1;
      },
      "Availability input spend lies above the canonical boundary",
    ],
    [
      "the tip moves during the height read",
      (f: Fixture) => {
        f.chain.tipOverrides = [
          undefined,
          undefined,
          { slot: f.chain.slot + 1 },
        ];
      },
      "awaits the next canonical committee node L1 scan",
    ],
    [
      "the height is read at a tip other than the cursor's point",
      (f: Fixture) => {
        f.chain.tipOverrides = [
          undefined,
          { blockHash: "ef".repeat(32) },
          { blockHash: "ef".repeat(32) },
        ];
      },
      "awaits the next canonical committee node L1 scan",
    ],
    [
      "Ogmios serves the spend without its raw transaction",
      (f: Fixture) => {
        delete f.chain.evidence!.cbor;
      },
      "Ogmios must run with --include-transaction-cbor to verify a rival spend",
    ],
  ])(
    "refuses and releases nothing when %s",
    async (_label, arrange, message) => {
      const f = await fixture("settle", 3);
      try {
        arrange(f);
        await expectRefused(f, message);
      } finally {
        f.journal.close();
      }
    },
  );
});
