/**
 * Forced-order carriage from a real ledger (N10, plan §12.3), both
 * polarities. The publication and the order are built by the SDK and
 * submitted on the Lucid emulator, and their signed bytes become the blocks
 * the follower applies, so the follower decodes a real order transaction:
 * its datum, its reference inputs as the ledger sorts them and the mint
 * redeemer whose carriage vector the SDK computed against them.
 *
 * - The order's carriage was created in an earlier block, so the order is
 *   `carriage_pending`; a content source serving the publication's signed
 *   transaction resolves it (its body hashes to the outref's id) and the
 *   node admits the order with the submitted transaction's exact bytes.
 * - The same flow over a publication whose bytes were changed after the
 *   order committed to them: the source's answer passes the hash check
 *   (it is the real, on-ledger transaction), and the field-commitment check
 *   refuses it. No row, and `/readyz` names the failure.
 *
 * The tx-order policy is the always-succeeds placeholder every emulator
 * suite in this package uses: the mint's §8.11 walk is covered in Aiken, and
 * the node must refuse what a permissive policy let through.
 */
import {
  applyChainSyncEvent,
  blake2b256,
  decodeTransaction,
  type FactStore,
  type Point,
  transportPoint,
  type TxContentSource,
} from "@al-ft/midgard-l1-follower";
import { cbor as c, SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  FORCED_ORDER_CARRIAGE_PENDING,
  FORCED_ORDER_INGESTION_FAILED,
  type ForcedOrderConfig,
  forcedOrderConfigFromContracts,
  forcedOrderProjection,
} from "../src/forced-orders/index.js";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import { nativeTransactionCbor } from "./helpers/forced-orders-chain.js";
import {
  db,
  forcedRows,
  ingestionHook,
  openNodeFollowerStore,
  UNCHANGED,
} from "./helpers/forced-orders-node-store.js";
import { resetApplicationTables } from "./utils.js";

const K = 4;

const opened: FactStore[] = [];
beforeEach(async () => {
  await db(resetApplicationTables);
});
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

type Harness = {
  lucid: LucidEvolution;
  emulator: Emulator;
  contracts: SDK.MidgardValidators;
  config: ForcedOrderConfig;
  creatorAddress: string;
  creatorKeyHash: Buffer;
};

const harness = async (): Promise<Harness> => {
  const creator = generateEmulatorAccount({ lovelace: 60_000_000_000n });
  const emulator = new Emulator([creator], {
    ...PROTOCOL_PARAMETERS_DEFAULT,
    maxCollateralInputs: 3,
  });
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(creator.seedPhrase);
  const creatorAddress = await lucid.wallet().address();
  const credential = paymentCredentialOf(creatorAddress);
  if (credential.type !== "Key")
    throw new Error("the emulator creator must hold a key credential");
  const contracts = (await Effect.runPromise(
    AlwaysSucceedsContract.pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  )) as unknown as SDK.MidgardValidators;
  const h: Harness = {
    lucid,
    emulator,
    contracts,
    config: forcedOrderConfigFromContracts(contracts),
    creatorAddress,
    creatorKeyHash: Buffer.from(credential.hash, "hex"),
  };
  // A second, small wallet UTxO for the order to reference unrelatedly.
  await submit(
    h,
    await lucid
      .newTx()
      .pay.ToAddress(creatorAddress, { lovelace: 5_000_000n })
      .complete({ localUPLCEval: true }),
  );
  return h;
};

/** Signs and submits `tx` on the emulator; its signed bytes. */
const submit = async (h: Harness, tx: TxSignBuilder): Promise<Buffer> => {
  const signed = await tx.sign.withWallet().complete();
  await signed.submit();
  h.emulator.awaitBlock(1);
  return Buffer.from(signed.toCBOR(), "hex");
};

/**
 * The follower's chain: blocks of real signed transactions under a minimal
 * header (`[[height, slot, prev hash, branch], signature]`), applied as
 * chain-sync roll-forwards.
 */
const realChain = (store: FactStore) => {
  let tip: Point = SIM_ORIGIN.point;
  let height = SIM_ORIGIN.height;
  let seq = 0n;
  const split = (tx: Buffer): { body: Buffer; witnesses: Buffer } => {
    // [body, witnesses, true, null]: a valid tx without auxiliary data.
    if (tx[0] !== 0x84 || !tx.subarray(-2).equals(Buffer.from([0xf5, 0xf6])))
      throw new Error("expected [body, witnesses, true, null]");
    const body = decodeTransaction(tx).bodyCbor;
    return { body, witnesses: tx.subarray(1 + body.length, tx.length - 2) };
  };
  return {
    forward: async (txs: readonly Buffer[]): Promise<void> => {
      const parts = txs.map(split);
      height += 1;
      const slot = tip.slot + 1;
      const header = c.array(
        c.array(c.uint(height), c.uint(slot), c.bytes(tip.hash), c.uint(0)),
        c.bytes(Buffer.alloc(8)),
      );
      const point = { slot, hash: blake2b256(header) };
      const step = await applyChainSyncEvent(store, {
        kind: "roll_forward",
        seq: (seq += 1n),
        point: transportPoint(point),
        blockNo: BigInt(height),
        blockType: 7,
        prevHash: tip.hash.toString("hex"),
        tip: { point: transportPoint(point), blockNo: BigInt(height) },
        block: c.array(
          header,
          c.array(...parts.map((p) => p.body)),
          c.array(...parts.map((p) => p.witnesses)),
          c.map(),
          c.array(),
        ),
      });
      if (step.result.kind !== "applied")
        throw new Error(`apply: ${step.result.kind}`);
      tip = point;
    },
  };
};

/** A content source serving signed transactions by their ids. */
const signedTxSource = (txs: readonly Buffer[]): TxContentSource => {
  const byId = new Map(
    txs.map((tx) => [decodeTransaction(tx).hash.toString("hex"), tx]),
  );
  return {
    name: "emulator",
    fetchTx: (txHash) =>
      Promise.resolve(byId.get(txHash.toString("hex")) ?? null),
  };
};

/**
 * Publishes the order's one tier-2 field (its bytes changed after the
 * commitment when `tamper`), then mints the order naming it. Returns the
 * signed publication and order and the submitted transaction.
 */
const publishAndOrder = async (h: Harness, tamper: boolean) => {
  const submitted = nativeTransactionCbor([0x11, 0x22]);
  const material = SDK.deriveTxOrderMaterial({
    submittedTxCbor: submitted,
    owner: h.creatorKeyHash,
  });
  const plan = SDK.planTxOrderMaterialCarriage({
    material,
    owner: h.creatorKeyHash,
    inlineReserveBytes: 0,
  });
  const [field] = plan.referenced;
  if (plan.referenced.length !== 1 || field?.plan.tier !== "RawUtxo")
    throw new Error("the fixture must publish exactly one tier-2 field");
  const [output] = SDK.fieldPreimagePublicationOutputs(field.plan);
  const bytes = Buffer.from(field.preimage);
  // Same length, last byte changed: only the field commitment can refuse it.
  if (tamper) bytes[bytes.length - 1] ^= 0xff;
  const publication = await submit(
    h,
    await Effect.runPromise(
      SDK.buildUnsignedFieldPreimagePublicationProgram(h.lucid, {
        publication: {
          ...output!,
          datumCbor: SDK.fieldPreimagePublicationDatumCbor(bytes),
        },
        publisherAddress: h.creatorAddress,
      }),
    ),
  );
  const [carriage] = await h.lucid.utxosByOutRef([
    {
      txHash: decodeTransaction(publication).hash.toString("hex"),
      outputIndex: 0,
    },
  ]);
  // The nonce is the biggest wallet UTxO, so coin selection never reaches
  // for the small unrelated one the order also references (as a hub-oracle
  // reference rides along in a real order, shifting the carriage's index).
  const wallet = [...(await h.lucid.wallet().getUtxos())]
    .filter((u) => u.datum == null)
    .sort((a, b) => Number(b.assets.lovelace - a.assets.lovelace));
  const nonce = wallet[0];
  const unrelated = wallet[wallet.length - 1];
  if (carriage === undefined || nonce === undefined || unrelated === nonce)
    throw new Error("the emulator wallet must hold the carriage and two UTxOs");
  const referenceInputs = [carriage, unrelated!];
  // The carriage's position among the ledger-sorted reference inputs. The
  // SDK computes it by finding the publication's digest, so for tampered
  // bytes the adversary writes it by hand; for honest bytes both agree.
  const sorted = [...referenceInputs].sort((a, b) =>
    a.txHash === b.txHash
      ? a.outputIndex - b.outputIndex
      : a.txHash < b.txHash
        ? -1
        : 1,
  );
  const vector: SDK.TxOrderMintRedeemer["material_carriage"] = [
    { RawUtxo: { ref_input_index: BigInt(sorted.indexOf(carriage)) } },
  ];
  if (!tamper)
    expect([
      ...SDK.txOrderMaterialCarriageVector({
        plan,
        certificatePolicyId: h.contracts.fieldPreimageCertificate.policyId,
        referenceInputs,
      }),
    ]).toEqual(vector);
  const unit = `${h.contracts.txOrder.policyId}${Buffer.from("order").toString("hex")}`;
  const datum: SDK.TxOrderDatum = {
    event: {
      id: SDK.outputReferenceFromUTxO(nonce),
      tx: {
        tx_id: material.transactionId,
        transaction_commitment: material.transactionCommitment,
        submitted_source: material.submitted_source,
      },
    },
    inclusion_time: BigInt(h.emulator.now()),
    witness: h.contracts.txOrder.policyId,
    refund_address: {
      paymentCredential: {
        PublicKeyCredential: [h.creatorKeyHash.toString("hex")],
      },
      stakeCredential: null,
    },
    refund_datum: "NoDatum",
  };
  const redeemer: SDK.TxOrderMintRedeemer = {
    event: {
      AuthenticateEvent: {
        nonce_input_index: 0n,
        event_output_index: 0n,
        hub_ref_input_index: 0n,
        witness_registration_redeemer_index: 0n,
      },
    },
    material_carriage: vector,
  };
  const order = await submit(
    h,
    await h.lucid
      .newTx()
      .collectFrom([nonce])
      .readFrom(referenceInputs)
      .mintAssets({ [unit]: 1n }, Data.to(redeemer, SDK.TxOrderMintRedeemer))
      .attach.MintingPolicy(h.contracts.txOrder.mintingScript)
      .pay.ToAddressWithData(
        h.contracts.txOrder.spendingScriptAddress,
        { kind: "inline", value: Data.to(datum, SDK.TxOrderDatum) },
        { lovelace: 3_000_000n, [unit]: 1n },
      )
      .complete({ localUPLCEval: true }),
  );
  return { submitted, publication, order };
};

describe("forced-order carriage from a real emulator chain (§12.3)", () => {
  it("admits a real forced order once a source supplies its carriage's creating transaction", async () => {
    const h = await harness();
    const f = await publishAndOrder(h, false);
    const store = await openNodeFollowerStore(
      [forcedOrderProjection(h.config)],
      K,
    );
    opened.push(store);
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    const chain = realChain(store);
    await chain.forward([f.publication]);
    await chain.forward([f.order]);
    // No source yet: pending by name, nothing written.
    const pending = ingestionHook(store, h.config);
    expect(await pending.hook(UNCHANGED)).toMatchObject({
      reason: FORCED_ORDER_CARRIAGE_PENDING,
    });
    expect(await forcedRows()).toEqual([]);
    const { hook, logs } = ingestionHook(store, h.config, {
      sources: [signedTxSource([f.publication])],
    });
    expect(await hook(UNCHANGED)).toBeUndefined();
    expect(logs.join("\n")).toMatch(/resolved \(content emulator\)/u);
    const rows = await forcedRows();
    expect(rows).toHaveLength(1);
    expect(Buffer.from(rows[0]!.native_tx_cbor)).toEqual(f.submitted);
    expect(Buffer.from(rows[0]!.tx_order_l1_tx_hash)).toEqual(
      decodeTransaction(f.order).hash,
    );
  });

  it("refuses tampered carriage bytes at the field-commitment check", async () => {
    const h = await harness();
    const f = await publishAndOrder(h, true);
    const store = await openNodeFollowerStore(
      [forcedOrderProjection(h.config)],
      K,
    );
    opened.push(store);
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    const chain = realChain(store);
    await chain.forward([f.publication]);
    await chain.forward([f.order]);
    const { hook } = ingestionHook(store, h.config, {
      sources: [signedTxSource([f.publication])],
    });
    expect(await hook(UNCHANGED)).toMatchObject({
      reason: FORCED_ORDER_INGESTION_FAILED,
      detail: expect.stringMatching(
        /field preimage does not match the committed field hash/u,
      ) as unknown,
    });
    expect(await forcedRows()).toEqual([]);
  });
});
