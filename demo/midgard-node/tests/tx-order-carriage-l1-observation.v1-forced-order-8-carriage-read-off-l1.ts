import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  observeVisibleTxOrderCarriage,
  reconstructTxOrderMaterial,
} from "../src/fibers/fetch-and-insert-tx-order-utxos.js";
import {
  fetchKupoAncestorPoint,
  fetchKupoCreationPoint,
  readOgmiosBlockTransaction,
  resolveCarriageReferenceInputs,
  txOrderMintRedeemer,
} from "../src/l1-tx-order-carriage.js";
import { NodeConfig } from "../src/services/index.js";
import { nativeTransactionCbor } from "./tx-order-carriage-l1-observation.native-transaction-cbor.js";
import {
  kupoMatchesOf,
  readCarriage,
  readCarriageFailure,
  submitForcedOrder,
  withHarness,
} from "./tx-order-carriage-l1-observation.submit-forced-order.js";

describe("V1 forced-order §8 carriage, read off L1", () => {
  it("ingests a tier-1 order whose preimage rides the mint redeemer", async () => {
    const harness = await withHarness();
    const submittedTxCbor = nativeTransactionCbor([0x11, 0x22]);
    const { plan, orderUtxo } = await submitForcedOrder({
      harness,
      submittedTxCbor,
    });
    expect(plan.carriage.map((field) => field.plan.tier)).toEqual(["Inline"]);

    // The observed transaction really is the one the order came out of, and the
    // redeemer the read takes is the one the tx-order policy ran — reached through
    // the mint's policy list rather than by grabbing a redeemer. This order mints
    // one policy, so the pointer here is 0; the multi-policy case, where that
    // distinction has teeth, is pinned in the selection suite at the foot of this
    // file (the Lucid builder cannot put a second policy in this transaction).
    const createdAt = await fetchKupoCreationPoint({
      kupoUrl: harness.l1.kupoUrl,
      outRef: {
        txHash: orderUtxo.utxo.txHash,
        outputIndex: orderUtxo.utxo.outputIndex,
      },
    });
    const observed = await readOgmiosBlockTransaction({
      ogmiosUrl: harness.l1.ogmiosUrl,
      intersection: await fetchKupoAncestorPoint({
        kupoUrl: harness.l1.kupoUrl,
        slot: createdAt.slot,
      }),
      blockPoint: createdAt,
      txHash: orderUtxo.utxo.txHash,
    });
    expect(observed.mintPolicyIds).toEqual([
      harness.contracts.txOrder.policyId,
    ]);
    expect(
      txOrderMintRedeemer(observed, harness.contracts.txOrder.policyId),
    ).toBe(
      observed.redeemers.find((redeemer) => redeemer.purpose === "mint")
        ?.redeemer,
    );

    const material = await readCarriage(harness, orderUtxo);
    expect(material?.carriage.map((entry) => entry.carriage)).toEqual([
      "Inline",
    ]);
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: orderUtxo.datum.event.tx,
          material,
        }),
      ),
    ).resolves.toEqual(submittedTxCbor);
  }, 120_000);

  it("ingests a tier-2 order whose preimage is a published raw UTxO", async () => {
    const harness = await withHarness();
    const submittedTxCbor = nativeTransactionCbor([0x11, 0x22]);
    // The same preimage as tier 1. §8.4's partition puts it inside `K`, so which
    // of tiers 1–2 carries it is the creator's budget decision: an order with no
    // inline reserve publishes it instead, which is the demotion
    // `planTxOrderMaterialCarriage` exists for.
    const { plan, orderUtxo } = await submitForcedOrder({
      harness,
      submittedTxCbor,
      inlineReserveBytes: 0,
    });
    expect(plan.carriage.map((field) => field.plan.tier)).toEqual(["RawUtxo"]);

    const material = await readCarriage(harness, orderUtxo);
    expect(material?.carriage.map((entry) => entry.carriage)).toEqual([
      "RawUtxo",
    ]);
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: orderUtxo.datum.event.tx,
          material,
        }),
      ),
    ).resolves.toEqual(submittedTxCbor);
  }, 120_000);

  it("ingests a tier-3 order whose preimage is chunked under a certificate", async () => {
    const harness = await withHarness();
    // Four 5 kB-datum outputs put field 2 above §8.3's `K`, where §8.4's
    // partition makes tier 3 the only admissible carriage — the tier whose bytes
    // arrive split, whose length claim comes from a separate datum, and whose
    // reference inputs the reader has to resolve in two different shapes.
    const submittedTxCbor = nativeTransactionCbor([0x11, 0x22, 0x33, 0x44]);
    const { plan, orderUtxo } = await submitForcedOrder({
      harness,
      submittedTxCbor,
    });
    expect(plan.carriage.map((field) => field.plan.tier)).toEqual([
      "Certified",
    ]);

    const material = await readCarriage(harness, orderUtxo);
    const [entry] = material?.carriage ?? [];
    expect(entry?.carriage).toBe("Certified");
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: orderUtxo.datum.event.tx,
          material,
        }),
      ),
    ).resolves.toEqual(submittedTxCbor);

    // The whole reference-input set is resolved positionally, not just the
    // carriage: the manifest and its chunks land where the redeemer says, and the
    // unrelated input that rides along resolves to nothing openable while still
    // occupying its position.
    const resolved = material?.referenceInputs ?? [];
    expect(resolved.length).toBe(
      1 + plan.carriage[0]!.plan.publications.length + 1,
    );
    const certified = entry as {
      certRefInputIndex: number;
      chunkRefInputIndices: readonly number[];
    };
    expect(resolved[certified.certRefInputIndex]?.certificate).toBeDefined();
    for (const index of certified.chunkRefInputIndices) {
      expect(resolved[index]?.inlineDatumBytes).toBeDefined();
    }
    expect(
      resolved.filter(
        (input) =>
          input.certificate === undefined &&
          input.inlineDatumBytes === undefined,
      ).length,
    ).toBe(1);

    // And the reader's canonical ordering is load-bearing rather than incidental.
    // A validator is handed `reference_inputs` as a **set**, so the list its
    // positional indices address is `(txHash, outputIndex)` ascending — which is
    // what the SDK sorts into when it emits those indices, and what the reader
    // sorts into when it reads them back. The observation here arrives in the
    // *transaction's own CBOR order*, which this builder writes in insertion
    // order, so the two already differ above; reversing it changes nothing, which
    // is what "the wire order is not trusted" means operationally.
    const createdAt = await fetchKupoCreationPoint({
      kupoUrl: harness.l1.kupoUrl,
      outRef: {
        txHash: orderUtxo.utxo.txHash,
        outputIndex: orderUtxo.utxo.outputIndex,
      },
    });
    const observed = await readOgmiosBlockTransaction({
      ogmiosUrl: harness.l1.ogmiosUrl,
      intersection: await fetchKupoAncestorPoint({
        kupoUrl: harness.l1.kupoUrl,
        slot: createdAt.slot,
      }),
      blockPoint: createdAt,
      txHash: orderUtxo.utxo.txHash,
    });
    expect(
      await resolveCarriageReferenceInputs({
        kupoUrl: harness.l1.kupoUrl,
        referenceInputs: [...observed.referenceInputs].reverse(),
      }),
    ).toEqual(resolved);
  }, 180_000);

  it("refuses an order whose creating transaction mints no tx-order NFT", async () => {
    const harness = await withHarness();
    const { orderUtxo } = await submitForcedOrder({
      harness,
      submittedTxCbor: nativeTransactionCbor([0x11, 0x22]),
    });

    // The mint redeemer is selected by *policy*, through the mint's ascending
    // policy list, and never as "the transaction's first redeemer". Asking under a
    // policy the transaction does not mint therefore finds nothing rather than
    // finding something else's redeemer.
    await expect(
      Effect.runPromise(
        observeVisibleTxOrderCarriage(orderUtxo, "ab".repeat(28), {
          timeoutMs: 10_000,
        }).pipe(
          Effect.provideService(NodeConfig, {
            L1_OGMIOS_KEY: harness.l1.ogmiosUrl,
            L1_KUPO_KEY: harness.l1.kupoUrl,
          } as never),
        ),
      ),
    ).rejects.toMatchObject({
      message:
        "Failed to read a forced order's §8 carriage from L1 (Ogmios chain-sync + Kupo)",
    });
  }, 120_000);

  it("refuses an order the index has never seen", async () => {
    const harness = await withHarness();
    const { orderUtxo } = await submitForcedOrder({
      harness,
      submittedTxCbor: nativeTransactionCbor([0x11, 0x22]),
    });

    await expect(
      Effect.runPromise(
        observeVisibleTxOrderCarriage(
          {
            ...orderUtxo,
            utxo: { ...orderUtxo.utxo, txHash: "cd".repeat(32) },
          },
          harness.contracts.txOrder.policyId,
          { timeoutMs: 10_000 },
        ).pipe(
          Effect.provideService(NodeConfig, {
            L1_OGMIOS_KEY: harness.l1.ogmiosUrl,
            L1_KUPO_KEY: harness.l1.kupoUrl,
          } as never),
        ),
      ),
    ).rejects.toMatchObject({
      message:
        "Failed to read a forced order's §8 carriage from L1 (Ogmios chain-sync + Kupo)",
    });
  }, 120_000);

  it("refuses a tier-2 order whose carriage datum Kupo does not resolve", async () => {
    const harness = await withHarness();
    const submittedTxCbor = nativeTransactionCbor([0x11, 0x22]);
    const { orderUtxo } = await submitForcedOrder({
      harness,
      submittedTxCbor,
      inlineReserveBytes: 0,
    });
    // The order reads correctly first, so what the negatives below change is only
    // the datum leg and not the order.
    await expect(readCarriage(harness, orderUtxo)).resolves.toBeDefined();

    // What the wire actually holds, before anything is sabotaged: the datum is on
    // the match **because the flag asked for it**, and the same match without
    // `?resolve_hashes` carries no `datum` key at all. That asymmetry is the
    // v2.10.0 contract, and pinning it here is what stops a later "simplification"
    // of the harness from inlining the bytes unconditionally — which would green a
    // reader that had quietly stopped asking the deployment for them.
    const [flagless] = await kupoMatchesOf(harness, orderUtxo.utxo);
    const [resolved] = await kupoMatchesOf(
      harness,
      orderUtxo.utxo,
      "?resolve_hashes",
    );
    expect(flagless).toBeDefined();
    expect(Object.hasOwn(flagless ?? {}, "datum")).toBe(false);
    expect(typeof resolved?.datum).toBe("string");

    // A match carries its datum inline, because the read asks for it with
    // `?resolve_hashes`; a datum Kupo does not hold comes back as that field
    // present and `null`. The one thing that must never do is resolve the
    // carriage index to *nothing* — an emptied index reads as "this input carries
    // no carriage", which is a silent downgrade of a readable order to an
    // unreadable one. It is a read failure, and the next reconciliation tick
    // tries again.
    harness.l1.rewriteDatums(() => null);
    const unresolved = await readCarriageFailure(harness, orderUtxo);
    expect(unresolved.message).toBe(
      "Failed to read a forced order's §8 carriage from L1 (Ogmios chain-sync + Kupo)",
    );
    expect(String(unresolved.cause)).toContain(
      "Kupo resolved no datum for hash",
    );

    // The deployment-mismatch shape, which is what the ≥2.10.0 floor buys and the
    // reason the floor is checked on the wire. A Kupo older than v2.10.0 does not
    // reject `?resolve_hashes` — it ignores it — so its match comes back with no
    // `datum` field at all. A missing field is not an output without a datum, and
    // reading it as one would empty every tier-2/3 carriage index in silence. It
    // refuses instead, naming the version rather than the order.
    harness.l1.rewriteDatums(null);
    harness.l1.ignoreResolveHashes(true);
    const misdeployed = await readCarriageFailure(harness, orderUtxo);
    expect(misdeployed.message).toBe(
      "Failed to read a forced order's §8 carriage from L1 (Ogmios chain-sync + Kupo)",
    );
    expect(String(misdeployed.cause)).toContain(
      "the match carries no `datum` field",
    );
    expect(String(misdeployed.cause)).toContain("Run Kupo v2.10.0 or newer");
    harness.l1.ignoreResolveHashes(false);

    // And bytes that arrive but are not the committed ones get no further: they
    // are still structurally raw carriage, so the read itself succeeds, and it is
    // the §4 hash door in `reconstructTxOrderMaterial` that refuses them. That
    // is the invariant this whole module rests on — the source is never trusted.
    harness.l1.rewriteDatums(
      (datum) => `${datum.slice(0, -2)}${datum.endsWith("ff") ? "ee" : "ff"}`,
    );
    const corrupted = await readCarriage(harness, orderUtxo);
    expect(corrupted?.carriage.map((entry) => entry.carriage)).toEqual([
      "RawUtxo",
    ]);
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: orderUtxo.datum.event.tx,
          material: corrupted,
        }),
      ),
    ).rejects.toThrow();

    // Restored, the same order reads and reconstructs — so the refusals above
    // were the corruption and not the harness.
    harness.l1.rewriteDatums(null);
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: orderUtxo.datum.event.tx,
          material: await readCarriage(harness, orderUtxo),
        }),
      ),
    ).resolves.toEqual(submittedTxCbor);
  }, 120_000);

  it("reads nothing for a canonically-empty order, which needs no carriage", async () => {
    const harness = await withHarness();
    const submittedTxCbor = nativeTransactionCbor([]);
    const { plan, orderUtxo } = await submitForcedOrder({
      harness,
      submittedTxCbor,
    });
    expect(plan.carriage).toEqual([]);

    // No L1 read at all: nine empty-field commitments consume no carriage entry,
    // so the walk reconstructs the order from its own payload and an Ogmios
    // outage cannot stop it.
    const material = await readCarriage(harness, orderUtxo);
    expect(material).toBeUndefined();
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: orderUtxo.datum.event.tx,
          material,
        }),
      ),
    ).resolves.toEqual(submittedTxCbor);
  }, 120_000);
});
