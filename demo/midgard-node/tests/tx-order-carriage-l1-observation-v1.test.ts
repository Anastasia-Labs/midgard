import "./utils.js";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid as makeLucid,
  type LucidEvolution,
  paymentCredentialOf,
  PROTOCOL_PARAMETERS_DEFAULT,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  ForcedTransactionsDB,
  MigrationRunner,
} from "../src/database/index.js";
import {
  observeVisibleTxOrderCarriageV1,
  reconcileVisibleTxOrderUTxOs,
  reconstructTxOrderMaterialV1,
} from "../src/fibers/fetch-and-insert-tx-order-utxos.js";
import {
  fetchKupoAncestorPointV1,
  fetchKupoCreationPointV1,
  readOgmiosBlockTransactionV1,
  resolveCarriageReferenceInputsV1,
  txOrderMintRedeemerV1,
} from "../src/l1-tx-order-carriage-v1.js";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {
  ContractDeploymentIdentity,
  Database,
  Lucid as LucidService,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import {
  type LocalL1V1,
  startLocalL1ObservationV1,
} from "./helpers/local-l1-observation.js";

/**
 * #599: the node ingests a **material-bearing** forced order, at all three §8
 * carriage tiers, with the carriage read off L1 rather than handed to the
 * authenticator by a test.
 *
 * `tests/tx-order-material-chain-v1.test.ts` covers the authenticator by feeding
 * it carriage directly, which is the right shape for testing a door. This file
 * covers the thing that file cannot: that a *source* exists, that it is the
 * node's own (Ogmios chain-sync + Kupo, no watcher), and that what it returns
 * opens the door for an order nobody handed us.
 *
 * **What is real here.** The order transaction is built on the Lucid emulator and
 * really submitted, so its bytes are a ledger's. Its mint redeemer is produced by
 * the SDK's own `txOrderMaterialCarriageVectorV1` against the transaction's actual
 * reference-input set, its datum by the SDK's `TxOrderDatumV1`, and its carriage
 * by the SDK's publication and certification builders — no §8 value in the
 * observation is written by this file. The observation surfaces
 * (`helpers/local-l1-observation.ts`) implement Ogmios chain-sync over a real
 * WebSocket and Kupo over real HTTP, serving that transaction's own CBOR-derived
 * view, so the read under test performs every step it performs in production:
 * Kupo match → ancestor checkpoint → `findIntersection` → `nextBlock` forward →
 * mint-redeemer extraction → reference-input resolution.
 *
 * **What is not real, and why it cannot be here.** There is no local cardano-node
 * (`l1-services/docker-compose.yml` is a docker deployment, not a test fixture),
 * so slots and header hashes are the harness's and the tx-order minting policy is
 * the always-succeeds placeholder every emulator suite in this package uses. The
 * mint's own §8.11 walk is therefore not re-run here — it is covered in Aiken —
 * and what is asserted is exactly this ticket's claim: the node can now *source*
 * the vector that walk authenticated.
 */

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxCollateralInputs: 3,
} as const;

/**
 * One 5 kB-datum output per fill byte, exactly as
 * `tx-order-material-chain-v1.test.ts` sizes them: two outputs put field 2 inside
 * §8.3's `K` and four put it above, which is what makes the tier assertions below
 * §8.4's partition rather than this file's preference.
 */
const nativeTransactionCbor = (outputFills: readonly number[]): Buffer =>
  encodeMidgardNativeTxCanonicalV1(
    materializeMidgardNativeTxFromCanonicalV1({
      version: MIDGARD_NATIVE_TX_V1_VERSION,
      validity: "TxIsValid",
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: encodeCbor(
          outputFills.map((fill) =>
            encodeMidgardTxOutput({
              address: Buffer.concat([
                Buffer.from([0x60]),
                Buffer.alloc(28, fill),
              ]),
              value: { lovelace: 2_000_000n, assets: new Map() },
              datum: {
                kind: "inline",
                cbor: Buffer.from(
                  aikenSerialisedPlutusDataCborPreservingMapOrder(
                    encodeCbor(Buffer.alloc(5_000, fill)).toString("hex"),
                  ),
                  "hex",
                ),
              },
            }),
          ),
        ),
        fee: 0n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        mintPreimageCbor: EMPTY_CBOR_LIST,
        scriptIntegrityHash: EMPTY_NULL_ROOT,
        auxiliaryDataHash: EMPTY_NULL_ROOT,
        networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    }),
  );

type Harness = {
  readonly lucid: LucidEvolution;
  readonly emulator: Emulator;
  readonly contracts: SDK.MidgardValidators;
  readonly creatorAddress: string;
  readonly creatorKeyHash: Buffer;
  readonly l1: LocalL1V1;
};

const loadAlwaysSucceedsContracts = (): Promise<SDK.MidgardValidators> =>
  Effect.runPromise(
    Effect.gen(function* () {
      const contracts = yield* AlwaysSucceedsContract;
      return contracts as unknown as SDK.MidgardValidators;
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );

const makeHarness = async (): Promise<Harness> => {
  const creator = generateEmulatorAccount({ lovelace: 60_000_000_000n });
  const emulator = new Emulator([creator], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await makeLucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(creator.seedPhrase);
  const creatorAddress = await lucid.wallet().address();
  const credential = paymentCredentialOf(creatorAddress);
  if (credential.type !== "Key") {
    throw new Error("the emulator creator must hold a key credential");
  }
  const contracts = await loadAlwaysSucceedsContracts();
  const harness: Harness = {
    lucid,
    emulator,
    contracts,
    creatorAddress,
    creatorKeyHash: Buffer.from(credential.hash, "hex"),
    l1: await startLocalL1ObservationV1(),
  };
  // One self-payment, so the creator always holds a second UTxO for the order to
  // reference. It is observed like every other transaction, because the reader
  // resolves *every* reference input through Kupo and a UTxO the local L1 has
  // never seen is not a reference input it can answer for.
  await submitAndObserve(
    harness,
    await lucid
      .newTx()
      .pay.ToAddress(creatorAddress, { lovelace: 5_000_000n })
      .complete({ localUPLCEval: true }),
  );
  return harness;
};

/**
 * Submits a built transaction and publishes it to the local L1 as its own block,
 * which is what gives it a chain point for the read to find.
 */
const submitAndObserve = async (
  harness: Harness,
  tx: TxSignBuilder,
): Promise<string> => {
  const signed = await tx.sign.withWallet().complete();
  const cbor = signed.toCBOR();
  await signed.submit();
  harness.emulator.awaitBlock(1);
  harness.l1.appendBlock([cbor]);
  return signed.toHash();
};

const publishCarriage = async (
  harness: Harness,
  plan: SDK.TxOrderCarriagePlanV1,
): Promise<readonly UTxO[]> => {
  const published: UTxO[] = [];
  for (const field of plan.referenced) {
    for (const publication of SDK.fieldPreimagePublicationOutputsV1(
      field.plan,
    )) {
      const tx = await Effect.runPromise(
        SDK.buildUnsignedFieldPreimagePublicationV1Program(harness.lucid, {
          publication,
          publisherAddress: harness.creatorAddress,
        }),
      );
      const txHash = await submitAndObserve(harness, tx);
      published.push(
        ...(await harness.lucid.utxosByOutRef([{ txHash, outputIndex: 0 }])),
      );
    }
    if (field.plan.tier !== "Certified") {
      continue;
    }
    const chunkUtxos = published.slice(-field.plan.publications.length);
    const tx = await Effect.runPromise(
      SDK.buildUnsignedFieldPreimageCertificationV1Program(harness.lucid, {
        plan: field.plan,
        certificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        certificateAddress:
          harness.contracts.fieldPreimageCertificate.spendingScriptAddress,
        certificateWitness: {
          kind: "inline_emulator_only",
          certificateScript:
            harness.contracts.fieldPreimageCertificate.mintingScript,
        },
        chunkUtxos,
        compactCbor: "",
        witnessSetCompactCbor: "",
      }),
    );
    const txHash = await submitAndObserve(harness, tx);
    published.push(
      ...(await harness.lucid.utxosByOutRef([{ txHash, outputIndex: 0 }])),
    );
  }
  return published;
};

/**
 * Builds, submits and observes a forced order carrying `nativeTxCbor`'s material
 * under whichever tiers `inlineReserveBytes` leaves the planner.
 */
const submitForcedOrder = async ({
  harness,
  nativeTxCbor,
  inlineReserveBytes,
}: {
  readonly harness: Harness;
  readonly nativeTxCbor: Buffer;
  readonly inlineReserveBytes?: number;
}): Promise<{
  readonly plan: SDK.TxOrderCarriagePlanV1;
  readonly orderUtxo: SDK.TxOrderUTxOV1;
}> => {
  const material = SDK.deriveTxOrderMaterialV1({
    nativeTxCbor,
    owner: harness.creatorKeyHash,
  });
  const plan = SDK.planTxOrderMaterialCarriageV1({
    material,
    owner: harness.creatorKeyHash,
    ...(inlineReserveBytes === undefined ? {} : { inlineReserveBytes }),
  });
  const carriageUtxos = await publishCarriage(harness, plan);
  // Largest first: the order spends the biggest UTxO, so coin selection never
  // needs to reach for the small one the transaction is also referencing.
  const walletUtxos = [...(await harness.lucid.wallet().getUtxos())].sort(
    (left, right) => Number(right.assets.lovelace - left.assets.lovelace),
  );
  const nonceInput = walletUtxos[0];
  const unrelatedReference = walletUtxos[walletUtxos.length - 1];
  if (
    nonceInput === undefined ||
    unrelatedReference === undefined ||
    walletUtxos.length < 2
  ) {
    throw new Error("the emulator creator must hold two spendable UTxOs");
  }
  // The order's reference-input set is deliberately *not* only carriage: an
  // unrelated input rides along the way a hub-oracle reference input does in a
  // real order, so the vector's positional indices have to skip it and the
  // reader's own canonical ordering has to agree with the ledger's about where
  // it lands.
  const referenceInputs = [...carriageUtxos, unrelatedReference];
  const unit = `${harness.contracts.txOrder.policyId}${Buffer.from("order").toString("hex")}`;
  const datum: SDK.TxOrderDatumV1 = {
    event: {
      id: SDK.outputReferenceFromUTxO(nonceInput),
      tx: {
        tx_id: material.transactionId,
        transaction_commitment: material.transactionCommitment,
        source: material.source,
      },
    },
    inclusion_time: BigInt(harness.emulator.now()),
    witness: harness.contracts.txOrder.policyId,
    refund_address: {
      paymentCredential: {
        PublicKeyCredential: [harness.creatorKeyHash.toString("hex")],
      },
      stakeCredential: null,
    },
    refund_datum: "NoDatum",
  };
  const mintRedeemer = Data.to(
    {
      event: {
        AuthenticateEvent: {
          nonce_input_index: 0n,
          event_output_index: 0n,
          hub_ref_input_index: 0n,
          witness_registration_redeemer_index: 0n,
        },
      },
      material_carriage: [
        ...SDK.txOrderMaterialCarriageVectorV1({
          plan,
          certificatePolicyId:
            harness.contracts.fieldPreimageCertificate.policyId,
          referenceInputs,
        }),
      ],
    } satisfies SDK.TxOrderMintRedeemerV1,
    SDK.TxOrderMintRedeemerV1,
  );
  const tx = await harness.lucid
    .newTx()
    .collectFrom([nonceInput])
    .readFrom([...referenceInputs])
    .mintAssets({ [unit]: 1n }, mintRedeemer)
    .attach.MintingPolicy(harness.contracts.txOrder.mintingScript)
    .pay.ToAddressWithData(
      harness.contracts.txOrder.spendingScriptAddress,
      { kind: "inline", value: Data.to(datum, SDK.TxOrderDatumV1) },
      { lovelace: 3_000_000n, [unit]: 1n },
    )
    .complete({ localUPLCEval: true });
  await submitAndObserve(harness, tx);

  // Read the order back off the ledger through the same authenticator the
  // ingestion walk uses, so the payload under test is the one L1 holds.
  const orderUtxos = await harness.lucid.utxosAt(
    harness.contracts.txOrder.spendingScriptAddress,
  );
  const [orderUtxo] = await Effect.runPromise(
    SDK.utxosToTxOrderUTxOsV1(orderUtxos, harness.contracts.txOrder.policyId),
  );
  if (orderUtxo === undefined) {
    throw new Error("the submitted forced order is not visible on the ledger");
  }
  return { plan, orderUtxo };
};

const carriageEffect = (harness: Harness, orderUtxo: SDK.TxOrderUTxOV1) =>
  observeVisibleTxOrderCarriageV1(
    orderUtxo,
    harness.contracts.txOrder.policyId,
    { timeoutMs: 10_000 },
  ).pipe(
    Effect.provideService(NodeConfig, {
      L1_OGMIOS_KEY: harness.l1.ogmiosUrl,
      L1_KUPO_KEY: harness.l1.kupoUrl,
    } as never),
  );

const readCarriage = (harness: Harness, orderUtxo: SDK.TxOrderUTxOV1) =>
  Effect.runPromise(carriageEffect(harness, orderUtxo));

/**
 * The `LucidError` a read failed with, kept whole.
 *
 * `Effect.runPromise` rejects with a fiber failure that copies the error's
 * message and drops its `cause`, so a negative that wants to pin *which* refusal
 * fired — rather than only that the walk refused — flips the failure into the
 * success channel instead of catching the rejection. A read that unexpectedly
 * succeeds still fails the test: the flip puts its value in the error channel.
 */
const readCarriageFailure = (
  harness: Harness,
  orderUtxo: SDK.TxOrderUTxOV1,
): Promise<SDK.LucidError> =>
  Effect.runPromise(Effect.flip(carriageEffect(harness, orderUtxo)));

/** Kupo's answer for one output reference, asked for directly. */
const kupoMatchesOf = async (
  harness: Harness,
  outRef: { readonly txHash: string; readonly outputIndex: number },
  query = "",
): Promise<readonly Record<string, unknown>[]> => {
  const response = await fetch(
    `${harness.l1.kupoUrl}/matches/${outRef.outputIndex.toString()}@${outRef.txHash}${query}`,
  );
  return (await response.json()) as readonly Record<string, unknown>[];
};

let openHarness: Harness | null = null;

const withHarness = async (): Promise<Harness> => {
  const harness = await makeHarness();
  openHarness = harness;
  return harness;
};

afterEach(async () => {
  await openHarness?.l1.close();
  openHarness = null;
});

describe("V1 forced-order §8 carriage, read off L1", () => {
  it("ingests a tier-1 order whose preimage rides the mint redeemer", async () => {
    const harness = await withHarness();
    const nativeTxCbor = nativeTransactionCbor([0x11, 0x22]);
    const { plan, orderUtxo } = await submitForcedOrder({
      harness,
      nativeTxCbor,
    });
    expect(plan.carriage.map((field) => field.plan.tier)).toEqual(["Inline"]);

    // The observed transaction really is the one the order came out of, and the
    // redeemer the read takes is the one the tx-order policy ran — reached through
    // the mint's policy list rather than by grabbing a redeemer. This order mints
    // one policy, so the pointer here is 0; the multi-policy case, where that
    // distinction has teeth, is pinned in the selection suite at the foot of this
    // file (the Lucid builder cannot put a second policy in this transaction).
    const createdAt = await fetchKupoCreationPointV1({
      kupoUrl: harness.l1.kupoUrl,
      outRef: {
        txHash: orderUtxo.utxo.txHash,
        outputIndex: orderUtxo.utxo.outputIndex,
      },
    });
    const observed = await readOgmiosBlockTransactionV1({
      ogmiosUrl: harness.l1.ogmiosUrl,
      intersection: await fetchKupoAncestorPointV1({
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
      txOrderMintRedeemerV1(observed, harness.contracts.txOrder.policyId),
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
        reconstructTxOrderMaterialV1({
          payload: orderUtxo.datum.event.tx,
          material,
        }),
      ),
    ).resolves.toEqual(nativeTxCbor);
  }, 120_000);

  it("ingests a tier-2 order whose preimage is a published raw UTxO", async () => {
    const harness = await withHarness();
    const nativeTxCbor = nativeTransactionCbor([0x11, 0x22]);
    // The same preimage as tier 1. §8.4's partition puts it inside `K`, so which
    // of tiers 1–2 carries it is the creator's budget decision: an order with no
    // inline reserve publishes it instead, which is the demotion
    // `planTxOrderMaterialCarriageV1` exists for.
    const { plan, orderUtxo } = await submitForcedOrder({
      harness,
      nativeTxCbor,
      inlineReserveBytes: 0,
    });
    expect(plan.carriage.map((field) => field.plan.tier)).toEqual(["RawUtxo"]);

    const material = await readCarriage(harness, orderUtxo);
    expect(material?.carriage.map((entry) => entry.carriage)).toEqual([
      "RawUtxo",
    ]);
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterialV1({
          payload: orderUtxo.datum.event.tx,
          material,
        }),
      ),
    ).resolves.toEqual(nativeTxCbor);
  }, 120_000);

  it("ingests a tier-3 order whose preimage is chunked under a certificate", async () => {
    const harness = await withHarness();
    // Four 5 kB-datum outputs put field 2 above §8.3's `K`, where §8.4's
    // partition makes tier 3 the only admissible carriage — the tier whose bytes
    // arrive split, whose length claim comes from a separate datum, and whose
    // reference inputs the reader has to resolve in two different shapes.
    const nativeTxCbor = nativeTransactionCbor([0x11, 0x22, 0x33, 0x44]);
    const { plan, orderUtxo } = await submitForcedOrder({
      harness,
      nativeTxCbor,
    });
    expect(plan.carriage.map((field) => field.plan.tier)).toEqual([
      "Certified",
    ]);

    const material = await readCarriage(harness, orderUtxo);
    const [entry] = material?.carriage ?? [];
    expect(entry?.carriage).toBe("Certified");
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterialV1({
          payload: orderUtxo.datum.event.tx,
          material,
        }),
      ),
    ).resolves.toEqual(nativeTxCbor);

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
    const createdAt = await fetchKupoCreationPointV1({
      kupoUrl: harness.l1.kupoUrl,
      outRef: {
        txHash: orderUtxo.utxo.txHash,
        outputIndex: orderUtxo.utxo.outputIndex,
      },
    });
    const observed = await readOgmiosBlockTransactionV1({
      ogmiosUrl: harness.l1.ogmiosUrl,
      intersection: await fetchKupoAncestorPointV1({
        kupoUrl: harness.l1.kupoUrl,
        slot: createdAt.slot,
      }),
      blockPoint: createdAt,
      txHash: orderUtxo.utxo.txHash,
    });
    expect(
      await resolveCarriageReferenceInputsV1({
        kupoUrl: harness.l1.kupoUrl,
        referenceInputs: [...observed.referenceInputs].reverse(),
      }),
    ).toEqual(resolved);
  }, 180_000);

  it("refuses an order whose creating transaction mints no tx-order NFT", async () => {
    const harness = await withHarness();
    const { orderUtxo } = await submitForcedOrder({
      harness,
      nativeTxCbor: nativeTransactionCbor([0x11, 0x22]),
    });

    // The mint redeemer is selected by *policy*, through the mint's ascending
    // policy list, and never as "the transaction's first redeemer". Asking under a
    // policy the transaction does not mint therefore finds nothing rather than
    // finding something else's redeemer.
    await expect(
      Effect.runPromise(
        observeVisibleTxOrderCarriageV1(orderUtxo, "ab".repeat(28), {
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
      nativeTxCbor: nativeTransactionCbor([0x11, 0x22]),
    });

    await expect(
      Effect.runPromise(
        observeVisibleTxOrderCarriageV1(
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
    const nativeTxCbor = nativeTransactionCbor([0x11, 0x22]);
    const { orderUtxo } = await submitForcedOrder({
      harness,
      nativeTxCbor,
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
    // the §4 hash door in `reconstructTxOrderMaterialV1` that refuses them. That
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
        reconstructTxOrderMaterialV1({
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
        reconstructTxOrderMaterialV1({
          payload: orderUtxo.datum.event.tx,
          material: await readCarriage(harness, orderUtxo),
        }),
      ),
    ).resolves.toEqual(nativeTxCbor);
  }, 120_000);

  it("reads nothing for a canonically-empty order, which needs no carriage", async () => {
    const harness = await withHarness();
    const nativeTxCbor = nativeTransactionCbor([]);
    const { plan, orderUtxo } = await submitForcedOrder({
      harness,
      nativeTxCbor,
    });
    expect(plan.carriage).toEqual([]);

    // No L1 read at all: nine empty-field commitments consume no carriage entry,
    // so the walk reconstructs the order from its own payload and an Ogmios
    // outage cannot stop it.
    const material = await readCarriage(harness, orderUtxo);
    expect(material).toBeUndefined();
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterialV1({
          payload: orderUtxo.datum.event.tx,
          material,
        }),
      ),
    ).resolves.toEqual(nativeTxCbor);
  }, 120_000);
});

/**
 * The same read, reached the way production reaches it.
 *
 * Everything above calls {@link observeVisibleTxOrderCarriageV1} directly, which
 * leaves the wiring between it and the fiber's own entry point proved only by the
 * type-checker: `reconcileVisibleTxOrderUTxOs` threads the tx-order policy id and
 * the transport overrides down through `txOrderUTxOToEntry`, and a wrong thread
 * there fails *closed* — a material-bearing order simply stops being ingested,
 * with no red test to say so. This drives the whole path, from the visible order
 * set to the row in `forced_transaction_utxos`, and asserts on the bytes that
 * landed.
 *
 * It needs a live Postgres on 5433 (`docker-compose.dev.yaml`), which the suites
 * above do not, so it takes the package's existing opt-out for database-backed
 * tests — the one `retention-enforcement-v1.test.ts` uses — rather than making the
 * whole file undrivable without a database.
 */
const dbEnabled = process.env.MIDGARD_SKIP_DB_TESTS !== "1";

const runWithDatabase = <A, E>(
  effect: Effect.Effect<A, E, Database | NodeConfig>,
): Promise<A> =>
  Effect.runPromise(
    effect.pipe(
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );

describe.skipIf(!dbEnabled)(
  "V1 forced-order ingestion, into the database",
  () => {
    beforeAll(async () => {
      await runWithDatabase(
        MigrationRunner.migrate({
          appVersion: "test",
          actor: "tx-order-carriage-l1-observation-v1.test",
        }),
      );
    }, 300_000);

    it("reconciles a material-bearing order through the fiber into forced_transaction_utxos", async () => {
      const harness = await withHarness();
      await runWithDatabase(ForcedTransactionsDB.clear);

      // Tier 2, so the row can only exist if the Kupo reference-input resolution
      // ran: the preimage is in a published UTxO, not in the redeemer.
      const nativeTxCbor = nativeTransactionCbor([0x11, 0x22]);
      const { plan, orderUtxo } = await submitForcedOrder({
        harness,
        nativeTxCbor,
        inlineReserveBytes: 0,
      });
      expect(plan.carriage.map((field) => field.plan.tier)).toEqual([
        "RawUtxo",
      ]);

      // An address the emulator has never funded, so `utxosAt` on it is empty.
      const unusedAddress = generateEmulatorAccount({ lovelace: 0n }).address;

      const nodeConfig = await Effect.runPromise(
        Effect.gen(function* () {
          const base = yield* NodeConfig;
          return {
            ...base,
            L1_OGMIOS_KEY: harness.l1.ogmiosUrl,
            L1_KUPO_KEY: harness.l1.kupoUrl,
          };
        }).pipe(Effect.provide(NodeConfig.layer)),
      );

      const result = await Effect.runPromise(
        reconcileVisibleTxOrderUTxOs(undefined, { timeoutMs: 10_000 }).pipe(
          Effect.provideService(LucidService, { api: harness.lucid } as never),
          Effect.provideService(MidgardContracts, {
            ...harness.contracts,
            // Every validator in the always-succeeds blueprint compiles to the
            // *same* trivial script and therefore to one shared address, so in
            // this fixture — and only in this fixture — the CEK program-material
            // address is also the tx-order address. Left alone, the reconciler
            // would read the order's own UTxO as program material and refuse the
            // whole pass as malformed before reaching the carriage read. A real
            // deployment has two distinct scripts and two distinct addresses;
            // naming an unfunded one here restores that separation without
            // touching anything the read authenticates.
            cekProgramMaterial: {
              ...harness.contracts.cekProgramMaterial,
              spendingScriptAddress: unusedAddress,
            },
            consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
          } as never),
          Effect.provideService(
            ContractDeploymentIdentity,
            ContractDeploymentIdentity.make({
              kind: "derived",
              consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
            }),
          ),
          Effect.provideService(NodeConfig, nodeConfig as never),
          Effect.provide(Database.layer),
        ) as Effect.Effect<
          Awaited<ReturnType<typeof Effect.runPromise>>,
          unknown,
          never
        >,
      );
      expect(result).toMatchObject({ reconciledCount: 1 });

      const rows = await runWithDatabase(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql<{
            readonly native_tx_cbor: Buffer;
            readonly tx_order_l1_tx_hash: Buffer;
            readonly operator_validity: string;
            readonly consensus_profile_id: string;
          }>`
          SELECT ${sql(ForcedTransactionsDB.Columns.NATIVE_TX_CBOR)},
                 ${sql(ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH)},
                 ${sql(ForcedTransactionsDB.Columns.OPERATOR_VALIDITY)},
                 ${sql(ForcedTransactionsDB.Columns.CONSENSUS_PROFILE_ID)}
          FROM ${sql(ForcedTransactionsDB.tableName)}
        `;
        }),
      );

      // The stored transaction is the one the order committed to, byte for byte —
      // which is only reachable by opening the published carriage against the
      // payload's own §4 commitments. Nothing in this file hands the fiber those
      // bytes; it read them off the local L1 itself.
      expect(rows.length).toBe(1);
      expect(Buffer.from(rows[0]!.native_tx_cbor)).toEqual(nativeTxCbor);
      expect(Buffer.from(rows[0]!.tx_order_l1_tx_hash).toString("hex")).toBe(
        orderUtxo.utxo.txHash,
      );

      await runWithDatabase(ForcedTransactionsDB.clear);
    }, 300_000);
  },
);

/**
 * The one piece of ledger arithmetic the read has to get right on its own.
 *
 * An order transaction observed above mints a single policy, which puts its
 * redeemer at pointer index 0 and cannot tell "this policy's redeemer" apart from
 * "the first mint redeemer". The Lucid builder cannot mix a Plutus mint with a
 * second policy in one emulator transaction, so the multi-policy case is pinned
 * here against the observation shape the reader consumes rather than through a
 * transaction that cannot be built.
 */
describe("tx-order mint redeemer selection", () => {
  const transaction = (
    mintPolicyIds: readonly string[],
    redeemers: readonly { purpose: string; index: number; redeemer: string }[],
  ) => ({
    txHash: "aa".repeat(32),
    referenceInputs: [],
    mintPolicyIds,
    redeemers,
  });
  const below = "11".repeat(28);
  const target = "22".repeat(28);

  it("selects by the policy's position in the ascending mint list", () => {
    expect(
      txOrderMintRedeemerV1(
        transaction(
          [below, target],
          [
            { purpose: "mint", index: 0, redeemer: "d87980" },
            { purpose: "mint", index: 1, redeemer: "d87a80" },
            { purpose: "spend", index: 1, redeemer: "d87b80" },
          ],
        ),
        target,
      ),
    ).toBe("d87a80");
  });

  it("refuses a transaction that mints nothing under the policy", () => {
    expect(() =>
      txOrderMintRedeemerV1(
        transaction(
          [below],
          [{ purpose: "mint", index: 0, redeemer: "d87980" }],
        ),
        target,
      ),
    ).toThrow(/mints nothing under the tx-order policy/u);
  });

  it("refuses a mint whose pointer names another policy", () => {
    // The redeemer is present, it is a mint redeemer, and it is the only one —
    // and it belongs to the policy at index 0. Taking it would authenticate an
    // order against a vector the tx-order validator never saw.
    expect(() =>
      txOrderMintRedeemerV1(
        transaction(
          [below, target],
          [{ purpose: "mint", index: 0, redeemer: "d87980" }],
        ),
        target,
      ),
    ).toThrow(/carries no mint redeemer for the tx-order policy/u);
  });
});
