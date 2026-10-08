/**
 * Forced orders on a simulated chain (N10, plan §12): the deployment the
 * projection follows, real order material from the SDK, the §8 carriage it
 * publishes, and the order transaction whose mint redeemer names that
 * carriage. Also a ledger and content sources the resolver can be pointed
 * at, built from the simulated chain itself.
 */
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  computeMidgardForcedTxProofCommitment,
  computeMidgardNativeTxId,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  materializeMidgardForcedTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { encodeMidgardFieldArrayHeader } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { deriveMidgardTxFieldPreimages } from "@al-ft/midgard-core/consensus-validation";
import {
  compareOutRefs,
  decodeLedgerUtxos,
  type LedgerOutputs,
  type OutRef,
  outRefKey,
  type TxContentSource,
} from "@al-ft/midgard-l1-follower";
import {
  encodeTxBody,
  encodeUtxoAnswer,
  type SimTx,
  simTxHash,
  type SimUtxo,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { ForcedOrderConfig } from "../../src/forced-orders/index.js";
import type { ChainDriver } from "./l1-events-store.js";

export const FORCED_CONFIG: ForcedOrderConfig = {
  policyId: "f1".repeat(28),
  orderAddress: `70${"f2".repeat(28)}`,
  cekMaterialCredential: "f3".repeat(28),
};

/** The order creator's wallet: untracked, so its outputs are not facts. */
export const WALLET = Buffer.concat([
  Buffer.from([0x60]),
  Buffer.alloc(28, 0x5a),
]);

const OWNER = Buffer.alloc(28, 0x66);

/**
 * A canonical native transaction with one 5 kB-datum output per fill: two
 * put its output field inside §8.3's `K`, so it can be carried whole.
 */
export const nativeTransactionCbor = (outputFills: readonly number[]): Buffer =>
  encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
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

/**
 * An order's material and its carriage plan with every field published
 * (tier 2; nothing inline), so the order reads its preimage off a
 * reference input.
 */
export const publishedOrderMaterial = (submittedTxCbor: Buffer) => {
  const material = SDK.deriveTxOrderMaterial({
    submittedTxCbor,
    owner: OWNER,
  });
  const plan = SDK.planTxOrderMaterialCarriage({
    material,
    owner: OWNER,
    inlineReserveBytes: 0,
  });
  const [field] = plan.referenced;
  if (field === undefined || field.plan.tier !== "RawUtxo")
    throw new Error("the fixture material must publish one tier-2 field");
  return { material, preimage: field.preimage };
};

/**
 * The order material of any decodable forced transaction, every non-empty
 * field carried inline, built from the codec alone: the SDK builder's
 * admission screen is the creator's, and the order policy does not run it,
 * so ingestion has to meet transactions the screen would have refused.
 */
export const inlineOrderMaterial = (submittedTxCbor: Buffer) => {
  const tx = decodeMidgardForcedTxFullFromCanonicalCbor(submittedTxCbor);
  const source = deriveMidgardForcedTxProofSource(tx);
  const empty = encodeMidgardFieldArrayHeader(0);
  return {
    material: {
      transactionId: computeMidgardNativeTxId(tx.compact).toString("hex"),
      transactionCommitment:
        computeMidgardForcedTxProofCommitment(source).toString("hex"),
      submitted_source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
    },
    inline: deriveMidgardTxFieldPreimages(submittedTxCbor, "forced")
      .map((field) => field.preimageCbor)
      .filter((preimage) => !preimage.equals(empty)),
  };
};

/** A tx publishing `bytes` as one raw carriage output at the wallet. */
export const publicationTx = (
  bytes: Uint8Array,
  input: OutRef,
  nonce: number,
): SimTx => ({
  inputs: [input],
  outputs: [
    {
      address: WALLET,
      lovelace: 2_000_000n,
      datum: Buffer.from(SDK.fieldPreimagePublicationDatumCbor(bytes), "hex"),
    },
  ],
  nonce,
});

const ORDER_NAME = Buffer.from("order").toString("hex");

/**
 * The order tx: mints the order token, pays the order output with its
 * datum, and names `carriage` (one tier-2 outref per carried field, in field
 * order) by its position among the sorted reference inputs, as the ledger
 * presents them to the mint. With `inline`, the vector is those preimages
 * instead, each carried in the redeemer.
 */
export const orderTx = (input: {
  material: Pick<
    SDK.TxOrderMaterial,
    "transactionId" | "transactionCommitment" | "submitted_source"
  >;
  nonceInput: OutRef;
  carriage: readonly OutRef[];
  inline?: readonly Buffer[];
  otherReferences?: readonly OutRef[];
  inclusionTime: bigint;
  nonce: number;
}): SimTx => {
  const referenceInputs = [
    ...input.carriage,
    ...(input.otherReferences ?? []),
  ].sort(compareOutRefs);
  const position = (outRef: OutRef): bigint =>
    BigInt(
      referenceInputs.findIndex(
        (other) => outRefKey(other) === outRefKey(outRef),
      ),
    );
  const datum: SDK.TxOrderDatum = {
    event: {
      id: {
        transactionId: input.nonceInput.txHash.toString("hex"),
        outputIndex: BigInt(input.nonceInput.index),
      },
      tx: {
        tx_id: input.material.transactionId,
        transaction_commitment: input.material.transactionCommitment,
        submitted_source: input.material.submitted_source,
      },
    },
    inclusion_time: input.inclusionTime,
    witness: FORCED_CONFIG.policyId,
    refund_address: {
      paymentCredential: {
        PublicKeyCredential: [OWNER.toString("hex")],
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
    material_carriage:
      input.inline === undefined
        ? input.carriage.map((outRef) => ({
            RawUtxo: { ref_input_index: position(outRef) },
          }))
        : input.inline.map((preimage) => ({
            Inline: { preimage: preimage.toString("hex") },
          })),
  };
  const token = new Map([
    [FORCED_CONFIG.policyId, new Map([[ORDER_NAME, 1n]])],
  ]);
  return {
    inputs: [input.nonceInput],
    referenceInputs,
    outputs: [
      {
        address: Buffer.from(FORCED_CONFIG.orderAddress, "hex"),
        lovelace: 3_000_000n,
        assets: token,
        datum: SDK.encodeTxOrderDatumCbor(datum),
      },
    ],
    mint: token,
    redeemers: [
      {
        purpose: "mint",
        index: 0,
        data: Buffer.from(Data.to(redeemer, SDK.TxOrderMintRedeemer), "hex"),
      },
    ],
    nonce: input.nonce,
  };
};

/**
 * A ledger over the simulated chain: the UTxO set after each block it was
 * shown (`record`), readable while that block is at most `k` blocks below
 * the tip (deeper is `point_unavailable`, as a node's volatile window), and
 * the live set at the tip. `calls` lists the points asked.
 */
export const simLedger = (driver: ChainDriver, k: number) => {
  const states = new Map<string, { height: number; utxos: SimUtxo[] }>();
  const calls: string[] = [];
  const answer = (utxos: readonly SimUtxo[], outRefs: readonly OutRef[]) => {
    const wanted = new Set(outRefs.map(outRefKey));
    return decodeLedgerUtxos(
      encodeUtxoAnswer(utxos.filter((u) => wanted.has(outRefKey(u.outRef)))),
    );
  };
  const ledger: LedgerOutputs = (at, outRefs) => {
    if (at === "tip") {
      calls.push("tip");
      return Promise.resolve(answer(driver.chain.live(), outRefs));
    }
    calls.push(at.slot.toString());
    const state = states.get(at.hash.toString("hex"));
    if (state === undefined || driver.tip.height - state.height > k)
      return Promise.resolve({
        kind: "point_unavailable" as const,
        detail: "acquire_point_too_old",
      });
    return Promise.resolve(answer(state.utxos, outRefs));
  };
  return {
    ledger,
    calls,
    /** Remembers the UTxO set at the current tip. */
    record: () => {
      states.set(driver.tip.point.hash.toString("hex"), {
        height: driver.tip.height,
        utxos: driver.chain.live(),
      });
    },
  };
};

/** A content source serving the bodies of `txs` by id (`bytes` overrides). */
export const simContentSource = (
  name: string,
  txs: readonly SimTx[],
  bytes: (tx: SimTx) => Buffer = encodeTxBody,
): TxContentSource & { asked: string[] } => {
  const asked: string[] = [];
  const byId = new Map(txs.map((tx) => [simTxHash(tx).toString("hex"), tx]));
  return {
    name,
    asked,
    fetchTx: (txHash) => {
      asked.push(txHash.toString("hex"));
      const tx = byId.get(txHash.toString("hex"));
      return Promise.resolve(tx === undefined ? null : bytes(tx));
    },
  };
};
