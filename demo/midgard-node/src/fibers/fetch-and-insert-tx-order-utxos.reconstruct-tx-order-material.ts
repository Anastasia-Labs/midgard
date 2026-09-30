import { type MidgardCekProgramMaterialEntry } from "@al-ft/midgard-core/cek-proof";
import {
  authenticatedMidgardFieldView,
  encodeMidgardFieldArrayHeader,
  MIDGARD_EMPTY_FIELD_COMMITMENT,
  midgardFieldReadRange,
  midgardFieldTotalLength,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { isMidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import {
  midgardTxFieldCommitmentsFromSource,
  reconstructMidgardTransaction,
} from "@al-ft/midgard-core/consensus-validation";
import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  observeTxOrderMaterialCarriageProgram,
  type TxOrderCarriageReadOptions,
  type TxOrderMaterialCarriage,
} from "../l1-tx-order-carriage.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { type UserEventFetchBounds } from "./user-event-ingestion.js";

export const rawDatum = (
  txOrderUTxO: SDK.TxOrderUTxOV1,
): Effect.Effect<Buffer, SDK.LucidError> =>
  Effect.try({
    try: () => {
      const datum = txOrderUTxO.utxo.datum;
      if (datum === undefined || datum === null) {
        throw new Error(
          `Missing inline datum for tx-order UTxO ${txOrderUTxO.utxo.txHash}#${txOrderUTxO.utxo.outputIndex.toString()}`,
        );
      }
      return Buffer.from(datum, "hex");
    },
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to read tx-order inline datum",
        cause,
      }),
  });

/**
 * Fetches the currently visible tx-order UTxO set.
 *
 * This mirrors deposit and withdrawal ingestion: reconciling the full visible
 * set is safer than cursor-only scans when provider visibility lags.
 */
export const fetchTxOrderUTxOs = (
  lucid: LucidEvolution,
  consensusProfile: ContractDeploymentIdentity["consensusProfile"],
  config?: UserEventFetchBounds,
): Effect.Effect<SDK.TxOrderUTxOV1[], SDK.LucidError, MidgardContracts> =>
  Effect.gen(function* () {
    const { txOrder } = yield* MidgardContracts;
    const fetchConfig: SDK.UserEventFetchConfig = {
      eventAddress: txOrder.spendingScriptAddress,
      eventPolicyId: txOrder.policyId,
      ...config,
    };
    if (!isMidgardConsensusProfile(consensusProfile)) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Unsupported consensus profile",
          cause: consensusProfile,
        }),
      );
    }
    return yield* SDK.fetchTxOrderUTxOsProgram(lucid, fetchConfig);
  });

type TxOrderPayload = SDK.TxOrderUTxOV1["datum"]["event"]["tx"];

/**
 * The valid program material visible under the material script's credential.
 *
 * That script is a plain always-fails validator, so its credential is shared
 * with every other deployment of the same code and anyone can pay to it. An
 * output there that is not a self-authenticating entry carries no information
 * (a root is the typed hash of its preimage) and the on-chain resolver never
 * reads an output a proof does not select, so such outputs are counted and
 * skipped rather than allowed to stop ingestion.
 */
export type PublishedProgramMaterialSnapshot = {
  readonly entries: readonly MidgardCekProgramMaterialEntry[];
  readonly ignoredCount: number;
};

/**
 * How many of §2.5's nine slots a forced order's payload commits material to.
 *
 * This is what decides whether the walk reads L1 at all: a slot whose committed
 * hash is `field_commitment(#"80")` consumes no carriage entry (§8.11), so an
 * order with none of them commits nothing and is reconstructible from its own
 * payload. The count comes out of the payload's compact structures positionally,
 * which is the same extraction {@link reconstructTxOrderMaterial} makes and the
 * same one the mint's walk makes.
 *
 * `midgard-watcher`'s `forcedOrderMaterialFieldCount` computes this number for
 * the same reason — it is the input §8.11's exhaustion rule needs beside the
 * vector — and the duplication is deliberate under #599's ruling that the node
 * may not depend on the watcher package.
 */
const forcedOrderMaterialFieldCount = (payload: TxOrderPayload): number =>
  midgardTxFieldCommitmentsFromSource(
    {
      compactCbor: Buffer.from(payload.submitted_source.compact_cbor, "hex"),
      witnessSetCompactCbor: Buffer.from(
        payload.submitted_source.witness_set_compact_cbor,
        "hex",
      ),
      fieldPreimageLengthsCbor: Buffer.from(
        payload.submitted_source.field_preimage_lengths_cbor,
        "hex",
      ),
    },
    "forced",
  ).filter((commitment) => !commitment.equals(MIDGARD_EMPTY_FIELD_COMMITMENT))
    .length;

/**
 * Reconstructs the canonical native-V1 transaction an L1 forced order committed
 * to, from the §8 carriage of its nine field preimages.
 *
 * **What this used to be.** It walked the counted per-item publication receipt
 * chain backwards from `payload.terminal_receipt_reference`, checking each
 * receipt's minted asset name and its `collection_proof`, and rebuilt each field
 * preimage byte-range by byte-range with per-field arithmetic that only made
 * sense under the counted item grammar — a `[0, 1, 2, 3, 4, 7]` byte-list branch
 * and a ±1 chunk offset that existed solely because field 5 was a raw CBOR map.
 * All of that retired with the chain in #587: under `docs/spec/midgard-tx.md` §4
 * a field is committed by one flat hash over its whole preimage, so a preimage is
 * authenticated once and read whole rather than assembled from openings.
 *
 * It then spent one round refusing every order that carried material at all,
 * because the tx-order mint did: `verify_order_material` admitted only the
 * canonically-empty transaction while §8's availability re-expression was
 * unwired. #594's owner ruling wired it, and this is the ingestion half.
 *
 * **What it is now.** The same §8.8 door the mint runs, read in the same order.
 * The nine committed hashes are extracted positionally from the payload's own
 * compact structures (§4), and for each slot whose hash is not the empty-field
 * constant one carriage entry is consumed and opened through
 * `authenticatedMidgardFieldView`. The vector must be exhausted exactly, which
 * is the mint's own exhaustion rule and is what makes this a re-derivation of the
 * mint's verdict rather than a lenient re-read of it.
 *
 * The Aiken counterpart for the property that matters here — the whole preimage
 * hashed against the commitment at **every** tier, tier 3 included — is
 * `authenticated_whole_field_view`, which is what the tx-order mint opens. Plain
 * `authenticated_field_view` is the *lazy* tier-3 form and leaves a `Certified`
 * field's chunk bytes unhashed until an accessor touches one; naming it here would
 * claim a check it does not make. The two TypeScript/Aiken acceptance sets are not
 * identical at tier 3 either, in both directions — see
 * `authenticatedMidgardFieldView`'s own note in
 * `@al-ft/midgard-core/codec/native-tx-field-access-v1` for the two divergences
 * and why the certificate's digest pin makes both unreachable.
 *
 * `reconstructMidgardTransaction` then re-checks all nine preimages against the
 * compact structures and re-derives the tx-id and the proof commitment from the
 * same bytes. That is a second, independent pass over the same claim and it is
 * kept: it is what refuses a payload whose lengths, hashes and id do not agree,
 * and it costs one hash per field.
 *
 * **Where the bytes come from.** L1 history, which is what #594's ruling means by
 * durable availability and what §8.11 names as the ingestion walk's source: once
 * the order mint has authenticated a field's preimage against its committed hash,
 * those bytes are permanent history and nothing afterwards depends on the carriage
 * UTxO surviving. `l1-tx-order-carriage.ts` reads that history for the caller
 * below — Kupo for the order's creating chain point and for the carriage datums,
 * Ogmios chain-sync for the creating transaction's mint redeemer, and no other
 * dependency (#599's owner ruling makes the Ogmios + Kupo boundary binding).
 *
 * `material` stays a parameter rather than a fetch this function makes, and the
 * separation is the point: **the source is never trusted.** Whatever the read
 * returns is opened here against the *payload's own* §4 commitments, so a hostile
 * or merely stale observation can cost an order its ingestion and can never buy it
 * one. It is also why the parameter is optional — an order committing no material
 * needs no read, and its nine empty-field commitments reconstruct it alone.
 */
export const reconstructTxOrderMaterial = ({
  payload,
  material,
}: {
  readonly payload: TxOrderPayload;
  readonly material?: TxOrderMaterialCarriage;
}): Effect.Effect<Buffer, SDK.LucidError> =>
  Effect.try({
    try: () => {
      const transactionId = Buffer.from(payload.tx_id, "hex");
      const source = {
        compactCbor: Buffer.from(payload.submitted_source.compact_cbor, "hex"),
        witnessSetCompactCbor: Buffer.from(
          payload.submitted_source.witness_set_compact_cbor,
          "hex",
        ),
        fieldPreimageLengthsCbor: Buffer.from(
          payload.submitted_source.field_preimage_lengths_cbor,
          "hex",
        ),
      };
      const emptyFieldPreimage = encodeMidgardFieldArrayHeader(0);
      const commitments = midgardTxFieldCommitmentsFromSource(source, "forced");
      const referenceInputs = material?.referenceInputs ?? [];
      const unconsumed = [...(material?.carriage ?? [])];
      const fieldPreimages = commitments.map((commitment, fieldIndex) => {
        if (commitment.equals(MIDGARD_EMPTY_FIELD_COMMITMENT)) {
          return emptyFieldPreimage;
        }
        const carriage = unconsumed.shift();
        if (carriage === undefined) {
          // §8.11's exhaustion rule, failing in the missing direction: the vector
          // is short of what the nine commitments name, so a field with material
          // in it has no carriage. The mint refuses the same order for the same
          // reason, which is what makes this a re-derivation of its verdict.
          throw new Error(
            `forced order carries material in field ${fieldIndex.toString()} ` +
              `with no §8 carriage supplied for it (${(
                material?.carriage ?? []
              ).length.toString()} entr${
                (material?.carriage ?? []).length === 1 ? "y" : "ies"
              } supplied)`,
          );
        }
        const view = authenticatedMidgardFieldView({
          fieldIndex,
          txId: transactionId,
          expectedCommitment: commitment,
          carriage,
          referenceInputs,
        });
        return midgardFieldReadRange(view, 0, midgardFieldTotalLength(view));
      });
      if (unconsumed.length > 0) {
        // The mint's exhaustion rule, re-derived. A spare entry means the vector
        // being read is not the one the mint authenticated.
        throw new Error(
          `forced order supplied ${unconsumed.length.toString()} §8 carriage ` +
            "entries more than its nine commitments name",
        );
      }
      return reconstructMidgardTransaction({
        sourceKind: "forced",
        transactionId,
        transactionCommitment: Buffer.from(
          payload.transaction_commitment,
          "hex",
        ),
        source,
        fieldPreimages,
      });
    },
    catch: (cause) =>
      new SDK.LucidError({
        message:
          "Failed to reconstruct the authenticated V1 tx-order material from its §8 carriage",
        cause,
      }),
  });

/**
 * Reads the §8 carriage a visible forced order's mint authenticated, or
 * `undefined` when the order commits no material and there is nothing to read.
 *
 * **The skip is deliberate and is stated rather than implied.** An order with
 * nine empty-field commitments consumes no carriage entry, so the read would only
 * establish one further thing: that the mint redeemer's vector really was empty,
 * §8.11's exhaustion rule in the spare direction. Paying an Ogmios round trip per
 * such order to re-derive a clause the mint enforced — and making an endpoint
 * outage stop ingesting orders that ingest today without it — buys less than it
 * costs. The clause is not unobserved: `midgard-watcher` re-derives it, from the
 * same redeemer, for every order and every burn.
 *
 * **Cost, stated rather than hidden.** Reconciliation walks the whole *visible*
 * order set every tick, so a material-bearing order that stays visible is read
 * off L1 once per tick for as long as it does. Nothing here caches: an order's
 * carriage is immutable once its creating transaction is on chain, so a read
 * keyed by the order's out-ref would be sound, and it is left out of this change
 * deliberately rather than by oversight — it is a throughput question, it belongs
 * with the reconciler's own "already persisted" handling (which deposits and
 * withdrawals share), and adding process-lifetime state to an authentication path
 * is not something to do as a side effect of giving it a source.
 */
export const observeVisibleTxOrderCarriage = (
  txOrderUTxO: SDK.TxOrderUTxOV1,
  txOrderPolicyId: string,
  read: TxOrderCarriageReadOptions | undefined,
): Effect.Effect<
  TxOrderMaterialCarriage | undefined,
  SDK.LucidError,
  NodeConfig
> =>
  Effect.gen(function* () {
    const payload = txOrderUTxO.datum.event.tx;
    const materialFieldCount = yield* Effect.try({
      try: () => forcedOrderMaterialFieldCount(payload),
      catch: (cause) =>
        new SDK.LucidError({
          message:
            "Failed to read a forced order's nine committed field hashes",
          cause,
        }),
    });
    if (materialFieldCount === 0) {
      return undefined;
    }
    const nodeConfig = yield* NodeConfig;
    return yield* observeTxOrderMaterialCarriageProgram({
      ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
      kupoUrl: nodeConfig.L1_KUPO_KEY,
      txOrderOutRef: {
        txHash: txOrderUTxO.utxo.txHash,
        outputIndex: txOrderUTxO.utxo.outputIndex,
      },
      txOrderPolicyId,
      ...read,
    });
  });
