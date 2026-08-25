import { createHash } from "node:crypto";

import {
  decodeMidgardCekProgramMaterialEntryV1,
  encodeMidgardCekProgramMaterialEntryV1,
  encodeMidgardCekProgramMaterialSidecarV1,
  type MidgardCekProgramMaterialEntryV1,
  midgardCekProgramMaterialKindFromTagV1,
  MidgardCekProgramMaterialMissingRootError,
} from "@al-ft/midgard-core/cek-proof";
import { decodeMidgardNativeTxFullV1FromCanonicalCbor } from "@al-ft/midgard-core/codec";
import {
  authenticatedMidgardFieldViewV1,
  encodeMidgardFieldArrayHeaderV1,
  MIDGARD_EMPTY_FIELD_COMMITMENT_V1,
  midgardFieldReadRangeV1,
  midgardFieldTotalLengthV1,
} from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import {
  isMidgardConsensusProfileV1,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  midgardV1TxFieldCommitmentsFromSourceV1,
  reconstructMidgardTransactionV1,
} from "@al-ft/midgard-core/consensus-validation-v1";
import { collectMidgardV1AttachedProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import {
  CekProgramMaterialDB,
  ForcedTransactionsDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  logReconciledVisibleUserEvents,
  persistVisibleUserEventUTxOs,
  repeatVisibleUserEventIngestionFiber,
  runCommitTimeUserEventIngestionBarrier,
  type UserEventFetchBounds,
  type UserEventReconcileResult,
} from "@/fibers/user-event-ingestion.js";
import {
  observeTxOrderMaterialCarriageProgram,
  type TxOrderCarriageReadOptionsV1,
  type TxOrderMaterialCarriageV1,
} from "@/l1-tx-order-carriage-v1.js";
import {
  ContractDeploymentIdentity,
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";

const rawDatum = (
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
const fetchTxOrderUTxOs = (
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
    if (!isMidgardConsensusProfileV1(consensusProfile)) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Unsupported consensus profile",
          cause: consensusProfile,
        }),
      );
    }
    return yield* SDK.fetchTxOrderUTxOsV1Program(lucid, fetchConfig);
  });

type TxOrderPayloadV1 = SDK.TxOrderUTxOV1["datum"]["event"]["tx"];

export type PublishedProgramMaterialSnapshot = {
  readonly entries: readonly MidgardCekProgramMaterialEntryV1[];
  readonly malformedCount: number;
  readonly sourceStatus: "clean" | "malformed";
};

export const isDeferrablePublishedProgramMaterialError = (
  snapshot: PublishedProgramMaterialSnapshot,
  cause: unknown,
): cause is MidgardCekProgramMaterialMissingRootError =>
  snapshot.malformedCount === 0 &&
  snapshot.sourceStatus === "clean" &&
  cause instanceof MidgardCekProgramMaterialMissingRootError;

export const publishedProgramMaterialSnapshotError = (
  snapshot: PublishedProgramMaterialSnapshot,
  sourceAddress: string,
): SDK.LucidError | undefined =>
  snapshot.malformedCount !== 0 || snapshot.sourceStatus === "malformed"
    ? new SDK.LucidError({
        message:
          "V1 L1 CEK program-material publication contains malformed UTxOs",
        cause: {
          sourceAddress,
          sourceStatus: snapshot.sourceStatus,
          malformedCount: snapshot.malformedCount,
        },
      })
    : undefined;

/**
 * How many of §2.5's nine slots a forced order's payload commits material to.
 *
 * This is what decides whether the walk reads L1 at all: a slot whose committed
 * hash is `field_commitment(#"80")` consumes no carriage entry (§8.11), so an
 * order with none of them commits nothing and is reconstructible from its own
 * payload. The count comes out of the payload's compact structures positionally,
 * which is the same extraction {@link reconstructTxOrderMaterialV1} makes and the
 * same one the mint's walk makes.
 *
 * `midgard-watcher`'s `forcedOrderMaterialFieldCount` computes this number for
 * the same reason — it is the input §8.11's exhaustion rule needs beside the
 * vector — and the duplication is deliberate under #599's ruling that the node
 * may not depend on the watcher package.
 */
const forcedOrderMaterialFieldCountV1 = (payload: TxOrderPayloadV1): number =>
  midgardV1TxFieldCommitmentsFromSourceV1({
    compactCbor: Buffer.from(payload.source.compact_cbor, "hex"),
    witnessSetCompactCbor: Buffer.from(
      payload.source.witness_set_compact_cbor,
      "hex",
    ),
    fieldPreimageLengthsCbor: Buffer.from(
      payload.source.field_preimage_lengths_cbor,
      "hex",
    ),
  }).filter(
    (commitment) => !commitment.equals(MIDGARD_EMPTY_FIELD_COMMITMENT_V1),
  ).length;

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
 * `authenticatedMidgardFieldViewV1`. The vector must be exhausted exactly, which
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
 * `authenticatedMidgardFieldViewV1`'s own note in
 * `@al-ft/midgard-core/codec/native-tx-field-access-v1` for the two divergences
 * and why the certificate's digest pin makes both unreachable.
 *
 * `reconstructMidgardTransactionV1` then re-checks all nine preimages against the
 * compact structures and re-derives the tx-id and the proof commitment from the
 * same bytes. That is a second, independent pass over the same claim and it is
 * kept: it is what refuses a payload whose lengths, hashes and id do not agree,
 * and it costs one hash per field.
 *
 * **Where the bytes come from.** L1 history, which is what #594's ruling means by
 * durable availability and what §8.11 names as the ingestion walk's source: once
 * the order mint has authenticated a field's preimage against its committed hash,
 * those bytes are permanent history and nothing afterwards depends on the carriage
 * UTxO surviving. `l1-tx-order-carriage-v1.ts` reads that history for the caller
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
export const reconstructTxOrderMaterialV1 = ({
  payload,
  material,
}: {
  readonly payload: TxOrderPayloadV1;
  readonly material?: TxOrderMaterialCarriageV1;
}): Effect.Effect<Buffer, SDK.LucidError> =>
  Effect.try({
    try: () => {
      const transactionId = Buffer.from(payload.tx_id, "hex");
      const source = {
        compactCbor: Buffer.from(payload.source.compact_cbor, "hex"),
        witnessSetCompactCbor: Buffer.from(
          payload.source.witness_set_compact_cbor,
          "hex",
        ),
        fieldPreimageLengthsCbor: Buffer.from(
          payload.source.field_preimage_lengths_cbor,
          "hex",
        ),
      };
      const emptyFieldPreimage = encodeMidgardFieldArrayHeaderV1(0);
      const commitments = midgardV1TxFieldCommitmentsFromSourceV1(source);
      const referenceInputs = material?.referenceInputs ?? [];
      const unconsumed = [...(material?.carriage ?? [])];
      const fieldPreimages = commitments.map((commitment, fieldIndex) => {
        if (commitment.equals(MIDGARD_EMPTY_FIELD_COMMITMENT_V1)) {
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
        const view = authenticatedMidgardFieldViewV1({
          fieldIndex,
          txId: transactionId,
          expectedCommitment: commitment,
          carriage,
          referenceInputs,
        });
        return midgardFieldReadRangeV1(
          view,
          0,
          midgardFieldTotalLengthV1(view),
        );
      });
      if (unconsumed.length > 0) {
        // The mint's exhaustion rule, re-derived. A spare entry means the vector
        // being read is not the one the mint authenticated.
        throw new Error(
          `forced order supplied ${unconsumed.length.toString()} §8 carriage ` +
            "entries more than its nine commitments name",
        );
      }
      return reconstructMidgardTransactionV1({
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
export const observeVisibleTxOrderCarriageV1 = (
  txOrderUTxO: SDK.TxOrderUTxOV1,
  txOrderPolicyId: string,
  read: TxOrderCarriageReadOptionsV1 | undefined,
): Effect.Effect<
  TxOrderMaterialCarriageV1 | undefined,
  SDK.LucidError,
  NodeConfig
> =>
  Effect.gen(function* () {
    const payload = txOrderUTxO.datum.event.tx;
    const materialFieldCount = yield* Effect.try({
      try: () => forcedOrderMaterialFieldCountV1(payload),
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

const txOrderUTxOToEntry = (
  txOrderUTxO: SDK.TxOrderUTxOV1,
  consensusProfile: ContractDeploymentIdentity["consensusProfile"],
  publishedProgramMaterial: PublishedProgramMaterialSnapshot,
  txOrderPolicyId: string,
  read: TxOrderCarriageReadOptionsV1 | undefined,
): Effect.Effect<
  ForcedTransactionsDB.Entry,
  SDK.LucidError | DatabaseError,
  Database | NodeConfig
> =>
  Effect.gen(function* () {
    const inclusionTime = txOrderUTxO.inclusionTime;
    const datum = yield* rawDatum(txOrderUTxO);
    if (!isMidgardConsensusProfileV1(consensusProfile)) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Unsupported consensus profile",
          cause: consensusProfile,
        }),
      );
    }
    const txOrderUTxOV1 = txOrderUTxO;
    const payload = txOrderUTxOV1.datum.event.tx;
    const material = yield* observeVisibleTxOrderCarriageV1(
      txOrderUTxOV1,
      txOrderPolicyId,
      read,
    );
    const nativeTxCbor = yield* reconstructTxOrderMaterialV1({
      payload,
      material,
    });
    const decoded = decodeMidgardNativeTxFullV1FromCanonicalCbor(nativeTxCbor);
    const attachedProgramEnvelopes = yield* Effect.try({
      try: () => collectMidgardV1AttachedProgramEnvelopes(decoded),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to collect V1 attached CEK program envelopes",
          cause,
        }),
    });
    if (attachedProgramEnvelopes.length > 0) {
      yield* CekProgramMaterialDB.persistVerifiedBundles(
        attachedProgramEnvelopes,
        publishedProgramMaterial.entries,
      ).pipe(
        Effect.catchIf(
          (cause): cause is MidgardCekProgramMaterialMissingRootError =>
            isDeferrablePublishedProgramMaterialError(
              publishedProgramMaterial,
              cause,
            ),
          () =>
            Effect.logWarning(
              `V1 tx-order ${payload.tx_id} is visible before its complete L1 CEK material bundle`,
            ),
        ),
        Effect.mapError((cause) =>
          cause instanceof MidgardCekProgramMaterialMissingRootError
            ? new DatabaseError({
                table: CekProgramMaterialDB.entryTableName,
                message:
                  "Unexpected missing CEK material root outside a clean publication snapshot",
                cause,
              })
            : cause,
        ),
      );
    }
    // The verdict recorded at ingest is provisional: the operator has not run
    // Phase A/B yet, so it can only mirror the submitted transaction's own
    // validity scalar — which is what the #640 forced-arm predicate binds the
    // verdict to (`ForcedTxValid` ⇔ code 0, `ForcedTxInvalid { _ }` ⇔ code 1).
    // A self-declared-invalid preimage carries no reason of its own, so it
    // takes the classifier's unattributed-rejection bucket (`ValueNotPreserved`,
    // the `forcedVerdictForRejection` default); block commitment recomputes and
    // overwrites both this row's verdict and its `forced_inclusion_value`.
    const provisionalVerdict: SDK.OperatorVerdictV1 =
      decoded.validity === "TxIsValid"
        ? "ForcedTxValid"
        : { ForcedTxInvalid: { reason: "ValueNotPreserved" } };
    const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValueV1({
      nativeTxCbor,
      verdict: provisionalVerdict,
      consensusProfile: consensusProfile satisfies MidgardConsensusProfileV1,
    });
    // The two identity columns this row carries are **recomputed** from the
    // reconstructed canonical bytes, not copied out of the datum:
    // `encodeForcedInclusionValueV1` re-derives the proof source from
    // `nativeTxCbor` and hashes it. The datum's own values are already bound —
    // `reconstructTxOrderMaterialV1` fed both to `reconstructMidgardTransactionV1`,
    // whose per-field `verifyMidgardV1TxFieldPreimage` refuses a source that does
    // not hash to the datum's `transaction_commitment` — but that binding is
    // transitive, through a round-trip. These two checks make it direct, so a
    // reconstruction that lost identity on the way out cannot be persisted under
    // the datum's name.
    if (payload.tx_id !== encoded.txId.toString("hex")) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message:
            "V1 tx-order transaction id does not match its canonical transaction",
          cause: `datum=${payload.tx_id},derived=${encoded.txId.toString("hex")}`,
        }),
      );
    }
    if (
      payload.transaction_commitment !==
      encoded.transactionCommitment.toString("hex")
    ) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message:
            "V1 tx-order transaction commitment does not match its canonical transaction",
          cause: `datum=${payload.transaction_commitment},derived=${encoded.transactionCommitment.toString("hex")}`,
        }),
      );
    }
    const programMaterialSidecarCbor = encodeMidgardCekProgramMaterialSidecarV1(
      [],
    );
    return {
      [ForcedTransactionsDB.Columns.TX_ORDER_ID]: Buffer.from(
        txOrderUTxOV1.idCbor,
      ),
      [ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH]: Buffer.from(
        txOrderUTxOV1.utxo.txHash,
        "hex",
      ),
      [ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX]:
        txOrderUTxOV1.utxo.outputIndex,
      [ForcedTransactionsDB.Columns.ASSET_NAME]: Buffer.from(
        txOrderUTxOV1.assetName,
        "hex",
      ),
      [ForcedTransactionsDB.Columns.RAW_DATUM]: datum,
      [ForcedTransactionsDB.Columns.TX_ID]: encoded.txId,
      [ForcedTransactionsDB.Columns.TX_COMPACT]: encoded.txCompact,
      [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: encoded.value,
      [ForcedTransactionsDB.Columns.OPERATOR_VALIDITY]:
        ForcedTransactionsDB.midgardTxValidityOfVerdictV1(provisionalVerdict),
      [ForcedTransactionsDB.Columns.CONSENSUS_PROFILE_ID]:
        consensusProfile.profileId,
      [ForcedTransactionsDB.Columns.NATIVE_TX_CBOR]: nativeTxCbor,
      [ForcedTransactionsDB.Columns.TRANSACTION_COMMITMENT]:
        encoded.transactionCommitment,
      [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]:
        programMaterialSidecarCbor,
      [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]:
        createHash("sha256").update(programMaterialSidecarCbor).digest(),
      [ForcedTransactionsDB.Columns.INCLUSION_TIME]: inclusionTime,
      [ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH]: null,
      [ForcedTransactionsDB.Columns.STATUS]:
        ForcedTransactionsDB.Status.Awaiting,
    };
  });

export const publishedProgramMaterialEntries = (
  utxos: readonly UTxO[],
): PublishedProgramMaterialSnapshot => {
  const entries: MidgardCekProgramMaterialEntryV1[] = [];
  let malformedCount = 0;
  for (const utxo of utxos) {
    try {
      if (utxo.datum == null) {
        throw new Error("material UTxO has no inline datum");
      }
      const datum = SDK.decodeCekProgramMaterialDatumV1Cbor(
        Buffer.from(utxo.datum, "hex"),
      );
      entries.push(
        decodeMidgardCekProgramMaterialEntryV1(
          encodeMidgardCekProgramMaterialEntryV1({
            kind: midgardCekProgramMaterialKindFromTagV1(datum.kind),
            root: Buffer.from(
              datum.root,
              "hex",
            ) as MidgardCekProgramMaterialEntryV1["root"],
            preimage: Buffer.from(datum.preimage, "hex"),
          }),
        ),
      );
    } catch {
      malformedCount += 1;
    }
  }
  return {
    entries: Object.freeze(entries),
    malformedCount,
    sourceStatus: malformedCount === 0 ? "clean" : "malformed",
  };
};

export const reconcileVisibleTxOrderUTxOs = (
  config?: UserEventFetchBounds,
  /**
   * Transport overrides for the §8 carriage read. Production passes nothing and
   * gets the configured Ogmios and Kupo endpoints over the platform's own
   * `fetch` and `WebSocket`; the seam exists so a test can point the same read at
   * a local L1 rather than stub out the read itself.
   */
  read?: TxOrderCarriageReadOptionsV1,
): Effect.Effect<
  UserEventReconcileResult,
  SDK.LucidError | DatabaseError,
  MidgardContracts | ContractDeploymentIdentity | Lucid | Database | NodeConfig
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const { consensusProfile } = yield* ContractDeploymentIdentity;
    if (!isMidgardConsensusProfileV1(consensusProfile)) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Unsupported consensus profile",
          cause: consensusProfile,
        }),
      );
    }
    const txOrderUTxOs: SDK.TxOrderUTxOV1[] = [
      ...(yield* fetchTxOrderUTxOs(lucid, consensusProfile, config)),
    ];
    const { cekProgramMaterial, txOrder } = yield* MidgardContracts;
    const materialEntries = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(cekProgramMaterial.spendingScriptAddress),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to resolve V1 L1 CEK program material",
          cause,
        }),
    }).pipe(Effect.map(publishedProgramMaterialEntries));
    const material = {
      ...materialEntries,
      sourceAddress: cekProgramMaterial.spendingScriptAddress,
    };
    const malformedMaterialError = publishedProgramMaterialSnapshotError(
      material,
      material.sourceAddress,
    );
    if (malformedMaterialError !== undefined) {
      return yield* Effect.fail(malformedMaterialError);
    }
    if (material.entries.length > 0) {
      yield* CekProgramMaterialDB.persistVerifiedBundles(
        [],
        material.entries,
      ).pipe(
        Effect.mapError((cause) =>
          cause instanceof MidgardCekProgramMaterialMissingRootError
            ? new DatabaseError({
                table: CekProgramMaterialDB.entryTableName,
                message:
                  "Unexpected missing CEK material root in an empty-envelope publication snapshot",
                cause,
              })
            : cause,
        ),
      );
    }
    return yield* persistVisibleUserEventUTxOs({
      visibleUtxos: txOrderUTxOs,
      toEntry: (utxo) =>
        txOrderUTxOToEntry(
          utxo,
          consensusProfile,
          material,
          txOrder.policyId,
          read,
        ),
      insertEntries: ForcedTransactionsDB.insertEntries,
      emptyLogMessage: "No tx-order UTxOs found.",
      foundLogMessage: (count) => `${count} tx-order UTxO(s) found.`,
    });
  });

export const fetchAndInsertTxOrderUTxOs: Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | ContractDeploymentIdentity | Lucid | Database | NodeConfig
> = Effect.gen(function* () {
  yield* Effect.logDebug("fetching TxOrderUTxOs...");
  const { reconciledCount } = yield* reconcileVisibleTxOrderUTxOs();
  yield* logReconciledVisibleUserEvents({
    reconciledCount,
    message: (count) =>
      `Reconciled ${count} visible tx-order UTxO(s) into forced_transaction_utxos.`,
  });
});

export const fetchAndInsertTxOrderUTxOsForCommitBarrier = (
  inclusionTimeUpperBound: Date,
): Effect.Effect<
  Date,
  SDK.LucidError | DatabaseError,
  MidgardContracts | ContractDeploymentIdentity | Lucid | Database | NodeConfig
> =>
  runCommitTimeUserEventIngestionBarrier({
    inclusionTimeUpperBound,
    inclusionTimeUpperBoundOffsetMs: 1,
    startLogMessage: (upperBound) =>
      `Running commit-time tx-order ingestion barrier up to ${upperBound.toISOString()}.`,
    completedLogMessage: ({
      reconciledCount,
      completedAt,
      inclusionTimeUpperBound: upperBound,
    }) =>
      `Commit-time tx-order barrier reconciled ${reconciledCount} tx-order UTxO(s); fetch completed at ${completedAt.toISOString()} and locked the visibility barrier at ${upperBound.toISOString()}.`,
    reconcile: reconcileVisibleTxOrderUTxOs,
  });

export const fetchAndInsertTxOrderUTxOsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  | MidgardContracts
  | ContractDeploymentIdentity
  | Lucid
  | Database
  | NodeConfig
  | Globals
> =>
  repeatVisibleUserEventIngestionFiber({
    schedule,
    startLogMessage: "Fetch and insert TxOrderUTxOs.",
    spanName: "fetch-and-insert-tx-order-utxos-fiber",
    action: fetchAndInsertTxOrderUTxOs,
  });
