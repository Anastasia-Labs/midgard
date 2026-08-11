import { createHash } from "node:crypto";

import {
  decodeMidgardCekProgramMaterialEntryV1,
  encodeMidgardCekProgramMaterialEntryV1,
  encodeMidgardCekProgramMaterialSidecarV1,
  type MidgardCekProgramMaterialEntryV1,
  midgardCekProgramMaterialKindFromTagV1,
  MidgardCekProgramMaterialMissingRootError,
} from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardNativeTxProofFieldLengthsV1,
} from "@al-ft/midgard-core/codec";
// `authenticatedMidgardFieldViewV1` is deliberately not imported — see
// `reconstructTxOrderMaterialV1`'s note on why the §8.8 door has no carriage to
// open on this path until #589 lands the §8.6 certificate.
import { encodeMidgardFieldArrayHeaderV1 } from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import {
  isMidgardConsensusProfileV1,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import { reconstructMidgardTransactionV1 } from "@al-ft/midgard-core/consensus-validation-v1";
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
 * authenticated once and read whole rather than assembled from openings. #585
 * then made the TypeScript side actually derive those hashes that way.
 *
 * **What it is now.** It reads the committed field lengths, refuses any order that
 * carries material, and reconstructs the transaction from nine §5.1 empty-field
 * preimages. `reconstructMidgardTransactionV1` is what authenticates: it checks
 * every preimage against the field hash the compact structure carries, and
 * re-derives the tx-id and the proof commitment from the same bytes, so a payload
 * whose lengths, hashes and id do not agree is refused.
 *
 * **Why the §8.8 door is not the authenticator here.** #587's ruling named
 * `authenticatedMidgardFieldViewV1` — deliberately not imported, so this is a
 * name and not a `{@link}` that would dangle — and that is still the destination.
 * What has changed since that ruling is the reason it is not reached yet.
 *
 * It used to be that `deriveNativeTxBodyCompact` derived the nine field hashes
 * under the retired counted scheme, so no §4 commitment existed on this path
 * except the empty field's constant, and a door call would have compared a
 * constant against a constant. #585 swapped that: the nine hashes are now §4 flat
 * commitments, and `verifyMidgardV1TxFieldPreimage` — which is what
 * `reconstructMidgardTransactionV1` calls per field — performs exactly the §4
 * check the door would, taking each expected hash from the compact structure in
 * view as §4's positional-identity invariant requires. So the check here is real,
 * and it is the same check.
 *
 * What is still missing is the door's *subject*: it opens a field out of §8
 * carriage, and this path has no carriage to open. The tx-order mint's
 * `verify_order_material` admits only the canonically-empty transaction, so the
 * only preimages reachable here are the nine empty ones, which arrive as a
 * constant rather than through a carriage tier. #589 owns the §8.6 certificate
 * work that gives this path real carriage, and owns moving this function onto the
 * door at the same time — the two are one change, not two.
 *
 * The material limit is the other half: the tx-order mint's
 * `verify_order_material` admits only the canonically-empty transaction. See its
 * `field_carriage_availability` note and #587's Deviation for why; issue #589
 * owns the §8.6 certificate work that lifts it, and owns moving this function
 * onto the door.
 */
export const reconstructTxOrderMaterialV1 = ({
  payload,
}: {
  readonly payload: TxOrderPayloadV1;
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
      const fieldLengths = decodeMidgardNativeTxProofFieldLengthsV1(
        source.fieldPreimageLengthsCbor,
      );
      const emptyFieldPreimage = encodeMidgardFieldArrayHeaderV1(0);
      const withMaterial = fieldLengths.flatMap((length, fieldIndex) =>
        length === emptyFieldPreimage.length ? [] : [fieldIndex],
      );
      if (withMaterial.length > 0) {
        throw new Error(
          `forced order carries material in field(s) ${withMaterial.join(", ")}; ` +
            "the §8.6-certified carriage the tx-order mint needs for non-empty " +
            "material is not deployable yet (see #587's Deviation and issue #589, " +
            "which owns the blocker), so no such order can be authenticated on L1 " +
            "and none can be ingested here",
        );
      }
      return reconstructMidgardTransactionV1({
        transactionId,
        transactionCommitment: Buffer.from(
          payload.transaction_commitment,
          "hex",
        ),
        source,
        fieldPreimages: fieldLengths.map(() => emptyFieldPreimage),
      });
    },
    catch: (cause) =>
      new SDK.LucidError({
        message:
          "Failed to reconstruct the authenticated V1 tx-order material from its §8 carriage",
        cause,
      }),
  });

const txOrderUTxOToEntry = (
  txOrderUTxO: SDK.TxOrderUTxOV1,
  consensusProfile: ContractDeploymentIdentity["consensusProfile"],
  publishedProgramMaterial: PublishedProgramMaterialSnapshot,
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
    const nativeTxCbor = yield* reconstructTxOrderMaterialV1({ payload });
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
    const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValueV1({
      nativeTxCbor,
      operatorValidity: decoded.validity,
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
      [ForcedTransactionsDB.Columns.OPERATOR_VALIDITY]: decoded.validity,
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
    const { cekProgramMaterial } = yield* MidgardContracts;
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
      toEntry: (utxo) => txOrderUTxOToEntry(utxo, consensusProfile, material),
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
