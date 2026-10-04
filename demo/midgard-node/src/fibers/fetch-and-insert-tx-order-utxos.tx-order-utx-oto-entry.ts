import { createHash } from "node:crypto";

import {
  decodeMidgardCekProgramMaterialEntry,
  encodeMidgardCekProgramMaterialEntry,
  encodeMidgardCekProgramMaterialSidecar,
  type MidgardCekProgramMaterialEntry,
  midgardCekProgramMaterialKindFromTag,
  MidgardCekProgramMaterialMissingRootError,
} from "@al-ft/midgard-core/cek-proof";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import {
  isMidgardConsensusProfile,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import { collectMidgardAttachedProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { Effect, Option, Ref } from "effect";

import {
  CekProgramMaterialDB,
  ForcedTransactionsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { type TxOrderCarriageReadOptions } from "../l1-tx-order-carriage.js";
import {
  ContractDeploymentIdentity,
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import {
  fetchTxOrderUTxOs,
  observeVisibleTxOrderCarriage,
  type PublishedProgramMaterialSnapshot,
  rawDatum,
  reconstructTxOrderMaterial,
} from "./fetch-and-insert-tx-order-utxos.reconstruct-tx-order-material.js";
import {
  logReconciledVisibleUserEvents,
  persistVisibleUserEventUTxOs,
  runCommitTimeUserEventIngestionBarrier,
  type UserEventFetchBounds,
  type UserEventReconcileResult,
} from "./user-event-ingestion.js";

const txOrderUTxOToEntry = (
  txOrderUTxO: SDK.TxOrderUTxOV1,
  consensusProfile: ContractDeploymentIdentity["consensusProfile"],
  publishedProgramMaterial: PublishedProgramMaterialSnapshot,
  txOrderPolicyId: string,
  read: TxOrderCarriageReadOptions | undefined,
): Effect.Effect<
  ForcedTransactionsDB.Entry,
  SDK.LucidError | DatabaseError,
  Database | NodeConfig
> =>
  Effect.gen(function* () {
    const inclusionTime = txOrderUTxO.inclusionTime;
    const datum = yield* rawDatum(txOrderUTxO);
    if (!isMidgardConsensusProfile(consensusProfile)) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Unsupported consensus profile",
          cause: consensusProfile,
        }),
      );
    }
    const txOrderUTxOV1 = txOrderUTxO;
    const payload = txOrderUTxOV1.datum.event.tx;
    const material = yield* observeVisibleTxOrderCarriage(
      txOrderUTxOV1,
      txOrderPolicyId,
      read,
    );
    const nativeTxCbor = yield* reconstructTxOrderMaterial({
      payload,
      material,
    });
    const decoded = decodeMidgardForcedTxFullFromCanonicalCbor(nativeTxCbor);
    const attachedProgramEnvelopes = yield* Effect.try({
      try: () => collectMidgardAttachedProgramEnvelopes(decoded),
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
            cause instanceof MidgardCekProgramMaterialMissingRootError,
          () =>
            Effect.logWarning(
              `V1 tx-order ${payload.tx_id} is visible before its complete L1 CEK material bundle`,
            ),
        ),
      );
    }
    // The verdict recorded at ingest is provisional: the operator has not run
    // Phase A/B yet. Admission requires the submitted bytes to claim
    // TxIsValid (`E_IS_VALID_FALSE_FORBIDDEN`, enforced inside
    // `encodeForcedInclusionValueV1`), so the only verdict an unadjudicated
    // admitted preimage can carry is `ForcedTxValid`; block commitment
    // recomputes and overwrites both this row's verdict and its
    // `forced_inclusion_value`, and the encoder stamps the committed leaf's
    // validity scalar from whatever verdict it is given (`ForcedTxValid` ⇔
    // code 0, `ForcedTxInvalid { _ }` ⇔ code 1).
    const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValueV1({
      nativeTxCbor,
      verdict: "ForcedTxValid",
      consensusProfile: consensusProfile satisfies MidgardConsensusProfile,
    });
    // The two identity columns this row carries are **recomputed** from the
    // reconstructed canonical bytes, not copied out of the datum:
    // `encodeForcedInclusionValueV1` re-derives the proof source from
    // `nativeTxCbor` and hashes it. The datum's own values are already bound —
    // `reconstructTxOrderMaterial` fed both to `reconstructMidgardTransaction`,
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
    const programMaterialSidecarCbor = encodeMidgardCekProgramMaterialSidecar(
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
  const entries: MidgardCekProgramMaterialEntry[] = [];
  let ignoredCount = 0;
  for (const utxo of utxos) {
    try {
      if (utxo.datum == null) {
        throw new Error("material UTxO has no inline datum");
      }
      const datum = SDK.decodeCekProgramMaterialDatumCbor(
        Buffer.from(utxo.datum, "hex"),
      );
      entries.push(
        decodeMidgardCekProgramMaterialEntry(
          encodeMidgardCekProgramMaterialEntry({
            kind: midgardCekProgramMaterialKindFromTag(datum.kind),
            root: Buffer.from(
              datum.root,
              "hex",
            ) as MidgardCekProgramMaterialEntry["root"],
            preimage: Buffer.from(datum.preimage, "hex"),
          }),
        ),
      );
    } catch {
      ignoredCount += 1;
    }
  }
  return { entries: Object.freeze(entries), ignoredCount };
};

/**
 * The instant through which a successful reconcile proves every forced
 * transaction ingested, or undefined when its bounds do not cover the whole
 * past (a lower bound). An order's inclusion time is its mint's validity upper
 * bound plus `event_wait_duration` (enforced on chain), so an order included
 * at or before the fetch start was minted before it and is in the visible set
 * the fetch reads; a later inclusion time may still be unminted. A bounded
 * fetch reads only inclusion times below its exclusive upper bound. The
 * watermark is the earlier of the two.
 */
export const txOrdersIngestedThroughMs = (
  fetchStartedAtMs: number,
  config: UserEventFetchBounds | undefined,
): number | undefined =>
  config?.inclusionTimeLowerBound !== undefined
    ? undefined
    : config?.inclusionTimeUpperBound === undefined
      ? fetchStartedAtMs
      : Math.min(fetchStartedAtMs, Number(config.inclusionTimeUpperBound) - 1);

/**
 * Advances `Globals.TX_ORDERS_INGESTED_THROUGH_MS`, never moving it back.
 * The commit worker thread has no `Globals` and records nothing; the periodic
 * tx-order fiber and the barrier refresher run on the main thread and do.
 */
const recordTxOrdersIngestedThrough = (
  throughMs: number | undefined,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    const globals = yield* Effect.serviceOption(Globals);
    if (throughMs === undefined || Option.isNone(globals)) return;
    yield* Ref.update(globals.value.TX_ORDERS_INGESTED_THROUGH_MS, (current) =>
      current === undefined || throughMs > current ? throughMs : current,
    );
  });

export const reconcileVisibleTxOrderUTxOs = (
  config?: UserEventFetchBounds,
  /**
   * Transport overrides for the §8 carriage read. Production passes nothing and
   * gets the configured Ogmios and Kupo endpoints over the platform's own
   * `fetch` and `WebSocket`; the seam exists so a test can point the same read at
   * a local L1 rather than stub out the read itself.
   */
  read?: TxOrderCarriageReadOptions,
): Effect.Effect<
  UserEventReconcileResult,
  SDK.LucidError | DatabaseError,
  MidgardContracts | ContractDeploymentIdentity | Lucid | Database | NodeConfig
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const { consensusProfile } = yield* ContractDeploymentIdentity;
    if (!isMidgardConsensusProfile(consensusProfile)) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message: "Unsupported consensus profile",
          cause: consensusProfile,
        }),
      );
    }
    const fetchStartedAtMs = Date.now();
    const txOrderUTxOs: SDK.TxOrderUTxOV1[] = [
      ...(yield* fetchTxOrderUTxOs(lucid, consensusProfile, config)),
    ];
    const { cekProgramMaterial, txOrder } = yield* MidgardContracts;
    // By payment credential, as the on-chain resolver matches: material
    // published under any stake part is as valid there as at the enterprise
    // address.
    const material = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosAt({
          type: "Script",
          hash: cekProgramMaterial.spendingScriptHash,
        }),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to resolve V1 L1 CEK program material",
          cause,
        }),
    }).pipe(Effect.map(publishedProgramMaterialEntries));
    if (material.ignoredCount > 0) {
      yield* Effect.logDebug(
        `Ignored ${material.ignoredCount.toString()} non-material output(s) under the CEK program-material credential`,
      );
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
    const reconciled = yield* persistVisibleUserEventUTxOs({
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
    yield* recordTxOrdersIngestedThrough(
      txOrdersIngestedThroughMs(fetchStartedAtMs, config),
    );
    return reconciled;
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
