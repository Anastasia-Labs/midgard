/**
 * A forced order's `forced_transaction_utxos` row, from the order and the
 * nine field preimages its carriage supplied.
 */
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
import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import type { MidgardForcedTxAdmissionStopped } from "@al-ft/midgard-core/consensus-validation";
import { collectMidgardAttachedProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  CekProgramMaterialDB,
  ForcedTransactionsDB,
} from "../database/index.js";
import type { DatabaseError } from "../database/utils/common.js";
import type { NodeConfig } from "../services/config.js";
import type { Database } from "../services/database.js";
import { reconstructTxOrderMaterial } from "./carriage.js";

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

export const publishedProgramMaterialEntries = (
  utxos: readonly UTxO[],
): PublishedProgramMaterialSnapshot => {
  const entries: MidgardCekProgramMaterialEntry[] = [];
  let ignoredCount = 0;
  for (const utxo of utxos) {
    try {
      if (utxo.datum == null)
        throw new Error("material UTxO has no inline datum");
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

const lucidError = (message: string, cause: unknown) =>
  new SDK.LucidError({ message, cause });

/**
 * The row for `order`, whose canonical transaction is rebuilt from
 * `fieldPreimages`. The two identity columns are recomputed from the
 * rebuilt bytes and checked against the datum's, so a reconstruction that
 * lost identity is never persisted under the datum's name. Attached CEK
 * program envelopes are persisted with the published material that roots
 * them, read lazily: only an order with envelopes needs it.
 */
export const forcedOrderEntry = (input: {
  readonly order: SDK.TxOrderUTxOV1;
  readonly fieldPreimages: readonly Uint8Array[];
  readonly consensusProfile: MidgardConsensusProfile;
  readonly programMaterial: () => readonly UTxO[];
}): Effect.Effect<
  ForcedTransactionsDB.Entry,
  SDK.LucidError | DatabaseError | MidgardForcedTxAdmissionStopped,
  Database | NodeConfig
> =>
  Effect.gen(function* () {
    const { order, consensusProfile } = input;
    const payload = order.datum.event.tx;
    const datum = order.utxo.datum;
    if (datum == null)
      return yield* Effect.fail(
        lucidError("Missing inline datum for a tx order", order.utxo.txHash),
      );
    const nativeTxCbor = yield* Effect.try({
      try: () =>
        reconstructTxOrderMaterial({
          payload,
          fieldPreimages: input.fieldPreimages,
        }),
      catch: (cause) =>
        lucidError(
          "Failed to reconstruct the authenticated V1 tx-order material from its field preimages",
          cause,
        ),
    });
    // Validate the kept screens before collecting attached program material:
    // a malformed program envelope is a typed stop, never a Lucid/DB wrapper.
    const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValueV1({
      nativeTxCbor,
      verdict: "ForcedTxValid",
      consensusProfile,
    });
    const attached = yield* Effect.try({
      try: () =>
        collectMidgardAttachedProgramEnvelopes(
          decodeMidgardForcedTxFullFromCanonicalCbor(nativeTxCbor),
          "forced",
        ),
      catch: (cause) =>
        lucidError(
          "Failed to collect V1 attached CEK program envelopes",
          cause,
        ),
    });
    if (attached.length > 0) {
      const material = publishedProgramMaterialEntries(input.programMaterial());
      yield* CekProgramMaterialDB.persistVerifiedBundles(
        attached,
        material.entries,
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
    if (payload.tx_id !== encoded.txId.toString("hex"))
      return yield* Effect.fail(
        lucidError(
          "V1 tx-order transaction id does not match its canonical transaction",
          `datum=${payload.tx_id},derived=${encoded.txId.toString("hex")}`,
        ),
      );
    if (
      payload.transaction_commitment !==
      encoded.transactionCommitment.toString("hex")
    )
      return yield* Effect.fail(
        lucidError(
          "V1 tx-order transaction commitment does not match its canonical transaction",
          `datum=${payload.transaction_commitment},derived=${encoded.transactionCommitment.toString("hex")}`,
        ),
      );
    const sidecar = encodeMidgardCekProgramMaterialSidecar([]);
    const C = ForcedTransactionsDB.Columns;
    return {
      [C.TX_ORDER_ID]: Buffer.from(order.idCbor),
      [C.TX_ORDER_L1_TX_HASH]: Buffer.from(order.utxo.txHash, "hex"),
      [C.TX_ORDER_L1_OUTPUT_INDEX]: order.utxo.outputIndex,
      [C.ASSET_NAME]: Buffer.from(order.assetName, "hex"),
      [C.RAW_DATUM]: Buffer.from(datum, "hex"),
      [C.TX_ID]: encoded.txId,
      [C.TX_COMPACT]: encoded.txCompact,
      [C.FORCED_INCLUSION_VALUE]: encoded.value,
      [C.CONSENSUS_PROFILE_ID]: consensusProfile.profileId,
      [C.NATIVE_TX_CBOR]: nativeTxCbor,
      [C.TRANSACTION_COMMITMENT]: encoded.transactionCommitment,
      [C.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]: sidecar,
      [C.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]: createHash("sha256")
        .update(sidecar)
        .digest(),
      [C.INCLUSION_TIME]: order.inclusionTime,
      [C.PROJECTED_HEADER_HASH]: null,
      [C.STATUS]: ForcedTransactionsDB.Status.Awaiting,
    };
  });
