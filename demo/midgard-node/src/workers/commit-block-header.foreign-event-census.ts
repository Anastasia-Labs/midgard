import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import {
  type CensusBlockRow,
  decodeAdmissions,
  type ForcedHistoryAdmission,
} from "../database/eventHistoryForeignCensus.js";
import { decodeJournalIncarnation } from "../database/eventHistoryJournalCodec.js";
import {
  type HistoryIncarnation,
  historyIncarnationDigest,
} from "../l1-event-history-provenance.js";
import { ForeignBlockVerificationError } from "../mpf/verified-block-import.js";
import { withHistoryWrite } from "../services/event-history-producer.js";
import {
  assertForeignVerificationSource,
  currentForeignVerificationSource,
  ForeignVerificationSource,
} from "../services/foreign-verification-source.js";
import { historyEligibilityHorizon } from "../services/history-commit-window.js";

export type ForeignEventCensus = Readonly<{
  deposits: readonly HistoryIncarnation[];
  withdrawals: readonly HistoryIncarnation[];
  forced: readonly ForcedHistoryAdmission[];
}>;

/** Complete immutable source census, including retired/pruned origins. Neither
 * DA claims nor current pending rows select this set. The source owner folds
 * every full canonical block from authenticated activation before retention. */
export const foreignEventCensus = (headerHash: string, header: SDK.Header) =>
  withHistoryWrite(
    Effect.gen(function* () {
      const provided = yield* Effect.serviceOption(ForeignVerificationSource);
      const source = Option.isSome(provided)
        ? provided.value
        : yield* currentForeignVerificationSource();
      yield* assertForeignVerificationSource(source);
      const fail = (reason: "missing" | "invalid", detail: string) =>
        Effect.fail(
          new ForeignBlockVerificationError({
            foreignHeaderHash: headerHash,
            reason,
            detail,
          }),
        );
      const { coverage, token } = source.binding;
      if (header.endTime > BigInt(historyEligibilityHorizon(coverage)))
        return yield* fail(
          "missing",
          "Canonical source has not covered the entire foreign event window",
        );
      const sql = yield* SqlClient.SqlClient;
      const binding = Buffer.from(coverage.bindingDigest, "hex");
      const frontier =
        yield* sql`SELECT 1 FROM event_history_census_frontier f JOIN event_history_census_blocks b ON b.binding_digest = f.binding_digest
    WHERE f.binding_digest = ${binding} AND f.manifest_id = ${Buffer.from(token.deploymentIdentity, "hex")}
      AND b.block_hash = ${Buffer.from(coverage.point.id, "hex")} AND b.block_slot = ${coverage.point.slot} AND b.canonical
      AND b.block_height >= f.activation_height AND b.block_height <= f.head_height`;
      if (frontier.length !== 1)
        return yield* fail(
          "missing",
          "Complete canonical source census is being reacquired by its owner",
        );
      const incarnations = yield* sql<{
        incarnation_record: string;
        incarnation_digest: Buffer;
      }>`SELECT incarnation_record,incarnation_digest FROM event_history_incarnations WHERE binding_digest = ${binding} AND origin_canonical`;
      const forcedBlocks =
        yield* sql<CensusBlockRow>`SELECT block_hash,parent_hash,block_slot::text,block_height::text,receipt_digest,admissions_record,admissions_digest
    FROM event_history_census_blocks WHERE binding_digest = ${binding} AND canonical AND block_slot <= ${coverage.point.slot} ORDER BY block_height`;
      return yield* Effect.try({
        try: () => {
          const inWindow = (time: bigint) =>
            time > header.startTime && time <= header.endTime;
          const deposits: HistoryIncarnation[] = [];
          const withdrawals: HistoryIncarnation[] = [];
          const forced: ForcedHistoryAdmission[] = [];
          for (const row of incarnations) {
            const value = decodeJournalIncarnation(row.incarnation_record);
            if (
              historyIncarnationDigest(value) !==
              row.incarnation_digest.toString("hex")
            )
              throw new Error("Canonical incarnation digest differs");
            if (
              value.bindingDigest !== coverage.bindingDigest ||
              value.placement === null
            )
              throw new Error("Canonical incarnation source binding differs");
            if (
              value.placement.admission.slot <= coverage.point.slot &&
              inWindow(value.event.inclusionTime)
            )
              (value.kind === "deposit" ? deposits : withdrawals).push(value);
          }
          for (const row of forcedBlocks)
            for (const admission of decodeAdmissions(row)) {
              const datum = SDK.decodeTxOrderDatumCbor(
                Buffer.from(admission.datumCbor, "hex"),
              );
              if (inWindow(datum.inclusion_time)) forced.push(admission);
            }
          return Object.freeze({
            deposits: Object.freeze(deposits),
            withdrawals: Object.freeze(withdrawals),
            forced: Object.freeze(forced),
          });
        },
        catch: (cause) =>
          new ForeignBlockVerificationError({
            foreignHeaderHash: headerHash,
            reason: "invalid",
            detail: `Canonical event census corrupt: ${String(cause)}`,
          }),
      });
    }),
  );

/** Equality is checked even when the claimed set is empty. */
export const assertForeignEventCensusMatchesPayload = (
  census: ForeignEventCensus,
  payload: SDK.DaPayload,
): void => {
  const exact = (
    expected: readonly string[],
    actual: readonly SDK.DaPayloadEntry[],
    kind: string,
  ) => {
    const keys = new Set(expected);
    if (
      keys.size !== expected.length ||
      actual.length !== keys.size ||
      new Set(actual.map(([key]) => key)).size !== actual.length ||
      actual.some(([key]) => !keys.has(key))
    )
      throw new Error(
        `Foreign ${kind} set differs from complete canonical source census`,
      );
  };
  exact(
    census.deposits.map((value) => value.event.idCbor),
    payload.block_body.deposits,
    "deposit",
  );
  exact(
    census.withdrawals.map((value) => value.event.idCbor),
    payload.block_body.withdrawals,
    "withdrawal",
  );
  exact(
    census.forced.map((value) => value.key),
    payload.block_body.forced_transactions,
    "forced transaction",
  );
};
