/**
 * The forced selector (`resolveIncludedForcedTransactionEntriesForWindow`)
 * includes exactly the forced orders that are due for the block: those whose
 * inclusion time falls in the block's event window. A forced transaction's
 * native validity interval (slots) plays no part in that choice, matching the
 * on-chain due predicate; the interval only decides the validation machine's
 * verdict at the block slot.
 */
import "./utils.js";

import { createHash } from "node:crypto";

import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect } from "vitest";

import {
  ForcedTransactionsDB,
  MigrationRunner,
} from "../src/database/index.js";
import { resolveIncludedForcedTransactionEntriesForWindow } from "../src/mpf/event-window.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const isolatedDb = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      return yield* effect;
    }),
  );

beforeAll(async () => {
  await Effect.runPromise(
    provideDatabaseLayers(
      MigrationRunner.migrate({
        appVersion: "forced-transaction-window-selection-test",
        actor: "forced-transaction-window-selection-test",
      }),
    ),
  );
});

const BLOCK_START = new Date("2026-10-09T12:00:00.000Z");
const BLOCK_END = new Date(BLOCK_START.getTime() + 20_000);
// The block slot the machine checks intervals against; scalars are slots.
const BLOCK_SLOT = 70_000_000n;

const forcedEntry = (
  label: number,
  inclusionTime: Date,
  validityIntervalStart: bigint,
  validityIntervalEnd: bigint,
): Effect.Effect<ForcedTransactionsDB.Entry> =>
  Effect.gen(function* () {
    const nativeTxCbor = encodeMidgardForcedTxCanonical(
      materializeMidgardForcedTxFromCanonical({
        version: 1n,
        body: {
          spendInputsPreimageCbor: EMPTY_CBOR_LIST,
          referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
          outputsPreimageCbor: EMPTY_CBOR_LIST,
          fee: BigInt(label),
          validityIntervalStart,
          validityIntervalEnd,
          requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
          requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
          mintPreimageCbor: EMPTY_CBOR_LIST,
          scriptIntegrityHash: EMPTY_NULL_ROOT,
          auxiliaryDataHash: EMPTY_NULL_ROOT,
          networkId: 255n,
        },
        witnessSet: {
          addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
          scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
          redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        },
      }),
    );
    const encoded = yield* ForcedTransactionsDB.encodeForcedInclusionValueV1({
      nativeTxCbor,
      verdict: "ForcedTxValid",
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    }).pipe(Effect.orDie);
    const sidecar = encodeMidgardCekProgramMaterialSidecar([]);
    return {
      [ForcedTransactionsDB.Columns.TX_ORDER_ID]: Buffer.from(
        Data.to(
          {
            transactionId: Buffer.alloc(32, label).toString("hex"),
            outputIndex: 0n,
          },
          SDK.OutputReference,
        ),
        "hex",
      ),
      [ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH]: Buffer.alloc(
        32,
        label,
      ),
      [ForcedTransactionsDB.Columns.TX_ORDER_L1_OUTPUT_INDEX]: 0,
      [ForcedTransactionsDB.Columns.ASSET_NAME]: Buffer.alloc(32, label),
      [ForcedTransactionsDB.Columns.RAW_DATUM]: Buffer.from([label]),
      [ForcedTransactionsDB.Columns.TX_ID]: encoded.txId,
      [ForcedTransactionsDB.Columns.TX_COMPACT]: encoded.txCompact,
      [ForcedTransactionsDB.Columns.FORCED_INCLUSION_VALUE]: encoded.value,
      [ForcedTransactionsDB.Columns.CONSENSUS_PROFILE_ID]:
        MIDGARD_CONSENSUS_PROFILE.profileId,
      [ForcedTransactionsDB.Columns.NATIVE_TX_CBOR]: nativeTxCbor,
      [ForcedTransactionsDB.Columns.TRANSACTION_COMMITMENT]:
        encoded.transactionCommitment,
      [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_CBOR]: sidecar,
      [ForcedTransactionsDB.Columns.CEK_PROGRAM_MATERIAL_SIDECAR_SHA256]:
        createHash("sha256").update(sidecar).digest(),
      [ForcedTransactionsDB.Columns.INCLUSION_TIME]: inclusionTime,
      [ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH]: null,
      [ForcedTransactionsDB.Columns.STATUS]:
        ForcedTransactionsDB.Status.Awaiting,
    };
  });

describe("forced transaction window selection", () => {
  it.effect(
    "includes exactly the orders due by inclusion time, whatever their validity interval",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const inside = new Date(BLOCK_START.getTime() + 5_000);
          const due = [
            // Bounded, containing the block slot.
            yield* forcedEntry(1, inside, BLOCK_SLOT - 10n, BLOCK_SLOT + 10n),
            // Bounded, entirely before the block slot.
            yield* forcedEntry(2, inside, BLOCK_SLOT - 20n, BLOCK_SLOT - 10n),
            // Bounded, entirely after the block slot.
            yield* forcedEntry(3, inside, BLOCK_SLOT + 10n, BLOCK_SLOT + 20n),
            // Open-ended either side, and fully open.
            yield* forcedEntry(4, inside, -1n, BLOCK_SLOT - 10n),
            yield* forcedEntry(5, inside, BLOCK_SLOT + 10n, -1n),
            yield* forcedEntry(6, BLOCK_END, -1n, -1n),
          ];
          // Bounded and containing the block slot, but its inclusion time is
          // one millisecond past the window: not due for this block.
          const late = yield* forcedEntry(
            7,
            new Date(BLOCK_END.getTime() + 1),
            BLOCK_SLOT - 10n,
            BLOCK_SLOT + 10n,
          );
          yield* ForcedTransactionsDB.insertEntries([...due, late]);

          const selected =
            yield* resolveIncludedForcedTransactionEntriesForWindow({
              currentBlockStartTime: BLOCK_START,
              effectiveEndTime: BLOCK_END,
            });

          const ids = (entries: readonly ForcedTransactionsDB.Entry[]) =>
            entries
              .map((entry) =>
                entry[ForcedTransactionsDB.Columns.TX_ORDER_ID].toString("hex"),
              )
              .sort();
          expect(ids(selected)).toEqual(ids(due));
        }),
      ),
  );
});
