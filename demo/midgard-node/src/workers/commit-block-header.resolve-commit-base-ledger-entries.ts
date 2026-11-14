import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import {
  ConfirmedLedgerDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import * as Ledger from "../database/utils/ledger.js";
import {
  landedLedger,
  ledgerAt,
  ledgerEntries,
} from "../landed-blocks/ledger.js";
import { retrieveRows } from "../landed-blocks/store.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  utxoToLedgerInsertMaterial,
} from "../mpf/index.js";
import { ForeignBlockVerificationError } from "../mpf/verified-block-import.js";
import { Database, NodeConfig } from "../services/index.js";
import {
  deserializeStateQueueUTxO,
  type SerializedStateQueueUTxO,
} from "./utils/commit-block-header.js";

/** The ledger a block commit builds on, and where it came from. */
export type ResolvedCommitBaseLedgerEntries = {
  readonly source: string;
  readonly entries?: readonly Ledger.MinimalEntry[];
  readonly root: string;
};

/** Why a commit waits on a tail landed-block processing has not applied to
 * the working ledger yet, or on this node's own block in flight. */
export const LANDED_COMMIT_BASE_PENDING =
  "The commit base is not a processed landed block the working ledger holds yet";

export const resolveCommitBaseLedgerEntries = ({
  availableConfirmedBlock,
  nativeMpfRoot,
  requireEntries,
}: {
  readonly availableConfirmedBlock: "" | SerializedStateQueueUTxO;
  readonly nativeMpfRoot: string;
  readonly requireEntries: boolean;
}): Effect.Effect<
  ResolvedCommitBaseLedgerEntries,
  unknown,
  Database | NodeConfig
> =>
  Effect.gen(function* () {
    // A live, unlanded own block is never a base, and the tail it builds on
    // is not one either while it is in flight: the next block waits for it
    // to land and be applied. This holds when it leaves the root unchanged,
    // too, where the tail checks below would pass.
    const inFlight = yield* PendingBlockFinalizationsDB.retrieveLive();
    if (Option.isSome(inFlight))
      return yield* Effect.fail(
        new ForeignBlockVerificationError({
          foreignHeaderHash:
            inFlight.value[
              PendingBlockFinalizationsDB.Columns.HEADER_HASH
            ].toString("hex"),
          reason: "missing",
          detail: LANDED_COMMIT_BASE_PENDING,
        }),
      );
    if (availableConfirmedBlock !== "") {
      const latestBlock = yield* deserializeStateQueueUTxO(
        availableConfirmedBlock,
      );
      if (latestBlock.datum.key === "Empty") {
        const confirmedState = (yield* SDK.getConfirmedStateFromStateQueueDatum(
          latestBlock.datum,
        )).data;
        if (!requireEntries && nativeMpfRoot === confirmedState.utxoRoot) {
          return {
            source: "persistent-ledger-mpf",
            root: nativeMpfRoot,
          } satisfies ResolvedCommitBaseLedgerEntries;
        }
        yield* Effect.logInfo(
          "🔹 Commit base state_queue tip is the confirmed-state root; using confirmed_ledger as base.",
        );
        const confirmedEntriesForGenesis = yield* ConfirmedLedgerDB.retrieve;
        if (confirmedEntriesForGenesis.length === 0) {
          const nodeConfig = yield* NodeConfig;
          const genesisEntries = yield* Effect.forEach(
            nodeConfig.GENESIS_UTXOS,
            (utxo) =>
              utxoToLedgerInsertMaterial(utxo).pipe(
                Effect.map(({ ledgerOp, outputCbor }) => ({
                  [Ledger.Columns.OUTREF]: ledgerOp.key,
                  [Ledger.Columns.OUTPUT]: outputCbor,
                })),
              ),
          );
          if (genesisEntries.length > 0) {
            const root =
              yield* computeLedgerMpfRootFromLedgerEntries(genesisEntries);
            yield* Effect.logInfo(
              `🔹 Commit base ledger snapshot resolved from configured genesis UTxOs (entries=${genesisEntries.length.toString()}).`,
            );
            return {
              source: "genesis",
              entries: genesisEntries,
              root,
            } satisfies ResolvedCommitBaseLedgerEntries;
          }
        }
      } else {
        const header = yield* SDK.getHeaderFromStateQueueDatum(
          latestBlock.datum,
        );
        const currentLedgerRootHex = nativeMpfRoot;
        const headerHash = yield* SDK.hashBlockHeader(header);
        // A tail, foreign or this node's own, is built on only once
        // landed-block processing took it in at its header's root and the
        // rebase applied it: its post-state is the processed landed ledger
        // at that header.
        const landed = yield* landedLedger(yield* retrieveRows);
        const at =
          landed === undefined ? undefined : ledgerAt(landed, headerHash);
        if (
          at === undefined ||
          at.root !== header.utxosRoot ||
          currentLedgerRootHex !== header.utxosRoot ||
          (at.row !== undefined && !at.row.applied)
        )
          return yield* Effect.fail(
            new ForeignBlockVerificationError({
              foreignHeaderHash: headerHash,
              reason: "missing",
              detail: LANDED_COMMIT_BASE_PENDING,
            }),
          );
        const entries = ledgerEntries(at.ledger);
        return {
          source: `landed:${headerHash}`,
          entries,
          root: at.root,
        } satisfies ResolvedCommitBaseLedgerEntries;
      }
    }

    const confirmedEntries = yield* ConfirmedLedgerDB.retrieve;
    const confirmedRoot =
      yield* computeLedgerMpfRootFromLedgerEntries(confirmedEntries);
    yield* Effect.logInfo(
      `🔹 Commit base ledger snapshot resolved from confirmed_ledger (entries=${confirmedEntries.length.toString()}).`,
    );
    return {
      source: "confirmed_ledger",
      entries: confirmedEntries,
      root: confirmedRoot,
    } satisfies ResolvedCommitBaseLedgerEntries;
  });
