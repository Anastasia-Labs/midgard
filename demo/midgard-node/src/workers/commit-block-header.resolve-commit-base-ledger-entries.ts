import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import {
  ConfirmedLedgerDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import * as Ledger from "../database/utils/ledger.js";
import {
  landedLedger,
  ledgerEntries,
  onOneSnapshot,
} from "../landed-blocks/ledger.js";
import {
  Frontier,
  type HeaderRoot,
  retrieveRows,
} from "../landed-blocks/store.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../mpf/index.js";
import { utxoToLedgerInsertMaterial } from "../mpf/ledger-hydration.js";
import { checkLedgerRoot } from "../mpf/local-ledger-divergence.js";
import { ForeignBlockVerificationError } from "../mpf/verified-block-import.js";
import { NodeConfig } from "../services/config.make-config.js";
import { Database } from "../services/database.js";
import { verifiedLandedLedgerAt } from "./commit-block-header.landed-commit-base.js";
import {
  deserializeStateQueueUTxO,
  type SerializedStateQueueUTxO,
  type VerifiedFrontier,
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

const pending = (headerHash: string) =>
  Effect.fail(
    new ForeignBlockVerificationError({
      foreignHeaderHash: headerHash,
      reason: "missing",
      detail: LANDED_COMMIT_BASE_PENDING,
    }),
  );

/** `confirmed_ledger` and the frontier naming the header it is at. */
type ConfirmedLedger = Readonly<{
  frontier: HeaderRoot | undefined;
  entries: readonly Ledger.Entry[];
}>;

const readConfirmedLedger: Effect.Effect<ConfirmedLedger, unknown, Database> =
  onOneSnapshot(
    Effect.all({
      frontier: Frontier.retrieve,
      entries: ConfirmedLedgerDB.retrieve,
    }),
  );

/**
 * Checks `derivedRoot`, the root of `confirmed_ledger` a commit on the
 * confirmed state `state` would build on, against the root this node's
 * stores give that state. Where the landed frontier read with the ledger
 * (`readConfirmedLedger`) is at `state`, `confirmed_ledger` holds its
 * post-state: entries that do not derive the state's root (at genesis, the
 * frontier's: the genesis state's root is empty whatever ledger the node
 * was configured with) are a local ledger divergence. Where it is not,
 * landed-block processing has not folded `confirmed_ledger` to `state` yet:
 * entries that do not derive the state's root wait for it (genesis aside,
 * which no fold reaches).
 */
const checkConfirmedBase = (
  state: SDK.ConfirmedState,
  frontier: HeaderRoot | undefined,
  derivedRoot: string,
) =>
  Effect.gen(function* () {
    const genesis = state.headerHash === SDK.GENESIS_HEADER_HASH;
    if (frontier?.headerHash === state.headerHash)
      return yield* checkLedgerRoot({
        check: "commit_base_confirmed_ledger",
        headerHash: state.headerHash,
        root: genesis ? frontier.utxosRoot : state.utxoRoot,
        derivedRoot,
      });
    if (!genesis && derivedRoot !== state.utxoRoot)
      return yield* pending(state.headerHash);
  });

/**
 * The ledger a commit builds on. A landed base builds from
 * `verifiedFrontier`, the frontier the main thread verified
 * `confirmed_ledger` at: while the frontier is another, it waits
 * (`CommitBaseUnverifiedError`), and the deltas after it are checked to
 * apply (`verifiedLandedLedgerAt`). `confirmed_ledger` on a confirmed-state
 * tip is checked against the root this node's stores give its header. A
 * divergence either check finds fails with `LocalLedgerDivergenceError`
 * (`local-ledger-divergence.ts`), and the commit is refused, never built
 * on it. The configured genesis ledger, the base at the genesis state while
 * `confirmed_ledger` is empty, has no stored root to check: its root is the
 * one its entries derive.
 */
export const resolveCommitBaseLedgerEntries = ({
  availableConfirmedBlock,
  nativeMpfRoot,
  requireEntries,
  verifiedFrontier,
}: {
  readonly availableConfirmedBlock: "" | SerializedStateQueueUTxO;
  readonly nativeMpfRoot: string;
  readonly requireEntries: boolean;
  readonly verifiedFrontier: VerifiedFrontier | undefined;
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
      return yield* pending(
        inFlight.value[
          PendingBlockFinalizationsDB.Columns.HEADER_HASH
        ].toString("hex"),
      );
    let confirmedState: SDK.ConfirmedState | undefined;
    let confirmed: ConfirmedLedger | undefined;
    if (availableConfirmedBlock !== "") {
      const latestBlock = yield* deserializeStateQueueUTxO(
        availableConfirmedBlock,
      );
      if (latestBlock.datum.key === "Empty") {
        confirmedState = (yield* SDK.getConfirmedStateFromStateQueueDatum(
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
        confirmed = yield* readConfirmedLedger;
        if (
          confirmedState.headerHash === SDK.GENESIS_HEADER_HASH &&
          confirmed.entries.length === 0
        ) {
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
        const landed = yield* onOneSnapshot(
          Effect.flatMap(retrieveRows, landedLedger),
        );
        if (landed === undefined) return yield* pending(headerHash);
        const at = yield* verifiedLandedLedgerAt(
          landed,
          headerHash,
          verifiedFrontier,
        );
        if (
          at === undefined ||
          at.root !== header.utxosRoot ||
          currentLedgerRootHex !== header.utxosRoot ||
          (at.row !== undefined && !at.row.applied)
        )
          return yield* pending(headerHash);
        const entries = ledgerEntries(at.ledger);
        return {
          source: `landed:${headerHash}`,
          entries,
          root: at.root,
        } satisfies ResolvedCommitBaseLedgerEntries;
      }
    }

    confirmed ??= yield* readConfirmedLedger;
    const confirmedEntries = confirmed.entries;
    const confirmedRoot =
      yield* computeLedgerMpfRootFromLedgerEntries(confirmedEntries);
    if (confirmedState !== undefined)
      yield* checkConfirmedBase(
        confirmedState,
        confirmed.frontier,
        confirmedRoot,
      );
    yield* Effect.logInfo(
      `🔹 Commit base ledger snapshot resolved from confirmed_ledger (entries=${confirmedEntries.length.toString()}).`,
    );
    return {
      source: "confirmed_ledger",
      entries: confirmedEntries,
      root: confirmedRoot,
    } satisfies ResolvedCommitBaseLedgerEntries;
  });
