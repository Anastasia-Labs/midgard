import type {
  Ledger,
  LedgerSource,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKStateQueue from "@al-ft/midgard-sdk/state-queue";
import { Effect, Option, Runtime } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import {
  generatedLandedChain,
  onOneSnapshot,
} from "../landed-blocks/ledger.js";
import { readLedgerOutputs } from "../landed-blocks/ledger-outputs.js";
import { retrieveRows } from "../landed-blocks/store.js";
import { ForeignBlockVerificationError } from "../mpf/verified-block-import.js";
import { Database } from "../services/database.js";
import { verifiedLandedRootAt } from "./commit-block-header.landed-commit-base.js";
import {
  deserializeStateQueueUTxO,
  type SerializedStateQueueUTxO,
  type VerifiedFrontier,
} from "./utils/commit-block-header.js";

/** The ledger a block commit builds on, its root, and where it came from. */
export type ResolvedCommitBase = {
  readonly source: string;
  readonly root: string;
  readonly ledger: Ledger;
};

/** The ledger with `root` over `outputs`, its nodes from the ledger store. */
export type OpenCommitBaseLedger = (
  root: string,
  outputs: LedgerSource["outputs"],
) => Ledger;

/** Why a commit waits on a tail landed-block processing has not applied to
 * the working ledger yet, on a confirmed state the native root is not at
 * yet, or on this node's own block in flight. */
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

/**
 * The ledger a commit builds on: the ledger store's at the native owner's
 * durable root `nativeMpfRoot`, once that root is the one the state queue
 * tip names. A confirmed-state tip names the state's root (at genesis, the
 * deployment's genesis root, which startup admitted the native store at).
 * A tail, foreign or this node's own, is built on only once landed-block
 * processing took it in at its header's root and the rebase applied it,
 * while the landed frontier is `verifiedFrontier`, the frontier the main
 * thread verified `confirmed_ledger` at (else `CommitBaseUnverifiedError`).
 * With no confirmed block the native root is the base. Until then the
 * commit waits; it reads no whole ledger.
 */
export const resolveCommitBase = ({
  availableConfirmedBlock,
  nativeMpfRoot,
  openLedger,
  verifiedFrontier,
}: {
  readonly availableConfirmedBlock: "" | SerializedStateQueueUTxO;
  readonly nativeMpfRoot: string;
  readonly openLedger: OpenCommitBaseLedger;
  readonly verifiedFrontier: VerifiedFrontier | undefined;
}): Effect.Effect<ResolvedCommitBase, unknown, Database> =>
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
    const runtime = yield* Effect.runtime<Database>();
    const base = (source: string): ResolvedCommitBase => ({
      source,
      root: nativeMpfRoot,
      ledger: openLedger(nativeMpfRoot, (outRefs) =>
        Runtime.runPromise(runtime)(readLedgerOutputs(outRefs)),
      ),
    });
    if (availableConfirmedBlock === "") return base("native");
    const latestBlock = yield* deserializeStateQueueUTxO(
      availableConfirmedBlock,
    );
    if (latestBlock.datum.key === "Empty") {
      const state = (yield* SDKStateQueue.getConfirmedStateFromStateQueueDatum(
        latestBlock.datum,
      )).data;
      if (nativeMpfRoot !== state.utxoRoot)
        return yield* pending(state.headerHash);
      return base(`confirmed:${state.headerHash}`);
    }
    const header = yield* SDK.getHeaderFromStateQueueDatum(latestBlock.datum);
    const headerHash = yield* SDK.hashBlockHeader(header);
    const landed = yield* onOneSnapshot(
      Effect.flatMap(retrieveRows, generatedLandedChain),
    );
    if (landed === undefined) return yield* pending(headerHash);
    const at = yield* verifiedLandedRootAt(
      landed,
      headerHash,
      verifiedFrontier,
    );
    if (
      at === undefined ||
      at.root !== header.utxosRoot ||
      nativeMpfRoot !== header.utxosRoot ||
      (at.row !== undefined && !at.row.applied)
    )
      return yield* pending(headerHash);
    return base(`landed:${headerHash}`);
  });
