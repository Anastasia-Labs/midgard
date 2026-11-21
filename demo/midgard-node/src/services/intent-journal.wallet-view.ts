/**
 * The node's wallet view (plan §8.5): what one of its own addresses may
 * spend now. Every node builder selects its wallet inputs from it
 * (`presetWalletInputs`), and the submit seam signs over it, so a build can
 * spend the predicted change of the node's own live intents, and nothing a
 * live intent still holds is offered to another build.
 *
 * - Under a follower, it is the follower's `walletView` over the facts at
 *   the store's head and the live intents of the journal, read on one
 *   snapshot of the node database. It is read afresh at every call, so it
 *   follows the head: a dead intent's inputs are offered again, a rolled
 *   back own transaction is predicted again.
 * - A phase or process with no follower (`IntentJournalWithoutFollower`)
 *   has no facts and journals nothing: its view is the provider's UTxOs at
 *   the address, read afresh at every call (never pinned).
 */
import {
  postgresDialect,
  readWalletViewIn,
  type WalletViewRead,
} from "@al-ft/midgard-l1-follower";
import { toLucidUtxo } from "@al-ft/midgard-l1-follower/provider";
import { SqlClient } from "@effect/sql";
import {
  getAddressDetails,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Data, Effect } from "effect";

import { inFollowerSnapshot } from "../database/follower-schema.js";
import { causeText } from "./intent-journal.refusals.js";

export type NodeWalletView = Readonly<{
  address: string;
  /** `follower`: facts and live intents; `provider`: no follower runs here. */
  source: "follower" | "provider";
  /** The spendable outputs at `address`: facts first, then predicted change. */
  utxos: readonly UTxO[];
  /**
   * The predicted outputs (`txHash#index`) and the live intent each comes
   * from (its tx id). Empty from the provider.
   */
  predictedBy: ReadonlyMap<string, string>;
  /**
   * The inputs and collaterals of the live own intents (`txHash#index`, any
   * address): spent by a transaction of the node that has not landed, so
   * no other build may spend them. Empty from the provider.
   */
  held: ReadonlySet<string>;
}>;

/** The wallet view could not be read; nothing is built on a guess. */
export class WalletViewUnavailable extends Data.TaggedError(
  "WalletViewUnavailable",
)<{
  readonly address: string;
  readonly message: string;
  readonly cause: unknown;
}> {}

const label = (outRef: Readonly<{ txHash: Buffer; index: number }>): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index}`;

const unavailable = (address: string, cause: unknown) =>
  new WalletViewUnavailable({
    address,
    message: `the wallet view of ${address} is unavailable: ${causeText(cause)}`,
    cause,
  });

/** A follower read as the node's view of `address`. */
export const nodeWalletViewOf = (
  address: string,
  read: WalletViewRead,
): NodeWalletView => ({
  address,
  source: "follower",
  utxos: read.view.available.map((entry) =>
    toLucidUtxo(entry.outRef, entry.output),
  ),
  predictedBy: new Map(
    read.view.available.flatMap((entry) =>
      entry.predictedBy === null
        ? []
        : [[label(entry.outRef), entry.predictedBy.toString("hex")] as const],
    ),
  ),
  held: new Set(read.view.held.map(label)),
});

/** The follower's view of `address` on one snapshot of the node database. */
export const readFollowerWalletView = (
  address: string,
): Effect.Effect<NodeWalletView, WalletViewUnavailable, SqlClient.SqlClient> =>
  Effect.try({
    try: () => Buffer.from(getAddressDetails(address).address.hex, "hex"),
    catch: (cause) => unavailable(address, cause),
  }).pipe(
    Effect.flatMap((bytes) =>
      inFollowerSnapshot((tx) =>
        readWalletViewIn(tx, postgresDialect, bytes),
      ).pipe(Effect.mapError((cause) => unavailable(address, cause))),
    ),
    Effect.map((read) => nodeWalletViewOf(address, read)),
  );

/** The provider's UTxOs at `address`, read now: the view where no follower runs. */
export const readProviderWalletView = (
  lucid: LucidEvolution,
  address: string,
): Effect.Effect<NodeWalletView, WalletViewUnavailable> =>
  Effect.tryPromise({
    try: () => lucid.utxosAt(address),
    catch: (cause) => unavailable(address, cause),
  }).pipe(
    Effect.map((utxos) => ({
      address,
      source: "provider" as const,
      utxos,
      predictedBy: new Map<string, string>(),
      held: new Set<string>(),
    })),
  );
