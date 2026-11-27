/**
 * The services one generation of the production lifecycle
 * (`production-lifecycle.ts`) runs under: the node's config, globals, Lucid
 * service, contracts, deployment identity and database, with the mempool
 * ledger cache, write-behind and intent journal over them, as `runNode`
 * composes them; and, when given, the snapshot peers a node started
 * without a ledger asks in place of the configured ones.
 */
import { type Context, Layer } from "effect";

import { NodeConfig } from "../../src/services/config.make-config.js";
import { Database } from "../../src/services/database.js";
import { Globals } from "../../src/services/globals.globals.js";
import { IntentJournalLive } from "../../src/services/intent-journal.js";
import {
  LedgerSnapshotPeers,
  type LedgerSnapshotPeerSource,
} from "../../src/services/ledger-snapshot-start.js";
import { Lucid } from "../../src/services/lucid.js";
import { mempoolLedgerCacheLayer } from "../../src/services/mempool-ledger-cache.make-mempool-ledger-cache.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../../src/services/midgard-contracts.make-midgard-contract-runtime.js";
import { WriteBehindLive } from "../../src/services/write-behind.make-write-behind.js";

export const productionServices = (input: {
  readonly nodeConfig: Context.Tag.Service<typeof NodeConfig>;
  readonly globals: Globals;
  readonly lucid: Parameters<typeof Lucid.make>[0];
  readonly contracts: Parameters<typeof MidgardContracts.make>[0];
  readonly identity: Parameters<typeof ContractDeploymentIdentity.make>[0];
  readonly ledgerSnapshotPeers?: LedgerSnapshotPeerSource;
}) =>
  Layer.mergeAll(
    mempoolLedgerCacheLayer,
    WriteBehindLive,
    IntentJournalLive,
  ).pipe(
    Layer.provideMerge(
      Layer.mergeAll(
        Layer.succeed(NodeConfig, input.nodeConfig),
        Layer.succeed(Globals, input.globals),
        Layer.succeed(Lucid, Lucid.make(input.lucid)),
        Layer.succeed(MidgardContracts, MidgardContracts.make(input.contracts)),
        Layer.succeed(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make(input.identity),
        ),
        Database.layer,
        input.ledgerSnapshotPeers === undefined
          ? Layer.empty
          : Layer.succeed(LedgerSnapshotPeers, input.ledgerSnapshotPeers),
      ),
    ),
  );
