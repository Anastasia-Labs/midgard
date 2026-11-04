import { Deferred, Effect, pipe } from "effect";

import { NodeConfig } from "../services/config.js";
import * as Services from "../services/index.js";
import { Globals } from "../services/index.js";
import {
  type IntentJournal,
  IntentJournalWithoutFollower,
} from "../services/intent-journal.js";
import { FollowerLucidAndContracts } from "../services/l1-adapter.follower.js";
import { runNode } from "./listen.run-node.js";
import { withStartupHttpServer } from "./listen.startup-http.js";
import { publicHttpTransport } from "./listen-public-http.js";

/**
 * The node's runtime services, its Lucid service over its follower and its
 * contracts beneath it (`FollowerLucidAndContracts`): a role reads L1 only
 * through its follower and the local node's ledger, so this
 * module never loads the command layer's tool access (`cli-runtime.ts`).
 * `runNode` provides its own intent journal.
 */
const provideListenRuntimeServices = <A, E>(
  effect: Effect.Effect<
    A,
    E,
    | Services.NodeConfig
    | Services.HttpReadSql
    | Services.Database
    | Services.AdmissionWriter
    | Services.AdmissionSql
    | Services.BatchSql
    | Services.WriteBehind
    | Services.ContractDeploymentIdentity
    | Services.MidgardContracts
    | Services.Lucid
    | Services.Globals
    | IntentJournal
  >,
): Effect.Effect<
  A,
  E | Services.ConfigError | Services.DatabaseInitializationError,
  never
> =>
  pipe(
    effect,
    Effect.provide(IntentJournalWithoutFollower),
    Effect.provide(Services.AdmissionWriterLive),
    Effect.provide(Services.WriteBehindLive),
    Effect.provide(Services.HttpReadSqlLive),
    Effect.provide(Services.NodeConfig.layer),
    Effect.provide(Services.Database.layer),
    Effect.provide(FollowerLucidAndContracts),
    Effect.provide(Services.Globals.Default),
  );

/**
 * Bind after local config, before any provider-dependent service acquisition.
 * A transient failure that outlives its bound (`transient-exhaustion.ts`)
 * ends the node, at startup or later, so that it exits non-zero.
 */
export const runListen = (withMonitoring?: boolean) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    const transport = yield* Effect.try(() => publicHttpTransport(config));
    return yield* withStartupHttpServer(
      config.PORT,
      (startup) =>
        provideListenRuntimeServices(
          Effect.flatMap(Globals, (globals) =>
            Effect.raceFirst(
              runNode(startup, withMonitoring),
              Deferred.await(globals.TRANSIENT_EXHAUSTION),
            ),
          ),
        ),
      transport,
    );
  }).pipe(Effect.provide(NodeConfig.layer));
