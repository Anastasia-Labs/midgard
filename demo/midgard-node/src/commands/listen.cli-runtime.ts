import { Deferred, Effect } from "effect";

import { NodeConfig } from "../services/config.js";
import { Globals } from "../services/index.js";
import { provideNodeRuntimeServices } from "./cli-runtime.js";
import { runNode } from "./listen.run-node.js";
import { withStartupHttpServer } from "./listen.startup-http.js";

/**
 * Bind after local config, before any provider-dependent service acquisition.
 * A transient failure that outlives its bound (`transient-exhaustion.ts`)
 * ends the node, at startup or later, so that it exits non-zero.
 */
export const runListen = (withMonitoring?: boolean) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    return yield* withStartupHttpServer(config.PORT, (startup) =>
      provideNodeRuntimeServices(
        Effect.flatMap(Globals, (globals) =>
          Effect.raceFirst(
            runNode(startup, withMonitoring),
            Deferred.await(globals.TRANSIENT_EXHAUSTION),
          ),
        ),
      ),
    );
  }).pipe(Effect.provide(NodeConfig.layer));
