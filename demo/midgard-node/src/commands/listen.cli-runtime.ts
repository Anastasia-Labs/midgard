import { Effect } from "effect";

import { NodeConfig } from "../services/config.js";
import { provideNodeRuntimeServices } from "./cli-runtime.js";
import { runNode } from "./listen.run-node.js";
import { withStartupHttpServer } from "./listen.startup-http.js";

/** Bind after local config, before any provider-dependent service acquisition. */
export const runListen = (withMonitoring?: boolean) =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    return yield* withStartupHttpServer(config.PORT, (startup) =>
      provideNodeRuntimeServices(runNode(startup, withMonitoring)),
    );
  }).pipe(Effect.provide(NodeConfig.layer));
