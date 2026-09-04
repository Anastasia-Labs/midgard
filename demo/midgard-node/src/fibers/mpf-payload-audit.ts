import { Effect, Schedule } from "effect";

import { runMpfAudit } from "../commands/mpf-audit.js";
import { MpfEngineStateDB } from "../database/index.js";
import {
  Database,
  Globals,
  NodeConfig,
  type NodeConfigDep,
} from "../services/index.js";

export const shouldRunMpfPayloadAudit = (
  payloadRootCheck: NodeConfigDep["MPF_PAYLOAD_ROOT_CHECK"],
): boolean => payloadRootCheck !== "off";

export const mpfPayloadAuditFiber: Effect.Effect<
  number,
  never,
  Database | Globals | NodeConfig
> = Effect.gen(function* () {
  const config = yield* NodeConfig;
  if (!shouldRunMpfPayloadAudit(config.MPF_PAYLOAD_ROOT_CHECK)) return 0;
  const globals = yield* Globals;
  const runWhenDue = MpfEngineStateDB.ledgerAuditIsDue({
    intervalBlocks: config.MPF_PAYLOAD_AUDIT_INTERVAL_BLOCKS,
    intervalMs: config.MPF_PAYLOAD_AUDIT_INTERVAL_MS,
  }).pipe(
    Effect.flatMap((due) =>
      due
        ? Effect.gen(function* () {
            const owner = yield* globals.NATIVE_MPF_OWNER;
            const persistedRootOverride =
              owner === undefined
                ? undefined
                : (yield* Effect.promise(() => owner.diagnostics()))
                    .durableRoot;
            return yield* runMpfAudit({ persistedRootOverride });
          })
        : Effect.void,
    ),
    Effect.catchAllCause((cause) =>
      Effect.logError(`MPF payload audit failed: ${String(cause)}`),
    ),
  );
  return yield* runWhenDue.pipe(
    Effect.repeat(
      Schedule.spaced(
        `${Math.min(
          config.MPF_PAYLOAD_AUDIT_INTERVAL_MS,
          config.WAIT_BETWEEN_BLOCK_COMMITMENT,
        )} millis`,
      ),
    ),
  );
});
