import { Effect } from "effect";

import type { NativeMpfOwnerService } from "../services/mpf-native-owner/protocol.js";
import type { WorkerInput } from "../workers/utils/commit-block-header.js";
import { WorkerError } from "../workers/utils/common.js";

/**
 * The native owner handoff for one commit worker. The port is created only
 * once diagnostics succeeds: a port that never reaches a worker stays
 * registered with the owner for good.
 */
export const nativeMpfWorkerInput = (
  owner: Pick<NativeMpfOwnerService, "diagnostics" | "createWorkerPort">,
  worker: string,
  ownerBinarySha256: string,
): Effect.Effect<NonNullable<WorkerInput["nativeMpf"]>, WorkerError> =>
  Effect.gen(function* () {
    const { durableRoot } = yield* Effect.tryPromise({
      try: () => owner.diagnostics(),
      catch: (cause) =>
        new WorkerError({
          worker,
          message: "Architecture G native owner diagnostics failed",
          cause,
        }),
    });
    return {
      port: owner.createWorkerPort(),
      durableRoot,
      ownerBinarySha256,
    };
  });
