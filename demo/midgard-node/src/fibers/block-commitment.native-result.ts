import { Effect } from "effect";

import type { NativeMpfOwnerService } from "../services/mpf-native-owner/protocol.js";
import type { WorkerOutput } from "../workers/utils/commit-block-header.js";
import { WorkerError } from "../workers/utils/common.js";
import { promoteOrRecoverNativeMpf } from "./block-commitment.promote-or-recover-native-mpf.js";

export const promoteCommitWorkerNativeResult = (
  owner: NativeMpfOwnerService | undefined,
  output: WorkerOutput,
) =>
  Effect.gen(function* () {
    const nativeMpfPromotion =
      "nativeMpfPromotion" in output ? output.nativeMpfPromotion : undefined;
    if (nativeMpfPromotion !== undefined) {
      if (owner === undefined) {
        return yield* Effect.fail(
          new WorkerError({
            worker: "commit-block-header",
            message: "Native MPF promotion returned without an owner",
            cause: nativeMpfPromotion.handle.baseRoot,
          }),
        );
      }
      yield* promoteOrRecoverNativeMpf({
        owner: owner,
        handle: nativeMpfPromotion.handle,
      }).pipe(
        Effect.mapError(
          (cause) =>
            new WorkerError({
              worker: "commit-block-header",
              message: "Architecture G post-submit promotion failed",
              cause,
            }),
        ),
      );
    }
  });
