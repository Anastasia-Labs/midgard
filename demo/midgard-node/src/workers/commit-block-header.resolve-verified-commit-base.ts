import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import { ForeignBlockVerificationError } from "../mpf/verified-block-import.js";
import { VerifiedForeignBase } from "./commit-block-header.verify-foreign-base.js";

/** Every foreign resolver path requires the exact fully replayed preflight.
 * Its source/topology revalidation runs again immediately before root reuse. */
export const resolveVerifiedCommitBase = (latestBlock: SDK.StateQueueUTxO) =>
  Effect.gen(function* () {
    const header = yield* SDK.getHeaderFromStateQueueDatum(latestBlock.datum);
    const hash = yield* SDK.hashBlockHeader(header);
    const verified = yield* Effect.serviceOption(VerifiedForeignBase);
    if (
      Option.isNone(verified) ||
      verified.value.base.headerHash !== hash ||
      verified.value.base.root !== header.utxosRoot
    )
      return yield* Effect.fail(
        new ForeignBlockVerificationError({
          foreignHeaderHash: hash,
          reason: "missing",
          detail:
            "Exact foreign commit base has not passed complete canonical verification",
        }),
      );
    yield* verified.value.assertCurrent;
    return verified.value.base;
  });
