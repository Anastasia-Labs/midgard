import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import * as Adoptions from "../database/foreignNativeAdoptions.js";
import {
  revalidateForeignCommitBase,
  type VerifiedForeignCommitBase,
} from "../workers/commit-block-header.verify-foreign-base.js";
import { withHistoryWrite } from "./event-history-producer.js";
import type { NativeMpfOwnerService } from "./mpf-native-owner/protocol.js";

/** Request adoption without disturbing live admission or its cache. The source
 * owner drains producers and performs the durable/native operation in recovery.
 */
export const requestForeignNativeAdoption = ({
  base,
  owner,
}: {
  readonly base: VerifiedForeignCommitBase;
  readonly owner: Pick<NativeMpfOwnerService, "diagnostics">;
}) =>
  Effect.gen(function* () {
    yield* revalidateForeignCommitBase(base);
    if (base.authority !== "ready")
      return yield* Effect.fail(
        new Error("Foreign adoption requests require a Ready producer"),
      );
    const state = yield* Effect.tryPromise({
      try: () => owner.diagnostics(),
      catch: (cause) => cause,
    });
    const sql = yield* SqlClient.SqlClient;
    const stamped = yield* sql<{
      root_hex: string;
    }>`SELECT root_hex FROM mpf_engine_state WHERE store_name = 'ledger'`;
    if (
      state.durableRoot === base.root &&
      stamped[0]?.root_hex === base.root &&
      (yield* Adoptions.hasAppliedForeignBase(base))
    )
      return false;
    yield* withHistoryWrite(Adoptions.request(base));
    // The request is durable, but never grants authority to a changed source.
    yield* revalidateForeignCommitBase(base);
    return true;
  });
