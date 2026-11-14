/**
 * The startup rebuild of the first-seen input claims
 * (`admission-input-claims.ts`) from the durable admission backlog.
 */
import {
  decodeMidgardSubmittedNativeTxFromCanonicalCbor,
  midgardOutRefToCborHex,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";

import * as TxAdmissionsDB from "../database/txAdmissions.js";
import { DatabaseError } from "../database/utils/common.js";
import { isConnectionClassError } from "../provider-retry.js";
import {
  type InputClaimRequest,
  UNKNOWN_SPENDABLE_VIEW,
} from "./admission-input-claims.js";
import { Database } from "./database.js";
import { Globals } from "./globals.js";
import {
  ADMISSION_BACKLOG_UNREAD,
  retryStartupStep,
  STARTUP_DATABASE_BUDGET,
  type StartupStepBudget,
  type StartupStepFailedError,
} from "./startup-waiting.js";

/** Rows read per page while rebuilding the claims at startup. */
const REBUILD_PAGE_ROWS = 1_000;

/**
 * Rebuilds the claims of every queued or validating admission row, in
 * arrival order so the first-seen tx claims first. Runs at startup, after
 * the database is initialized and before the node serves or validates.
 * Idempotent: a retried run finds the txs it already registered held. A
 * stored row that no longer decodes takes no claims; validation rejects it.
 */
export const rebuildAdmissionInputClaims: Effect.Effect<
  number,
  DatabaseError,
  Database | Globals
> = Effect.gen(function* () {
  const claims = (yield* Globals).ADMISSION_INPUT_CLAIMS;
  let afterArrivalSeq = -1n;
  let registered = 0;
  for (;;) {
    const rows = yield* TxAdmissionsDB.retrieveBacklogPayloads({
      afterArrivalSeq,
      limit: REBUILD_PAGE_ROWS,
    });
    for (const row of rows) {
      const outRefs = backlogTxOutRefs(row.tx_canonical_cbor);
      if (outRefs === undefined) {
        yield* Effect.logWarning(
          `Admission ${row.tx_id.toString("hex")} no longer decodes; it holds no input claims`,
        );
        continue;
      }
      const outcome = claims.take({
        txIdHex: row.tx_id.toString("hex"),
        ...outRefs,
        check: false,
        spendable: UNKNOWN_SPENDABLE_VIEW,
      });
      if (outcome._tag === "Claimed") registered += 1;
    }
    if (rows.length < REBUILD_PAGE_ROWS) return registered;
    afterArrivalSeq = rows[rows.length - 1]!.arrival_seq;
  }
});

/**
 * The startup step: retries a connection-class failure within the startup
 * database budget, like the backlog gauge's first read.
 */
export const rebuildAdmissionInputClaimsOnStartup = (
  budget: StartupStepBudget = { maxElapsed: STARTUP_DATABASE_BUDGET },
): Effect.Effect<void, StartupStepFailedError, Database | Globals> =>
  retryStartupStep(
    rebuildAdmissionInputClaims.pipe(
      Effect.tap((registered) =>
        Effect.logInfo(
          `Rebuilt the input claims of ${registered.toString()} backlog admissions`,
        ),
      ),
    ),
    {
      key: "admission_input_claims_rebuild",
      reason: ADMISSION_BACKLOG_UNREAD,
      retryable: isConnectionClassError,
      budget,
    },
  );

const backlogTxOutRefs = (
  txCanonicalCbor: Buffer,
): Pick<InputClaimRequest, "spentOutRefs" | "producedOutRefs"> | undefined => {
  try {
    const { ledgerTx } =
      decodeMidgardSubmittedNativeTxFromCanonicalCbor(
        txCanonicalCbor,
      ).submittedTx;
    return {
      spentOutRefs: ledgerTx.spendInputs.map(midgardOutRefToCborHex),
      producedOutRefs: ledgerTx.outputs.map((_, index) =>
        midgardOutRefToCborHex({ txId: ledgerTx.txId, index: BigInt(index) }),
      ),
    };
  } catch {
    return undefined;
  }
};
