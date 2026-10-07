import "./listen-router.get-pipeline-status-handler.js";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { toHex } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";

import {
  AddressHistoryDB,
  BlocksDB,
  MempoolDB,
  MutationJobsDB,
  TxAdmissionsDB,
} from "../database/index.js";
import { mergeAction } from "../fibers/index.js";
import { l1NowUnixTimeMs } from "../l1-heads.js";
import {
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import {
  classifyOldestQueuedBlockReadiness,
  DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
  mergeMaturityWindow,
  planMergePreflight,
} from "../transactions/state-queue/merge-readiness.js";
import { parseAddressArgument } from "./command-utils.js";
import { failWith500, handleStateQueueGetFailure } from "./listen-response.js";
import {
  ADDRESS_HISTORY_ENDPOINT,
  MERGE_ENDPOINT,
} from "./listen-router.l1-provider-readiness-evidence-is-fresh.js";

/**
 * `GET /merge`: triggers manual merge of the oldest queued block into
 * confirmed state.
 */
export const getMergeHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`GET /${MERGE_ENDPOINT} - Manual merge order received`);
  const attempt = yield* mergeAction(true).pipe(
    Effect.map((result) => ({ _tag: "Ran" as const, result })),
    Effect.catchTag("MergeProducerPermitUnavailable", (unavailable) =>
      Effect.succeed({ _tag: "PermitUnavailable" as const, unavailable }),
    ),
  );
  if (attempt._tag === "PermitUnavailable") {
    // Nothing ran: the history owner is absent or not Ready. Retry later.
    const cause = formatUnknownError(attempt.unavailable.cause, {
      includeCause: true,
    });
    yield* Effect.logWarning(
      `GET /${MERGE_ENDPOINT} - ${attempt.unavailable.message}: ${cause}`,
    );
    return yield* HttpServerResponse.json(
      { error: attempt.unavailable.message, cause },
      { status: 503 },
    );
  }
  const { result } = attempt;
  yield* Effect.logInfo(
    `GET /${MERGE_ENDPOINT} - Merge result: ${JSON.stringify(result)}`,
  );
  return yield* HttpServerResponse.json({
    message: "Merge request processed",
    result,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      MERGE_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
  Effect.catchTag("TxSubmitError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, `${e._tag}: ${e.message}`),
  ),
  Effect.catchTag("TxConfirmError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, `${e._tag}: ${e.message}`),
  ),
  Effect.catchTag("TxSignError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, `${e._tag}: ${e.message}`),
  ),
  Effect.catchTag("CmlDeserializationError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("DataCoercionError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("LinkedListError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("HashingError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("LucidError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("StateQueueError", (e) =>
    handleStateQueueGetFailure(MERGE_ENDPOINT, e),
  ),
);

/**
 * `GET /txs`: returns the address-history tx payloads for an address.
 */
export const getTxsOfAddressHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const addr = params["address"];

  if (typeof addr !== "string") {
    yield* Effect.logInfo(
      `GET /${ADDRESS_HISTORY_ENDPOINT} - Invalid address type: ${String(addr)}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid address type: ${String(addr)}` },
      { status: 400 },
    );
  }
  try {
    const address = parseAddressArgument(addr);
    const cbors = yield* AddressHistoryDB.retrieve(address);
    yield* Effect.logInfo(`Found ${cbors.length} CBORs with ${addr}`);
    return yield* HttpServerResponse.json({
      txs: cbors.map(SDK.bufferToHex),
    });
  } catch (_error) {
    yield* Effect.logInfo(`Invalid address: ${addr}`);
    return yield* HttpServerResponse.json(
      { error: `Invalid address: ${addr}` },
      { status: 400 },
    );
  }
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "txs", e)),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      ADDRESS_HISTORY_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /stateQueue`: logs and returns the current ordered state-queue headers.
 */
export const getStateQueueHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Drawing state queue UTxOs...`);
  const globals = yield* Globals;
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const nodeConfig = yield* NodeConfig;
  const fetchConfig: SDK.StateQueueFetchConfig = {
    stateQueuePolicyId: contracts.stateQueue.policyId,
    stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
  };
  const sortedUTxOs = yield* SDK.fetchSortedStateQueueUTxOsProgram(
    lucid.api,
    fetchConfig,
  );
  const headers = sortedUTxOs.flatMap((u) =>
    u.datum.key === "Empty" ? [] : [u.datum.key.Key.key],
  );
  const [
    unconfirmedSubmittedBlockTxHash,
    localFinalizationPending,
    resetInProgress,
    durableAdmissionBacklog,
    mempoolTxCount,
    unfinishedMutationJobs,
  ] = yield* Effect.all(
    [
      Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
      Ref.get(globals.LOCAL_FINALIZATION_PENDING),
      Ref.get(globals.RESET_IN_PROGRESS),
      TxAdmissionsDB.countBacklog,
      MempoolDB.retrieveTxCount,
      MutationJobsDB.countUnfinished,
    ],
    { concurrency: "unbounded" },
  );
  const mergeReadiness = planMergePreflight({
    force: false,
    queueLength: headers.length,
    minQueueLength:
      nodeConfig.MIN_QUEUE_LENGTH_FOR_MERGING ??
      DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
    unresolvedSubmittedBlockTxHash: unconfirmedSubmittedBlockTxHash,
    localFinalizationPending,
    resetInProgress,
    durableAdmissionBacklog,
    mempoolTxCount,
    unfinishedMutationJobs,
  });
  const oldestQueuedBlock = sortedUTxOs.find((u) => u.datum.key !== "Empty");
  const oldestBlockReadiness =
    oldestQueuedBlock === undefined
      ? null
      : yield* Effect.gen(function* () {
          const blockHeader = yield* SDK.getHeaderFromStateQueueDatum(
            oldestQueuedBlock.datum,
          );
          const stateQueueNode =
            yield* SDK.getStateQueueNodeFromStateQueueDatum(
              oldestQueuedBlock.datum,
            );
          const headerHash = yield* SDK.hashBlockHeader(blockHeader);
          const maturity = mergeMaturityWindow(
            lucid.api,
            Number(blockHeader.endTime),
          );
          return classifyOldestQueuedBlockReadiness({
            headerHash,
            currentDaAvailability: stateQueueNode.da_attestation,
            provenFraud: stateQueueNode.proven_fraud,
            readyAfterUnixTime: maturity.readyAfterUnixTime,
            // Maturity at the L1 `slotNow`, as the merge fiber judges it.
            nowUnixTime: yield* l1NowUnixTimeMs(lucid.api),
          });
        });
  let drawn = `
---------------------------- STATE QUEUE ----------------------------`;
  yield* Effect.allSuccesses(
    sortedUTxOs.map((u) =>
      Effect.gen(function* () {
        let info = "";
        const isHead = u.datum.key === "Empty";
        const isEnd = u.datum.next === "Empty";
        const emoji = isHead ? "🚢" : isEnd ? "⚓" : "⛓ ";
        if (u.datum.key !== "Empty") {
          const icon = isEnd ? "  " : emoji;
          info = `
${icon} ╰─ header: ${u.datum.key.Key.key}`;
        }
        drawn = `${drawn}
${emoji} ${u.utxo.txHash}#${u.utxo.outputIndex}${info}`;
      }),
    ),
  );
  drawn += `
---------------------------------------------------------------------
`;
  yield* Effect.logInfo(drawn);
  return yield* HttpServerResponse.json({
    headers,
    mergeReadiness: {
      ...mergeReadiness,
      durableAdmissionBacklog: durableAdmissionBacklog.toString(),
      mempoolTxCount: mempoolTxCount.toString(),
      unfinishedMutationJobs: unfinishedMutationJobs.toString(),
      oldestBlock: oldestBlockReadiness,
    },
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", "logStateQueue", e),
  ),
  Effect.catchTag("LinkedListError", (e) =>
    failWith500("GET", "logStateQueue", e.cause, e.message),
  ),
  Effect.catchTag("DataCoercionError", (e) =>
    failWith500("GET", "logStateQueue", e.cause, e.message),
  ),
  Effect.catchTag("HashingError", (e) =>
    failWith500("GET", "logStateQueue", e.cause, e.message),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      "logStateQueue",
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
  Effect.catchTag("LucidError", (e) =>
    failWith500("GET", "logStateQueue", e.cause, e.message),
  ),
);

/**
 * `GET /logBlocksDB`: logs a compact summary of block-link rows.
 */
export const getLogBlocksDBHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Querying BlocksDB...`);
  const allBlocksData = yield* BlocksDB.retrieve;
  const keyValues: Record<string, number> = allBlocksData.reduce(
    (acc: Record<string, number>, entry) => {
      const bHex = toHex(entry.header_hash);
      if (!acc[bHex]) {
        acc[bHex] = 1;
      } else {
        acc[bHex] += 1;
      }
      return acc;
    },
    {} as Record<string, number>,
  );
  let drawn = `
------------------------------ BLOCKS DB ----------------------------`;
  for (const bHex in keyValues) {
    drawn = `${drawn}
${bHex} -──▶ ${keyValues[bHex]} tx(s)`;
  }
  drawn += `
---------------------------------------------------------------------
`;
  yield* Effect.logInfo(drawn);
  return yield* HttpServerResponse.json({
    message: `BlocksDB drawn in server logs!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "logBlocksDB", e)),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      "logBlocksDB",
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /logGlobals`: logs the current process-global coordination state.
 */
export const OPERATOR_STATUS_ENDPOINT = "operator/status";
