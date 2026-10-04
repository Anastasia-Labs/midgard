import { Effect } from "effect";

import {
  DaPayloadsDB,
  ForeignTipReconciliationsDB,
} from "../database/index.js";
import { runHistoryProducer } from "../services/event-history-producer.js";
import { reconcileRetainedForeignTipEntry } from "../workers/t2-foreign-event-reconciliation.reconcile-retained-foreign-tip-entry.js";

/** Bytes are durable before attempting the owner-fenced event projection. */
export const consumeDownloadedForeignDa = (
  entry: ForeignTipReconciliationsDB.Entry,
  row: DaPayloadsDB.InsertInput,
) =>
  DaPayloadsDB.upsertAvailable(row).pipe(
    Effect.zipRight(
      reconcileRetainedForeignTipEntry(entry).pipe(runHistoryProducer),
    ),
  );
