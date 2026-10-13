import { canonicalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { Effect } from "effect";

import * as DepositsDB from "../database/deposits.js";
import * as Ledger from "../database/utils/ledger.js";
import { transitionEffectToLedgerOps } from "./ledger-delta.js";
import type { TransitionTraceSourceEvent } from "./trace-events.js";
import { depositTraceEventKey } from "./trace-events.js";

/** The deposit phase's transition-trace source events, in entry order. */
export const buildDepositSourceEvents = (
  entries: readonly DepositsDB.Entry[],
) =>
  Effect.forEach(entries, (entry) =>
    Effect.gen(function* () {
      const ledgerEntry = yield* DepositsDB.toLedgerEntry(entry);
      const effect = canonicalDepositTransitionEffect({
        outRefCbor: ledgerEntry[Ledger.Columns.OUTREF],
        outputCbor: ledgerEntry[Ledger.Columns.OUTPUT],
      });
      return {
        eventKey: yield* depositTraceEventKey(entry),
        phase: "Deposit" as const,
        ledgerOps: transitionEffectToLedgerOps(effect),
      } satisfies TransitionTraceSourceEvent;
    }),
  );
