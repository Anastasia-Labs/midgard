import { Effect } from "effect";

import { makeEventHistoryOwner } from "./event-history-owner.make-event-history-owner.js";

export type EventHistoryOwner = Effect.Effect.Success<
  ReturnType<typeof makeEventHistoryOwner>
>;
