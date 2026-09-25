import type * as Journal from "../../src/database/eventHistoryJournal.js";

/** For fixtures that exercise no pruning: no block is past any horizon. */
export const retainEverything: Journal.Retention = Object.freeze({
  tipHeight: 0,
  horizon: 1,
  holdSlot: undefined,
});
