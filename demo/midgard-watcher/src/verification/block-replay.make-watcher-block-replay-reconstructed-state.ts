import type { MidgardValidationPhaseName } from "@al-ft/midgard-core/validation-trace";
import { MidgardValidationPhase } from "@al-ft/midgard-core/validation-trace";

import {
  makeWatcherDurablePayload,
  type WatcherReconstructedState,
} from "../storage/durable-store.js";
import {
  failRecord,
  replayedStateBytes,
} from "./block-replay.evaluate-watcher-block-replay.js";
import {
  WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_SCHEMA_VERSION,
  WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODE_JUSTIFICATIONS,
} from "./block-replay.watcher-block-replay-reason-codes.js";
import { HEX_32 } from "./block-replay.watcher-block-replay-rejection-projection.js";
import { type WatcherBlockReplayResult } from "./block-replay.watcher-block-replay-result.js";

/**
 * Builds the W03-reserved `WatcherReconstructedState` record for a completed
 * replay.
 *
 * This is the sibling of `makeWatcherHeaderRootReconstructedStateV1`, not a
 * duplicate of it: W22's record binds the header's claimed roots, this one
 * binds the post state the replay actually produced. Both are needed, because
 * the whole point of W25 is that the two can disagree.
 */
export const makeWatcherBlockReplayReconstructedState = (input: {
  readonly result: WatcherBlockReplayResult;
  readonly chainPointId: string;
  readonly inputIds: readonly string[];
}): WatcherReconstructedState => {
  const result = input.result;
  if (result.schemaVersion !== WATCHER_BLOCK_REPLAY_SCHEMA_VERSION) {
    failRecord("unsupported_schema", "$.result.schemaVersion");
  }
  if (
    result.action !== "accept" ||
    result.headerHash === null ||
    result.priorStateRoot === null ||
    result.postStateRoot === null ||
    !HEX_32.test(result.postStateRoot)
  ) {
    failRecord("result_not_accepted", "$.result.action");
  }
  if (typeof input.chainPointId !== "string" || input.chainPointId === "") {
    failRecord("invalid_input_ids", "$.chainPointId");
  }
  if (input.inputIds.length === 0) {
    failRecord("invalid_input_ids", "$.inputIds");
  }
  const seen = new Set<string>();
  for (const [index, inputId] of input.inputIds.entries()) {
    if (typeof inputId !== "string" || inputId === "" || seen.has(inputId)) {
      failRecord("invalid_input_ids", `$.inputIds[${index.toString()}]`);
    }
    seen.add(inputId);
  }
  return Object.freeze({
    blockHash: result.headerHash as string,
    chainPointId: input.chainPointId,
    priorStateRoot: result.priorStateRoot as string,
    postStateRoot: result.postStateRoot as string,
    inputIds: Object.freeze([...input.inputIds]),
    state: makeWatcherDurablePayload(
      replayedStateBytes(result).toString("hex"),
    ),
  });
};

/**
 * The canonical phase enumeration, re-exported as a frozen list so a suite can
 * assert the stage map covers exactly the Phase-B-reachable phases and no
 * others. Declared here rather than imported at the use site so the assertion
 * and the map cannot drift apart.
 */
export const WATCHER_BLOCK_REPLAY_CANONICAL_PHASES = Object.freeze(
  Object.keys(MidgardValidationPhase) as readonly MidgardValidationPhaseName[],
);

/** Codes this lane claims, published as the CG3 waiver requires. */
export const WATCHER_BLOCK_REPLAY_CLAIMED_REJECT_CODES =
  WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES;

/**
 * The union of the codes W24 and W25 claim plus the codes both disclaim must be
 * the whole canonical vocabulary. Exposed as data so the suite can assert
 * totality rather than restate the arithmetic.
 */
export const WATCHER_BLOCK_REPLAY_PROTOCOL_MINUS_UNCLAIMED = Object.freeze(
  WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES.filter(
    (code) =>
      !(code in WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODE_JUSTIFICATIONS),
  ),
);
