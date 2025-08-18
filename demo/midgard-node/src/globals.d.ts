import { TxHash } from "@lucid-evolution/lucid";
import { SerializedStateQueueUTxO } from "./workers/utils/commit-block-header.ts";

export {};

declare global {
  // In-memory state queue length.
  var BLOCKS_IN_QUEUE: number;

  // Latest moment the in-memory state queue length was synchronized with
  // on-chain state.
  var LATEST_SYNC_OF_STATE_QUEUE_LENGTH: number;

  // Needed for development to prevent other actions triggering while spending all
  // UTxOs at state queue.
  var RESET_IN_PROGRESS: boolean;

  // The state queue UTxO confirmed by the confirmation worker, unused for block
  // commitment.
  var AVAILABLE_CONFIRMED_BLOCK: "" | SerializedStateQueueUTxO;

  // Accumulator for the number of processed mempool transactions (only used in
  // metrics)
  var PROCESSED_UNSUBMITTED_TXS_COUNT: number;

  // Accumulator for the total size of L2 transactions submitted in a state
  // queue block.
  var PROCESSED_UNSUBMITTED_TXS_SIZE: number;

  var UNCONFIRMED_SUBMITTED_BLOCK: "" | TxHash;
}
