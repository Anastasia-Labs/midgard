// In-memory state queue length.
declare var BLOCKS_IN_QUEUE: number;

// Latest moment the in-memory state queue length was synchronized with
// on-chain state.
declare var LATEST_SYNC_OF_STATE_QUEUE_LENGTH: number;

// Needed for development to prevent other actions triggering while spending all
// UTxOs at state queue.
declare var RESET_IN_PROGRESS: boolean;
