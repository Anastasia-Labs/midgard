/** The watcher projection's table names (ticket W1). */
export const WATCHER_QUEUE_OUTPUTS_TABLE = "watcher_queue_outputs";
export const WATCHER_QUEUE_UNIT_HISTORY_TABLE = "watcher_queue_unit_history";
export const WATCHER_QUEUE_CHECKPOINTS_TABLE = "watcher_queue_checkpoints";
export const WATCHER_DA_ATTESTATIONS_TABLE = "watcher_da_attestations";
export const WATCHER_PROTOCOL_INIT_FAULTS_TABLE =
  "watcher_protocol_init_faults";
export const WATCHER_DEPARTED_HEADERS_TABLE = "watcher_departed_headers";
export const WATCHER_UNIT_CARRIERS_TABLE = "watcher_unit_carriers";
export const WATCHER_UNIT_HISTORY_TABLE = "watcher_unit_history";
/** Owner-managed (class B): the proof objectives whose L1 history is held. */
export const WATCHER_PROOF_PINS_TABLE = "watcher_proof_pins";
/** Owner-managed (class B): the followed units a held objective's proof reads. */
export const WATCHER_PROOF_PIN_UNITS_TABLE = "watcher_proof_pin_units";
/** Owner-managed (class B): the deposit and withdrawal events a held objective's proof reads. */
export const WATCHER_PROOF_PIN_EVENTS_TABLE = "watcher_proof_pin_events";
/** Class C: the resolved inputs of every tx a unit history records. */
export const WATCHER_TX_INPUTS_TABLE = "watcher_tx_inputs";
/** Class B: the followed units a prune step deleted closed history rows of. */
export const WATCHER_PRUNED_UNITS_TABLE = "watcher_pruned_units";
/** Class B: the headers a prune step deleted closed queue history rows of. */
export const WATCHER_PRUNED_HEADERS_TABLE = "watcher_pruned_headers";
