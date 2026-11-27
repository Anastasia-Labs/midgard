/**
 * CBOR encoders for transition-trace integers, phases, event keys, steps, and
 * event-to-step values. They live with the shared block replay
 * (`@al-ft/midgard-fault-proofs`), which the node's foreign-block import and
 * the watcher's ledger both run.
 */
export {
  encodeEventToStepValueCbor,
  encodeTransitionEventKeyCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionPhaseCbor,
  encodeTransitionStepCbor,
} from "@al-ft/midgard-fault-proofs/block-replay";
