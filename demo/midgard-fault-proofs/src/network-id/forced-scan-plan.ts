/**
 * Pure planning half of the §10 resumable forced outputs scan.
 *
 * The forced (wrongful-rejection) direction has to prove that **no** output of
 * the rejected forced transaction names a foreign network. `forced_scan` walks
 * the outputs field (§2.5 field 2) in batches, carrying a 32-byte checkpoint
 * hash in thread state and re-supplying the checkpoint bytes through the
 * redeemer. This module re-derives those bytes off chain exactly as the chain
 * does, so a builder never has to guess what the validator committed.
 *
 * The checkpoint encodings themselves are the repository's single
 * implementation — `src/staged-field-walk/` — which pins
 * §2.5 field 6. §4 removed field-index domain separation, so the index travels
 * inside the checkpoint and is the only byte that differs here; it is patched
 * exactly as `mint-declared-asset-limit` and `observer-order-invalid` do,
 * rather than forking the encoder.
 */

import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "../staged-field-walk/index.js";
import "./submit-common.js";
import "./forced-scan-plan.advance-grammar-by.js";
import "./forced-scan-plan.plan-network-id-forced-scan.js";
export {
  encodeNetworkIdForcedScanGrammarCheckpoint,
  encodeNetworkIdForcedScanWalkCheckpoint,
  hashNetworkIdForcedScanGrammarCheckpoint,
  hashNetworkIdForcedScanWalkCheckpoint,
  NETWORK_ID_FORCED_CERTIFIED_SCAN_DRIVER_BATCH,
  NETWORK_ID_FORCED_GRAMMAR_DRIVER_BATCH,
  NETWORK_ID_OUTPUTS_FIELD_INDEX,
  type NetworkIdForcedScanGrammarCheckpoint,
  type NetworkIdForcedScanPlan,
  type NetworkIdForcedScanStep,
  type NetworkIdForcedScanWalkCheckpoint,
} from "./forced-scan-plan.advance-grammar-by.js";
export {
  networkIdForcedScanExpectedStateHash,
  networkIdForcedScanPriorGrammar,
  networkIdForcedScanPriorWalk,
  networkIdForcedScanStepForState,
  networkIdForcedScanStepOrdinal,
  networkIdForcedScanSuccessorStateHash,
  planNetworkIdForcedScan,
} from "./forced-scan-plan.plan-network-id-forced-scan.js";
