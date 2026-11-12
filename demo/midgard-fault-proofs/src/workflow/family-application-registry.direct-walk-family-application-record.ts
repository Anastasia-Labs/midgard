import { directWalkApplicationRecord } from "../direct-purpose-family/application-record.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_DIRECT_WALK } from "../distinct-asset-accumulation-limit/workflow.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_WALK } from "../execution-native-script-invalid/workflow.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_DIRECT_WALK } from "../execution-source-script-decoding/workflow.js";
import { MISSING_REDEEMER_DIRECT_WALK } from "../missing-redeemer/workflow.js";
import { MISSING_SCRIPT_SOURCE_DIRECT_WALK } from "../missing-script-source/workflow.js";
import { RECEIVE_PURPOSE_LANGUAGE_DIRECT_WALK } from "../receive-purpose-language/workflow.js";
import { SCRIPT_INTEGRITY_HASH_MISMATCH_DIRECT_WALK } from "../script-integrity-hash-mismatch/workflow.js";
import { UNUSED_REDEEMER_DIRECT_WALK } from "../unused-redeemer/workflow.js";
import { UNUSED_SCRIPT_WITNESS_DIRECT_WALK } from "../unused-script-witness/workflow.js";

/**
 * The direct-walk cursor families: each binds its event and pre-state, then
 * walks the committed transaction directly against the replayed ledger. They
 * need the replay context and read their reference scripts from the shared
 * bundle.
 */
export const EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD =
  directWalkApplicationRecord(EXECUTION_SOURCE_SCRIPT_DECODING_DIRECT_WALK);

export const EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_APPLICATION_RECORD =
  directWalkApplicationRecord(EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_WALK);

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY_APPLICATION_RECORD =
  directWalkApplicationRecord(DISTINCT_ASSET_ACCUMULATION_LIMIT_DIRECT_WALK);

export const UNUSED_SCRIPT_WITNESS_FAMILY_APPLICATION_RECORD =
  directWalkApplicationRecord(UNUSED_SCRIPT_WITNESS_DIRECT_WALK);

export const MISSING_SCRIPT_SOURCE_FAMILY_APPLICATION_RECORD =
  directWalkApplicationRecord(MISSING_SCRIPT_SOURCE_DIRECT_WALK);

export const UNUSED_REDEEMER_FAMILY_APPLICATION_RECORD =
  directWalkApplicationRecord(UNUSED_REDEEMER_DIRECT_WALK);

export const RECEIVE_PURPOSE_LANGUAGE_FAMILY_APPLICATION_RECORD =
  directWalkApplicationRecord(RECEIVE_PURPOSE_LANGUAGE_DIRECT_WALK);

export const MISSING_REDEEMER_FAMILY_APPLICATION_RECORD =
  directWalkApplicationRecord(MISSING_REDEEMER_DIRECT_WALK);

export const SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_APPLICATION_RECORD =
  directWalkApplicationRecord(SCRIPT_INTEGRITY_HASH_MISMATCH_DIRECT_WALK);
