/**
 * Security-grade evidence preparation for the same-block `double-withdraw`
 * family. The bare same-outref predicate is deliberately insufficient: a
 * pair is submittable only when both distinct committed leaves are payable.
 */

import "node:fs/promises";
import "node:path";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-core/da-transport";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "./json-file.js";
import "./prepare-double-spend.js";
import "./transition-trace/fetch.js";
import "./transition-trace/phas.js";
import "./prepare-double-withdraw.require-selected-pair.js";
import "./prepare-double-withdraw.prepare-double-withdraw-from-committed-leaves.js";
export {
  type DoubleWithdrawBlockEvidence,
  doubleWithdrawBlockEvidenceFromVerifiedPayload,
  prepareDoubleWithdrawFromCommittedLeaves,
  prepareDoubleWithdrawFromRetainedDa,
} from "./prepare-double-withdraw.prepare-double-withdraw-from-committed-leaves.js";
export {
  DOUBLE_WITHDRAW_EVIDENCE_SCHEMA_VERSION,
  type DoubleWithdrawCommittedLeaf,
  DoubleWithdrawRejection,
  type DoubleWithdrawRejectionCode,
  type PreparedDoubleWithdrawInclusion,
  type PreparedDoubleWithdrawOutput,
} from "./prepare-double-withdraw.require-selected-pair.js";
