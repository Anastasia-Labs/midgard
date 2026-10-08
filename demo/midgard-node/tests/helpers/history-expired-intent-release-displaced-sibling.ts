/**
 * The state-queue correction rewind authority the SQL-modelled history
 * recovery suites run under.
 */
import { hex } from "./history-expired-intent-release-before-ttl.js";

export const authority = {
  manifestId: hex("manifest"),
  stateQueuePolicyId: hex("policy").slice(0, 56),
  requiredFinalityDepth: 3n,
};
