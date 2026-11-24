/**
 * State-queue terminal transitions as a projection over L1 follower facts:
 * which headers a landed tx merged or removed,
 * rewound with the facts, and the SQL the node reads them with.
 */
export { queueTerminalDerivation } from "./derive.js";
export { queueTerminalProjection } from "./projection.js";
export {
  deploymentIdentityDigestOf,
  followerCoveredTipHeight,
  latestMergedHeader,
  liveQueueNodeHeader,
  nonFinalTerminalHeader,
  owedPayload,
  pruneFinalQueueTerminals,
  removedHeader,
} from "./reads.js";
export {
  QUEUE_TERMINAL_TABLES,
  QUEUE_TERMINALS_TABLE,
  queueTerminalMigrations,
} from "./schema.js";
export { newestTerminalOutcome } from "./terminal-outcome.js";
