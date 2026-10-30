import type { openAttachedJourneySession } from "./journey-session.open-attached-journey-session.js";
import { openJourneySession } from "./journey-session.open-journey-session.js";

/** A session that runs its own watcher against a journey devnet. */
export type JourneySession = Awaited<ReturnType<typeof openJourneySession>>;

/** A session attached to a running e2e-stack's own watcher. */
export type AttachedJourneySession = Awaited<
  ReturnType<typeof openAttachedJourneySession>
>;

/** What a journey reads from a running watcher, whoever runs it. */
export type JourneyWatcherOperations = Pick<
  AttachedJourneySession["watcher"],
  "requireLive" | "operations" | "diagnostics" | "watcherProgress"
>;
