import type { FollowerProjection } from "../projection.js";
import { INTENT_PRUNE_HOOK } from "./prune.js";
import { intentMigrations } from "./schema.js";

/**
 * The intent journal as a follower projection: its class B tables and the
 * retention hook. No retention pin is needed: a retained intent that landed
 * landed above the prune boundary (its tx row is kept), and a retained
 * intent's landed dependency created an output it spends, which keeps that
 * tx row while the output is retained. A role adds it to its projections;
 * its own families add predicates and submission on top
 * (`createIntentReconciler`).
 */
export const intentJournalProjection: FollowerProjection = {
  name: "intent-journal",
  migrations: intentMigrations,
  pruneHooks: [INTENT_PRUNE_HOOK],
};
