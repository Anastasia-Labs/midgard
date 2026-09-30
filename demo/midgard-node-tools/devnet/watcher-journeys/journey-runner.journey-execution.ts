import type { JourneyFixture } from "./fixture.js";

export const WATCHER_LAUNCH_TIMEOUT_MS = 1_800_000;

export type JourneyExecution =
  | { kind: "journey"; fixture: JourneyFixture; waitForAnchors: boolean }
  | { kind: "prepare_duplicate_event_history" };
