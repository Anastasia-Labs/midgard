import { afterAll } from "vitest";

import type { JourneyBlock } from "./fixture.js";
import { type JourneySession, openJourneySession } from "./journey-session.js";
import { decodeJourneyRetainedBlock } from "./staging.decode-journey-retained-block.js";

const sessions = new Map<string, Promise<JourneySession>>();
afterAll(async () => {
  await Promise.all(
    [...sessions.values()].map(async (session) => (await session).close()),
  );
});

/** A checkpoint archive supplies bytes; the live authenticated keeper supplies its ledger. */
export const readCheckpointJourneyLedgerBlock = async (
  runDirectory: string,
  block: JourneyBlock,
) => {
  let session = sessions.get(runDirectory);
  if (session === undefined) {
    session = openJourneySession(runDirectory);
    sessions.set(runDirectory, session);
  }
  return decodeJourneyRetainedBlock(block, (await session).ledgerAt);
};
