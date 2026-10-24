import { openJourneySession } from "./journey-session.open-journey-session.js";

export type JourneySession = Awaited<ReturnType<typeof openJourneySession>>;
