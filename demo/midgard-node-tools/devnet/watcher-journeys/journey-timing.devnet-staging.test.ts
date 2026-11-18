import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import { USER_EVENT_TX_TTL_MS } from "@al-ft/midgard-sdk";
import { expect, it } from "vitest";

import { DEPLOYMENT_PARAMETERS } from "../../src/devnet-stack/node-env.js";
import { genericJourneyTiming } from "./journey-timing.js";

const preprod = {
  slotLengthSeconds: 1,
  activeSlotsCoeff: 0.05,
  confirmationDepth: 30,
};

/**
 * A devnet family's staging as it runs on a chain that produces a block
 * every `blockGapMs`: each staging transaction is included `actionDepth`
 * blocks after it is sent. It commits each history block and has it
 * attested (an init, then an apply); publishes its user events one after
 * another, each built with the SDK's validity (from a minute behind the
 * clock to `USER_EVENT_TX_TTL_MS` ahead) and never below its predecessor's
 * protection, which runs `protectionMs` past the predecessor's validity;
 * waits the event wait once after the last of them; and then commits and
 * attests its fault. Returns when the fault's attestation is included.
 */
const simulatedStagingMs = ({
  blockGapMs,
  actionDepth,
  historyBlocks,
  userEvents,
  eventWaitMs,
  protectionMs,
}: {
  blockGapMs: number;
  actionDepth: number;
  historyBlocks: number;
  userEvents: number;
  eventWaitMs: number;
  protectionMs: number;
}) => {
  let now = 0;
  const included = () => {
    now = (Math.floor(now / blockGapMs) + actionDepth) * blockGapMs;
  };
  const committedAndAttested = () => {
    ["commit", "init", "apply"].forEach(() => included());
  };
  for (let block = 0; block < historyBlocks; block += 1) committedAndAttested();
  let predecessorValidToMs: number | undefined;
  for (let event = 0; event < userEvents; event += 1) {
    if (predecessorValidToMs !== undefined)
      now = Math.max(now, predecessorValidToMs + protectionMs + 60_000);
    predecessorValidToMs = now + USER_EVENT_TX_TTL_MS;
    included();
  }
  if (userEvents > 0) now += eventWaitMs;
  committedAndAttested();
  return now;
};

it.each([0.05, 0.25])(
  "gives a devnet family's limit room for its staging at active slot coefficient %s",
  (activeSlotsCoeff) => {
    const { event_wait_ms } =
      DEPLOYMENT_PROFILES["local-devnet-testing"].timing;
    const protectionMs = Number(
      DEPLOYMENT_PARAMETERS.MIDGARD_EVENT_HISTORY_PROTECTION_DURATION_MS,
    );
    const cadence = { ...preprod, activeSlotsCoeff, actionDepth: 1 };
    const limitMs = (plan?: { historyBlocks: number; userEvents: number }) =>
      genericJourneyTiming({
        ...cadence,
        ...(plan === undefined
          ? {}
          : {
              fixtureStaging: {
                ...plan,
                eventWaitMs: event_wait_ms,
                eventHistoryProtectionMs: protectionMs,
              },
            }),
      }).journeyTimeoutMs;
    const unstagedMs = limitMs();
    for (const plan of [
      { historyBlocks: 0, userEvents: 0 },
      { historyBlocks: 1, userEvents: 2 },
      { historyBlocks: 0, userEvents: 3 },
      { historyBlocks: 4, userEvents: 0 },
    ])
      expect(limitMs(plan) - unstagedMs).toBeGreaterThanOrEqual(
        simulatedStagingMs({
          ...plan,
          blockGapMs: (cadence.slotLengthSeconds * 1000) / activeSlotsCoeff,
          actionDepth: cadence.actionDepth,
          eventWaitMs: event_wait_ms,
          protectionMs,
        }),
      );
    expect(() =>
      genericJourneyTiming({
        ...cadence,
        fixtureStaging: {
          historyBlocks: -1,
          eventWaitMs: event_wait_ms,
          eventHistoryProtectionMs: protectionMs,
        },
      }),
    ).toThrow(/blocks/u);
    expect(() =>
      genericJourneyTiming({
        ...cadence,
        fixtureStaging: {
          userEvents: 0.5,
          eventWaitMs: event_wait_ms,
          eventHistoryProtectionMs: protectionMs,
        },
      }),
    ).toThrow(/user events/u);
  },
);
