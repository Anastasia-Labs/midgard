import { describe, expect, it, vi } from "vitest";

import {
  AvailabilityResponder,
  type AvailabilityResponderChallenge,
} from "../src/availability/responder.js";
import {
  challengeFixture,
  commitment,
  deploymentFingerprint,
  deploymentIdentity,
} from "./helpers/availability-challenge.js";

describe("availability responder draining every open challenge", () => {
  /**
   * A challenge whose every tranche is settled, so the next action is the
   * close: it needs no payload, which lets each fixture carry its own header.
   */
  const closable = (
    headerHash: string,
    responseDeadline: bigint,
  ): AvailabilityResponderChallenge => {
    const { challenge } = challengeFixture();
    const record = challenge.record.datum;
    return {
      ...challenge,
      record: {
        ...challenge.record,
        datum: {
          ...record,
          commitment: { ...record.commitment, header_hash: headerHash },
          response_deadline: responseDeadline,
        },
      },
      terminal: {
        ...challenge.terminal,
        datum: {
          ...challenge.terminal.datum,
          next_tranche_index: BigInt(
            record.commitment.tranche_descriptors.length,
          ),
          has_timed_out_tranche: false,
        },
      },
      tranches: [],
    };
  };

  const twoNearDeadlines = () => {
    let open = [
      closable("bb".repeat(28), 9_000n),
      closable("aa".repeat(28), 3_000n),
    ];
    const closed: string[] = [];
    const responder = new AvailabilityResponder({
      deploymentIdentity,
      deploymentFingerprint,
      store: { getDaPayload: async () => undefined },
      discover: async () => open,
      reconcile: async () => "ready",
      now: () => 2_000,
      execute: async (action) => {
        const headerHash = action.challenge.record.datum.commitment.header_hash;
        closed.push(headerHash);
        open = open.filter(
          (entry) => entry.record.datum.commitment.header_hash !== headerHash,
        );
        return "confirmed";
      },
    });
    return { responder, closed };
  };

  it("answers two open challenges in one drain, nearest deadline first, each exactly once", async () => {
    const { responder, closed } = twoNearDeadlines();
    expect(await responder.drain()).toMatchObject({
      status: "idle",
      challenges: 0,
    });
    expect(closed).toEqual(["aa".repeat(28), "bb".repeat(28)]);
  });

  it("answers only the nearest one per single step, as before", async () => {
    const { responder, closed } = twoNearDeadlines();
    expect(await responder.tick()).toMatchObject({
      action: "close",
      headerHash: "aa".repeat(28),
    });
    expect(closed).toEqual(["aa".repeat(28)]);
  });

  it("stops a drain at a step that did not confirm", async () => {
    const fixture = challengeFixture();
    const execute = vi.fn(async () => "included" as const);
    const report = await new AvailabilityResponder({
      deploymentIdentity,
      deploymentFingerprint,
      store: { getDaPayload: async () => fixture.stored },
      discover: async () => [fixture.challenge],
      reconcile: async () => "ready",
      now: () => 2_000,
      execute,
    }).drain();
    expect(report).toMatchObject({ action: "publish", status: "included" });
    expect(execute).toHaveBeenCalledOnce();
  });

  it("reports an unanswered challenge past its deadline as missed and never acts on it", async () => {
    const fixture = challengeFixture();
    const deadline = fixture.challenge.record.datum.response_deadline;
    const execute = vi.fn(async () => "confirmed" as const);
    const report = await new AvailabilityResponder({
      deploymentIdentity,
      deploymentFingerprint,
      store: { getDaPayload: async () => fixture.stored },
      discover: async () => [fixture.challenge],
      reconcile: async () => "ready",
      now: () => Number(deadline),
      execute,
    }).drain();
    expect(report).toMatchObject({
      status: "unavailable",
      missedDeadlines: [
        {
          headerHash: commitment.header_hash,
          responseDeadline: deadline.toString(),
        },
      ],
    });
    expect(execute).not.toHaveBeenCalled();
  });
});
