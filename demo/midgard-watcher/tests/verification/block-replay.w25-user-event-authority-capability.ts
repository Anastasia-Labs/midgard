import "./block-replay.w25-roots-and-deterministic-replay.js";

import * as SDK from "@al-ft/midgard-sdk";
import {
  makeNativeTx,
  outRefFromTxId,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { evaluateWatcherBlockReplay } from "../../src/index.js";
import { validateEventAuthority } from "../../src/verification/block-replay.validate-event-authority.js";
import {
  buildPublicReplayFixture,
  type CommittedEffectGroup,
  committedStepsForEffects,
  depositEffectFromOrigin,
  entries,
  nativeEffect,
  originEventAuthority,
  originEventWindow,
  publicEventFromOrigin,
  publicInput,
  RULE_BUNDLE,
} from "../support/block-replay-public-fixture.js";
import {
  admitFixtureUserEventAt,
  fixtureDepositEvent,
  fixtureHeaderCutoff,
} from "../support/user-event-authority-fixture.js";
import { depositOrigin, FIXED_KEY } from "./block-replay.registration.js";

/**
 * The replay admits a user event only through a current, uncopied capability
 * whose read is scoped to the replayed header's own cutoff and whose event
 * bytes name the event its id does. Each refusal is pinned at its own check.
 */
describe("W25 user-event authority capability", () => {
  it("refuses a retired, copied, wrong-header or misnamed user-event read", async () => {
    const origin = depositOrigin;
    const event = publicEventFromOrigin(origin);
    const effect = depositEffectFromOrigin(origin);
    const inserted = effect.operations[0];
    if (inserted === undefined || inserted.type !== "insert") {
      throw new Error("genuine deposit did not derive one canonical insert");
    }
    const native = makeNativeTx({
      spendInputs: [inserted.outRefCbor],
      outputs: [inserted.outputCbor],
      privateKey: FIXED_KEY,
    });
    const produced = outRefFromTxId(native.txId);
    const groups: readonly CommittedEffectGroup[] = [
      { eventKey: event.eventKey, phase: "Deposit", effect },
      {
        eventKey: {
          L2TransactionEventKey: { tx_id: native.txId.toString("hex") },
        },
        phase: "L2Transaction",
        effect: nativeEffect({
          spent: [inserted.outRefCbor],
          native,
          outputs: [inserted.outputCbor],
        }),
      },
    ];
    const steps = await committedStepsForEffects([], groups);
    const fixtureAt = (eventWindow: ReturnType<typeof originEventWindow>) =>
      buildPublicReplayFixture({
        txCbors: [native.txCbor],
        events: [event],
        steps,
        priorState: [],
        postState: entries([[produced, inserted.outputCbor]]),
        eventAuthorities: [originEventAuthority({ event, origin, effect })],
        eventWindow,
      });
    const fixture = await fixtureAt(originEventWindow(origin));
    const authority = fixture.eventAuthorities[0]!;
    await expect(
      evaluateWatcherBlockReplay(publicInput(fixture)),
    ).resolves.toMatchObject({ action: "accept", reasonCodes: [] });

    // The capability is the authority, not the record carrying it: a read
    // retired by a rewind below the cutoff, or a copy of the capability, is
    // refused before anything it says is used.
    const cutoff = fixtureHeaderCutoff(fixture.observation);
    const retired = await evaluateWatcherBlockReplay({
      ...publicInput(fixture),
      eventAuthorities: [
        {
          ...authority,
          userEvent: admitFixtureUserEventAt(origin, cutoff, () => false),
        },
      ],
    });
    expect(retired).toMatchObject({
      action: "error",
      reasonCodes: ["user_event_authority_invalid"],
    });
    const copied = await evaluateWatcherBlockReplay({
      ...publicInput(fixture),
      eventAuthorities: [
        { ...authority, userEvent: Object.freeze({ ...authority.userEvent }) },
      ],
    });
    expect(copied).toMatchObject({
      action: "error",
      reasonCodes: ["user_event_authority_invalid"],
    });

    // A read scoped to another header's cutoff, and a read whose event bytes
    // name another event than its id, are refused at their own checks.
    const otherHeader = await fixtureAt({
      start: originEventWindow(origin).start - 1n,
      end: originEventWindow(origin).end,
    });
    expect(otherHeader.observation.headerHash).not.toBe(
      fixture.observation.headerHash,
    );
    const wrongHeader = otherHeader.eventAuthorities[0]!;
    await expect(
      evaluateWatcherBlockReplay({
        ...publicInput(fixture),
        eventAuthorities: [wrongHeader],
      }),
    ).resolves.toMatchObject({
      action: "error",
      reasonCodes: ["user_event_authority_identity_mismatch"],
    });
    await expect(
      validateEventAuthority(wrongHeader, [], RULE_BUNDLE, cutoff),
    ).rejects.toMatchObject({
      code: "user_event_authority_identity_mismatch",
      path: "$.userEvent.throughHeader",
    });
    const foreignPayload = fixtureDepositEvent({
      nonceByte: "e9",
      l2Address: Data.from(origin.event.eventCborHex, SDK.DepositEvent).info
        .l2_address,
      originalAssets: { lovelace: 3_000_000n },
    });
    const misnamed = {
      ...authority,
      userEvent: admitFixtureUserEventAt(
        {
          ...origin,
          event: {
            ...origin.event,
            eventCborHex: foreignPayload.event.eventCborHex,
          },
        },
        cutoff,
      ),
    };
    await expect(
      evaluateWatcherBlockReplay({
        ...publicInput(fixture),
        eventAuthorities: [misnamed],
      }),
    ).resolves.toMatchObject({
      action: "error",
      reasonCodes: ["user_event_authority_identity_mismatch"],
    });
    await expect(
      validateEventAuthority(misnamed, [], RULE_BUNDLE, cutoff),
    ).rejects.toMatchObject({
      code: "user_event_authority_identity_mismatch",
      path: "$.eventAuthority.eventId",
    });
    // The honest read passes both checks and stops only at the missing claim.
    await expect(
      validateEventAuthority(authority, [], RULE_BUNDLE, cutoff),
    ).rejects.toMatchObject({
      code: "event_authority_identity_mismatch",
      path: "$.committedEventClaim",
    });
  });
});
