import { buildOpenDaAvailabilityChallengeTxProgram } from "@al-ft/midgard-sdk";
import { Effect, Either } from "effect";
import {
  assertAvailabilityRefusal,
  attestAvailability,
  createAvailabilityFixture,
  openAvailability,
} from "midgard-node/tests/helpers/availability-challenge-emulator";
import { describe, expect, it } from "vitest";

import {
  CURATED_EMULATOR_SEEDS,
  curatedKeyHash,
  curatedPaymentKey,
} from "./support/curated-emulator.js";

describe("preprod-emulator-curated DA challenger allow-list", () => {
  it("opens an availability challenge for a listed challenger and refuses one the list omits", async () => {
    // Negative first: a fresh challenger key, otherwise the same journey.
    const outsider = await createAvailabilityFixture();
    // The SDK's Open builder refuses the key under the profile's list before
    // it reads anything; the journey below builds its own Open past it.
    const built = await Effect.runPromise(
      Effect.either(
        buildOpenDaAvailabilityChallengeTxProgram(
          {} as never,
          {} as never,
          {
            challenger: outsider.challengerKey,
          } as unknown as Parameters<
            typeof buildOpenDaAvailabilityChallengeTxProgram
          >[2],
        ),
      ),
    );
    expect(Either.isLeft(built) && built.left.reason).toBe(
      "challenger-not-allow-listed",
    );
    const refused = await openAvailability(
      outsider,
      await attestAvailability(outsider),
    );
    await assertAvailabilityRefusal(refused.build(), {
      purpose: "withdraw",
      script: "availability-challenge open withdrawal",
    });

    const listed = await createAvailabilityFixture(undefined, 0, 0, {
      challengerPrivateKey: curatedPaymentKey(
        CURATED_EMULATOR_SEEDS.daChallenger,
      ),
    });
    expect(listed.challengerKey).toBe(
      curatedKeyHash(CURATED_EMULATOR_SEEDS.daChallenger),
    );
    const open = await openAvailability(
      listed,
      await attestAvailability(listed),
    );
    const opened = await open.submit();
    expect(
      opened.record.assets[open.policy + open.plan.challengeAssetName],
    ).toBe(1n);
  }, 600_000);
});
