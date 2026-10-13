import { AllowListRefusal } from "@al-ft/midgard-core/deployment-profile";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildOpenDaAvailabilityChallengeTxProgram,
  DaAvailabilityTransactionError,
} from "../src/availability-challenge-transactions.js";
import { buildRegisterOperatorTx } from "../src/operator-lifecycle.build-register-operator-tx.js";

const listed = "11".repeat(28);
const unlisted = "22".repeat(28);

// The checks run before the builders read anything else, so a key the list
// admits gets past them and fails on the absent build inputs instead.
const absent = {} as never;

const registerFailure = (operatorKeyHash: string) => {
  try {
    buildRegisterOperatorTx({
      operatorKeyHash,
      operatorAllowList: [listed],
    } as unknown as Parameters<typeof buildRegisterOperatorTx>[0]);
  } catch (error) {
    return error;
  }
  return undefined;
};

const openFailure = async (challenger: string) =>
  Either.getLeft(
    await Effect.runPromise(
      Effect.either(
        buildOpenDaAvailabilityChallengeTxProgram(absent, absent, {
          challenger,
          challengerAllowList: [listed],
        } as unknown as Parameters<
          typeof buildOpenDaAvailabilityChallengeTxProgram
        >[2]),
      ),
    ),
  );

describe("allow-list refusals before building", () => {
  it("refuses to build a registration for an operator the list omits", () => {
    expect(registerFailure(unlisted)).toBeInstanceOf(AllowListRefusal);
    expect(registerFailure(listed)).not.toBeInstanceOf(AllowListRefusal);
  });

  it("refuses to build an availability-challenge Open for a challenger the list omits", async () => {
    const refused = await openFailure(unlisted);
    expect(refused._tag).toBe("Some");
    if (refused._tag !== "Some") return;
    expect(refused.value).toBeInstanceOf(DaAvailabilityTransactionError);
    expect(refused.value.reason).toBe("challenger-not-allow-listed");
    const admitted = await openFailure(listed);
    expect(
      admitted._tag === "Some" ? admitted.value.reason : undefined,
    ).not.toBe("challenger-not-allow-listed");
  });
});
