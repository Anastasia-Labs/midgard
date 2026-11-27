import { AllowListRefusal } from "@al-ft/midgard-core/deployment-profile";
import { describe, expect, it } from "vitest";

import { buildRegisterOperatorTx } from "../src/operator-lifecycle.build-register-operator-tx.js";

const listed = "11".repeat(28);
const unlisted = "22".repeat(28);

// The check runs before the builder reads anything else, so a key the list
// admits gets past it and fails on the absent build inputs instead.
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

describe("allow-list refusals before building", () => {
  it("refuses to build a registration for an operator the list omits", () => {
    expect(registerFailure(unlisted)).toBeInstanceOf(AllowListRefusal);
    expect(registerFailure(listed)).not.toBeInstanceOf(AllowListRefusal);
  });
});
