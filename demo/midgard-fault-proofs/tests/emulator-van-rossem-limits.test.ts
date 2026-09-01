import { describe, expect, it } from "vitest";

import {
  EMULATOR_PROTOCOL_PARAMETERS,
  VAN_ROSSEM_TRANSACTION_LIMITS,
} from "./support/submit-init-emulator-shared.js";

describe("fault-proof emulator protocol limits", () => {
  it("uses the Van Rossem transaction-size and ExUnit limits", () => {
    expect(EMULATOR_PROTOCOL_PARAMETERS.maxTxSize).toBe(16_384);
    expect(EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem).toBe(16_500_000n);
    expect(EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps).toBe(10_000_000_000n);
    expect(EMULATOR_PROTOCOL_PARAMETERS).toMatchObject(
      VAN_ROSSEM_TRANSACTION_LIMITS,
    );
  });
});
