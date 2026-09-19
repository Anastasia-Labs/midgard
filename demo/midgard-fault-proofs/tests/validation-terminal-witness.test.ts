import { describe, expect, it } from "vitest";

import { rejectingTerminalWorkRoot } from "./support/emulator/validation-dispute-fixtures.js";

describe("validation-dispute terminal witness", () => {
  it("commits the canonical Aiken rejection vector in fixture work roots", () => {
    // validation-tail-controls-v1-abi.test.ak independently pins this work root
    // for Terminal, program counter 9, E_VALUE_NOT_PRESERVED and root h'73…73'.
    expect(
      rejectingTerminalWorkRoot({
        programCounter: 9,
        rejectionCode: "E_VALUE_NOT_PRESERVED",
        priorLedgerRoot: Buffer.alloc(32, 0x73),
      }).toString("hex"),
    ).toBe("6b15a4122dc6437ca54930248e9df11979d21dc484d5ea373373b43c489f1ce6");
  });
});
