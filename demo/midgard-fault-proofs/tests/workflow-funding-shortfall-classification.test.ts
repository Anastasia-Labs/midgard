/**
 * Which transaction-build failures pause a proof action until the prover
 * wallet is topped up: only a wallet short of ada. A build needing a native
 * token the wallet does not hold, or handed an empty input list, fails the
 * same way after any top-up, so it keeps its own failure.
 */
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  asWorkflowFundingShortfall,
  WorkflowFundingReservationUnavailableError,
  WorkflowFundingShortfallError,
} from "../src/workflow/funding-reservation-permit.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

const ACTION = "init:00#0";
const TOKEN = `${"bb".repeat(28)}746f6b656e`;

const buildFailure = async (build: () => Promise<unknown>) => {
  try {
    await build();
  } catch (failure) {
    return failure;
  }
  throw new Error("expected the build to fail");
};

describe("a transaction-build failure as a funding shortfall", () => {
  let lucid: LucidEvolution;
  let address: string;

  beforeAll(async () => {
    const account = generateEmulatorAccount({ lovelace: 10_000_000n });
    lucid = await Lucid(
      new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS),
      "Custom",
    );
    lucid.selectWallet.fromSeed(account.seedPhrase);
    address = account.address;
  });

  it("pauses a build the wallet is short of ada for", async () => {
    const failure = await buildFailure(() =>
      lucid
        .newTx()
        .pay.ToAddress(address, { lovelace: 100_000_000n })
        .complete(),
    );
    const classified = asWorkflowFundingShortfall(ACTION, failure);
    expect(classified).toBeInstanceOf(WorkflowFundingShortfallError);
    expect(classified).toBeInstanceOf(
      WorkflowFundingReservationUnavailableError,
    );
  });

  it("keeps the failure of a build needing a token the wallet does not hold", async () => {
    const failure = await buildFailure(() =>
      lucid
        .newTx()
        .pay.ToAddress(address, { lovelace: 2_000_000n, [TOKEN]: 1n })
        .complete(),
    );
    expect(asWorkflowFundingShortfall(ACTION, failure)).toBe(failure);
  });

  it("keeps the failure of a build handed an empty input list", async () => {
    const failure = await buildFailure(async () =>
      lucid.newTx().collectFrom([]).complete(),
    );
    expect(asWorkflowFundingShortfall(ACTION, failure)).toBe(failure);
  });
});
