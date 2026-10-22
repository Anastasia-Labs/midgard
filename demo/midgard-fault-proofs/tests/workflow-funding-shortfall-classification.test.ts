/**
 * Which transaction-build failures pause a proof action until the prover
 * wallet is topped up: only a wallet short of ada, whether for the outputs,
 * the collateral, the change output's minimum ada or a proof step's fee coin.
 * A build needing a native token the wallet does not hold, or handed an
 * empty input list, fails the same way after any top-up, so it keeps its own
 * failure. Every failure here comes from a real build, so a Lucid release
 * that rewords one turns its row red.
 */
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  mintingPolicyToId,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { selectFeeInput } from "../src/step-support.js";
import {
  asWorkflowFundingShortfall,
  WorkflowFundingReservationUnavailableError,
  WorkflowFundingShortfallError,
} from "../src/workflow/funding-reservation-permit.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

const ACTION = "init:00#0";
const TOKEN = `${"bb".repeat(28)}746f6b656e`;
/** A Plutus V3 minting policy that accepts any redeemer. */
const ALWAYS_MINT = {
  type: "PlutusV3",
  script: "5101010023259800a518a4d136564004ae69",
} as const;

/** A wallet holding a single plain coin of `lovelace`. */
const walletWith = async (lovelace: bigint) => {
  const account = generateEmulatorAccount({ lovelace });
  const lucid = await Lucid(
    new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS),
    "Custom",
  );
  lucid.selectWallet.fromSeed(account.seedPhrase);
  return { lucid, address: account.address };
};

const expectShortfall = (failure: unknown) => {
  const classified = asWorkflowFundingShortfall(ACTION, failure);
  expect(classified).toBeInstanceOf(WorkflowFundingShortfallError);
  expect(classified).toBeInstanceOf(WorkflowFundingReservationUnavailableError);
};

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
    expectShortfall(failure);
  });

  it("pauses a script build the wallet is short of collateral for", async () => {
    const small = await walletWith(3_000_000n);
    const minted = `${mintingPolicyToId(ALWAYS_MINT)}746f6b656e`;
    const failure = await buildFailure(() =>
      small.lucid
        .newTx()
        .mintAssets({ [minted]: 1n }, Data.void())
        .attach.MintingPolicy(ALWAYS_MINT)
        .pay.ToAddress(small.address, { lovelace: 1_500_000n, [minted]: 1n })
        .complete(),
    );
    expect(String(failure)).toContain("Lovelace collateral");
    expectShortfall(failure);
  });

  it("pauses a build whose change would fall below the minimum ada", async () => {
    const small = await walletWith(3_000_000n);
    const failure = await buildFailure(() =>
      small.lucid
        .newTx()
        .pay.ToAddress(address, { lovelace: 2_500_000n })
        .complete(),
    );
    expect(String(failure)).toContain("minimum ADA for change output");
    expectShortfall(failure);
  });

  it("pauses a proof step whose wallet has no plain coin large enough for its fee", async () => {
    const small = await walletWith(3_000_000n);
    const failure = await buildFailure(async () =>
      selectFeeInput(await small.lucid.wallet().getUtxos()),
    );
    expect(String(failure)).toContain("pure-ADA UTxO");
    expectShortfall(failure);
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
