import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  generateSeedPhrase,
  paymentCredentialOf,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { verifyJourneyProverPayout } from "./readiness-evidence.js";
import type { JourneyResult } from "./readiness-evidence.read-journey-canonical-transactions.js";

/**
 * A stack's reference scripts were deployed by one wallet and its watcher
 * proves with another: the deployer's base address and the prover's
 * enterprise address hold different keys.
 */
const deployer = walletFromSeed(generateSeedPhrase(), {
  network: "Preprod",
}).address;
const prover = walletFromSeed(generateSeedPhrase(), {
  network: "Preprod",
  addressType: "Enterprise",
}).address;
const keyOf = (address: string) => paymentCredentialOf(address).hash;
const datumNaming = (address: string) =>
  Data.to({ fraud_prover: keyOf(address) }, SDK.FraudProofTokenDatum);
const paidTo = (address: string) =>
  credentialToAddress("Preprod", { type: "Key", hash: keyOf(address) });
const result = (proverAddress?: string) =>
  ({
    status: "passed",
    ...(proverAddress === undefined ? {} : { proverAddress }),
  }) as unknown as JourneyResult;

describe("journey result evidence: prover payout", () => {
  it("accepts a proof and reward naming the prover the run used, not the deployer", () => {
    expect(keyOf(prover)).not.toBe(keyOf(deployer));
    expect(() =>
      verifyJourneyProverPayout(
        result(prover),
        datumNaming(prover),
        paidTo(prover),
      ),
    ).not.toThrow();
  });

  it("refuses a proof token naming another key than the run's prover", () => {
    expect(() =>
      verifyJourneyProverPayout(
        result(prover),
        datumNaming(deployer),
        paidTo(prover),
      ),
    ).toThrow();
  });

  it("refuses a reward paid to another address than the run's prover", () => {
    expect(() =>
      verifyJourneyProverPayout(
        result(prover),
        datumNaming(prover),
        paidTo(deployer),
      ),
    ).toThrow();
  });

  it("refuses a result that does not name the prover the run used", () => {
    expect(() =>
      verifyJourneyProverPayout(
        result(),
        datumNaming(deployer),
        paidTo(deployer),
      ),
    ).toThrow(/does not name the prover/u);
  });
});
