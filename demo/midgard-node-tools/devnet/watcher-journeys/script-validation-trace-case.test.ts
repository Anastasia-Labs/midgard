/** The validation-trace journey's staged fault, classified by the installed
 * full-catalogue selector: a committed mint whose Plutus policy fails is a
 * validation-trace fault, and the same mint under a passing policy is
 * healthy. */
import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import {
  buildCanonicalBlockFixture,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import {
  CML,
  generateEmulatorAccount,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { classifyFullCatalogueTransactionFixture } from "./journey-classification.test-support.js";
import { checkedFixtureBlock } from "./journey-fixture-ledger.test-support.js";
import {
  buildJourneyValidationTraceControl,
  buildJourneyValidationTraceFault,
  type JourneyValidationTraceInput,
} from "./script-validation-trace-case.js";

describe("validation-trace journey fault", () => {
  let input: JourneyValidationTraceInput;
  beforeAll(async () => {
    const account = generateEmulatorAccount({ lovelace: 100_000_000n });
    const wallet = walletFromSeed(account.seedPhrase, { network: "Custom" });
    const owner = CML.PrivateKey.from_bech32(wallet.paymentKey)
      .to_public()
      .hash()
      .to_raw_bytes();
    const predecessor = await checkedFixtureBlock(
      await buildCanonicalBlockFixture({
        transactions: [],
        minFeeA: 44n,
        minFeeB: 155_381n,
        utxos: [
          {
            key: outRefCbor(0x81, 0n),
            value: encodeMidgardTxOutput({
              address: Buffer.concat([Buffer.from([0x60]), Buffer.from(owner)]),
              value: { lovelace: 100_000_000n, assets: new Map() },
            }),
          },
        ],
      }),
    );
    input = {
      predecessor,
      ledgerOwnerSeedPhrase: account.seedPhrase,
      operatorVkey: "82".repeat(28),
      endTime: predecessor.header.endTime + 40_000n,
      blockSlot: predecessor.header.blockSlot + 40n,
    };
  });

  it("stages a committed mint whose Plutus policy fails, which the installed selector convicts as a validation-trace fault", async () => {
    const block = await buildJourneyValidationTraceFault(input);
    expect(block.payload.block_body.transaction_preimages).toHaveLength(1);
    const classified = await classifyFullCatalogueTransactionFixture({
      block,
      predecessor: input.predecessor,
      history: [],
    });
    expect(classified.decision).toMatchObject({
      decision: "fault_detected",
      category: "validationTraceDispute",
    });
  });

  it("leaves the same mint under a passing policy healthy", async () => {
    const block = await buildJourneyValidationTraceControl(input);
    const classified = await classifyFullCatalogueTransactionFixture({
      block,
      predecessor: input.predecessor,
      history: [],
    });
    expect(classified.decision).toMatchObject({ decision: "healthy" });
  });
});
