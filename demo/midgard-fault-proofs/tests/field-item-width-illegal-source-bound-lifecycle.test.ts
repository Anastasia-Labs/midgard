import {
  decodeMidgardNativeTxWitnessSetCompact,
  decodeSingleCbor,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeCbor,
  encodeMidgardFieldPreimage,
  materializeMidgardNativeTxFromCanonical,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { prepareFieldItemWidthEvidence } from "../src/field-item-width-illegal/field-item-width-illegal.js";
import { forcedBlock } from "./field-item-width-illegal-lifecycle.forced-block.js";
import { makeHarness } from "./field-item-width-illegal-lifecycle.make-harness.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  buildAcceptedWidthInclusions,
  widthNativeTx,
} from "./support/field-item-width-illegal-shapes.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";

// Same independent signed vectors as the FIWI witness-order oracle in
// midgard-validation/tests/signature-order.test.ts (public fixed seeds 01/02).
const signed = {
  descending:
    "84018c582981582682582011111111111111111111111111111111111111111111111111111111111111111900004180582c815829a200581d600d6a577e9441ad8ed9663931906e4d43ece8f82c712b1d0235affb0601821a004c4b40a0002020418041804180582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53008358cf8258658258208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c584094da6e57c26fd188c52a4b23b2801f9c2fad870b15b2d289a5189141d8e18ada511c79dc9f7b32351c2a90aaafccb85bc9785189433ebc38c5ad8482d7b79e0158658258208139770ea87d175f56a35466c34c7ecccb8d8a91b4ee37a25df60f5b8fc9b3945840aa08f5f3164fbe3da499efa9bc02d9ccb302c34e05b0089e82f2ffb30cbbb4732e9beaf9ce196bb62959de0f05e7c73de24f8d5d266758a74956d011b88e1a0f4180418000",
  duplicate:
    "84018c582981582682582011111111111111111111111111111111111111111111111111111111111111111900004180582c815829a200581d600d6a577e9441ad8ed9663931906e4d43ece8f82c712b1d0235affb0601821a004c4b40a0002020418041804180582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53008358cf8258658258208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c584094da6e57c26fd188c52a4b23b2801f9c2fad870b15b2d289a5189141d8e18ada511c79dc9f7b32351c2a90aaafccb85bc9785189433ebc38c5ad8482d7b79e0158658258208a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c584094da6e57c26fd188c52a4b23b2801f9c2fad870b15b2d289a5189141d8e18ada511c79dc9f7b32351c2a90aaafccb85bc9785189433ebc38c5ad8482d7b79e014180418000",
};

// The same two signed witnesses in ascending signer-hash order. Signatures
// cover the body only, so the swap keeps both valid.
const ascending = (() => {
  const itemHex = 2 * 103;
  const first = signed.descending.indexOf("5865825820");
  const second = first + itemHex;
  return (
    signed.descending.slice(0, first) +
    signed.descending.slice(second, second + itemHex) +
    signed.descending.slice(first, second) +
    signed.descending.slice(second + itemHex)
  );
})();

const sourceMaterial = (hex: string, corruptWrapper: boolean) => {
  const envelope = decodeSingleCbor(Buffer.from(hex, "hex")) as unknown[];
  const witnesses = envelope[2] as unknown[];
  const field = Buffer.from(witnesses[0] as Uint8Array);
  if (corruptWrapper) field[105] = 0x64;
  witnesses[0] = field;
  return deriveMidgardNativeTxFaultEvidenceMaterial(encodeCbor(envelope));
};

const replaceWitnessField = (hex: string, field: Buffer) => {
  const envelope = decodeSingleCbor(Buffer.from(hex, "hex")) as unknown[];
  (envelope[2] as unknown[])[0] = field;
  return deriveMidgardNativeTxFaultEvidenceMaterial(encodeCbor(envelope));
};

describe("source-bound fieldItemWidthIllegal physical chain", () => {
  it.each([
    ["ascending", ascending, false, false],
    ["descending", signed.descending, false, true],
    ["duplicate", signed.duplicate, false, true],
    ["malformed wrapper", ascending, true, true],
  ] as const)(
    "authenticates %s witnesses through all three validators",
    async (label, hex, corruptWrapper, illegal) => {
      const h = await makeHarness();
      const material = sourceMaterial(hex, corruptWrapper);
      // A witness-field change never moves the body-derived transaction id.
      expect(material.transactionId).toEqual(
        sourceMaterial(hex, false).transactionId,
      );
      const block = await buildAcceptedWidthInclusions([material]);
      const setup = await setupFraudulentBlock({
        funderLucid: h.harness.funderLucid,
        emulator: h.harness.emulator,
        contracts: h.harness.contracts,
        catalogue: h.catalogue,
        fixture: {
          transactionsRoot: block.transactionsRoot,
          l2TransactionCount: 1n,
        },
      });
      await h.publishReferences();
      const field = material.fieldPreimages[7]!;
      const evidence = prepareFieldItemWidthEvidence({
        finding: {
          subject: acceptedVerdictSubject(
            material.transactionId.toString("hex"),
          ),
          fieldIndex: 7n,
          itemIndex: 1n,
        },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      });
      expect(evidence.decisiveFaultHolds).toBe(illegal);
      const initialized = await h.init(setup.fraudulentBlockOutRef);
      const source = {
        nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
        witnessSetCompactCbor:
          material.proofSource.witnessSetCompactCbor.toString("hex"),
      };
      const bound = await h.step01Accepted(
        initialized.result,
        evidence,
        block.inclusions[0]!,
        setup.fraudulentBlockOutRef,
      );
      if (label === "descending") {
        const otherField = Buffer.from(field);
        otherField[otherField.length - 1] =
          otherField[otherField.length - 1]! ^ 1;
        const other = replaceWitnessField(hex, otherField);
        const witnessSet = decodeMidgardNativeTxWitnessSetCompact(
          other.proofSource.witnessSetCompactCbor,
        );
        await expectOnchainRefusal(
          () =>
            h.step02Raw(bound.result.nextThreadOutRef, evidence, source, {
              mutateOpening: (opening) => {
                if (!("WitnessFieldOpening" in opening))
                  throw new Error("witness opening expected");
                return {
                  WitnessFieldOpening: {
                    ...opening.WitnessFieldOpening,
                    carriage: {
                      Inline: { preimage: otherField.toString("hex") },
                    },
                  },
                };
              },
            }),
          {
            refusedBy: "fraud_proofs/field_item_width_illegal/step_02",
            check: /commitment|hash/u,
          },
        );
        await expectOnchainRefusal(
          () =>
            h.step02Raw(bound.result.nextThreadOutRef, evidence, source, {
              mutateOpening: (opening) => {
                if (!("WitnessFieldOpening" in opening))
                  throw new Error("witness opening expected");
                return {
                  WitnessFieldOpening: {
                    native_tx_compact_cbor:
                      other.proofSource.compactCbor.toString("hex"),
                    witness_set: {
                      addr_tx_wits_hash: Buffer.from(
                        witnessSet.addrTxWitsHash,
                      ).toString("hex"),
                      script_tx_wits_hash: Buffer.from(
                        witnessSet.scriptTxWitsHash,
                      ).toString("hex"),
                      redeemer_tx_wits_hash: Buffer.from(
                        witnessSet.redeemerTxWitsHash,
                      ).toString("hex"),
                    },
                    carriage: {
                      Inline: { preimage: otherField.toString("hex") },
                    },
                  },
                };
              },
            }),
          {
            refusedBy: "fraud_proofs/field_item_width_illegal/step_02",
            check: /witness_set_hash/u,
          },
        );
      }
      const authenticated = await h.step02(
        bound.result.nextThreadOutRef,
        evidence,
        source,
      );
      if (illegal) {
        const proven = await h.step03(
          authenticated.result.nextThreadOutRef,
          evidence,
        );
        expect(proven.result.fraudProofUnit).toBeTruthy();
      } else {
        await expectOnchainRefusal(
          () => h.step03Raw(authenticated.result.nextThreadOutRef),
          {
            refusedBy: "fraud_proofs/field_item_width_illegal/step_03",
            check: /^Validator returned false$/u,
          },
        );
      }
    },
    600_000,
  );

  it.each([
    [-1n, 0n],
    [4n, -1n],
    [1n << 80n, 0n],
    [4n, 1n << 80n],
    [4n, 0n],
  ])(
    "proves exact signed forced coordinate %s/%s through the public builders",
    async (fieldIndex, itemIndex) => {
      const h = await makeHarness();
      const base = widthNativeTx({});
      const fieldPreimage = encodeMidgardFieldPreimage([Buffer.alloc(28, 3)]);
      const nativeTx = materializeMidgardNativeTxFromCanonical({
        ...base,
        body: { ...base.body, requiredSignersPreimageCbor: fieldPreimage },
      });
      const shape = {
        label: "one legal required signer",
        nativeTx,
        fieldIndex: 4,
        itemIndex: 0,
        fieldPreimage,
        illegal: false,
      };
      const { setup, evidence, source } = await forcedBlock(h, shape, {
        fieldIndex,
        itemIndex,
      });
      const initialized = await h.init(setup.fraudulentBlockOutRef);
      const bound = await h.step01Forced(
        h.threadOf(initialized.result),
        evidence,
        source,
      );
      const authenticated = await h.step02(
        bound.result.nextThreadOutRef,
        evidence,
        shape,
      );
      expect(authenticated.measurements).toHaveLength(1);
      const proven = await h.step03(
        authenticated.result.nextThreadOutRef,
        evidence,
      );
      expect(proven.result.fraudProofUnit).toBeTruthy();
    },
    600_000,
  );
});
