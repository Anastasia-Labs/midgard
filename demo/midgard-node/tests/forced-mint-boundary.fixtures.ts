/**
 * Forced transactions on either side of the tx-order mint's admission
 * boundary, for the node tests that pin what reaches ingest and what verdict
 * it gets.
 *
 * `MINT_REFUSED` are transactions the mint refuses, with the check its
 * off-chain twin `screenMidgardForcedTxAdmission` names, so no order for them
 * can exist on chain. `MINT_ADMITTED_UNDECODABLE` are their admitted neighbours:
 * each is a §5.1 envelope the mint admits whose items do not decode, and the
 * reason is the rejection classification commits for it.
 */
import {
  encodeCbor,
  encodeMidgardFieldPreimage,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import type * as SDK from "@al-ft/midgard-sdk";

import {
  canonicalTransaction,
  makeOutput,
  TEST_ADDRESS,
} from "./forced-transactions.make-signed-effectful-transaction.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";

export const forced = (tx: ReturnType<typeof canonicalTransaction>): Buffer =>
  encodeMidgardForcedTxCanonical(materializeMidgardForcedTxFromCanonical(tx));

/** The mint admits only a transaction that spends something. */
const spending = (): ReturnType<typeof canonicalTransaction> => {
  const tx = canonicalTransaction();
  return {
    ...tx,
    body: {
      ...tx.body,
      spendInputsPreimageCbor: encodeMidgardFieldPreimage([
        makeOutRefCbor(0x11),
      ]),
    },
  };
};

const withOutputs = (outputs: readonly Buffer[]): Buffer => {
  const tx = spending();
  return forced({
    ...tx,
    body: {
      ...tx.body,
      outputsPreimageCbor: encodeMidgardFieldPreimage(outputs),
    },
  });
};

const withScriptWitnesses = (preimage: Buffer): Buffer => {
  const tx = spending();
  return forced({
    ...tx,
    witnessSet: { ...tx.witnessSet, scriptTxWitsPreimageCbor: preimage },
  });
};

/** An address-and-value output: `a2 00 <address> 01 <value>`. */
const plainOutput = makeOutput(100_000_000n);
const ADDRESS_END = 2 + 2 + TEST_ADDRESS.length;

export const MINT_REFUSED = [
  [
    "an auxiliary data hash",
    "has_no_auxiliary_data(body)",
    () => {
      const tx = spending();
      return forced({
        ...tx,
        body: { ...tx.body, auxiliaryDataHash: Buffer.alloc(32, 1) },
      });
    },
  ],
  [
    "a MidgardV1 script witness",
    "script_witnesses_are_native(script_witnesses)",
    () =>
      withScriptWitnesses(
        encodeMidgardVersionedScriptListPreimage([
          { language: "MidgardV1", scriptBytes: Buffer.from("80", "hex") },
        ]),
      ),
  ],
  [
    "a field-6 item with no native header",
    "script_witnesses_are_native(script_witnesses)",
    () =>
      withScriptWitnesses(
        encodeMidgardFieldPreimage([Buffer.from("ff", "hex")]),
      ),
  ],
  [
    "a MidgardV1 script_ref output",
    "outputs_are_admissible(outputs)",
    () =>
      withOutputs([
        encodeMidgardTxOutput({
          address: TEST_ADDRESS,
          value: { lovelace: 100_000_000n, assets: new Map() },
          script_ref: {
            language: "MidgardV1",
            scriptBytes: Buffer.from("80", "hex"),
          },
        }),
      ]),
  ],
  [
    "a 170-asset output value",
    "outputs_are_admissible(outputs)",
    () =>
      withOutputs([
        encodeMidgardTxOutput({
          address: TEST_ADDRESS,
          value: {
            lovelace: 100_000_000n,
            assets: new Map([
              [
                "01".repeat(28),
                new Map(
                  Array.from(
                    { length: 170 },
                    (_, i) => [i.toString(16).padStart(64, "0"), 1n] as const,
                  ),
                ),
              ],
            ]),
          },
        }),
      ]),
  ],
  [
    "an output with no address",
    "outputs_are_admissible(outputs)",
    () => withOutputs([Buffer.from("a0", "hex")]),
  ],
] as const;

export const MINT_ADMITTED_UNDECODABLE: readonly (readonly [
  string,
  SDK.RejectionReason,
  () => Buffer,
])[] = [
  [
    "an output with a non-canonical inline datum",
    { OutputNonCanonical: { output_index: 0n } },
    () =>
      withOutputs([
        Buffer.concat([
          Buffer.from([0xa3]),
          plainOutput.subarray(1),
          Buffer.from([0x02]),
          // The integer 0 in a two-byte head: decodable, not canonical.
          encodeCbor(Buffer.from("1800", "hex")),
        ]),
      ]),
  ],
  [
    "an output whose address does not decode",
    { OutputNonCanonical: { output_index: 0n } },
    () =>
      withOutputs([
        Buffer.concat([
          plainOutput.subarray(0, 2),
          encodeCbor(Buffer.from([0x99])),
          plainOutput.subarray(ADDRESS_END),
        ]),
      ]),
  ],
  [
    "a native script whose body does not decode",
    { WitnessNativeScriptMalformed: { script_index: 0n } },
    () =>
      withScriptWitnesses(
        encodeMidgardFieldPreimage([Buffer.from("820043820700", "hex")]),
      ),
  ],
];
