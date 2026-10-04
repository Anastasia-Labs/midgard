import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { journey } from "midgard-watcher/tests/verification/forced-redeemer-first-fault.journey";
import { expect, it } from "vitest";

import { eventKeyFingerprint } from "../src/da/payload.source-event-fingerprints.js";
import { validateDaPayloadConsensus } from "../src/da/payload.validate-da-payload-consensus.js";
import { validateRetainedValidationWitnesses } from "../src/da/payload.validate-retained-validation-witnesses.js";
import { makePayloadFixture } from "./helpers.js";

it.each([
  { data: "1801", honest: "01" },
  { data: "d8799f1801ff", honest: "d8799f01ff" },
  { data: "d8799f011801ff", honest: "d8799f0101ff" },
  { data: "bf0101ff", honest: "a10101" },
  { data: "d8668218808101", honest: "d8668218809f01ff" },
])(
  "committee retains actual node malformed $data and canonical $honest traces",
  async ({ data, honest }) => {
    const base = await makePayloadFixture(1);
    for (const [bytes, missing, verdict] of [
      [data, true, "Rejected"],
      [honest, false, "Accepted"],
    ] as const) {
      const j = await journey(bytes, false, missing);
      expect(j.retained).toHaveLength(verdict === "Rejected" ? 1 : 2);
      const orderKey = Data.to(
        { transactionId: "33".repeat(32), outputIndex: 0n },
        SDK.OutputReference,
      );
      expect(() =>
        validateDaPayloadConsensus({
          ...base.payload.block_body,
          counts: {
            ...base.payload.block_body.counts,
            l2TransactionCount: 0n,
            forcedTransactionCount: 1n,
          },
          transactions: [],
          transaction_preimages: [],
          forced_transactions: [
            [orderKey, Data.to(j.leaf, SDK.ForcedInclusionTxV1)],
          ],
          forced_transaction_preimages: [[orderKey, j.txCbor.toString("hex")]],
        }),
      ).not.toThrow();
      const descriptors = new Map(
        j.retained.map((member) => [
          eventKeyFingerprint(member.eventKey),
          {
            keyCbor: member.keyCbor,
            descriptor: SDK.validationTraceDescriptorCoreFromData(member.value),
          },
        ]),
      );
      const rawTransactions = new Map(
        j.retained.map((member) => [
          eventKeyFingerprint(member.eventKey),
          "ForcedTransactionEventKey" in member.eventKey
            ? j.txCbor
            : j.normalTxCbor,
        ]),
      );
      const retained = j.retained.flatMap((member) => [...member.witnesses]);
      expect(
        j.retained.every((member) => member.value.verdict === verdict),
      ).toBe(true);
      expect(() =>
        validateRetainedValidationWitnesses(
          retained,
          descriptors,
          rawTransactions,
        ),
      ).not.toThrow();
      const changed = retained.map((entry) => [...entry] as SDK.DaPayloadEntry);
      const tampered = SDK.decodeRetainedValidationWitness(
        Buffer.from(changed[0]![1], "hex"),
      );
      tampered.machine_state.work_root = "00".repeat(32);
      changed[0]![1] =
        SDK.encodeRetainedValidationWitness(tampered).toString("hex");
      expect(() =>
        validateRetainedValidationWitnesses(
          changed,
          descriptors,
          rawTransactions,
        ),
      ).toThrow(/state\/proof does not open/);
    }
  },
);
