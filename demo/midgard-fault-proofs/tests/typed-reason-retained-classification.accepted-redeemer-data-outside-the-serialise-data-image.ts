import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardRedeemerWitnessItem,
} from "@al-ft/midgard-core/codec";
import { GENESIS_HEADER_HASH } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  createCompleteCanonicalReplayUnion,
  REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
  VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  buildRetainedValidationBlockFixture,
  classifyRetainedReasonFixture,
} from "./support/retained-reason-classifier.js";
import {
  base,
  deploymentFingerprint,
  output,
  releaseFinalityAuthority,
} from "./typed-reason-retained-classification.reason-case.js";

describe("accepted redeemer data outside the serialiseData image", () => {
  it.each(["d8798101", "d87980"])(
    "classifies an accepted normal transaction with redeemer data %s through the validation replay",
    async (redeemerData) => {
      const predecessor = await buildCanonicalBlockFixture({
        transactions: [],
        prevHeaderHash: GENESIS_HEADER_HASH,
        utxos: [{ key: outRefCbor(71, 0n), value: output() }],
      });
      const transaction = buildFixtureTransaction({
        ...base,
        redeemerWitnesses: [
          encodeMidgardRedeemerWitnessItem({
            purpose: "Spend",
            index: 0n,
            redeemerCbor: Buffer.from(redeemerData, "hex"),
            executionUnits: { memory: 1n, steps: 2n },
          }),
        ],
      });
      // The operator commits the transaction as Accepted. The validation
      // replay has no bounded trace for the non-canonical spelling, so the
      // block classifies only if the redeemer finding covers that gap.
      const fixture = await buildRetainedValidationBlockFixture({
        subject: {
          kind: "normal",
          nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
            transaction.canonicalCbor,
          ),
        },
        priorLedgerRoot: predecessor.header.utxosRoot,
        prevHeaderHash: predecessor.headerHash,
        blockEndTimeMs: 1_900_000_000_000,
        blockSlot: 0n,
      });
      const { decision } = await classifyRetainedReasonFixture({
        observation: authenticatedHeaderObservation(fixture),
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        deploymentFingerprint,
        releaseFinalityAuthority,
        replayer: createCompleteCanonicalReplayUnion([
          VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
          REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
        ]),
        predecessor: {
          observation: authenticatedHeaderObservation(predecessor),
          payloadEnvelopeCbor: predecessor.payloadEnvelopeCbor,
        },
      });
      expect(decision).toMatchObject(
        redeemerData === "d8798101"
          ? {
              decision: "fault_detected",
              category: "redeemerCanonicity",
              violationId: "redeemer-malformed",
              position: "0",
              headerHash: fixture.headerHash,
            }
          : { decision: "healthy", headerHash: fixture.headerHash },
      );
    },
  );
});
