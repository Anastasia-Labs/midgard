import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  admitCompleteCanonicalReplayPredecessor,
  admitValidationTraceReplayContext,
  canonicalBlockEvidenceFromVerifiedPayload,
  VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
} from "@al-ft/midgard-fault-proofs";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { buildRetainedValidationBlockFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-reason-classifier";
import { GENESIS_HEADER_HASH } from "@al-ft/midgard-sdk";
import {
  makeNativeTx,
  makeRedeemersCbor,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { describe, expect, it } from "vitest";

// The dishonest operator can retain bytes normal intake refuses. Watcher replay
// must still reach its typed prerequisite instead of dying while decoding them.
describe("dishonest normal redeemer block replay", () => {
  it.each(["d8798101", "60"])(
    "maps refused redeemer %s through DirectValidationTraceUnavailable without a codec defect",
    async (dataHex) => {
      const provenance = {
        trustClass: "public_or_permissionless_da",
        sourceId: "retained-fixture/canonical-redeemer",
        grade: "security",
      } as const;
      const previous = await buildCanonicalBlockFixture({
        transactions: [],
        prevHeaderHash: GENESIS_HEADER_HASH,
      });
      const transaction = makeNativeTx({
        redeemerTxWitsPreimageCbor: makeRedeemersCbor([
          { tag: 0, index: 0n, data: Buffer.from(dataHex, "hex") },
        ]),
        scriptLanguages: ["PlutusV3"],
      });
      const block = await buildRetainedValidationBlockFixture({
        subject: {
          kind: "normal",
          nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
            transaction.txCbor,
          ),
        },
        priorLedgerRoot: previous.header.utxosRoot,
        prevHeaderHash: previous.headerHash,
        blockEndTimeMs: 1_750_000_000_000,
        blockSlot: 100n,
      });
      const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
        observation: authenticatedHeaderObservation(block),
        payloadEnvelopeCbor: block.payloadEnvelopeCbor,
        daProvenance: provenance,
      });
      const predecessor = await admitCompleteCanonicalReplayPredecessor({
        value: {
          observation: authenticatedHeaderObservation(previous),
          payloadEnvelopeCborHex: previous.payloadEnvelopeCbor.toString("hex"),
          daProvenance: provenance,
        },
        currentEvidence: evidence,
        minimumConfirmationDepth:
          DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
      });
      const validationTraceReplay = await admitValidationTraceReplayContext({
        evidence,
        predecessor,
      });
      const failure =
        await VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY.replay(
          evidence,
          { predecessor, validationTraceReplay },
        ).then(
          () => null,
          (error: unknown) => error,
        );
      expect(failure).toBeInstanceOf(Error);
      expect(failure).toMatchObject({
        name: "CanonicalReplayPrerequisiteError",
        failures: [
          {
            headerHash: evidence.headerHash,
            prerequisite: "representable_field_shape",
          },
        ],
      });
    },
  );
});
