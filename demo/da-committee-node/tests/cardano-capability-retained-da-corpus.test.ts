import { readFileSync } from "node:fs";
import { isAbsolute } from "node:path";

import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardVersionedScriptListPreimage,
  hashMidgardVersionedScript,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { RejectCodes, runPhaseAValidation } from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { buildStrictRetainedDaPairFixture } from "../../midgard-fault-proofs/tests/helpers/cardano-capability-retained-da.js";
import { verifyDaPayloadAgainstHeader } from "../src/da/payload.js";

type CapabilityCorpusEntry = {
  readonly label: string;
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
  readonly canonicalCborHex: string;
  readonly sourceScriptHash?: string;
  readonly productionAdmission:
    | "required"
    | "diagnostic-synthetic-script-witnesses";
  readonly resolvedReferenceUtxos?: readonly SDK.DaPayloadEntry[];
};

const plutusScriptLabels = new Set([
  "balanced-nested-redeemer",
  "maximum-constructor-redeemer-breadth",
  "maximum-list-redeemer-breadth",
  "maximum-map-redeemer-breadth",
  "maximum-redeemers",
]);

const boundaryCorpusInput = (): string | URL => {
  const override = process.env.MIDGARD_BOUNDARY_CORPUS_JSON;
  if (override === undefined) {
    return new URL(
      "../../midgard-fault-proofs/tests/fixtures/cardano-capability-p2-boundary-corpus-v1.json",
      import.meta.url,
    );
  }
  if (!isAbsolute(override)) {
    throw new Error("MIDGARD_BOUNDARY_CORPUS_JSON must be an absolute path");
  }
  return override;
};

const corpus = JSON.parse(readFileSync(boundaryCorpusInput(), "utf8")) as {
  readonly entries: readonly CapabilityCorpusEntry[];
};

describe("Cardano capability corpus production DA admission", () => {
  describe.each(corpus.entries)("$label", (boundary) => {
    it("enforces the exact generated row classification", async () => {
      const label = boundary.label;
      const canonicalCbor = Buffer.from(boundary.canonicalCborHex, "hex");
      const fixture = await buildStrictRetainedDaPairFixture({
        canonicalTransactionCbor: canonicalCbor,
        resolvedReferenceUtxos: boundary.resolvedReferenceUtxos,
        withForcedTwin: false,
      });

      if (
        boundary.productionAdmission === "diagnostic-synthetic-script-witnesses"
      ) {
        expect(label).toBe("mixed-size-balanced");
        // DA framing is independent of script validity, so this diagnostic
        // row is refused by admission, at its first fault in machine phase
        // order: its spend and reference inputs share an outref, and the
        // input-set phase runs before the carried-script decoding phases.
        const phaseA = await Effect.runPromise(
          runPhaseAValidation(
            [
              {
                txId: Buffer.from(boundary.transactionIdHex, "hex"),
                txCbor: canonicalCbor,
                arrivalSeq: 0n,
                createdAt: new Date(0),
              },
            ],
            {
              consensusProfile: MIDGARD_CONSENSUS_PROFILE,
              expectedNetworkId: 0n,
              minFeeA: 0n,
              minFeeB: 0n,
              concurrency: 1,
              strictnessProfile: "phase1_midgard",
            },
          ),
        );
        expect(phaseA.accepted).toEqual([]);
        expect(phaseA.rejected).toEqual([
          expect.objectContaining({
            code: RejectCodes.DuplicateInputInTx,
            consensusPhase: "inputSets",
            subject: expect.objectContaining({ arm: "DuplicateInput" }),
          }),
        ]);
        return;
      }

      expect(boundary.productionAdmission).toBe("required");
      const admitted = await verifyDaPayloadAgainstHeader(
        fixture.payloadEnvelopeCbor,
        fixture.headerHash,
        fixture.header,
        {
          payloadSchemaVersion: 1,
          stateQueueOutRef: `${"00".repeat(32)}#0`,
        },
      );
      expect(admitted.payload).toEqual(fixture.payload);
      expect({
        transactionIdHex: fixture.transactionIdHex,
        transactionCommitmentHex: fixture.transactionCommitmentHex,
      }).toEqual({
        transactionIdHex: boundary.transactionIdHex,
        transactionCommitmentHex: boundary.transactionCommitmentHex,
      });

      const transaction =
        decodeMidgardNativeTxFullFromCanonicalCbor(canonicalCbor);
      const scripts = decodeMidgardVersionedScriptListPreimage(
        transaction.witnessSet.scriptTxWitsPreimageCbor,
      );
      if (boundary.sourceScriptHash === undefined) {
        expect(
          scripts.filter(({ language }) => language !== "NativeCardano"),
        ).toEqual([]);
      } else {
        expect(plutusScriptLabels.has(label)).toBe(true);
        expect(boundary.sourceScriptHash).toMatch(/^[0-9a-f]{56}$/u);
        expect(scripts).toHaveLength(1);
        expect(hashMidgardVersionedScript(scripts[0]!)).toBe(
          boundary.sourceScriptHash,
        );
      }
    }, 120_000);
  });
});
