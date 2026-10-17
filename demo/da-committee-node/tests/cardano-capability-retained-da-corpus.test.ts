import { readFileSync } from "node:fs";
import { isAbsolute } from "node:path";

import {
  collectMidgardAttachedProgramEnvelopes,
  decodeMidgardCekProgramMaterialDaEntry,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardVersionedScriptListPreimage,
  hashMidgardVersionedScript,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { buildStrictRetainedDaPairFixture } from "../../midgard-fault-proofs/tests/helpers/cardano-capability-retained-da.js";
import {
  DaPayloadValidationError,
  decodeDaPayloadStrict,
  verifyDaPayloadAgainstHeader,
} from "../src/da/payload.js";

type CapabilityCorpusEntry = {
  readonly label: string;
  readonly transactionIdHex: string;
  readonly transactionCommitmentHex: string;
  readonly canonicalCborHex: string;
  readonly canonicalMaterialSidecarCborHex?: string;
  readonly sourceRawScriptAuditHash?: string;
  readonly productionAdmission:
    | "required"
    | "diagnostic-synthetic-script-witnesses";
  readonly resolvedReferenceUtxos?: readonly SDK.DaPayloadEntry[];
};

const scriptMaterialLabels = new Set([
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

const materialFor = (payload: SDK.DaPayload) =>
  payload.block_body.cek_program_material.map(([rootHex, valueHex]) =>
    decodeMidgardCekProgramMaterialDaEntry(
      Buffer.from(rootHex, "hex"),
      Buffer.from(valueHex, "hex"),
    ),
  );

describe("Cardano capability corpus production DA admission", () => {
  describe.each(corpus.entries)("$label", (boundary) => {
    it("enforces the exact generated row classification", async () => {
      const label = boundary.label;
      const canonicalCbor = Buffer.from(boundary.canonicalCborHex, "hex");
      const materialSidecar =
        boundary.canonicalMaterialSidecarCborHex === undefined
          ? undefined
          : Buffer.from(boundary.canonicalMaterialSidecarCborHex, "hex");
      const fixture = await buildStrictRetainedDaPairFixture({
        canonicalTransactionCbor: canonicalCbor,
        canonicalMaterialSidecarCbor: materialSidecar,
        resolvedReferenceUtxos: boundary.resolvedReferenceUtxos,
        withForcedTwin: false,
      });

      if (
        boundary.productionAdmission === "diagnostic-synthetic-script-witnesses"
      ) {
        expect(label).toBe("mixed-size-balanced");
        let rejection: unknown;
        try {
          decodeDaPayloadStrict(SDK.encodeDaPayload(fixture.payload));
        } catch (cause) {
          rejection = cause;
        }
        expect(rejection).toBeInstanceOf(DaPayloadValidationError);
        expect(rejection).toMatchObject({ code: "unsupported_feature" });
        expect((rejection as Error).message).toMatch(
          /E_SCRIPT_PROGRAM_ENCODING/u,
        );
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
      const envelopes = collectMidgardAttachedProgramEnvelopes(transaction);
      const verification = verifyMidgardCekProgramMaterialBundle(
        envelopes,
        materialFor(admitted.payload),
      );
      if (materialSidecar === undefined) {
        expect(envelopes).toHaveLength(0);
        expect(admitted.payload.block_body.cek_program_material).toEqual([]);
        expect(verification).toEqual([]);
      } else {
        expect(scriptMaterialLabels.has(label)).toBe(true);
        expect(boundary.sourceRawScriptAuditHash).toMatch(/^[0-9a-f]{56}$/u);
        const scripts = decodeMidgardVersionedScriptListPreimage(
          transaction.witnessSet.scriptTxWitsPreimageCbor,
        );
        expect(scripts).toHaveLength(1);
        expect(hashMidgardVersionedScript(scripts[0]!)).not.toBe(
          boundary.sourceRawScriptAuditHash,
        );
        expect(envelopes).toHaveLength(1);
        expect(verification).toHaveLength(1);
        expect(verification[0]!.nodeCount).toBe(envelopes[0]!.nodeCount);
        expect(verification[0]!.materialByteLength).toBe(
          envelopes[0]!.materialByteLength,
        );
      }
    }, 120_000);
  });
});
