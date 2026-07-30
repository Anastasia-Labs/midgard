import { readFileSync } from "node:fs";
import { isAbsolute } from "node:path";

import {
  collectMidgardV1AttachedProgramEnvelopes,
  decodeMidgardCekProgramMaterialDaEntryV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScriptListPreimage,
  encodeMidgardCekProgramMaterialDaValueV1,
  encodeMidgardTxOutput,
  hashMidgardVersionedScript,
  verifyMidgardCekProgramMaterialBundleV1,
} from "@al-ft/midgard-core";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { buildStrictRetainedDaPairFixtureV1 } from "../../midgard-fault-proofs/tests/helpers/cardano-capability-retained-da-v1.js";
import {
  DaPayloadValidationError,
  decodeDaPayloadV1Strict,
  verifyDaPayloadV1AgainstHeader,
} from "../src/da/payload.js";

type CapabilityCorpusEntryV1 = {
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
  readonly entries: readonly CapabilityCorpusEntryV1[];
};

const corpusEntryFor = (label: string): CapabilityCorpusEntryV1 => {
  const matches = corpus.entries.filter((entry) => entry.label === label);
  if (matches.length !== 1) {
    throw new Error(`expected exactly one capability corpus row for ${label}`);
  }
  return matches[0]!;
};

const materialFor = (payload: SDK.DaPayloadV1) =>
  payload.block_body.cek_program_material.map(([rootHex, valueHex]) =>
    decodeMidgardCekProgramMaterialDaEntryV1(
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
      const fixture = await buildStrictRetainedDaPairFixtureV1({
        canonicalTransactionCbor: canonicalCbor,
        canonicalMaterialSidecarCbor: materialSidecar,
        resolvedReferenceUtxos: boundary.resolvedReferenceUtxos,
      });

      if (
        boundary.productionAdmission === "diagnostic-synthetic-script-witnesses"
      ) {
        expect(label).toBe("mixed-size-balanced");
        let rejection: unknown;
        try {
          decodeDaPayloadV1Strict(SDK.encodeDaPayloadV1(fixture.payload));
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
      const admitted = await verifyDaPayloadV1AgainstHeader(
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
        decodeMidgardNativeTxFullV1FromCanonicalCbor(canonicalCbor);
      const envelopes = collectMidgardV1AttachedProgramEnvelopes(transaction);
      const verification = verifyMidgardCekProgramMaterialBundleV1(
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

  it("rejects missing, mutated, or wrongly keyed typed material", async () => {
    const boundary = corpusEntryFor("maximum-list-redeemer-breadth");
    const canonicalCbor = Buffer.from(boundary.canonicalCborHex, "hex");
    const materialSidecar = Buffer.from(
      boundary.canonicalMaterialSidecarCborHex!,
      "hex",
    );
    const fixture = await buildStrictRetainedDaPairFixtureV1({
      canonicalTransactionCbor: canonicalCbor,
      canonicalMaterialSidecarCbor: materialSidecar,
    });
    const [rootHex, valueHex] =
      fixture.payload.block_body.cek_program_material[0]!;
    const [entry] = materialFor(fixture.payload);
    const mutatedPreimage = Buffer.from(entry!.preimage);
    mutatedPreimage[mutatedPreimage.length - 1] ^= 0x01;

    const malformedMaterialSets: readonly SDK.DaPayloadEntry[][] = [
      [],
      [
        [
          rootHex,
          encodeMidgardCekProgramMaterialDaValueV1({
            kind: entry!.kind,
            preimage: mutatedPreimage,
          }).toString("hex"),
        ],
        ...fixture.payload.block_body.cek_program_material.slice(1),
      ],
      [
        ["ff".repeat(32), valueHex] satisfies SDK.DaPayloadEntry,
        ...fixture.payload.block_body.cek_program_material.slice(1),
      ].sort(([left], [right]) => left.localeCompare(right)),
    ];

    for (const cekProgramMaterial of malformedMaterialSets) {
      const malformed: SDK.DaPayloadV1 = {
        ...fixture.payload,
        block_body: {
          ...fixture.payload.block_body,
          cek_program_material: cekProgramMaterial,
        },
      };
      let rejection: unknown;
      try {
        decodeDaPayloadV1Strict(SDK.encodeDaPayloadV1(malformed));
      } catch (cause) {
        rejection = cause;
      }
      expect(rejection).toBeInstanceOf(DaPayloadValidationError);
      expect(rejection).toMatchObject({ code: "coverage_mismatch" });
      expect((rejection as Error).message).toMatch(
        /CEK program material does not exactly cover/u,
      );
    }
  });

  it("rejects missing or substituted resolved reference UTxO material", async () => {
    const boundary = corpusEntryFor("maximum-reference-inputs");
    const canonicalCbor = Buffer.from(boundary.canonicalCborHex, "hex");
    const fixture = await buildStrictRetainedDaPairFixtureV1({
      canonicalTransactionCbor: canonicalCbor,
      resolvedReferenceUtxos: boundary.resolvedReferenceUtxos,
    });
    const missing: SDK.DaPayloadV1 = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        utxos: [],
      },
    };
    expect(() =>
      decodeDaPayloadV1Strict(SDK.encodeDaPayloadV1(missing)),
    ).toThrow(
      expect.objectContaining({
        code: "malformed_transaction",
      }),
    );

    const [first, ...remaining] = fixture.payload.block_body.utxos;
    expect(first).toBeDefined();
    const decodedOutput = decodeMidgardTxOutput(Buffer.from(first![1], "hex"));
    const substitutedOutput = encodeMidgardTxOutput({
      ...decodedOutput,
      value: {
        ...decodedOutput.value,
        lovelace: decodedOutput.value.lovelace + 1n,
      },
    });
    const substituted: SDK.DaPayloadV1 = {
      ...fixture.payload,
      block_body: {
        ...fixture.payload.block_body,
        utxos: [[first![0], substitutedOutput.toString("hex")], ...remaining],
      },
    };
    await expect(
      verifyDaPayloadV1AgainstHeader(
        await wrapDaPayloadV1(SDK.encodeDaPayloadV1(substituted), {
          mode: "identity",
        }),
        fixture.headerHash,
        fixture.header,
        {
          payloadSchemaVersion: 1,
          stateQueueOutRef: `${"00".repeat(32)}#0`,
        },
      ),
    ).rejects.toMatchObject({ code: "root_mismatch" });
  }, 30_000);
});
