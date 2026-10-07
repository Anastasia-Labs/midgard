import type { ShelleyGenesisSlotEvidence } from "@al-ft/midgard-core/ogmios-slot";

import {
  boundedNonEmptyString,
  canonicalAbsolutePath,
  exactKeysRecord,
  nonNegativeFiniteNumber,
  nonNegativeSafeInteger,
  positiveFiniteNumber,
  positiveSafeInteger,
  sha256Digest,
} from "../../artifact-schema.js";
import {
  type ArchitectureGCommitCandidateInput,
  type ArchitectureGCorpusFunding,
  type ArchitectureGFixtureCreation,
  architectureGFixtureDiagnosticKeys,
  sameJson,
} from "./mpf-commit-candidate-artifacts.architecture-gcommit-candidate-input.js";

export const assertArchitectureGCandidateSlotRuntimeIdentity = ({
  input,
  runtimeNetwork,
  customGenesis,
}: {
  readonly input: ArchitectureGCommitCandidateInput;
  readonly runtimeNetwork: "Mainnet" | "Preview" | "Preprod" | "Custom";
  readonly customGenesis?: ShelleyGenesisSlotEvidence;
}): void => {
  const document = input.forcedValidationSlotConfigArtifact.document;
  if (document.network !== runtimeNetwork) {
    throw new Error(
      "Architecture G candidate slot-config network does not match NodeConfig.NETWORK",
    );
  }
  if (runtimeNetwork !== "Custom") return;
  if (
    document.source.kind !== "local_ogmios_genesis" ||
    customGenesis === undefined ||
    document.source.configurationSha256 !== customGenesis.configurationSha256 ||
    JSON.stringify(document.slotConfig) !==
      JSON.stringify({
        zeroTime: customGenesis.startTimeMs,
        zeroSlot: 0,
        slotLength: customGenesis.slotLengthMs,
      })
  ) {
    throw new Error(
      "Architecture G Custom slot configuration does not match the live configured Ogmios genesis",
    );
  }
};

export const decodeArchitectureGFixtureCreation = ({
  value,
  expectedFixturePath,
  expectedMarker,
  expectedUtxos,
  expectedAggregate,
  expectedFundingMapSha256,
}: {
  readonly value: unknown;
  readonly expectedFixturePath: string;
  readonly expectedMarker: string;
  readonly expectedUtxos: number;
  readonly expectedAggregate: {
    readonly entryCount: number;
    readonly encodedTupleBytes: number;
  };
  readonly expectedFundingMapSha256: string | null;
}): ArchitectureGFixtureCreation => {
  const artifact = exactKeysRecord(
    value,
    "Architecture G fixture-creation artifact",
    [
      "fixtureCreated",
      "fixturePath",
      "initialUtxoCount",
      "marker",
      "durationMs",
      "diagnostics",
      "utxoPayloadAggregate",
      "canonicalFunding",
    ],
  );
  const aggregate = exactKeysRecord(
    artifact.utxoPayloadAggregate,
    "Architecture G fixture payload aggregate",
    ["entryCount", "encodedTupleBytes"],
  );
  const diagnostics = exactKeysRecord(
    artifact.diagnostics,
    "Architecture G fixture diagnostics",
    architectureGFixtureDiagnosticKeys,
  );
  for (const field of architectureGFixtureDiagnosticKeys) {
    if (field.endsWith("Ms")) {
      nonNegativeFiniteNumber(
        diagnostics[field],
        `fixtureCreation.diagnostics.${field}`,
      );
    } else {
      nonNegativeSafeInteger(
        diagnostics[field],
        `fixtureCreation.diagnostics.${field}`,
      );
    }
  }
  const fixturePath = canonicalAbsolutePath(
    artifact.fixturePath,
    "fixtureCreation.fixturePath",
  );
  const expectedPath = canonicalAbsolutePath(
    expectedFixturePath,
    "expectedFixturePath",
  );
  const marker = sha256Digest(artifact.marker, "fixtureCreation.marker");
  const expectedRoot = sha256Digest(expectedMarker, "expectedMarker");
  const initialUtxoCount = positiveSafeInteger(
    artifact.initialUtxoCount,
    "fixtureCreation.initialUtxoCount",
  );
  const expectedCount = positiveSafeInteger(expectedUtxos, "expectedUtxos");
  const aggregateEntryCount = positiveSafeInteger(
    aggregate.entryCount,
    "fixtureCreation.utxoPayloadAggregate.entryCount",
  );
  const aggregateEncodedTupleBytes = positiveSafeInteger(
    aggregate.encodedTupleBytes,
    "fixtureCreation.utxoPayloadAggregate.encodedTupleBytes",
  );
  const expectedAggregateEntryCount = positiveSafeInteger(
    expectedAggregate.entryCount,
    "expectedAggregate.entryCount",
  );
  const expectedAggregateEncodedTupleBytes = positiveSafeInteger(
    expectedAggregate.encodedTupleBytes,
    "expectedAggregate.encodedTupleBytes",
  );
  positiveFiniteNumber(artifact.durationMs, "fixtureCreation.durationMs");
  let canonicalFundingSha256: string | null = null;
  if (expectedFundingMapSha256 === null) {
    if (artifact.canonicalFunding !== null) {
      throw new Error(
        "Architecture G fixture creation unexpectedly claims canonical funding",
      );
    }
  } else {
    const fundingMapSha256 = sha256Digest(
      expectedFundingMapSha256,
      "expectedFundingMapSha256",
    );
    const canonicalFunding = exactKeysRecord(
      artifact.canonicalFunding,
      "Architecture G fixture canonical-funding identity",
      ["path", "sha256", "entryCount"],
    );
    canonicalAbsolutePath(
      canonicalFunding.path,
      "fixtureCreation.canonicalFunding.path",
    );
    canonicalFundingSha256 = sha256Digest(
      canonicalFunding.sha256,
      "fixtureCreation.canonicalFunding.sha256",
    );
    positiveSafeInteger(
      canonicalFunding.entryCount,
      "fixtureCreation.canonicalFunding.entryCount",
    );
    if (canonicalFundingSha256 !== fundingMapSha256) {
      throw new Error(
        "Architecture G fixture creation canonical funding SHA-256 drifted",
      );
    }
  }
  if (
    artifact.fixtureCreated !== true ||
    fixturePath !== expectedPath ||
    marker !== expectedRoot ||
    initialUtxoCount !== expectedCount ||
    aggregateEntryCount !== expectedCount ||
    aggregateEntryCount !== expectedAggregateEntryCount ||
    aggregateEncodedTupleBytes !== expectedAggregateEncodedTupleBytes
  ) {
    throw new Error(
      "Architecture G fixture creation does not bind the candidate path, root, cardinality, payload aggregate, and canonical funding",
    );
  }
  return artifact as ArchitectureGFixtureCreation;
};

export const decodeArchitectureGCorpusFunding = ({
  value,
  expectedCorpusSha256,
  expectedSliceSha256,
  expectedFundingRoots,
}: {
  readonly value: unknown;
  readonly expectedCorpusSha256: string;
  readonly expectedSliceSha256: string;
  readonly expectedFundingRoots?: readonly {
    readonly walletId: string;
    readonly outref: string;
  }[];
}): ArchitectureGCorpusFunding => {
  sha256Digest(expectedCorpusSha256, "expectedCorpusSha256");
  sha256Digest(expectedSliceSha256, "expectedSliceSha256");
  const funding = exactKeysRecord(value, "Architecture G corpus funding", [
    "schemaVersion",
    "corpusSha256",
    "sliceSha256",
    "entries",
  ]);
  if (
    funding.schemaVersion !== "midgard-architecture-g-corpus-funding-v1" ||
    funding.corpusSha256 !== expectedCorpusSha256 ||
    funding.sliceSha256 !== expectedSliceSha256 ||
    !Array.isArray(funding.entries) ||
    funding.entries.length === 0
  ) {
    throw new Error("Architecture G corpus funding identity is invalid");
  }
  const walletIds = new Set<string>();
  const outrefs = new Set<string>();
  const identities: { readonly walletId: string; readonly outref: string }[] =
    [];
  for (const [index, value] of funding.entries.entries()) {
    const entry = exactKeysRecord(
      value,
      `Architecture G funding entry ${index.toString()}`,
      ["walletId", "outref", "outputCbor"],
    );
    const walletId = boundedNonEmptyString(
      entry.walletId,
      `funding.entries[${index.toString()}].walletId`,
    );
    const outref = boundedNonEmptyString(
      entry.outref,
      `funding.entries[${index.toString()}].outref`,
    );
    const outputCbor = boundedNonEmptyString(
      entry.outputCbor,
      `funding.entries[${index.toString()}].outputCbor`,
      1_048_576,
    );
    if (
      walletIds.has(walletId) ||
      outrefs.has(outref) ||
      outref !== outref.toLowerCase() ||
      outputCbor !== outputCbor.toLowerCase() ||
      !/^[0-9a-f]{64}#(?:0|[1-9]\d*)$/u.test(outref) ||
      outputCbor.length % 2 !== 0 ||
      outputCbor.length > 1_048_576 ||
      Buffer.from(outputCbor, "hex").toString("hex") !== outputCbor
    ) {
      throw new Error(
        `Architecture G funding entry ${index.toString()} is invalid or duplicated`,
      );
    }
    walletIds.add(walletId);
    outrefs.add(outref);
    identities.push({ walletId, outref });
  }
  if (expectedFundingRoots !== undefined) {
    if (
      !Array.isArray(expectedFundingRoots) ||
      expectedFundingRoots.length === 0 ||
      !sameJson(identities, expectedFundingRoots)
    ) {
      throw new Error(
        "Architecture G corpus funding entries do not match the selected corpus roots",
      );
    }
    for (const [index, value] of expectedFundingRoots.entries()) {
      const expected = exactKeysRecord(
        value,
        `Expected Architecture G funding root ${index.toString()}`,
        ["walletId", "outref"],
      );
      boundedNonEmptyString(
        expected.walletId,
        `expectedFundingRoots[${index.toString()}].walletId`,
      );
      if (
        typeof expected.outref !== "string" ||
        !/^[0-9a-f]{64}#(?:0|[1-9]\d*)$/u.test(expected.outref)
      ) {
        throw new Error(
          `expectedFundingRoots[${index.toString()}].outref is invalid`,
        );
      }
    }
  }
  return funding as ArchitectureGCorpusFunding;
};
