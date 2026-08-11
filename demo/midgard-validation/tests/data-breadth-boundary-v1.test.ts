import { readFileSync } from "node:fs";
import { isDeepStrictEqual } from "node:util";

import {
  advanceMidgardCekDataTraverseV1,
  advanceMidgardRedeemerItemProofV1,
  buildMidgardLedgerOutputProofTraceV1,
  buildMidgardRedeemerItemProofTraceV1,
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
  computeScriptIntegrityHashForLanguages,
  decodeMidgardCekProgramMaterialSidecarV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScriptListPreimage,
  encodeMidgardCekDataFrameV1,
  encodeMidgardCekDataTraverseControlV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardVersionedScriptListPreimage,
  finalizeMidgardCekDataTraverseV1,
  finalizeMidgardRedeemerItemProofV1,
  hashMidgardVersionedScript,
  isExactMidgardLedgerOutputProofTerminalV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN_V1,
  midgardFieldCommitmentV1,
  midgardNativeTxFullToCardanoTxEncoding,
  midgardRedeemerItemDescriptorV1,
  MidgardRedeemerItemProofModesV1,
  nextMidgardCekDataTraverseSpanV1,
  nextMidgardRedeemerItemProofSpanV1,
  validateMidgardConsensusV1Tx,
  verifyMidgardCekProgramMaterialBundleV1,
} from "@al-ft/midgard-core";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import {
  applyDoubleCborEncoding,
  CML,
  Data,
  Emulator,
  Lucid,
  type SpendingValidator,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { buildMidgardCanonicalScriptArtifactV1 } from "../src/cek-program.js";
import { decodeMidgardRedeemers } from "../src/midgard-redeemers.js";
import { countedMachineFieldTraceV1 } from "../src/validation-machine.js";
import {
  buildCollateralFreeMidgardSchemaParallelCandidateV1,
  buildSignedCardanoNestedDatumCandidateV1,
  buildSignedCardanoSpendRedeemersCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  CARDANO_BOUNDARY_TOTAL_COLLATERAL_V1,
  deterministicCardanoBoundaryPrivateKeyV1,
  findSignedCardanoCollectionBoundaryV1,
  measureCollateralizedPlutusFeasibilityCandidateV1,
  measureMidgardCompleteItemCarriageFitV1,
  measureSignedCardanoNestedDatumV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";
import { exerciseMidgardRetainedDaCanonicalBoundaryV1 } from "./helpers/retained-da-boundary-v1.js";

type DataBreadthKind = "constructor" | "list" | "map";

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
};

const alwaysSucceedsBlueprint = JSON.parse(
  readFileSync(
    new URL(
      "../../midgard-node/blueprints/always-succeeds/plutus.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as {
  readonly validators: readonly BlueprintValidator[];
};

const alwaysSucceedsCompiledCode = alwaysSucceedsBlueprint.validators.find(
  (validator) => validator.title === "midgard.deposit_spend.else",
)?.compiledCode;
if (alwaysSucceedsCompiledCode === undefined) {
  throw new Error(
    "Missing always-succeeds blueprint entry midgard.deposit_spend.else",
  );
}

const spendingScript: SpendingValidator = {
  type: "PlutusV3",
  script: applyDoubleCborEncoding(alwaysSucceedsCompiledCode),
};

const cborUnsignedHex = (value: number): string => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("Data breadth integer must be non-negative");
  }
  if (value < 24) return value.toString(16).padStart(2, "0");
  if (value <= 0xff) {
    return `18${value.toString(16).padStart(2, "0")}`;
  }
  if (value <= 0xffff) {
    return `19${value.toString(16).padStart(4, "0")}`;
  }
  return `1a${value.toString(16).padStart(8, "0")}`;
};

const cborMapHeaderHex = (pairCount: number): string => {
  if (!Number.isSafeInteger(pairCount) || pairCount <= 0) {
    throw new Error("Data map breadth must be positive");
  }
  if (pairCount < 24) {
    return (0xa0 + pairCount).toString(16);
  }
  if (pairCount <= 0xff) {
    return `b8${pairCount.toString(16).padStart(2, "0")}`;
  }
  if (pairCount <= 0xffff) {
    return `b9${pairCount.toString(16).padStart(4, "0")}`;
  }
  return `ba${pairCount.toString(16).padStart(8, "0")}`;
};

const cardanoBreadthDataCborV1 = (
  kind: DataBreadthKind,
  breadth: number,
): string => {
  if (!Number.isSafeInteger(breadth) || breadth <= 0) {
    throw new Error("Cardano Data breadth must be positive");
  }
  if (kind === "list") {
    return `9f${"00".repeat(breadth)}ff`;
  }
  if (kind === "constructor") {
    return `d8668218809f${"00".repeat(breadth)}ff`;
  }
  const entries = Array.from(
    { length: breadth },
    (_, index) => `${cborUnsignedHex(index)}00`,
  ).join("");
  return `${cborMapHeaderHex(breadth)}${entries}`;
};

const dataNodeCount = (kind: DataBreadthKind, breadth: number): number =>
  kind === "map" ? breadth * 2 + 1 : breadth + 1;

type ProductionDataTraverseStepV1 = {
  readonly control: Parameters<
    typeof advanceMidgardCekDataTraverseV1
  >[0]["control"];
  readonly action: Parameters<
    typeof advanceMidgardCekDataTraverseV1
  >[0]["action"];
  readonly next: Parameters<
    typeof advanceMidgardCekDataTraverseV1
  >[0]["control"];
};

const unsignedByteLength = (value: number): number => {
  let size = 1;
  let remaining = value;
  while (remaining >= 256) {
    size += 1;
    remaining = Math.floor(remaining / 256);
  }
  return size;
};

const integerDataMemory = (value: number): bigint =>
  BigInt(4 + unsignedByteLength(value * 2));

const exactBreadthMemory = (kind: DataBreadthKind, breadth: number): bigint => {
  if (kind !== "map") return 4n + BigInt(breadth) * 5n;
  let memory = 4n;
  for (let index = 0; index < breadth; index += 1) {
    memory += integerDataMemory(index) + 5n;
  }
  return memory;
};

const assertExactBreadthSemantics = (
  kind: DataBreadthKind,
  breadth: number,
  cborHex: string,
): void => {
  const data = CML.PlutusData.from_cbor_hex(cborHex);
  expect(data.to_cbor_hex()).toBe(cborHex);
  if (kind === "constructor") {
    const constructor = data.as_constr_plutus_data();
    expect(constructor?.alternative()).toBe(128n);
    const fields = constructor?.fields();
    expect(fields?.len()).toBe(breadth);
    for (let index = 0; index < breadth; index += 1) {
      expect(fields!.get(index).to_cbor_hex()).toBe("00");
    }
    return;
  }
  if (kind === "list") {
    const list = data.as_list();
    expect(list?.len()).toBe(breadth);
    for (let index = 0; index < breadth; index += 1) {
      expect(list!.get(index).to_cbor_hex()).toBe("00");
    }
    return;
  }
  const map = data.as_map();
  expect(map?.len()).toBe(breadth);
  const keys = map!.keys();
  expect(keys.len()).toBe(breadth);
  for (let index = 0; index < breadth; index += 1) {
    const key = keys.get(index);
    expect(key.to_cbor_hex()).toBe(cborUnsignedHex(index));
    expect(key.as_integer()?.as_u64()).toBe(BigInt(index));
    const values = map!.get_all(key);
    expect(values?.len()).toBe(1);
    expect(values!.get(0).to_cbor_hex()).toBe("00");
  }
};

const assertExactProductionFoldSemantics = ({
  kind,
  breadth,
  steps,
}: {
  readonly kind: DataBreadthKind;
  readonly breadth: number;
  readonly steps: readonly ProductionDataTraverseStepV1[];
}): void => {
  const folds = steps.flatMap(({ action }) =>
    action?.kind === "foldList" || action?.kind === "foldMap" ? [action] : [],
  );
  if (folds.length !== breadth) {
    throw new Error(
      `${kind} production fold count ${folds.length.toString()} != ${breadth.toString()}`,
    );
  }
  const zeroRoots = new Set<string>();
  const keyRoots = new Set<string>();
  for (let position = 0; position < folds.length; position += 1) {
    const action = folds[position]!;
    const expectedIndex = breadth - position - 1;
    const expectedChildren = kind === "map" ? breadth * 2 : breadth;
    if (
      action.frame.expectedChildren !== expectedChildren ||
      action.frame.childCount !== expectedChildren ||
      action.frame.foldCursor !== position
    ) {
      throw new Error(
        `${kind} production frame lost exact child count or cursor at ${position.toString()}`,
      );
    }
    if (kind === "map") {
      if (
        action.kind !== "foldMap" ||
        action.pairIndex !== expectedIndex ||
        action.key.cborLength !==
          BigInt(cborUnsignedHex(expectedIndex).length / 2) ||
        action.key.memory !== integerDataMemory(expectedIndex) ||
        action.value.cborLength !== 1n ||
        action.value.memory !== 5n
      ) {
        throw new Error(
          `map production fold lost pair/key/value identity at ${expectedIndex.toString()}`,
        );
      }
      keyRoots.add(Buffer.from(action.key.root).toString("hex"));
      zeroRoots.add(Buffer.from(action.value.root).toString("hex"));
    } else {
      if (
        action.kind !== "foldList" ||
        action.childIndex !== expectedIndex ||
        action.child.cborLength !== 1n ||
        action.child.memory !== 5n
      ) {
        throw new Error(
          `${kind} production fold lost child identity at ${expectedIndex.toString()}`,
        );
      }
      zeroRoots.add(Buffer.from(action.child.root).toString("hex"));
    }
  }
  if (zeroRoots.size !== 1 || (kind === "map" && keyRoots.size !== breadth)) {
    throw new Error(`${kind} production fold lost exact scalar identities`);
  }
};

const assertExactTerminalSummary = ({
  kind,
  breadth,
  dataCborHex,
  summary,
}: {
  readonly kind: DataBreadthKind;
  readonly breadth: number;
  readonly dataCborHex: string;
  readonly summary: {
    readonly root: Uint8Array;
    readonly cborLength: bigint;
    readonly memory: bigint;
  };
}): void => {
  expect(Buffer.from(summary.root)).toHaveLength(32);
  expect(summary.cborLength).toBe(BigInt(dataCborHex.length / 2));
  expect(summary.memory).toBe(exactBreadthMemory(kind, breadth));
};

const jsonSummary = (summary: {
  readonly root: Uint8Array;
  readonly cborLength: bigint;
  readonly memory: bigint;
}) => ({
  rootHex: Buffer.from(summary.root).toString("hex"),
  cborLength: summary.cborLength.toString(),
  memory: summary.memory.toString(),
});

const jsonFrame = (
  frame: Parameters<typeof encodeMidgardCekDataFrameV1>[0],
) => ({
  cborHex: encodeMidgardCekDataFrameV1(frame).toString("hex"),
  kind: frame.kind,
  ...(frame.kind === "constrSmall"
    ? { constructor: frame.constructor.toString() }
    : frame.kind === "constrLarge"
      ? {
          constructorCborRootHex: Buffer.from(
            frame.constructorCborRoot,
          ).toString("hex"),
          constructorCborLength: frame.constructorCborLength.toString(),
          constructorMemory: frame.constructorMemory.toString(),
        }
      : {}),
  tailHex: Buffer.from(frame.tail).toString("hex"),
  expectedChildren: frame.expectedChildren,
  childCount: frame.childCount,
  childPeaks: frame.childFrontier.peaks.map(({ height, hash }) => ({
    height,
    hashHex: Buffer.from(hash).toString("hex"),
  })),
  foldCursor: frame.foldCursor,
  sequence: {
    rootHex: Buffer.from(frame.sequence.root).toString("hex"),
    length: frame.sequence.length.toString(),
    payloadCborLength: frame.sequence.payloadCborLength.toString(),
    memory: frame.sequence.memory.toString(),
  },
});

const exactBroadFrontierVector = (
  kind: DataBreadthKind,
  steps: readonly ProductionDataTraverseStepV1[],
) => {
  let step: ProductionDataTraverseStepV1 | undefined;
  let membershipDepth = -1;
  for (const candidate of steps) {
    const action = candidate.action;
    const candidateDepth =
      kind === "map"
        ? action?.kind === "foldMap" &&
          action.keySiblings.length > 0 &&
          action.valueSiblings.length > 0
          ? action.keySiblings.length + action.valueSiblings.length
          : -1
        : action?.kind === "foldList"
          ? action.siblings.length
          : -1;
    if (candidateDepth > membershipDepth) {
      step = candidate;
      membershipDepth = candidateDepth;
    }
  }
  if (step?.action?.kind !== "foldList" && step?.action?.kind !== "foldMap") {
    throw new Error("Broad Data trace lost its frontier fold");
  }
  expect(membershipDepth).toBeGreaterThan(0);
  const action = step.action;
  const mutatedAction =
    action.kind === "foldList"
      ? {
          ...action,
          childIndex: action.childIndex - 1,
        }
      : {
          ...action,
          pairIndex: action.pairIndex - 1,
        };
  expect(
    advanceMidgardCekDataTraverseV1({
      control: step.control,
      sourceBytes: null,
      action: mutatedAction,
    }),
  ).toBeNull();
  const mutateFirstSibling = (
    siblings: readonly Uint8Array[],
  ): readonly Buffer[] => {
    expect(siblings.length).toBeGreaterThan(0);
    const mutatedFirst = Buffer.from(siblings[0]!);
    mutatedFirst[0] = mutatedFirst[0]! ^ 0x01;
    return [
      mutatedFirst,
      ...siblings.slice(1).map((sibling) => Buffer.from(sibling)),
    ];
  };
  if (action.kind === "foldList") {
    expect(
      advanceMidgardCekDataTraverseV1({
        control: step.control,
        sourceBytes: null,
        action: {
          ...action,
          siblings: mutateFirstSibling(action.siblings),
        },
      }),
    ).toBeNull();
  } else {
    expect(
      advanceMidgardCekDataTraverseV1({
        control: step.control,
        sourceBytes: null,
        action: {
          ...action,
          keySiblings: mutateFirstSibling(action.keySiblings),
        },
      }),
    ).toBeNull();
    expect(
      advanceMidgardCekDataTraverseV1({
        control: step.control,
        sourceBytes: null,
        action: {
          ...action,
          valueSiblings: mutateFirstSibling(action.valueSiblings),
        },
      }),
    ).toBeNull();
  }
  return {
    preControlCborHex: encodeMidgardCekDataTraverseControlV1(
      step.control,
    ).toString("hex"),
    sourceBytesHex: null,
    membershipDepth,
    action:
      action.kind === "foldList"
        ? {
            kind: action.kind,
            frame: jsonFrame(action.frame),
            childIndex: action.childIndex,
            child: jsonSummary(action.child),
            siblingHexes: action.siblings.map((sibling) =>
              Buffer.from(sibling).toString("hex"),
            ),
          }
        : {
            kind: action.kind,
            frame: jsonFrame(action.frame),
            pairIndex: action.pairIndex,
            key: jsonSummary(action.key),
            value: jsonSummary(action.value),
            keySiblingHexes: action.keySiblings.map((sibling) =>
              Buffer.from(sibling).toString("hex"),
            ),
            valueSiblingHexes: action.valueSiblings.map((sibling) =>
              Buffer.from(sibling).toString("hex"),
            ),
          },
    postControlCborHex: encodeMidgardCekDataTraverseControlV1(
      step.next,
    ).toString("hex"),
  };
};

const exactTerminalVector = (
  steps: readonly ProductionDataTraverseStepV1[],
) => {
  const terminalStep = steps.at(-1);
  if (terminalStep?.action?.kind !== "finalizeFrame") {
    throw new Error("Broad Data trace lost its final frame");
  }
  const summary = finalizeMidgardCekDataTraverseV1(terminalStep.next);
  if (summary === null) {
    throw new Error("Broad Data trace did not terminate");
  }
  const mutatedFrame = {
    ...terminalStep.action.frame,
    sequence: {
      ...terminalStep.action.frame.sequence,
      root: Buffer.concat([
        Buffer.from([terminalStep.action.frame.sequence.root[0]! ^ 0x01]),
        Buffer.from(terminalStep.action.frame.sequence.root.subarray(1)),
      ]),
    },
  };
  expect(
    advanceMidgardCekDataTraverseV1({
      control: terminalStep.control,
      sourceBytes: null,
      action: {
        ...terminalStep.action,
        frame: mutatedFrame,
      },
    }),
  ).toBeNull();
  return {
    preControlCborHex: encodeMidgardCekDataTraverseControlV1(
      terminalStep.control,
    ).toString("hex"),
    frameCborHex: encodeMidgardCekDataFrameV1(
      terminalStep.action.frame,
    ).toString("hex"),
    postControlCborHex: encodeMidgardCekDataTraverseControlV1(
      terminalStep.next,
    ).toString("hex"),
    summary: {
      rootHex: Buffer.from(summary.root).toString("hex"),
      cborLength: summary.cborLength.toString(),
      memory: summary.memory.toString(),
    },
  };
};

const maximumSourceSpan = (
  steps: readonly ProductionDataTraverseStepV1[],
): number =>
  steps.reduce(
    (maximum, { control }) =>
      Math.max(maximum, nextMidgardCekDataTraverseSpanV1(control)?.length ?? 0),
    0,
  );

const extractAuthenticatedLedgerOutputDataSteps = (
  trace: ReturnType<typeof buildMidgardLedgerOutputProofTraceV1>,
): readonly ProductionDataTraverseStepV1[] => {
  const dataSteps: ProductionDataTraverseStepV1[] = [];
  let expectedControl = trace.initial;
  for (let index = 0; index < trace.steps.length; index += 1) {
    const { control, witness, next } = trace.steps[index]!;
    if (control !== expectedControl) {
      throw new Error(
        `ledger-output production trace lost successor identity at step ${index.toString()}`,
      );
    }
    expectedControl = next;
    if (
      witness?.kind === "datum" &&
      control.datum !== null &&
      next.datum !== null
    ) {
      dataSteps.push({
        control: control.datum,
        action: witness.action,
        next: next.datum,
      });
    }
  }
  if (
    expectedControl !== trace.terminal ||
    !isExactMidgardLedgerOutputProofTerminalV1(trace.terminal)
  ) {
    throw new Error("ledger-output production trace did not terminate");
  }
  return dataSteps;
};

const replayRedeemerItemProof = (
  trace: ReturnType<typeof buildMidgardRedeemerItemProofTraceV1>,
): readonly ProductionDataTraverseStepV1[] => {
  const dataSteps: ProductionDataTraverseStepV1[] = [];
  for (let index = 0; index < trace.steps.length; index += 1) {
    const { control, witness, next } = trace.steps[index]!;
    const replay = advanceMidgardRedeemerItemProofV1({
      control,
      witness,
    });
    if (replay === null || !isDeepStrictEqual(replay, next)) {
      throw new Error(
        `redeemer-item production replay diverged at step ${index.toString()}`,
      );
    }
    if (
      witness.action.kind === "traverseData" &&
      control.traversal !== null &&
      next.traversal !== null
    ) {
      dataSteps.push({
        control: control.traversal,
        action: witness.action.action,
        next: next.traversal,
      });
    }
  }
  return dataSteps;
};

const maximumDatumChunkBytes = (
  trace: ReturnType<typeof buildMidgardLedgerOutputProofTraceV1>,
): number =>
  trace.steps.reduce((maximum, { witness }) => {
    if (witness?.kind !== "datum") return maximum;
    return Math.max(
      maximum,
      witness.chunkProof?.chunk.length ?? 0,
      witness.nextChunkProof?.chunk.length ?? 0,
    );
  }, 0);

const maximumRedeemerChunkBytes = (
  trace: ReturnType<typeof buildMidgardRedeemerItemProofTraceV1>,
): number =>
  trace.steps.reduce(
    (maximum, { witness }) =>
      Math.max(
        maximum,
        witness.chunkProof?.chunk.length ?? 0,
        witness.nextChunkProof?.chunk.length ?? 0,
      ),
    0,
  );

/**
 * The exact genuine signed-Cardano breadth boundaries, per Data kind and per
 * carriage path. Before these pins the two searches were only bounded relative
 * to `maxTxSize`, and the measured vectors were printed rather than asserted —
 * a silently shrunk collection kept the suite green. Each entry is the maximum
 * breadth the search must land on, its adjacent overflow, and both signed
 * transaction sizes and Data CBOR sizes.
 */
const MAXIMUM_DATUM_BREADTH_BOUNDARY_V1 = {
  constructor: {
    acceptedBreadth: 16_166,
    acceptedDataCborBytes: 16_173,
    acceptedSignedBytes: 16_384,
    adjacentBreadth: 16_167,
    adjacentDataCborBytes: 16_174,
    adjacentSignedBytes: 16_385,
  },
  list: {
    acceptedBreadth: 16_171,
    acceptedDataCborBytes: 16_173,
    acceptedSignedBytes: 16_384,
    adjacentBreadth: 16_172,
    adjacentDataCborBytes: 16_174,
    adjacentSignedBytes: 16_385,
  },
  map: {
    acceptedBreadth: 4_112,
    acceptedDataCborBytes: 16_171,
    acceptedSignedBytes: 16_382,
    adjacentBreadth: 4_113,
    adjacentDataCborBytes: 16_175,
    adjacentSignedBytes: 16_386,
  },
} as const;

const MAXIMUM_REDEEMER_BREADTH_BOUNDARY_V1 = {
  constructor: {
    acceptedBreadth: 15_977,
    acceptedDataCborBytes: 15_984,
    acceptedSignedBytes: 16_384,
    adjacentBreadth: 15_978,
    adjacentDataCborBytes: 15_985,
    adjacentSignedBytes: 16_385,
  },
  list: {
    acceptedBreadth: 15_982,
    acceptedDataCborBytes: 15_984,
    acceptedSignedBytes: 16_384,
    adjacentBreadth: 15_983,
    adjacentDataCborBytes: 15_985,
    adjacentSignedBytes: 16_385,
  },
  map: {
    acceptedBreadth: 4_065,
    acceptedDataCborBytes: 15_983,
    acceptedSignedBytes: 16_383,
    adjacentBreadth: 4_066,
    adjacentDataCborBytes: 15_987,
    adjacentSignedBytes: 16_387,
  },
} as const;

describe("canonical V1 Cardano Data breadth boundaries", () => {
  it.each(["constructor", "list", "map"] as const)(
    "retains maximum %s breadth through the inline-datum path",
    async (kind) => {
      const privateKey = deterministicCardanoBoundaryPrivateKeyV1(0);
      const funder = {
        seedPhrase: "",
        privateKey: privateKey.to_bech32(),
        address: CML.EnterpriseAddress.new(
          0,
          CML.Credential.new_pub_key(privateKey.to_public().hash()),
        )
          .to_address()
          .to_bech32(),
        assets: { lovelace: 40_000_000_000n },
      };

      const buildCandidate = (breadth: number) =>
        buildSignedCardanoNestedDatumCandidateV1({
          privateKeyBech32: funder.privateKey,
          inputTransactionId: "00".repeat(32),
          inputOutputIndex: 0n,
          inputLovelace: funder.assets.lovelace,
          recipientAddress: funder.address,
          requestedNestedLeafCount: breadth,
          nestedDatumCborHex: cardanoBreadthDataCborV1(kind, breadth),
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
        });
      const boundary = await findSignedCardanoCollectionBoundaryV1({
        maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
        buildSignedCandidate: buildCandidate,
      });
      const accepted = measureSignedCardanoNestedDatumV1(
        boundary.accepted.cborHex,
      );
      const adjacent = measureSignedCardanoNestedDatumV1(
        boundary.adjacent.cborHex,
      );
      const acceptedDataCborHex = cardanoBreadthDataCborV1(
        kind,
        boundary.accepted.requestedItemCount,
      );
      const adjacentDataCborHex = cardanoBreadthDataCborV1(
        kind,
        boundary.adjacent.requestedItemCount,
      );
      expect(boundary.accepted.signedBytes).toBeLessThanOrEqual(
        CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
      );
      expect(boundary.adjacent.signedBytes).toBeGreaterThan(
        CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
      );
      expect(boundary.adjacent.requestedItemCount).toBe(
        boundary.accepted.requestedItemCount + 1,
      );

      // The genuine maximum and its immediately adjacent control are exact, not
      // merely "whatever the search returned".
      expect({
        acceptedBreadth: boundary.accepted.requestedItemCount,
        acceptedDataCborBytes: acceptedDataCborHex.length / 2,
        acceptedSignedBytes: boundary.accepted.signedBytes,
        adjacentBreadth: boundary.adjacent.requestedItemCount,
        adjacentDataCborBytes: adjacentDataCborHex.length / 2,
        adjacentSignedBytes: boundary.adjacent.signedBytes,
      }).toEqual(MAXIMUM_DATUM_BREADTH_BOUNDARY_V1[kind]);
      expect(accepted.datumCborHex).toBe(acceptedDataCborHex);
      expect(adjacent.datumCborHex).toBe(adjacentDataCborHex);
      assertExactBreadthSemantics(
        kind,
        boundary.accepted.requestedItemCount,
        acceptedDataCborHex,
      );
      assertExactBreadthSemantics(
        kind,
        boundary.adjacent.requestedItemCount,
        adjacentDataCborHex,
      );
      expect({
        outputCount: accepted.outputCount,
        hasWithdrawals: accepted.hasWithdrawals,
        hasMint: accepted.hasMint,
        hasPlutusScripts: accepted.hasPlutusScripts,
        hasRedeemers: accepted.hasRedeemers,
        collateralInputCount: accepted.collateralInputCount,
      }).toEqual({
        outputCount: 1,
        hasWithdrawals: false,
        hasMint: false,
        hasPlutusScripts: false,
        hasRedeemers: false,
        collateralInputCount: 0,
      });

      const canonical = cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
        Buffer.from(boundary.accepted.cborHex, "hex"),
      );
      const native = decodeMidgardNativeTxFullV1FromCanonicalCbor(canonical);
      expect(validateMidgardConsensusV1Tx(native, canonical.length)).toBeNull();
      const outputCbors = decodeMidgardNativeByteListPreimage(
        native.body.outputsPreimageCbor,
        "native.outputs",
      );
      expect(outputCbors).toHaveLength(1);
      const output = decodeMidgardTxOutput(outputCbors[0]!);
      expect(output.datum?.cbor.toString("hex")).toBe(acceptedDataCborHex);
      const outputTrace = buildMidgardLedgerOutputProofTraceV1({
        outputIndex: 0,
        outputCbor: outputCbors[0]!,
      });
      const dataSteps = extractAuthenticatedLedgerOutputDataSteps(outputTrace);
      expect(dataSteps.at(-1)?.action?.kind).toBe("finalizeFrame");
      assertExactProductionFoldSemantics({
        kind,
        breadth: boundary.accepted.requestedItemCount,
        steps: dataSteps,
      });
      const terminalSummary = finalizeMidgardCekDataTraverseV1(
        outputTrace.terminal.datum!,
      );
      expect(terminalSummary).not.toBeNull();
      assertExactTerminalSummary({
        kind,
        breadth: boundary.accepted.requestedItemCount,
        dataCborHex: acceptedDataCborHex,
        summary: terminalSummary!,
      });
      expect(maximumSourceSpan(dataSteps)).toBeLessThanOrEqual(
        MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN_V1,
      );
      expect(maximumSourceSpan(dataSteps)).toBeLessThanOrEqual(132);
      expect(maximumDatumChunkBytes(outputTrace)).toBeLessThanOrEqual(
        MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
      );
      const reconstructed = measureSignedCardanoNestedDatumV1(
        Buffer.from(midgardNativeTxFullToCardanoTxEncoding(native)).toString(
          "hex",
        ),
      );
      expect({
        address: reconstructed.outputAddress,
        lovelace: reconstructed.outputLovelace,
        datumCborHex: reconstructed.datumCborHex,
      }).toEqual({
        address: accepted.outputAddress,
        lovelace: accepted.outputLovelace,
        datumCborHex: accepted.datumCborHex,
      });

      const emulator = new Emulator(
        [funder],
        PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
      );
      const txHash = await emulator.submitTx(boundary.accepted.cborHex);
      await expect(emulator.awaitTx(txHash)).resolves.toBe(true);
      await exerciseMidgardRetainedDaCanonicalBoundaryV1({
        canonicalTransactionCbor: canonical,
        corpusLabel: `maximum-${kind}-datum-breadth`,
      });

      const vector = {
        breadth: boundary.accepted.requestedItemCount,
        nodeCount: dataNodeCount(kind, boundary.accepted.requestedItemCount),
        dataCborBytes: acceptedDataCborHex.length / 2,
        signedCardanoBytes: boundary.accepted.signedBytes,
        adjacentBreadth: boundary.adjacent.requestedItemCount,
        adjacentDataCborBytes: adjacentDataCborHex.length / 2,
        adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
        nativeCanonicalBytes: canonical.length,
        outputProofSteps: outputTrace.steps.length,
        dataTraverseSteps: dataSteps.length,
        maximumSourceSpan: maximumSourceSpan(dataSteps),
        maximumChunkBytes: maximumDatumChunkBytes(outputTrace),
        broadFrontier: exactBroadFrontierVector(kind, dataSteps),
        terminal: exactTerminalVector(dataSteps),
      };

      if (process.env.MIDGARD_PRINT_AIKEN_VECTOR === "1") {
        console.info(
          JSON.stringify({
            dataBreadthBoundaryV1: { [`datum_${kind}`]: vector },
          }),
        );
      }
    },
    600_000,
  );

  /**
   * §3.2 complete-item-first ordering for C23/C24/C25.
   *
   * Before any bounded Data traversal is admitted as a fallback, the complete
   * proof item carrying the maximum Data must be constructed and measured on
   * both complete routes: direct carriage in the proof transaction, and
   * single-publication inline-datum carriage consumed as a reference input.
   *
   * The measured outcome is recorded as found, not as hoped. Each maximum
   * breadth is bound by the 16,384-byte signed Cardano transaction, so the
   * complete item is itself ~16 KB and overflows both complete routes — which
   * is precisely the necessity that `transaction-field-chunk-v1.md` and
   * `ledger-output-incremental-proof-v1.md` record. The case therefore pins
   * both sides of the real carriage boundary per kind: the largest complete
   * Data item that both complete routes admit, its adjacent overflow, and the
   * maximum shape's exact overshoot.
   */
  it.each(["constructor", "list", "map"] as const)(
    "measures complete %s Data direct and reference carriage before any bounded fallback",
    async (kind) => {
      const privateKey = deterministicCardanoBoundaryPrivateKeyV1(0);
      const addressBytes = Buffer.from(
        CML.EnterpriseAddress.new(
          0,
          CML.Credential.new_pub_key(privateKey.to_public().hash()),
        )
          .to_address()
          .to_raw_bytes(),
      );
      const outputItemForBreadth = (breadth: number): Buffer =>
        encodeMidgardTxOutput({
          address: addressBytes,
          value: { lovelace: 30_000_000n, assets: new Map() },
          datum: {
            kind: "inline",
            cbor: Buffer.from(cardanoBreadthDataCborV1(kind, breadth), "hex"),
          },
        });
      const fitForBreadth = (breadth: number) =>
        measureMidgardCompleteItemCarriageFitV1({
          fieldIndex: 2,
          itemIndex: 0,
          itemCbor: outputItemForBreadth(breadth),
        });

      // Largest complete Data item both complete routes admit, found by
      // bisection on the exact encoded item length.
      const publicationBound =
        fitForBreadth(1).maxSinglePublicationCompleteItemBytes;
      let low = 1;
      let high = 32_768;
      while (low + 1 < high) {
        const middle = Math.floor((low + high) / 2);
        if (outputItemForBreadth(middle).length <= publicationBound) {
          low = middle;
        } else {
          high = middle;
        }
      }
      const acceptedBreadth = low;
      const adjacentBreadth = low + 1;
      const acceptedFit = fitForBreadth(acceptedBreadth);
      const adjacentFit = fitForBreadth(adjacentBreadth);
      expect(acceptedFit.itemBytes).toBeLessThanOrEqual(publicationBound);
      expect(adjacentFit.itemBytes).toBeGreaterThan(publicationBound);
      expect(acceptedFit).toMatchObject({
        carriage: "reference",
        fitsSinglePublicationCarriage: true,
        requiresBoundedFallback: false,
      });
      expect(acceptedFit.publicationTransactionBytes).toBeLessThanOrEqual(
        acceptedFit.maxL1TransactionBytes,
      );
      // Above 8,273 bytes the applied direct route is measured out, so the
      // complete item survives only through reference carriage — this is the
      // "reference before fallback" step, not a fallback.
      expect(acceptedFit.fitsDirectCarriage).toBe(false);
      expect(adjacentFit).toMatchObject({
        fitsDirectCarriage: false,
        fitsSinglePublicationCarriage: false,
        requiresBoundedFallback: true,
      });

      // The direct route is not vacuous either: a smaller complete Data item
      // of the same kind is admitted directly, so both complete routes are
      // exercised before any bounded traversal is considered.
      let directLow = 1;
      let directHigh = acceptedBreadth;
      const directBound = acceptedFit.maxReliableDirectCompleteItemBytes;
      while (directLow + 1 < directHigh) {
        const middle = Math.floor((directLow + directHigh) / 2);
        if (outputItemForBreadth(middle).length <= directBound) {
          directLow = middle;
        } else {
          directHigh = middle;
        }
      }
      const directFit = fitForBreadth(directLow);
      expect(directFit).toMatchObject({
        carriage: "direct",
        fitsDirectCarriage: true,
        fitsSinglePublicationCarriage: true,
        requiresBoundedFallback: false,
      });

      // The genuine Cardano maximum for this kind, measured against both
      // complete routes. Its overflow is the §3.2 necessity for the bounded
      // Data traversal the sibling cases exercise.
      const buildCandidate = (breadth: number) =>
        buildSignedCardanoNestedDatumCandidateV1({
          privateKeyBech32: privateKey.to_bech32(),
          inputTransactionId: "00".repeat(32),
          inputOutputIndex: 0n,
          inputLovelace: 40_000_000_000n,
          recipientAddress: CML.EnterpriseAddress.new(
            0,
            CML.Credential.new_pub_key(privateKey.to_public().hash()),
          )
            .to_address()
            .to_bech32(),
          requestedNestedLeafCount: breadth,
          nestedDatumCborHex: cardanoBreadthDataCborV1(kind, breadth),
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
        });
      const boundary = await findSignedCardanoCollectionBoundaryV1({
        maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
        buildSignedCandidate: buildCandidate,
      });
      const canonical = cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
        Buffer.from(boundary.accepted.cborHex, "hex"),
      );
      const native = decodeMidgardNativeTxFullV1FromCanonicalCbor(canonical);
      expect(validateMidgardConsensusV1Tx(native, canonical.length)).toBeNull();
      const outputCbors = decodeMidgardNativeByteListPreimage(
        native.body.outputsPreimageCbor,
        "native.outputs",
      );
      expect(outputCbors).toHaveLength(1);
      expect(
        decodeMidgardTxOutput(outputCbors[0]!).datum?.cbor.toString("hex"),
      ).toBe(
        cardanoBreadthDataCborV1(kind, boundary.accepted.requestedItemCount),
      );
      const maximumFit = measureMidgardCompleteItemCarriageFitV1({
        fieldIndex: 2,
        itemIndex: 0,
        itemCbor: outputCbors[0]!,
      });
      expect(maximumFit).toMatchObject({
        fitsDirectCarriage: false,
        fitsSinglePublicationCarriage: false,
        requiresBoundedFallback: true,
      });
      expect(maximumFit.itemBytes).toBeGreaterThan(
        maximumFit.maxSinglePublicationCompleteItemBytes,
      );
      expect(maximumFit.publicationTransactionBytes).toBeGreaterThan(
        maximumFit.maxL1TransactionBytes,
      );
      // Bounded chunk fallback is the only remaining representation, and it is
      // the one the deployed traversal uses.
      expect(maximumFit.boundedFallbackChunkCount).toBeGreaterThan(1);

      if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
        console.info(
          JSON.stringify({
            dataBreadthCompleteItemFitV1: {
              [kind]: {
                directCarriageBreadth: directLow,
                directCarriageItemBytes: directFit.itemBytes,
                referenceCarriageBreadth: acceptedBreadth,
                referenceCarriageItemBytes: acceptedFit.itemBytes,
                referenceCarriagePublicationTransactionBytes:
                  acceptedFit.publicationTransactionBytes,
                adjacentBreadth,
                adjacentItemBytes: adjacentFit.itemBytes,
                cardanoMaximumBreadth: boundary.accepted.requestedItemCount,
                cardanoMaximumItemBytes: maximumFit.itemBytes,
                cardanoMaximumPublicationTransactionBytes:
                  maximumFit.publicationTransactionBytes,
                cardanoMaximumOvershootBytes:
                  maximumFit.publicationTransactionBytes -
                  maximumFit.maxL1TransactionBytes,
                boundedFallbackChunkCount: maximumFit.boundedFallbackChunkCount,
              },
            },
          }),
        );
      }
    },
    600_000,
  );

  it("retains maximum constructor/list/map breadth through genuine Cardano redeemers and the Midgard schema projection", async () => {
    const privateKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const walletAddress = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(privateKey.to_public().hash()),
    )
      .to_address()
      .to_bech32();
    const scriptAddress = validatorToAddress("Custom", spendingScript);
    const genesis = [
      {
        seedPhrase: "",
        privateKey: privateKey.to_bech32(),
        address: walletAddress,
        assets: { lovelace: 1_000_000_000_000n },
      },
      {
        seedPhrase: "",
        privateKey: privateKey.to_bech32(),
        address: walletAddress,
        assets: { lovelace: 1_000_000_000_000n },
      },
      {
        seedPhrase: "",
        privateKey: "",
        address: scriptAddress,
        assets: { lovelace: 10_000_000n },
        outputData: { inline: Data.void() },
      },
    ];
    const seedEmulator = new Emulator(
      genesis,
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
    );
    const walletInputs = (await seedEmulator.getUtxos(walletAddress)).sort(
      (left, right) => left.outputIndex - right.outputIndex,
    );
    const scriptInputs = await seedEmulator.getUtxos(scriptAddress);
    const lucid = await Lucid(seedEmulator, "Custom");
    lucid.selectWallet.fromPrivateKey(privateKey.to_bech32());
    const completedSeed = await lucid
      .newTx()
      .collectFrom([walletInputs[0]!])
      .collectFrom([scriptInputs[0]!], Data.void())
      .pay.ToAddress(walletAddress, { lovelace: 10_000_000n })
      .attach.SpendingValidator(spendingScript)
      .complete({ localUPLCEval: true });
    const signedSeed = await completedSeed.sign.withWallet().complete();
    const seed = measureCollateralizedPlutusFeasibilityCandidateV1(
      signedSeed.toCBOR(),
    );
    const seedTransaction = CML.Transaction.from_cbor_hex(signedSeed.toCBOR());
    const seedScripts = seedTransaction.witness_set().plutus_v3_scripts();
    expect(seedScripts?.len()).toBe(1);
    const vectors: Record<string, unknown> = {};

    for (const kind of ["constructor", "list", "map"] as const) {
      const buildCandidate = async (breadth: number) => {
        const candidate = await buildSignedCardanoSpendRedeemersCandidateV1({
          privateKeyBech32: privateKey.to_bech32(),
          feeFundingInput: walletInputs[0]!,
          collateralInput: walletInputs[1]!,
          availableScriptInputs: scriptInputs,
          recipientAddress: walletAddress,
          plutusV3ScriptCborHex: seedScripts!.get(0).to_cbor_hex(),
          redeemerDataCborHex: cardanoBreadthDataCborV1(kind, breadth),
          executionMemory: seed.executionMemory,
          executionSteps: seed.executionSteps,
          requestedRedeemerCount: 1,
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
          priceMem: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.priceMem,
          priceStep: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.priceStep,
          collateralPercentage:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.collateralPercentage,
          costModels: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.costModels,
        });
        return { ...candidate, requestedItemCount: breadth };
      };
      const boundary = await findSignedCardanoCollectionBoundaryV1({
        maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
        buildSignedCandidate: buildCandidate,
      });
      const accepted = measureCollateralizedPlutusFeasibilityCandidateV1(
        boundary.accepted.cborHex,
      );
      const adjacent = measureCollateralizedPlutusFeasibilityCandidateV1(
        boundary.adjacent.cborHex,
      );
      const acceptedDataCborHex = cardanoBreadthDataCborV1(
        kind,
        boundary.accepted.requestedItemCount,
      );
      const adjacentDataCborHex = cardanoBreadthDataCborV1(
        kind,
        boundary.adjacent.requestedItemCount,
      );
      expect(boundary.accepted.signedBytes).toBeLessThanOrEqual(
        CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
      );
      expect(boundary.adjacent.signedBytes).toBeGreaterThan(
        CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
      );
      expect(boundary.adjacent.requestedItemCount).toBe(
        boundary.accepted.requestedItemCount + 1,
      );

      // The genuine maximum and its immediately adjacent control are exact, not
      // merely "whatever the search returned".
      expect({
        acceptedBreadth: boundary.accepted.requestedItemCount,
        acceptedDataCborBytes: acceptedDataCborHex.length / 2,
        acceptedSignedBytes: boundary.accepted.signedBytes,
        adjacentBreadth: boundary.adjacent.requestedItemCount,
        adjacentDataCborBytes: adjacentDataCborHex.length / 2,
        adjacentSignedBytes: boundary.adjacent.signedBytes,
      }).toEqual(MAXIMUM_REDEEMER_BREADTH_BOUNDARY_V1[kind]);
      expect(accepted.redeemerCount).toBe(1);
      expect(adjacent.redeemerCount).toBe(1);
      expect(accepted.redeemerTags).toEqual([CML.RedeemerTag.Spend]);
      expect(accepted.redeemerIndexes).toEqual([1n]);
      expect(accepted.redeemerDataCborHexes).toEqual([acceptedDataCborHex]);
      expect(adjacent.redeemerDataCborHexes).toEqual([adjacentDataCborHex]);
      assertExactBreadthSemantics(
        kind,
        boundary.accepted.requestedItemCount,
        acceptedDataCborHex,
      );
      assertExactBreadthSemantics(
        kind,
        boundary.adjacent.requestedItemCount,
        adjacentDataCborHex,
      );
      expect(accepted.executionMemory).toBe(seed.executionMemory);
      expect(accepted.executionSteps).toBe(seed.executionSteps);
      expect(accepted.totalCollateral).toBe(
        CARDANO_BOUNDARY_TOTAL_COLLATERAL_V1,
      );
      const acceptedTransaction = CML.Transaction.from_cbor_hex(
        boundary.accepted.cborHex,
      );
      expect(acceptedTransaction.body().withdrawals()).toBeUndefined();
      expect(acceptedTransaction.body().mint()).toBeUndefined();
      const acceptedScripts = acceptedTransaction
        .witness_set()
        .plutus_v3_scripts();
      expect(acceptedScripts?.len()).toBe(1);
      const sourceRawFlatProgramBytes = Buffer.from(
        acceptedScripts!.get(0).to_raw_bytes(),
      );
      const artifact = buildMidgardCanonicalScriptArtifactV1({
        language: "PlutusV3",
        sourceRawFlatProgramBytes,
      });
      const canonicalMaterialEntries = decodeMidgardCekProgramMaterialSidecarV1(
        artifact.canonicalMaterialSidecarCbor,
      );
      expect(canonicalMaterialEntries).toEqual(
        artifact.canonicalMaterialEntries,
      );
      expect(
        verifyMidgardCekProgramMaterialBundleV1(
          [artifact.canonicalProgram.envelope],
          canonicalMaterialEntries,
        ),
      ).toHaveLength(1);
      expect(artifact.canonicalMidgardCredentialScriptHash).toBe(
        hashMidgardVersionedScript(artifact.canonicalMidgardCredentialScript),
      );
      expect(artifact.sourceRawScriptAuditHash).toBe(
        hashMidgardVersionedScript({
          language: "PlutusV3",
          scriptBytes: sourceRawFlatProgramBytes,
        }),
      );
      expect(artifact.sourceRawScriptAuditHash).not.toBe(
        artifact.canonicalMidgardCredentialScriptHash,
      );

      let collateralRejection:
        | {
            readonly message: string;
            readonly code: string | null;
            readonly detail: string | null;
          }
        | undefined;
      try {
        cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
          Buffer.from(boundary.accepted.cborHex, "hex"),
        );
      } catch (error) {
        const structured = error as {
          readonly code?: unknown;
          readonly detail?: unknown;
        };
        collateralRejection = {
          message: error instanceof Error ? error.message : String(error),
          code: typeof structured.code === "string" ? structured.code : null,
          detail:
            typeof structured.detail === "string" ? structured.detail : null,
        };
      }
      expect(collateralRejection).toEqual({
        message:
          "Cardano tx cannot be converted to Midgard native format without dropping fields",
        code: "E_CONVERSION_UNSUPPORTED_FEATURE",
        detail: "collateral_inputs",
      });

      const schemaProjectionSource =
        buildCollateralFreeMidgardSchemaParallelCandidateV1({
          collateralizedCardanoCborHex: boundary.accepted.cborHex,
          privateKeyBech32: privateKey.to_bech32(),
        });
      const schemaProjectionSourceShape = CML.Transaction.from_cbor_hex(
        schemaProjectionSource.cborHex,
      );
      expect(
        schemaProjectionSourceShape.body().collateral_inputs(),
      ).toBeUndefined();
      expect(
        schemaProjectionSourceShape.body().collateral_return(),
      ).toBeUndefined();
      expect(
        schemaProjectionSourceShape.body().total_collateral(),
      ).toBeUndefined();
      expect(
        Array.from(
          {
            length: schemaProjectionSourceShape.body().inputs().len(),
          },
          (_, index) =>
            schemaProjectionSourceShape
              .body()
              .inputs()
              .get(index)
              .to_cbor_hex(),
        ),
      ).toEqual(
        Array.from(
          {
            length: acceptedTransaction.body().inputs().len(),
          },
          (_, index) =>
            acceptedTransaction.body().inputs().get(index).to_cbor_hex(),
        ),
      );
      expect(
        Array.from(
          {
            length: schemaProjectionSourceShape.body().outputs().len(),
          },
          (_, index) =>
            schemaProjectionSourceShape
              .body()
              .outputs()
              .get(index)
              .to_cbor_hex(),
        ),
      ).toEqual(
        Array.from(
          {
            length: acceptedTransaction.body().outputs().len(),
          },
          (_, index) =>
            acceptedTransaction.body().outputs().get(index).to_cbor_hex(),
        ),
      );
      expect(schemaProjectionSourceShape.body().fee()).toBe(
        acceptedTransaction.body().fee(),
      );
      expect(
        schemaProjectionSourceShape.body().script_data_hash()?.to_hex(),
      ).toBe(acceptedTransaction.body().script_data_hash()?.to_hex());
      const schemaSourceCanonical =
        cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
          Buffer.from(schemaProjectionSource.cborHex, "hex"),
        );
      const schemaSourceNative = decodeMidgardNativeTxFullV1FromCanonicalCbor(
        schemaSourceCanonical,
      );
      const sourceScripts = decodeMidgardVersionedScriptListPreimage(
        schemaSourceNative.witnessSet.scriptTxWitsPreimageCbor,
      );
      expect(sourceScripts).toEqual([
        {
          language: "PlutusV3",
          scriptBytes: sourceRawFlatProgramBytes,
        },
      ]);
      expect(hashMidgardVersionedScript(sourceScripts[0]!)).toBe(
        artifact.sourceRawScriptAuditHash,
      );
      expect(schemaSourceNative.witnessSet.addrTxWitsPreimageCbor).not.toEqual(
        Buffer.from([0x80]),
      );
      const projectedScriptIntegrityHash =
        computeScriptIntegrityHashForLanguages(
          midgardFieldCommitmentV1(
            schemaSourceNative.witnessSet.redeemerTxWitsPreimageCbor,
          ),
          ["PlutusV3"],
        );
      expect(projectedScriptIntegrityHash).toHaveLength(32);
      expect(artifact.canonicalMidgardCredentialScript.language).toBe(
        "PlutusV3",
      );
      expect(projectedScriptIntegrityHash).toEqual(
        computeScriptIntegrityHashForLanguages(
          midgardFieldCommitmentV1(
            schemaSourceNative.witnessSet.redeemerTxWitsPreimageCbor,
          ),
          ["PlutusV3"],
        ),
      );

      const schemaProjection = materializeMidgardNativeTxFromCanonicalV1({
        version: schemaSourceNative.version,
        validity: schemaSourceNative.validity,
        body: {
          ...schemaSourceNative.body,
          scriptIntegrityHash: projectedScriptIntegrityHash,
        },
        witnessSet: {
          ...schemaSourceNative.witnessSet,
          addrTxWitsPreimageCbor: Buffer.from([0x80]),
          scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
            artifact.canonicalMidgardCredentialScript,
          ]),
        },
      });
      const canonical = encodeMidgardNativeTxCanonicalV1(schemaProjection);
      const native = decodeMidgardNativeTxFullV1FromCanonicalCbor(canonical);
      expect(native.witnessSet.addrTxWitsPreimageCbor).toEqual(
        Buffer.from([0x80]),
      );
      expect(native.body.scriptIntegrityHash).toEqual(
        projectedScriptIntegrityHash,
      );
      const projectedScripts = decodeMidgardVersionedScriptListPreimage(
        native.witnessSet.scriptTxWitsPreimageCbor,
      );
      expect(projectedScripts).toEqual([
        artifact.canonicalMidgardCredentialScript,
      ]);
      expect(hashMidgardVersionedScript(projectedScripts[0]!)).toBe(
        artifact.canonicalMidgardCredentialScriptHash,
      );
      expect(validateMidgardConsensusV1Tx(native, canonical.length)).toBeNull();
      const redeemers = decodeMidgardRedeemers(
        native.witnessSet.redeemerTxWitsPreimageCbor,
      );
      expect(redeemers).toHaveLength(1);
      expect(redeemers[0]!.dataCborHex).toBe(acceptedDataCborHex);
      expect(redeemers[0]).toMatchObject({
        tag: CML.RedeemerTag.Spend,
        index: 1n,
        exUnits: {
          memory: seed.executionMemory,
          steps: seed.executionSteps,
        },
      });
      const field = countedMachineFieldTraceV1(
        8,
        native.witnessSet.redeemerTxWitsPreimageCbor,
      );
      expect(field.items).toHaveLength(1);
      const item = field.items[0]!;
      const itemTrace = buildMidgardRedeemerItemProofTraceV1({
        itemIndex: item.itemIndex,
        itemCount: field.items.length,
        itemBytes: item.bytes,
        mode: MidgardRedeemerItemProofModesV1.Data,
        expectedPurposeTag: CML.RedeemerTag.Spend,
        expectedPointerIndex: 1,
      });
      expect(itemTrace.steps[0]?.witness.action.kind).toBe("openHeader");
      expect(itemTrace.steps[1]?.witness.action.kind).toBe("openTail");
      const descriptor = midgardRedeemerItemDescriptorV1(
        itemTrace.steps[1]!.next,
      );
      expect(descriptor).toMatchObject({
        itemIndex: 0,
        itemCount: 1,
        totalLength: item.bytes.length,
        purposeTag: CML.RedeemerTag.Spend,
        pointerIndex: 1,
        dataLength: acceptedDataCborHex.length / 2,
        executionMemory: seed.executionMemory,
        executionSteps: seed.executionSteps,
      });
      expect(descriptor?.itemCommitment).toEqual(item.commitment);
      const dataSteps = replayRedeemerItemProof(itemTrace);
      expect(dataSteps.at(-1)?.action?.kind).toBe("finalizeFrame");
      assertExactProductionFoldSemantics({
        kind,
        breadth: boundary.accepted.requestedItemCount,
        steps: dataSteps,
      });
      const terminalSummary = finalizeMidgardRedeemerItemProofV1(
        itemTrace.terminal,
      );
      expect(terminalSummary).not.toBeNull();
      assertExactTerminalSummary({
        kind,
        breadth: boundary.accepted.requestedItemCount,
        dataCborHex: acceptedDataCborHex,
        summary: terminalSummary!,
      });
      const maximumItemSourceSpan = itemTrace.steps.reduce(
        (maximum, { control }) =>
          Math.max(
            maximum,
            nextMidgardRedeemerItemProofSpanV1(control)?.length ?? 0,
          ),
        0,
      );
      expect(maximumItemSourceSpan).toBeLessThanOrEqual(
        MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN_V1,
      );
      expect(maximumItemSourceSpan).toBeLessThanOrEqual(132);
      expect(maximumRedeemerChunkBytes(itemTrace)).toBeLessThanOrEqual(
        MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
      );
      const emulator = new Emulator(
        genesis,
        PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
      );
      const txHash = await emulator.submitTx(boundary.accepted.cborHex);
      await expect(emulator.awaitTx(txHash)).resolves.toBe(true);
      await exerciseMidgardRetainedDaCanonicalBoundaryV1({
        canonicalTransactionCbor: canonical,
        corpusLabel: `maximum-${kind}-redeemer-breadth`,
        canonicalMaterialSidecarCbor: artifact.canonicalMaterialSidecarCbor,
        sourceRawScriptAuditHash: artifact.sourceRawScriptAuditHash,
      });

      vectors[`redeemer_${kind}`] = {
        breadth: boundary.accepted.requestedItemCount,
        nodeCount: dataNodeCount(kind, boundary.accepted.requestedItemCount),
        dataCborBytes: acceptedDataCborHex.length / 2,
        signedCardanoBytes: boundary.accepted.signedBytes,
        adjacentBreadth: boundary.adjacent.requestedItemCount,
        adjacentDataCborBytes: adjacentDataCborHex.length / 2,
        adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
        cardanoCapacityAuthority: {
          signedCardanoBytes: boundary.accepted.signedBytes,
          adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
          collateralRejection,
        },
        midgardSchemaProjection: {
          sourceShapeBytes: schemaProjectionSource.cborHex.length / 2,
          nativeCanonicalBytes: canonical.length,
          sourceRawScriptAuditHash: artifact.sourceRawScriptAuditHash,
          canonicalMidgardCredentialScriptHash:
            artifact.canonicalMidgardCredentialScriptHash,
          canonicalMaterialEntryCount: canonicalMaterialEntries.length,
          canonicalMaterialSidecarBytes:
            artifact.canonicalMaterialSidecarCbor.length,
          redeemerFieldBytes:
            native.witnessSet.redeemerTxWitsPreimageCbor.length,
          redeemerItemBytes: item.bytes.length,
          itemProofSteps: itemTrace.steps.length,
          dataTraverseSteps: dataSteps.length,
          maximumSourceSpan: maximumItemSourceSpan,
          maximumChunkBytes: maximumRedeemerChunkBytes(itemTrace),
          broadFrontier: exactBroadFrontierVector(kind, dataSteps),
          terminal: exactTerminalVector(dataSteps),
        },
        collateralRejection,
      };
    }

    if (process.env.MIDGARD_PRINT_AIKEN_VECTOR === "1") {
      console.info(JSON.stringify({ dataBreadthBoundaryV1: vectors }));
    }
  }, 600_000);
});
