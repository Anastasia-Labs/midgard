import { readFileSync } from "node:fs";

import {
  advanceMidgardRedeemerItemProofV1,
  buildMidgardRedeemerItemProofTraceV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  encodeCbor,
  finalizeMidgardRedeemerItemProofV1,
  hashMidgardRedeemerItemProofControlV1,
  isWellFormedMidgardRedeemerItemProofControlV1,
  MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN_V1,
  midgardRedeemerItemDescriptorV1,
  MidgardRedeemerItemProofModesV1,
  MidgardRedeemerItemProofStagesV1,
  nextMidgardRedeemerItemProofSpanV1,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { countedMachineFieldTraceV1 } from "../src/validation-machine/index.js";

type RetainedCorpus = {
  readonly entries: readonly {
    readonly label: string;
    readonly canonicalCborHex: string;
  }[];
};

const retainedCorpus = JSON.parse(
  readFileSync(
    new URL(
      "../../midgard-fault-proofs/tests/fixtures/cardano-capability-p2-boundary-corpus-v1.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as RetainedCorpus;

const balancedRedeemerCanonicalCbor = Buffer.from(
  retainedCorpus.entries.find(
    ({ label }) => label === "balanced-nested-redeemer",
  )!.canonicalCborHex,
  "hex",
);

const smallItem = encodeCbor([0n, 0n, Buffer.from([0]), [10n, 20n]]);

describe("retained V1 redeemer item proof", () => {
  it("binds descriptor metadata and rejects mutated openings or controls", () => {
    const trace = buildMidgardRedeemerItemProofTraceV1({
      itemIndex: 0,
      itemCount: 1,
      itemBytes: smallItem,
      mode: MidgardRedeemerItemProofModesV1.Data,
      expectedPurposeTag: 0,
      expectedPointerIndex: 0,
    });
    const descriptor = midgardRedeemerItemDescriptorV1(trace.steps[1]!.next);
    expect(descriptor).toMatchObject({
      itemIndex: 0,
      itemCount: 1,
      totalLength: smallItem.length,
      purposeTag: 0,
      pointerIndex: 0,
      dataOffset: 4,
      dataLength: 1,
      executionMemory: 10n,
      executionSteps: 20n,
    });
    const descriptorOnly = buildMidgardRedeemerItemProofTraceV1({
      itemIndex: 0,
      itemCount: 1,
      itemBytes: smallItem,
      mode: MidgardRedeemerItemProofModesV1.Descriptor,
    });
    expect(descriptorOnly.steps).toHaveLength(2);
    expect(descriptorOnly.terminal).toMatchObject({
      stage: MidgardRedeemerItemProofStagesV1.Terminal,
      traversal: null,
    });
    expect(
      finalizeMidgardRedeemerItemProofV1(descriptorOnly.terminal),
    ).toBeNull();

    const header = trace.steps[0]!;
    const wrongPurpose = buildMidgardRedeemerItemProofTraceV1.bind(null, {
      itemIndex: 0,
      itemCount: 1,
      itemBytes: smallItem,
      mode: MidgardRedeemerItemProofModesV1.Data,
      expectedPurposeTag: 1,
      expectedPointerIndex: 0,
    });
    expect(wrongPurpose).toThrow();
    expect(() =>
      buildMidgardRedeemerItemProofTraceV1({
        itemIndex: 0,
        itemCount: 1,
        itemBytes: smallItem,
        mode: MidgardRedeemerItemProofModesV1.Data,
        expectedPurposeTag: 0,
        expectedPointerIndex: 1,
      }),
    ).toThrow();

    const wrongIndex = {
      ...header.control,
      itemIndex: 1,
      itemCount: 2,
    };
    expect(
      advanceMidgardRedeemerItemProofV1({
        control: wrongIndex,
        witness: header.witness,
      }),
    ).toBeNull();
    expect(
      hashMidgardRedeemerItemProofControlV1({
        ...header.control,
        itemCount: 2,
      }),
    ).not.toEqual(hashMidgardRedeemerItemProofControlV1(header.control));

    const wrongLength = {
      ...header.control,
      totalLength: header.control.totalLength + 1,
    };
    expect(
      advanceMidgardRedeemerItemProofV1({
        control: wrongLength,
        witness: header.witness,
      }),
    ).toBeNull();

    const wrongCommitment = {
      ...header.control,
      itemCommitment: Buffer.alloc(32, 0x7f),
    };
    expect(
      advanceMidgardRedeemerItemProofV1({
        control: wrongCommitment,
        witness: header.witness,
      }),
    ).toBeNull();

    const wrongChunk = {
      ...header.witness,
      chunkProof: {
        ...header.witness.chunkProof!,
        chunk: Buffer.from(
          header.witness.chunkProof!.chunk.map((byte, index) =>
            index === 0 ? byte ^ 0x01 : byte,
          ),
        ),
      },
    };
    expect(
      advanceMidgardRedeemerItemProofV1({
        control: header.control,
        witness: wrongChunk,
      }),
    ).toBeNull();
    expect(
      advanceMidgardRedeemerItemProofV1({
        control: header.control,
        witness: {
          ...header.witness,
          action: { kind: "openTail" },
        },
      }),
    ).toBeNull();
    expect(
      advanceMidgardRedeemerItemProofV1({
        control: header.control,
        witness: {
          ...header.witness,
          nextChunkProof: header.witness.chunkProof,
        },
      }),
    ).toBeNull();

    const tail = trace.steps[1]!;
    expect(
      advanceMidgardRedeemerItemProofV1({
        control: tail.control,
        witness: {
          ...tail.witness,
          chunkProof: {
            ...tail.witness.chunkProof!,
            chunk: Buffer.from(
              tail.witness.chunkProof!.chunk.map((byte, index, bytes) =>
                index === bytes.length - 1 ? byte ^ 0x01 : byte,
              ),
            ),
          },
        },
      }),
    ).toBeNull();

    const traversalStep = trace.steps.find(
      ({ control }) =>
        control.stage === MidgardRedeemerItemProofStagesV1.Data &&
        control.traversal !== null,
    )!;
    const wrongTraversal = {
      ...traversalStep.control,
      traversal: {
        ...traversalStep.control.traversal!,
        sourceStart: traversalStep.control.traversal!.sourceStart + 1,
      },
    };
    expect(isWellFormedMidgardRedeemerItemProofControlV1(wrongTraversal)).toBe(
      false,
    );

    const terminal = trace.terminal;
    const summary = finalizeMidgardRedeemerItemProofV1(terminal)!;
    const wrongSummaryTerminal = {
      ...terminal,
      traversal: {
        ...terminal.traversal!,
        result: {
          ...summary,
          root: Buffer.alloc(32, 0x55),
        },
      },
    };
    expect(
      hashMidgardRedeemerItemProofControlV1(wrongSummaryTerminal),
    ).not.toEqual(hashMidgardRedeemerItemProofControlV1(terminal));
    expect(
      finalizeMidgardRedeemerItemProofV1(wrongSummaryTerminal)!.root,
    ).not.toEqual(summary.root);
  });

  it("traverses the checked maximum balanced retained redeemer through exact bounded spans", () => {
    const native = decodeMidgardNativeTxFullV1FromCanonicalCbor(
      balancedRedeemerCanonicalCbor,
    );
    const collection = countedMachineFieldTraceV1(
      8,
      native.witnessSet.redeemerTxWitsPreimageCbor,
    );
    expect(collection.items).toHaveLength(1);
    const item = collection.items[0]!;
    const trace = buildMidgardRedeemerItemProofTraceV1({
      itemIndex: 0,
      itemCount: 1,
      itemBytes: item.bytes,
      mode: MidgardRedeemerItemProofModesV1.Data,
    });
    const descriptor = midgardRedeemerItemDescriptorV1(trace.steps[1]!.next)!;
    const summary = finalizeMidgardRedeemerItemProofV1(trace.terminal);
    expect(descriptor).toMatchObject({
      itemIndex: 0,
      itemCount: 1,
      purposeTag: 0,
      pointerIndex: 1,
      dataLength: 15_982,
    });
    expect(descriptor.itemCommitment).toEqual(item.commitment);
    expect(summary).toMatchObject({
      root: Buffer.from(
        "26ef420c9e803ba9d74f048b521bff6c99e6a6b4d8aefd077c300a8e31a4dc20",
        "hex",
      ),
      cborLength: 15_982n,
      memory: 47_924n,
    });
    expect(trace.terminal.stage).toBe(
      MidgardRedeemerItemProofStagesV1.Terminal,
    );
    expect(
      trace.steps.reduce(
        (maximum, { control }) =>
          Math.max(
            maximum,
            nextMidgardRedeemerItemProofSpanV1(control)?.length ?? 0,
          ),
        0,
      ),
    ).toBeLessThanOrEqual(MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN_V1);

    const crossing = trace.steps.find(
      ({ witness }) => witness.nextChunkProof !== null,
    );
    expect(crossing).toBeDefined();
    expect(
      advanceMidgardRedeemerItemProofV1({
        control: crossing!.control,
        witness: {
          ...crossing!.witness,
          nextChunkProof: null,
        },
      }),
    ).toBeNull();
  }, 120_000);
});
