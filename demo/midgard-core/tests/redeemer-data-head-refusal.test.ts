import { expect, it } from "vitest";

import { MidgardCekDataTraverseStages } from "../src/cek-data-traverse.js";
import {
  buildMidgardRedeemerDataHeadRejectionTrace,
  buildMidgardRedeemerItemProofTrace,
  encodeCbor,
  hasNonCanonicalDefiniteSequenceHead,
  inspectMidgardRedeemerSequenceHeads,
  isMidgardRedeemerDataHeadRejection,
  MidgardRedeemerItemProofModes,
} from "../src/index.js";

const item = (data: string) =>
  encodeCbor([0n, 0n, Buffer.from(data, "hex"), [10n, 20n]]);

it("authenticates the observed constructor refusal without treating wrong evidence as invalid Data", () => {
  const refusal = buildMidgardRedeemerDataHeadRejectionTrace({
    itemIndex: 0,
    itemCount: 1,
    itemBytes: item("d8798101"),
  });
  expect(
    isMidgardRedeemerDataHeadRejection(refusal.control, refusal.witness),
  ).toBe(true);
  expect(
    isMidgardRedeemerDataHeadRejection(refusal.control, {
      ...refusal.witness,
      chunkProof: null,
    }),
  ).toBe(false);
  expect(
    isMidgardRedeemerDataHeadRejection(refusal.control, {
      ...refusal.witness,
      nextChunkProof: refusal.witness.chunkProof,
    }),
  ).toBe(false);
  expect(
    isMidgardRedeemerDataHeadRejection(refusal.control, {
      ...refusal.witness,
      action: {
        kind: "traverseData",
        action: { kind: "headScalar" },
      },
    }),
  ).toBe(false);
  const honest = buildMidgardRedeemerItemProofTrace({
    itemIndex: 0,
    itemCount: 1,
    itemBytes: item("d8799f01ff"),
    mode: MidgardRedeemerItemProofModes.Data,
  });
  const head = honest.steps[2]!;
  expect(
    isMidgardRedeemerDataHeadRejection(head.control, {
      ...head.witness,
      action: { kind: "traverseData", action: null },
    }),
  ).toBe(false);
});

it.each(["80", "9f01ff", "d87980", "d8799f01ff", "d9050080", "d905009f01ff"])(
  "keeps canonical head %s outside the refusal",
  (data) => {
    expect(hasNonCanonicalDefiniteSequenceHead(Buffer.from(data, "hex"))).toBe(
      false,
    );
  },
);

it.each([
  { data: "d8799f8101ff", offset: 3, stage: MidgardCekDataTraverseStages.Head },
  {
    data: "d8799f01810102ff",
    offset: 4,
    stage: MidgardCekDataTraverseStages.Close,
  },
  {
    data: "d8799fa20101810102ff",
    offset: 6,
    stage: MidgardCekDataTraverseStages.Head,
  },
])(
  "authenticates nested sequence refusal $data through the canonical original prefix",
  ({ data, offset, stage }) => {
    expect(
      inspectMidgardRedeemerSequenceHeads(Buffer.from(data, "hex")),
    ).toEqual({ kind: "refusal", offset });
    const refusal = buildMidgardRedeemerDataHeadRejectionTrace({
      itemIndex: 0,
      itemCount: 1,
      itemBytes: item(data),
    });
    expect(refusal.control.traversal!.offset).toBe(offset);
    expect(refusal.control.traversal!.stage).toBe(stage);
    expect(refusal.steps[2]!.witness.action).toEqual({
      kind: "traverseData",
      action: { kind: "headSequence" },
    });
    expect(
      isMidgardRedeemerDataHeadRejection(refusal.control, refusal.witness),
    ).toBe(true);
    expect(
      isMidgardRedeemerDataHeadRejection(refusal.control, {
        ...refusal.witness,
        chunkProof: null,
      }),
    ).toBe(false);
    expect(
      isMidgardRedeemerDataHeadRejection(refusal.control, {
        ...refusal.witness,
        nextChunkProof: refusal.witness.chunkProof,
      }),
    ).toBe(false);
    expect(
      isMidgardRedeemerDataHeadRejection(refusal.control, {
        ...refusal.witness,
        action: {
          kind: "traverseData",
          action: { kind: "headSequence" },
        },
      }),
    ).toBe(false);
  },
);
it.each(["d8799f9f01ffff", "d8799f428101ff"])(
  "does not confuse honest nested Data/payload %s with a sequence refusal",
  (data) => {
    expect(
      inspectMidgardRedeemerSequenceHeads(Buffer.from(data, "hex")),
    ).toEqual({ kind: "canonical" });
    expect(() =>
      buildMidgardRedeemerDataHeadRejectionTrace({
        itemIndex: 0,
        itemCount: 1,
        itemBytes: item(data),
      }),
    ).toThrow("no authenticated sequence refusal");
  },
);
it("an earlier nonminimal scalar names its own fault before a later definite sequence", () => {
  expect(
    inspectMidgardRedeemerSequenceHeads(Buffer.from("d8799f18018101ff", "hex")),
  ).toEqual({ kind: "refusal", offset: 3 });
});

it("honest later nested Data refuses semantic invalidity under the same NoAction or a wrong action", () => {
  const honest = buildMidgardRedeemerItemProofTrace({
    itemIndex: 0,
    itemCount: 1,
    itemBytes: item("d8799f019f01ff02ff"),
    mode: MidgardRedeemerItemProofModes.Data,
  });
  const later = honest.steps.find(
    ({ control }) =>
      control.traversal?.stage === MidgardCekDataTraverseStages.Close &&
      control.traversal.offset === 4,
  )!;
  expect(
    isMidgardRedeemerDataHeadRejection(later.control, {
      ...later.witness,
      action: { kind: "traverseData", action: null },
    }),
  ).toBe(false);
  expect(
    isMidgardRedeemerDataHeadRejection(later.control, {
      ...later.witness,
      action: { kind: "traverseData", action: { kind: "headScalar" } },
    }),
  ).toBe(false);
  expect(
    isMidgardRedeemerDataHeadRejection(later.control, {
      ...later.witness,
      chunkProof: null,
      action: { kind: "traverseData", action: null },
    }),
  ).toBe(false);
});

it.each([
  {
    data: "1801",
    honest: "01",
    offset: 0,
    stage: MidgardCekDataTraverseStages.Head,
  },
  {
    data: "d8799f1801ff",
    honest: "d8799f01ff",
    offset: 3,
    stage: MidgardCekDataTraverseStages.Head,
  },
  {
    data: "d8799f011801ff",
    honest: "d8799f0101ff",
    offset: 4,
    stage: MidgardCekDataTraverseStages.Close,
  },
  {
    data: "bf0101ff",
    honest: "a10101",
    offset: 0,
    stage: MidgardCekDataTraverseStages.Head,
  },
  {
    data: "d8668218808101",
    honest: "d8668218809f01ff",
    offset: 5,
    stage: MidgardCekDataTraverseStages.LargeFields,
  },
])(
  "authenticates malformed Data $data while canonical $honest cannot claim the same refusal",
  ({ data, honest, offset, stage }) => {
    expect(
      inspectMidgardRedeemerSequenceHeads(Buffer.from(data, "hex")),
    ).toEqual({ kind: "refusal", offset });
    const refusal = buildMidgardRedeemerDataHeadRejectionTrace({
      itemIndex: 0,
      itemCount: 1,
      itemBytes: item(data),
    });
    expect(refusal.control.traversal).toMatchObject({ offset, stage });
    expect(
      isMidgardRedeemerDataHeadRejection(refusal.control, refusal.witness),
    ).toBe(true);
    expect(
      isMidgardRedeemerDataHeadRejection(refusal.control, {
        ...refusal.witness,
        chunkProof: null,
      }),
    ).toBe(false);
    expect(
      isMidgardRedeemerDataHeadRejection(
        { ...refusal.control, dataOffset: refusal.control.dataOffset + 1 },
        refusal.witness,
      ),
    ).toBe(false);
    expect(
      isMidgardRedeemerDataHeadRejection(refusal.control, {
        ...refusal.witness,
        action: { kind: "traverseData", action: { kind: "headScalar" } },
      }),
    ).toBe(false);
    expect(
      inspectMidgardRedeemerSequenceHeads(Buffer.from(honest, "hex")),
    ).toEqual({ kind: "canonical" });
    const canonical = buildMidgardRedeemerItemProofTrace({
      itemIndex: 0,
      itemCount: 1,
      itemBytes: item(honest),
      mode: MidgardRedeemerItemProofModes.Data,
    });
    const frontier = canonical.steps.find(
      ({ control }) =>
        control.traversal?.stage === stage &&
        control.traversal.offset === offset,
    )!;
    expect(frontier).toBeDefined();
    expect(
      isMidgardRedeemerDataHeadRejection(frontier.control, {
        ...frontier.witness,
        action: { kind: "traverseData", action: null },
      }),
    ).toBe(false);
  },
);

it.each(["d8799f1801", "d8799f1801ff", "d8799f18018101ff", "d8799f1801f6"])(
  "stops at the first scalar fault without parsing suffix %s",
  (data) => {
    expect(
      inspectMidgardRedeemerSequenceHeads(Buffer.from(data, "hex")),
    ).toEqual({ kind: "refusal", offset: 3 });
    const refusal = buildMidgardRedeemerDataHeadRejectionTrace({
      itemIndex: 0,
      itemCount: 1,
      itemBytes: item(data),
    });
    expect(refusal.control.traversal!.offset).toBe(3);
    expect(
      isMidgardRedeemerDataHeadRejection(refusal.control, refusal.witness),
    ).toBe(true);
  },
);

it.each([
  ["580100", 0],
  ["59000100", 0],
  ["5a0000000100", 0],
  ["5b000000000000000100", 0],
  ["d8799f580100ff", 3],
  ["d8799f01580100ff", 4],
  ["590018" + "ab".repeat(24), 0],
  ["b800", 0],
  ["b8010101", 0],
  ["d9007980", 0],
] as const)(
  "authenticates the first bounded noncanonical byte/map/tag head %s at %s",
  (data, offset) => {
    expect(
      inspectMidgardRedeemerSequenceHeads(Buffer.from(data, "hex")),
    ).toEqual({ kind: "refusal", offset });
    const refusal = buildMidgardRedeemerDataHeadRejectionTrace({
      itemIndex: 0,
      itemCount: 1,
      itemBytes: item(data),
    });
    expect(refusal.control.traversal!.offset).toBe(offset);
    expect(
      isMidgardRedeemerDataHeadRejection(refusal.control, refusal.witness),
    ).toBe(true);
    const substituted = {
      ...refusal.witness,
      chunkProof: {
        ...refusal.witness.chunkProof!,
        chunk: Buffer.alloc(refusal.witness.chunkProof!.chunk.length),
      },
    };
    expect(
      isMidgardRedeemerDataHeadRejection(refusal.control, substituted),
    ).toBe(false);
  },
);
it.each([
  "4100",
  "5840" + "ab".repeat(64),
  "5818" + "ab".repeat(24),
  "5f5840" + "ab".repeat(64) + "41abff",
  "a0",
  "a10101",
  "d87980",
])("canonical byte/map/tag head %s cannot claim a refusal", (data) => {
  expect(inspectMidgardRedeemerSequenceHeads(Buffer.from(data, "hex"))).toEqual(
    { kind: "canonical" },
  );
  const trace = buildMidgardRedeemerItemProofTrace({
    itemIndex: 0,
    itemCount: 1,
    itemBytes: item(data),
    mode: MidgardRedeemerItemProofModes.Data,
  });
  const head = trace.steps[2]!;
  expect(
    isMidgardRedeemerDataHeadRejection(head.control, {
      ...head.witness,
      action: { kind: "traverseData", action: null },
    }),
  ).toBe(false);
});
it.each(["58", "5900", "5a000000", "5b00000000000000", "b8", "d900"])(
  "incomplete argument %s stays unsupported",
  (data) => {
    expect(
      inspectMidgardRedeemerSequenceHeads(Buffer.from(data, "hex")).kind,
    ).toBe("unsupported");
  },
);
