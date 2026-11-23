import * as SDK from "@al-ft/midgard-sdk/core";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { isReplayLocalFault } from "../src/block-replay.faults.js";
import { checkedLedgerAsLedger } from "../src/checked-ledger-store.js";
import { checkLedgerAgainstRoot } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  LedgerNodeMissing,
} from "../src/ledger-store/index.js";
import {
  admitNativeScriptDecodingWorkflowArtifact,
  prepareNativeScriptDecodingWorkflowArtifact,
} from "../src/native-script-decoding/artifact.js";
import { prepareNativeScriptDecodingReplay } from "../src/native-script-decoding/replay.js";
import { keyValuePhasProof } from "../src/transition-trace/phas.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.reconstruct-da-payload.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.classify-canonical-block-violations.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
import { reencodeFixturePayload } from "./helpers/canonical-block-evidence-fixture.js";
import {
  block,
  NATIVE,
  OTHER,
  SPENT,
  type Step,
} from "./helpers/event-ledger-committed-steps.js";
import { referenceLedgerBefore } from "./helpers/event-ledger-reference.js";
import { nativeDecodingFixture } from "./support/native-script-decoding-retained.js";
import { ledgerEntriesAfter } from "./support/predecessor-ledger.js";

describe("nativeScriptDecoding authenticated retained replay", () => {
  it.each([0n, -1n, 1n])(
    "authenticates forced coordinate %s and JSON recovery",
    async (index) => {
      const { evidence, context } = await nativeDecodingFixture({ index });
      const decision =
        await NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY.replay(
          evidence,
          context,
        );
      expect(decision.detections).toHaveLength(1);
      const classification = await classifyCanonicalBlockViolations({
        evidence,
        detections: decision.detections,
        minimumConfirmationDepth: 1,
      });
      if (classification.decision !== "fault_detected")
        throw new Error("missing detection");
      const artifact = await prepareNativeScriptDecodingWorkflowArtifact({
        evidence,
        classification,
        replayContext: context,
      });
      const restored = await admitNativeScriptDecodingWorkflowArtifact(
        JSON.parse(JSON.stringify(artifact)),
      );
      expect(restored.coordinate.outpointCursor).toBe(index.toString());
      await expect(
        admitNativeScriptDecodingWorkflowArtifact({
          ...artifact,
          headerHash: "00".repeat(28),
        }),
      ).rejects.toThrow();
    },
  );
  it("detects malformed accepted source without treating its descriptor as canonical", async () => {
    const { evidence, context } = await nativeDecodingFixture({
      direction: 0,
      item: Buffer.from("8200428109", "hex"),
    });
    expect(
      (
        await NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY.replay(
          evidence,
          context,
        )
      ).detections,
    ).toHaveLength(1);
  });
  it("refuses honest acceptance and substantive rejection", async () => {
    const accepted = await nativeDecodingFixture({ direction: 0 });
    expect(
      (
        await NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY.replay(
          accepted.evidence,
          accepted.context,
        )
      ).detections,
    ).toHaveLength(0);
    const rejected = await nativeDecodingFixture({
      item: Buffer.from("8200428109", "hex"),
    });
    expect(
      (
        await NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY.replay(
          rejected.evidence,
          rejected.context,
        )
      ).detections,
    ).toHaveLength(0);
  });
  it("honors prior committed spending of the selected reference", async () => {
    const { evidence, context } = await nativeDecodingFixture({
      consumed: true,
    });
    expect(
      (
        await NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY.replay(
          evidence,
          context,
        )
      ).detections,
    ).toHaveLength(0);
  });
});

it.each([
  "ResolvedReferenceScriptMalformed",
  "ResolvedReferenceScriptNodeLimit",
  "ResolvedReferenceScriptDepthLimit",
] as const)("installs exact wrongful reason %s", async (reasonName) => {
  const { evidence, context } = await nativeDecodingFixture({ reasonName });
  expect(
    (
      await NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        context,
      )
    ).detections,
  ).toHaveLength(1);
});

it("reconstructs a block whose native script_ref does not decode, from the output bytes alone", async () => {
  const fixture = await nativeDecodingFixture({
    direction: 0,
    item: Buffer.from("8200428109", "hex"),
  });
  const body = fixture.predecessor.reconstruction.payload.block_body;
  expect(body.validation_trace_witnesses).toEqual([]);
  const reconstruction = await reconstructDaPayload({
    payloadEnvelopeCbor: await reencodeFixturePayload(
      fixture.predecessor.reconstruction.payload,
    ),
    expectedHeaderHash: fixture.predecessor.headerHash,
  });
  expect(reconstruction.payload.block_body.header.utxosRoot).toBe(
    body.header.utxosRoot,
  );
});

it.each(["key", "raw-script", "root"] as const)(
  "refuses %s substitution at the predecessor root check",
  async (mutation) => {
    const fixture = await nativeDecodingFixture({
      direction: 0,
      item: Buffer.from("8200428109", "hex"),
    });
    let entries = ledgerEntriesAfter(fixture.predecessor);
    if (mutation === "key")
      entries = [
        { outRef: Buffer.alloc(fixture.key.length), output: fixture.output },
      ];
    if (mutation === "raw-script")
      entries = [
        {
          outRef: fixture.key,
          output: Buffer.concat([
            fixture.output.subarray(0, -1),
            Buffer.from("08", "hex"),
          ]),
        },
      ];
    await expect(
      checkLedgerAgainstRoot({
        entries,
        utxosRoot:
          mutation === "root"
            ? "00".repeat(32)
            : fixture.predecessor.header.utxosRoot,
        label: "predecessor ledger",
      }),
    ).rejects.toThrow();
  },
);

/** An accepted transaction spending `spent` whose reference input is
 * `NATIVE`, a native script that does not decode: a wrongful acceptance. */
const nativeFault = (spent: Buffer): Step => ({
  input: {
    spendInputs: [spent],
    referenceInputs: [NATIVE.outRef],
    outputs: [makeOutput(1_000_000n)],
    fee: 7n,
  },
  applied: true,
});

/** The coordinate of reference input 0 of the transaction at `position`. */
const referenceCoordinate = (position: bigint) => ({
  sourceKind: 0 as const,
  sourceIndex: Number(position),
  outpointSourceKind: "1",
  outpointCursor: "0",
});

describe("nativeScriptDecoding resolved descriptor", () => {
  it("reads and proves the selected input against the ledger before its transaction", async () => {
    // Step 0 spends `SPENT`: the ledger before step 1 differs from the
    // predecessor in an entry other than the one step 1 reads.
    const fixture = await block([
      { input: { spendInputs: [SPENT.outRef], fee: 7n }, applied: true },
      nativeFault(OTHER.outRef),
    ]);
    const predecessor = completeCanonicalReplayPredecessorLedger(fixture)!;
    const before = await referenceLedgerBefore({
      block: fixture.evidence,
      predecessor,
      eventKey: fixture.key(1),
      stepIndex: 1n,
    });
    expect(before.utxosRoot).not.toBe(predecessor.utxosRoot);
    const { value } = before.trie.entries.find((entry) =>
      entry.key.equals(NATIVE.outRef),
    )!;
    const prepared = await prepareNativeScriptDecodingReplay({
      current: fixture.evidence.reconstruction,
      predecessor: await checkedLedgerAsLedger(predecessor),
      coordinate: referenceCoordinate(fixture.position(1)),
    });
    expect(prepared?.descriptorCbor).toStrictEqual(value);
    expect(prepared?.ledgerTrie?.rootHex).toBe(before.utxosRoot);
    expect(await prepared?.ledgerTrie?.prove(NATIVE.outRef)).toStrictEqual(
      Buffer.from(
        Data.to(
          await keyValuePhasProof(before.trie, NATIVE.outRef, value),
          SDK.Proof,
        ),
        "hex",
      ),
    );
  });

  type Prepare = () => ReturnType<typeof prepareNativeScriptDecodingReplay>;
  it.each([
    [
      "reading the selected input",
      async (dropLedger: () => Promise<void>, prepare: Prepare) => {
        await dropLedger();
        await prepare();
      },
    ],
    [
      "proving the selected input",
      async (dropLedger: () => Promise<void>, prepare: Prepare) => {
        const prepared = await prepare();
        await dropLedger();
        await prepared?.ledgerTrie?.prove(NATIVE.outRef);
      },
    ],
  ] as const)(
    "is this process's local fault %s when the store no longer holds the ledger",
    async (_, run) => {
      // The transaction is step 0, so it reads the predecessor itself, which
      // `retain` of nothing deletes.
      const fixture = await block([nativeFault(SPENT.outRef)]);
      const predecessor = completeCanonicalReplayPredecessorLedger(fixture)!;
      const store = createLedgerStore(createMemoryLedgerStorage());
      const ledger = await store.admit(
        predecessor.entries,
        predecessor.utxosRoot,
      );
      const error: unknown = await run(
        async () => {
          await store.retain(new Set());
        },
        async () =>
          await prepareNativeScriptDecodingReplay({
            current: fixture.evidence.reconstruction,
            predecessor: ledger,
            coordinate: referenceCoordinate(fixture.position(0)),
          }),
      ).catch((cause: unknown) => cause);
      expect(isReplayLocalFault(error)).toBe(true);
      expect((error as { cause: unknown }).cause).toBeInstanceOf(
        LedgerNodeMissing,
      );
    },
  );
});
