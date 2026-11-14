/**
 * The installed execution-native-script-invalid replay. An accepted or
 * forced-valid transaction whose observer's native script is false is
 * detected at that observer; one whose script holds is left alone. Forced
 * rejections are detected exactly when the recorded reason contradicts native
 * evaluation. A transaction whose source input has no predecessor descriptor
 * gives no detection, the block's replay completes, and a union with the
 * reference-input family still returns its finding. A transaction whose
 * address witness, output or mint item is not in the shape the replay reads
 * gives no detection, and a union with the family owning that shape still
 * returns its finding.
 */
import {
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import { encodeMidgardMintPolicyItem } from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { type LedgerEntry, singleLedgerAt } from "../src/ledger-at.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY,
  type CompleteCanonicalReplay,
  createCompleteCanonicalReplayUnion,
  EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
  TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import {
  admissibleNativeObservers,
  buildNativeBlock,
  machineNativeVerdict,
  native,
  nativeFalseAt,
  type NativeSource,
  type NativeSubjectShape,
  PAYER,
  sig,
  signingKey,
} from "./support/execution-native-script-invalid-emulator.subject.js";
import { protectedScriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import { unreadableItemViolations } from "./support/unreadable-item-block.js";

const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/execution-native-script-invalid-replay",
  grade: "security",
} as const;

const ENSI = EXECUTION_NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY;

const COSIGNER = signingKey(1);
const ABSENT = signingKey(2);

/** An absent key or an upper bound below 10: false without an upper bound. */
const NEITHER = native({
  type: "any",
  scripts: [sig(ABSENT), { type: "before", slot: 10n }],
});
/** Signed by the payer, valid only from slot 100. */
const NOT_YET = native({
  type: "all",
  scripts: [sig(PAYER), { type: "after", slot: 100n }],
});
/** Two of the payer, the co-signer and an absent key. */
const TWO_OF_THREE = native({
  type: "atLeast",
  required: 2n,
  scripts: [sig(PAYER), sig(ABSENT), sig(COSIGNER)],
});

const neitherShape = () => admissibleNativeObservers([NEITHER]);
const notYetShape = () =>
  admissibleNativeObservers([NOT_YET], { validityStart: 0n });
const twoOfThreeShape = () =>
  admissibleNativeObservers([TWO_OF_THREE], { signers: [COSIGNER] });

const rejected = (reason: SDK.RejectionReason): NativeSource => ({
  kind: "forced",
  verdict: { ForcedTxInvalid: { reason } },
});

/** The installed replay's detections for the block committing `shape` from
 * `source`, against `ledger` as the predecessor. */
const replayUnder = async (
  replayer: CompleteCanonicalReplay,
  source: NativeSource,
  shape: NativeSubjectShape,
  ledger: readonly LedgerEntry[] = [
    ...(shape.spend ?? []),
    ...(shape.reference ?? []),
  ],
) => {
  const built = await buildNativeBlock({ shape, source, ledger });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({
      header: built.block.header,
      headerHash: built.headerHash,
    }),
    payloadEnvelopeCbor: built.block.reconstruction.payloadEnvelopeCbor,
    daProvenance,
  });
  const decision = await replayer.replay(evidence, {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      ledgerAt: singleLedgerAt(evidence.header.prevHeaderHash, ledger),
      currentEvidence: evidence,
    }),
  });
  return decision.detections.map((detection) =>
    replayer === ENSI ? detection.detectionId : detection.violationId,
  );
};

const AT_OBSERVER = (sourceKind: number) =>
  `execution-native-script-invalid:${sourceKind.toString()}:0:2:0`;

describe("executionNativeScriptInvalid replay over accepted transactions", () => {
  it("detects an observer whose native script is false", async () => {
    await expect(
      replayUnder(ENSI, { kind: "normal" }, neitherShape()),
    ).resolves.toEqual([AT_OBSERVER(0)]);
  });

  it("leaves alone a transaction whose native script holds", async () => {
    await expect(
      replayUnder(ENSI, { kind: "normal" }, twoOfThreeShape()),
    ).resolves.toEqual([]);
  });
});

describe("executionNativeScriptInvalid replay over forced transactions", () => {
  it("detects a forced acceptance whose native script is false", async () => {
    await expect(
      replayUnder(
        ENSI,
        { kind: "forced", verdict: "ForcedTxValid" },
        neitherShape(),
      ),
    ).resolves.toEqual([AT_OBSERVER(1)]);
  });

  it("finds nothing in the node's own rejection of a native script that is not yet valid", async () => {
    const verdict = await machineNativeVerdict(notYetShape());
    expect(verdict).toStrictEqual({
      ForcedTxInvalid: { reason: nativeFalseAt(2n, 0n) },
    });
    await expect(
      replayUnder(ENSI, { kind: "forced", verdict }, notYetShape()),
    ).resolves.toEqual([]);
  });

  it.each([
    [
      "ExecutionNativeScriptFalse at an observer whose script holds",
      twoOfThreeShape,
      rejected(nativeFalseAt(2n, 0n)),
      [AT_OBSERVER(1)],
    ],
    [
      "a reason recorded after native evaluation",
      neitherShape,
      rejected("ValueNotPreserved"),
      [AT_OBSERVER(1)],
    ],
    [
      "a reason recorded before native evaluation",
      neitherShape,
      rejected("FeeBelowMinimum"),
      [],
    ],
  ] as const)("on %s", async (_label, shape, source, ids) => {
    await expect(replayUnder(ENSI, source, shape())).resolves.toEqual(ids);
  });
});

describe("executionNativeScriptInvalid replay over an input with no predecessor descriptor", () => {
  // The reference input carrying the observer's script is absent from the
  // predecessor ledger, so no purpose walk can resolve the native source.
  it("declines the leaf, completes the block's replay, and the reference-input family still convicts", async () => {
    const shape = neitherShape();
    const ledger = shape.spend ?? [];
    await expect(
      replayUnder(ENSI, { kind: "normal" }, shape, ledger),
    ).resolves.toEqual([]);
    const union = await replayUnder(
      createCompleteCanonicalReplayUnion([
        NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
        ENSI,
      ]),
      { kind: "normal" },
      shape,
      ledger,
    );
    expect(union).toEqual(["no-reference-input"]);
  });
});

describe("executionNativeScriptInvalid replay over an address witness that does not decode", () => {
  // Native evaluation reads field 7; one item there is five zero bytes.
  const shape = (): NativeSubjectShape => ({
    ...neitherShape(),
    rawAddressItems: [Buffer.alloc(5)],
  });

  it("declines an accepted transaction, completes the block's replay, and the field-shape family still convicts", async () => {
    await expect(
      replayUnder(ENSI, { kind: "normal" }, shape()),
    ).resolves.toEqual([]);
    await expect(
      replayUnder(
        createCompleteCanonicalReplayUnion([
          COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY,
          ENSI,
        ]),
        { kind: "normal" },
        shape(),
      ),
    ).resolves.toEqual(["committed-field-shape"]);
  });

  // Fixture-only: L1 forced admission refuses a field-7 item that does not
  // decode, so no such forced leaf is committed.
  it("declines a forced acceptance", async () => {
    await expect(
      replayUnder(ENSI, { kind: "forced", verdict: "ForcedTxValid" }, shape()),
    ).resolves.toEqual([]);
  });
});

describe("executionNativeScriptInvalid replay beside an item the purpose walk cannot read", () => {
  const OUTPUT = TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY;
  const MINT = MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY;
  // A native script that never holds, carried in field 6.
  const FALSE = native({ type: "any", scripts: [] });
  const witness = [encodeMidgardVersionedScript(FALSE)];
  // A protected output paying FALSE's credential: its receive runs FALSE.
  const receive = {
    scriptWitnesses: witness,
    outputs: [
      encodeMidgardTxOutput({
        address: protectedScriptAddress(FALSE),
        value: { lovelace: 2_000_000n, assets: new Map() },
      }),
    ],
  };
  // One unit of FALSE's policy minted: its mint runs FALSE.
  const mint = {
    scriptWitnesses: witness,
    mintPolicyItems: [
      encodeMidgardMintPolicyItem({
        policyId: Buffer.from(hashMidgardVersionedScript(FALSE), "hex"),
        assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
      }),
    ],
  };

  it.each([
    ["a native-protected output", receive],
    ["a native mint policy", mint],
  ] as const)(
    "detects %s whose script is false when every item reads",
    async (_label, items) => {
      await expect(
        unreadableItemViolations(ENSI, { kind: "normal" }, items),
      ).resolves.toEqual(["execution-native-script-invalid"]);
    },
  );

  it.each([
    [
      "an accepted output",
      { ...mint, outputs: [Buffer.alloc(64)] },
      [OUTPUT, ENSI],
      ["transaction-output-non-canonical"],
    ],
    [
      "an accepted mint item",
      { ...receive, mintPolicyItems: [Buffer.from("00", "hex")] },
      [ENSI, MINT],
      ["mint-item-non-canonical"],
    ],
  ] as const)(
    "finds nothing beside %s it cannot read and the union completes",
    async (_label, items, members, union) => {
      await expect(
        unreadableItemViolations(ENSI, { kind: "normal" }, items),
      ).resolves.toEqual([]);
      await expect(
        unreadableItemViolations(
          createCompleteCanonicalReplayUnion([...members]),
          { kind: "normal" },
          items,
        ),
      ).resolves.toEqual(union);
    },
  );
});
