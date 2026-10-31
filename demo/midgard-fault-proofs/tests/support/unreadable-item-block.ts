/** A block committing one transaction, accepted or forced, with no inputs and
 * raw field items the caller chooses, so a replay meets an output, mint item
 * or script witness that is not in the shape its walk reads. */
import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import type { CompleteCanonicalReplay } from "../../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildFixtureTransaction,
  type FixtureTransactionInput,
} from "../helpers/canonical-block-evidence-fixture.js";
import {
  FORCED_ORDER_KEY,
  type MissingRedeemerSource,
} from "./missing-redeemer-emulator.subject.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";

export type UnreadableItems = Pick<
  FixtureTransactionInput,
  "outputs" | "mintPolicyItems" | "scriptWitnesses"
>;

const unreadableItemBlock = async (
  source: MissingRedeemerSource,
  items: UnreadableItems,
) => {
  const nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(
    buildFixtureTransaction({ spendInputs: [], fee: 0n, ...items })
      .canonicalCbor,
  );
  const block = await buildDecodingBlockFixture({
    operatorVkey: "aa".repeat(28),
    startTime: 1_000_000n,
    priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    subject:
      source.kind === "normal"
        ? { kind: "normal", nativeTx }
        : {
            kind: "forced",
            nativeTx,
            orderKey: FORCED_ORDER_KEY,
            verdict: source.verdict,
          },
  });
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(block),
    payloadEnvelopeCbor: block.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "test/unreadable-item-block",
      grade: "security",
    },
  });
};

/** The violation ids `replayer` finds in the block committing `items` from
 * `source`. */
export const unreadableItemViolations = async (
  replayer: CompleteCanonicalReplay,
  source: MissingRedeemerSource,
  items: UnreadableItems,
): Promise<readonly string[]> =>
  (
    await replayer.replay(await unreadableItemBlock(source, items), {})
  ).detections.map((detection) => detection.violationId);
