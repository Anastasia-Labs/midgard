import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { encodeTransitionStepCbor } from "@al-ft/midgard-fault-proofs/block-replay";
import { keyValuePhasRootWithCount } from "@al-ft/midgard-fault-proofs/transition-trace/phas";
import * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKDa from "@al-ft/midgard-sdk/da";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  buildOpaqueNativeReferenceScriptLedgerOutputMaterial,
} from "@al-ft/midgard-validation/ledger-output-descriptor";
import { outRefFromTxId } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { ForeignBlockVerificationError } from "../src/mpf/verified-block-import.js";
import { rebind, verdict } from "./foreign-block-import.fixture.js";
import { normalFixture } from "./foreign-block-import.normal-fixture.js";

// A parent ledger may hold an output whose native script_ref does not
// decode: it has no canonical descriptor, and its trie leaf is the
// descriptor derived from its bytes alone. The node's foreign-block import
// builds its parent ledger with that leaf and reaches a verdict on the
// block above it.

const undecodable = {
  outRef: encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0x55),
    outputIndex: 0,
  }),
  // An output map whose key 3 holds a native script_ref (`8200 42 8109`)
  // whose body does not decode.
  output: Buffer.from(
    "a300581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0038200428109",
    "hex",
  ),
};

type Entry = { readonly outRef: Buffer; readonly output: Buffer };

const canonicalLeaf = ({ outRef, output }: Entry) => ({
  key: outRef,
  value: buildCanonicalMidgardLedgerEntryOutputMaterial({
    outRef,
    outputCbor: output,
  }).descriptorCbor,
});

/** The ledger root of `entries` beside the undecodable output, whose leaf
 * is its opaque native reference-script descriptor. */
const rootWithUndecodable = async (entries: readonly Entry[]) =>
  (
    await keyValuePhasRootWithCount([
      ...entries.map(canonicalLeaf),
      {
        key: undecodable.outRef,
        value: buildOpaqueNativeReferenceScriptLedgerOutputMaterial({
          outputIndex: 0,
          outputCbor: undecodable.output,
        }).descriptorCbor,
      },
    ])
  ).root;

/** `payload` re-rooted: its parent and post roots, and its one step's. */
const reRooted = (payload: SDKDa.DaPayload, prior: string, post: string) => {
  const body = payload.block_body;
  const [[key, value]] = body.transition_trace as [SDKDa.DaPayloadEntry];
  const step = Data.from(value, SDK.TransitionStep) as SDK.TransitionStep;
  return rebind({
    ...payload,
    block_body: {
      ...body,
      header: { ...body.header, prevUtxosRoot: prior, utxosRoot: post },
      transition_trace: [
        [
          key,
          encodeTransitionStepCbor({
            ...step,
            pre_utxos_root: prior,
            post_utxos_root: post,
          }).toString("hex"),
        ],
      ],
    },
  });
};

describe("foreign import over a parent ledger holding an output whose native script_ref does not decode", () => {
  it("reaches a verdict, and its post root is the header's", async () => {
    const f = await normalFixture();
    // The leaves are built as the fixture's own root is.
    expect(
      (await keyValuePhasRootWithCount([canonicalLeaf(f.spentEntry)])).root,
    ).toBe(f.payload.block_body.header.prevUtxosRoot);
    // The transfer pays its one input's output to its own first out-ref.
    const produced = {
      outRef: outRefFromTxId(Buffer.from(f.transactionId, "hex"), 0n),
      output: f.spentEntry.output,
    };
    const prior = await rootWithUndecodable([f.spentEntry]);
    const post = await rootWithUndecodable([produced]);
    const payload = await reRooted(f.payload, prior, post);
    const result = await verdict(payload, {
      parentUtxosRoot: prior,
      parentEntries: [f.spentEntry, undecodable].map(({ outRef, output }) => ({
        outref: outRef,
        output,
      })),
    });
    expect(
      result._tag === "Right"
        ? "imported"
        : result.left instanceof ForeignBlockVerificationError
          ? `${result.left.reason}: ${result.left.detail}`
          : String(result.left),
    ).toBe("imported");
    if (result._tag === "Left") return;
    expect(result.right.root).toBe(payload.block_body.header.utxosRoot);
    expect(result.right.root).toBe(post);
  });
});
