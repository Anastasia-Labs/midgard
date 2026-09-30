import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import { computeHash32 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  mpfProofFromWitness,
  normalizedMpfRoot,
} from "../src/transition-trace/detect.js";
import { detectTransitionTraceFaults } from "../src/transition-trace/index.js";
import { buildL2ReplayFixture } from "./transition-trace-challenger.build-l2-replay-fixture.js";
import {
  buildPayloadFixture,
  reconstruct,
  sdkProof,
  withdrawalEventKey,
} from "./transition-trace-challenger.build-payload-fixture.js";
import {
  encodedEntry,
  eventToStepEntry,
  LEDGER_OUTPUT_CBOR,
  ledgerTrieValue,
  outRef,
  spendInputItem,
  withdrawalInfo,
} from "./transition-trace-challenger.native-material.js";

// A deletion whose proof ends in a Branch must keep two other children in
// that branch (`terminal_branch_keeps_two_children`). Here the spent output
// has one neighbour, so the honest delete ends in a Leaf step; the same
// neighbour re-read as a lone Branch group still proves the spent output, but
// leaves a one-child branch no honest trie holds. The challenger tooling must
// refuse it before it can accuse an honest step.

const NULL = Buffer.alloc(32);
const combine = (left: Uint8Array, right: Uint8Array): Buffer =>
  Buffer.from(computeHash32(Buffer.concat([left, right])));
const nibble = (path: Uint8Array, cursor: number): number =>
  cursor % 2 === 0 ? path[cursor >> 1]! >> 4 : path[cursor >> 1]! & 15;
const suffix = (path: Uint8Array, cursor: number): Buffer =>
  cursor % 2 === 0
    ? Buffer.concat([Buffer.from([0xff]), path.subarray(cursor / 2)])
    : Buffer.concat([
        Buffer.from([0, nibble(path, cursor)]),
        path.subarray((cursor + 1) / 2),
      ]);
const merkle = (hashes: readonly Buffer[]): Buffer => {
  if (hashes.length === 1) return hashes[0]!;
  const next: Buffer[] = [];
  for (let index = 0; index < hashes.length; index += 2) {
    next.push(combine(hashes[index]!, hashes[index + 1]!));
  }
  return merkle(next);
};

const masquerade = (spent: SDK.LedgerDeleteWitness) => {
  const [step] = spent.delete_proof;
  if (
    spent.delete_proof.length !== 1 ||
    step === undefined ||
    !("Leaf" in step)
  )
    throw new Error("expected a one-neighbour delete ending in a Leaf step");
  const cursor = Number(step.Leaf.skip);
  const spentPath = Buffer.from(computeHash32(Buffer.from(spent.key, "hex")));
  const neighbourPath = Buffer.from(step.Leaf.key, "hex");
  const neighbour = combine(
    suffix(neighbourPath, cursor + 1),
    Buffer.from(step.Leaf.value, "hex"),
  );
  const children = Array.from({ length: 16 }, (_, slot) =>
    slot === nibble(neighbourPath, cursor) ? neighbour : NULL,
  );
  const me = nibble(spentPath, cursor);
  const neighbors = Buffer.concat(
    [8, 4, 2, 1].map((size) => {
      const start = (Math.floor(me / size) ^ 1) * size;
      return merkle(children.slice(start, start + size));
    }),
  );
  return {
    neighbour,
    proof: [
      {
        Branch: { skip: BigInt(cursor), neighbors: neighbors.toString("hex") },
      },
    ] satisfies SDK.Proof,
  };
};

describe("transition-trace terminal Branch masquerade", () => {
  it("refuses a Branch standing in for the spent output's lone neighbour", async () => {
    const fixture = await buildL2ReplayFixture({
      matchingCommittedRoot: true,
      survivorCount: 1,
    });
    const spent = fixture.evidence.spentUtxos[0]!;
    expect(spent.opening).toBe("");
    await expect(
      detectTransitionTraceFaults(fixture.reconstruction, {
        l2TransactionTransitions: [fixture.evidence],
      }),
    ).resolves.toEqual([]);

    const steered = masquerade(spent);
    const premise = mpfProofFromWitness({
      key: Buffer.from(spent.key, "hex"),
      value: Buffer.from(spent.value, "hex"),
      proof: steered.proof,
      label: "masquerade",
    });
    const honest = mpfProofFromWitness({
      key: Buffer.from(spent.key, "hex"),
      value: Buffer.from(spent.value, "hex"),
      proof: spent.delete_proof,
      label: "honest",
    });
    expect(normalizedMpfRoot(premise.verify(true), "masquerade pre")).toBe(
      normalizedMpfRoot(honest.verify(true), "honest pre"),
    );
    expect(
      normalizedMpfRoot(premise.verify(false), "masquerade post"),
    ).not.toBe(normalizedMpfRoot(honest.verify(false), "honest post"));

    for (const opening of [
      "",
      Buffer.concat([Buffer.from([1]), steered.neighbour, NULL]).toString(
        "hex",
      ),
    ]) {
      await expect(
        detectTransitionTraceFaults(fixture.reconstruction, {
          l2TransactionTransitions: [
            {
              ...fixture.evidence,
              spentUtxos: [{ ...spent, opening, delete_proof: steered.proof }],
            },
          ],
        }),
      ).rejects.toMatchObject({
        code: "missingWitnessData",
        message: expect.stringContaining("does not keep two other children"),
      });
    }
  });

  it("refuses a Branch standing in for the withdrawn output's lone neighbour", async () => {
    const id = outRef(81);
    const key = spendInputItem(id.transactionId, Number(id.outputIndex));
    const output = Buffer.from(LEDGER_OUTPUT_CBOR, "hex");
    const value = ledgerTrieValue(key, output);
    const neighbourKey = Array.from({ length: 256 }, (_, byte) =>
      spendInputItem(Buffer.alloc(32, byte).toString("hex"), 0),
    ).find(
      (candidate) =>
        nibble(computeHash32(candidate), 0) !== nibble(computeHash32(key), 0),
    )!;
    const neighbour = {
      key: neighbourKey,
      value: ledgerTrieValue(neighbourKey, output),
    };
    const pre = await Trie.fromList([{ key, value }, neighbour]);
    const post = await Trie.fromList([neighbour]);
    const eventKey = withdrawalEventKey(id);
    const reconstruction = await reconstruct(
      await buildPayloadFixture({
        prevUtxosRoot: pre.hash.toString("hex"),
        withdrawals: [
          encodedEntry({
            key: id,
            keySchema: SDK.OutputReferenceSchema,
            value: withdrawalInfo(81, "WithdrawalIsValid"),
            valueSchema: SDK.WithdrawalInfoSchema,
          }),
        ],
        steps: [
          {
            schema_version: 1n,
            step_index: 0n,
            event_key: eventKey,
            phase: "Withdrawal",
            pre_utxos_root: pre.hash.toString("hex"),
            post_utxos_root: post.hash.toString("hex"),
          },
        ],
        eventToStep: [
          eventToStepEntry(eventKey, { step_index: 0n, phase: "Withdrawal" }),
        ],
      }),
    );
    const spent: SDK.LedgerDeleteWitness = {
      key: key.toString("hex"),
      value: value.toString("hex"),
      opening: "",
      delete_proof: sdkProof(await pre.prove(key)),
    };
    const detect = (spentUtxo: SDK.LedgerDeleteWitness) =>
      detectTransitionTraceFaults(reconstruction, {
        withdrawalTransitions: [{ stepIndex: 0n, spentUtxo }],
      });
    expect(
      (await detect(spent)).filter(
        (d) =>
          d.invariant === "withdrawal_transition_matches_authenticated_replay",
      ),
    ).toHaveLength(0);
    await expect(
      detect({ ...spent, delete_proof: masquerade(spent).proof }),
    ).rejects.toMatchObject({
      code: "missingWitnessData",
      message: expect.stringContaining("does not keep two other children"),
    });
  });
});
