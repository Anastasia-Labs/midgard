/**
 * The witness field's publication through the watcher's installed
 * application when its own wallet spends a confirmed publication while the
 * next one is still waiting to be included.
 *
 * The first publication is published again; the publication that was
 * waiting lands just before that replacement and is the result for its
 * chunk, though a later publication was recorded since: it is never
 * published again.
 */
import { describe, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
  recovery: undefined as unknown,
}));
vi.mock("../src/workflow/deployment-manifest-binding.js", async (load) => ({
  ...(await load<
    typeof import("../src/workflow/deployment-manifest-binding.js")
  >()),
  bindFraudProofWorkflowDeployment: async () => hooks.binding,
}));
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  const { withFraudProofL1Recovery } = await import(
    "../src/workflow/l1-source.js"
  );
  return {
    ...actual,
    createFraudProofFamilyL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyL1ObservationPort
      >[0],
    ) =>
      withFraudProofL1Recovery(
        actual.createFraudProofFamilyRawL1ObservationPort({
          ...input,
          authority: hooks.authority as Parameters<
            typeof actual.createFraudProofFamilyRawL1ObservationPort
          >[0]["authority"],
        }),
        hooks.recovery as Parameters<typeof withFraudProofL1Recovery>[1],
      ),
  };
});

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  fieldCarriageOf,
  publicationsOf,
} from "./support/own-field-carriage-installed.js";
import {
  type LedgerTransaction,
  removeOutput,
} from "./support/own-outputs-installed.js";
import {
  expectOwnFieldCarriage,
  type PlaceContext,
  runWith,
  sitesOf,
} from "./support/witness-field-carriage-installed-run.js";

describe("witness field publication in flight when its own wallet spends an earlier one, in the installed watcher application", () => {
  let baseline: Awaited<ReturnType<typeof runWith>>["cost"];

  it("publishes and certifies the field: the cost the other row matches", async () => {
    const { run, cost } = await runWith(hooks, () => []);
    expect(run.convicted).toBe(true);
    baseline = cost;
  }, 900_000);

  it("takes the publication in flight when its own wallet spent an earlier one as the result for its chunk once it lands, after the earlier one is published again", async () => {
    let held: LedgerTransaction | undefined;
    let spent: string | undefined;
    let replacement: string | undefined;
    const context: { place?: PlaceContext } = {};
    const moved = new Set<string>();
    const { run, watch, harness, cost } = await runWith(hooks, () => [], {
      decide: async (transaction, { harness, watch }) => {
        const outputs = publicationsOf(transaction, sitesOf(harness));
        if (outputs.length === 0) return "accept";
        if (held === undefined) {
          // The first publication lands; the second is sent and held, as
          // a transaction still waiting to be included.
          if (
            fieldCarriageOf(watch, sitesOf(harness)).publications.length === 0
          )
            return "accept";
          held = transaction;
          return "withhold";
        }
        if (transaction.txHash === held.txHash)
          return replacement === undefined ? "withhold" : "accept";
        if (spent === undefined || replacement !== undefined) return "accept";
        // The first publication's replacement is sent. The held
        // publication is included just before it, and the replacement waits.
        replacement = transaction.txHash;
        await harness.emulator.submitTx(held.cbor);
        harness.emulator.awaitBlock();
        return "withhold";
      },
      during: (_, place) => {
        context.place = place;
      },
      // Between turns, once the second publication is held: the prover's
      // wallet spends the first publication's output back to itself, and
      // time passes until the first publication's transaction is past the
      // recovery depth.
      afterTurn: async () => {
        if (held === undefined || spent !== undefined) return;
        const { harness, watch } = context.place!;
        const [first] = fieldCarriageOf(watch, sitesOf(harness))
          .publications[0]!.outputs;
        const [txHash, index] = first!.outRef.split("#");
        const utxo = harness.emulator.ledger[`${txHash!}${index!}`]?.utxo;
        if (utxo === undefined) return;
        spent = first!.outRef;
        await removeOutput(
          watch,
          harness.proverLucid,
          utxo,
          harness.proverSigner.address,
        );
        moved.add(`${watch.accepted.at(-1)!.txHash}#0`);
        harness.emulator.awaitBlock(
          DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth,
        );
      },
    });
    expect(replacement).toBeDefined();
    expect(run.convicted).toBe(true);
    const carriage = expectOwnFieldCarriage(watch, harness, moved);
    // The held publication landed and is the one its chunk has.
    const heldOutputs = publicationsOf(held!, sitesOf(harness));
    const live = carriage.publications
      .flatMap(({ outputs }) => outputs)
      .filter(({ outRef }) => outRef !== spent);
    for (const { outRef, datum } of heldOutputs)
      expect(
        live.filter((output) => output.datum === datum).map((o) => o.outRef),
      ).toEqual([outRef]);
    expect(new Set(live.map(({ datum }) => datum)).size).toBe(live.length);
    // Exactly one publication is added: the first one's replacement.
    const publishes = (rows: readonly { kind: string }[]) =>
      rows.filter(({ kind }) => kind === "publish").length;
    expect(publishes(cost)).toBe(publishes(baseline) + 1);
  }, 900_000);
});
