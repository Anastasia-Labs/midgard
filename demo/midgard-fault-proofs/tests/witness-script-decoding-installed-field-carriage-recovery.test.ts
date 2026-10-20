/**
 * The witness field's carriage through the watcher's installed application
 * when its own carriage transactions or outputs do not stay as it sent them.
 *
 * A stopped process sends the carriage transaction it signed and reads the
 * outputs it names; a publication transaction that can never land is
 * published again, never served by an unrelated output with the same datum;
 * a confirmed publication its own wallet spends while its transaction stays
 * on chain is published once more in its place; and a certificate
 * transaction that can never land is certified again, as is a certificate its
 * own wallet retires while the certify transaction stays on chain.
 *
 * The emulator has no chain rollback, so a carriage transaction rolled back
 * and then included again, or rolled back and then expired, is not run here.
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
import { retireFieldPreimageCertificateRedeemer } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  certificateOf,
  fieldCarriageOf,
  placeSameDatum,
  publicationsOf,
} from "./support/own-field-carriage-installed.js";
import {
  type LedgerTransaction,
  type LedgerWatch,
  removeOutput,
} from "./support/own-outputs-installed.js";
import {
  expectBaselineCost,
  stopsOnceAndRestarts,
} from "./support/script-decode-installed-run.js";
import {
  expectOwnFieldCarriage,
  type Harness,
  type PlaceContext,
  POLL_CAP,
  runWith,
  sitesOf,
} from "./support/witness-field-carriage-installed-run.js";

/**
 * Polls the runner waits, the certificate retired, before its certify
 * transaction is past the recovery depth.
 */
const HOLD_POLLS = 100;

/**
 * The prover's wallet retires the application's certificate at `outRef`
 * (spends it and burns its token) in a block of its own. Returns the
 * outputs the retirement pays back to the wallet.
 */
const retireCertificate = async (
  watch: LedgerWatch,
  harness: Harness,
  outRef: string,
): Promise<readonly string[]> => {
  const [txHash, index] = outRef.split("#");
  const utxo = harness.emulator.ledger[`${txHash!}${index!}`]!.utxo;
  const { policyId, mintingScript } =
    harness.contracts.fieldPreimageCertificate;
  const unit = Object.keys(utxo.assets).find((asset) =>
    asset.startsWith(policyId),
  )!;
  const lucid = harness.proverLucid;
  await watch.asOtherParty(async () => {
    lucid.clearUTxOOverride();
    const unsigned = await lucid
      .newTx()
      .collectFrom([utxo], Data.void())
      .mintAssets({ [unit]: -1n }, retireFieldPreimageCertificateRedeemer())
      .attach.Script(mintingScript)
      .addSignerKey(harness.proverSigner.paymentKeyHash)
      .complete();
    await (await unsigned.sign.withWallet().complete()).submit();
    lucid.clearUTxOOverride();
  });
  harness.emulator.awaitBlock();
  const retirement = watch.accepted.at(-1)!;
  return retirement.outputs.map(
    (_, output) => `${retirement.txHash}#${output.toString()}`,
  );
};

describe("witness field carriage recovered from its own journal, in the installed watcher application", () => {
  let baseline: Awaited<ReturnType<typeof runWith>>["cost"];
  let baselineJournal: number;

  it("publishes and certifies the field: the cost every other row matches", async () => {
    const { run, cost } = await runWith(hooks, () => []);
    expect(run.convicted).toBe(true);
    baseline = cost;
    baselineJournal = run.journalEntries;
  }, 900_000);

  it.each(["publication", "certificate"] as const)(
    "sends the %s transaction its stopped process signed but never sent, and reads the outputs it names",
    async (kind) => {
      let withheld: string | undefined;
      const stopping = stopsOnceAndRestarts(POLL_CAP);
      const { run, watch, harness, cost } = await runWith(hooks, () => [], {
        decide: (transaction, { harness }) => {
          const sites = sitesOf(harness);
          const pays =
            kind === "publication"
              ? publicationsOf(transaction, sites).length > 0
              : certificateOf(transaction, sites) !== undefined;
          if (withheld !== undefined || !pays) return "accept";
          // Signed and journaled, lost before the ledger saw it; the
          // process stops at its next poll.
          withheld = transaction.txHash;
          stopping.stopAtNextPoll();
          return "withhold";
        },
        during: stopping.during,
        onStopped: stopping.onStopped,
      });
      expect(stopping.stops()).toBe(1);
      expect(run.convicted).toBe(true);
      const carriage = expectOwnFieldCarriage(watch, harness);
      const sent =
        kind === "publication"
          ? carriage.publications.map(({ transaction }) => transaction.txHash)
          : carriage.certificates.map(({ transaction }) => transaction.txHash);
      expect(sent[0]).toBe(withheld);
      expectBaselineCost(cost, baseline);
    },
    900_000,
  );

  it("publishes a confirmed publication once more when its own wallet spends it outside the workflow while its transaction stays on chain", async () => {
    let spent: string | undefined;
    const moved = new Set<string>();
    const { run, watch, harness, cost } = await runWith(hooks, () => [], {
      // While the runner waits on a confirmation, once a later publication
      // is on chain (so the first is confirmed) and before the certificate:
      // the prover's wallet spends the first publication's output back to
      // itself.
      during: ({ clock }, { harness, watch }) => {
        clock.state.onPoll = async () => {
          if (spent !== undefined) return;
          const carriage = fieldCarriageOf(watch, sitesOf(harness));
          if (
            carriage.publications.length < 2 ||
            carriage.certificates.length > 0
          )
            return;
          const [first] = carriage.publications[0]!.outputs;
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
          // Time passes until the publication's transaction is past the
          // recovery depth, its output still gone.
          harness.emulator.awaitBlock(
            DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth,
          );
        };
        return () => {
          clock.state.onPoll = undefined;
        };
      },
    });
    expect(spent).toBeDefined();
    expect(run.convicted).toBe(true);
    const carriage = expectOwnFieldCarriage(watch, harness, moved);
    const datum = carriage.publications
      .flatMap(({ outputs }) => outputs)
      .find(({ outRef }) => outRef === spent)!.datum;
    const published = carriage.publications
      .flatMap(({ outputs }) => outputs)
      .filter((output) => output.datum === datum);
    // The spent output and exactly one publication in its place.
    expect(published).toHaveLength(2);
    const certify = carriage.certificates[0]!.transaction;
    expect(certify.referenceInputs).toContain(published[1]!.outRef);
    expect(certify.referenceInputs).not.toContain(spent);
    // Exactly one publication is added, the one in its place. The
    // publication in flight when the output went lands and is the result for
    // its chunk: it is never published again.
    const others = (rows: readonly { kind: string }[]) =>
      rows.map(({ kind }) => kind).filter((kind) => kind !== "publish");
    const publishes = (rows: readonly { kind: string }[]) =>
      rows.filter(({ kind }) => kind === "publish").length;
    expect(others(cost)).toEqual(others(baseline));
    expect(publishes(cost)).toBe(publishes(baseline) + 1);
    // Apart from the spent output, every chunk is published exactly once.
    const live = carriage.publications
      .flatMap(({ outputs }) => outputs)
      .filter(({ outRef }) => outRef !== spent)
      .map(({ datum }) => datum);
    expect(new Set(live).size).toBe(live.length);
  }, 900_000);

  it("publishes again when its publication transaction is lost and another transaction of its wallet spends one of its coins, and never reads a copy for it", async () => {
    let lost: LedgerTransaction | undefined;
    let spent = false;
    const context: { place?: PlaceContext } = {};
    const moved = new Set<string>();
    const { run, watch, harness } = await runWith(hooks, () => [], {
      decide: (transaction, { harness }) => {
        if (lost !== undefined)
          return transaction.txHash === lost.txHash ? "withhold" : "accept";
        if (publicationsOf(transaction, sitesOf(harness)).length === 0)
          return "accept";
        // Signed and journaled, and never seen by the ledger, however often
        // it is sent.
        lost = transaction;
        return "withhold";
      },
      during: (_, place) => {
        context.place = place;
      },
      // Between turns: the lost transaction is sent again every turn, so
      // the coin goes elsewhere once it has been sent at least once.
      afterTurn: async () => {
        if (lost === undefined || spent) return;
        const { harness, watch, other } = context.place!;
        const [txHash, index] = lost.inputs[0]!.split("#");
        const utxo = harness.emulator.ledger[`${txHash!}${index!}`]?.utxo;
        if (utxo === undefined) return;
        spent = true;
        // Another transaction of the prover's wallet spends a coin the lost
        // transaction spends, so it can never land, and exact copies of what
        // it would have published are on chain instead.
        await removeOutput(
          watch,
          harness.proverLucid,
          utxo,
          harness.proverSigner.address,
        );
        moved.add(`${watch.accepted.at(-1)!.txHash}#0`);
        harness.emulator.awaitBlock();
        for (const output of publicationsOf(lost, sitesOf(harness)))
          await placeSameDatum(watch, other, { ...output, token: false });
        harness.emulator.awaitBlock();
      },
    });
    expect(spent).toBe(true);
    expect(run.convicted).toBe(true);
    expect(watch.ours().map(({ txHash }) => txHash)).not.toContain(
      lost!.txHash,
    );
    const carriage = expectOwnFieldCarriage(watch, harness, moved);
    const lostDatums = publicationsOf(lost!, sitesOf(harness)).map(
      ({ datum }) => datum,
    );
    const republished = carriage.publications
      .flatMap(({ outputs }) => outputs)
      .filter(({ datum }) => lostDatums.includes(datum));
    expect(republished.map(({ datum }) => datum)).toEqual(lostDatums);
    expect(watch.plantedOutRefs().size).toBeGreaterThan(0);
  }, 900_000);

  it("certifies the field again when its certificate transaction is lost and another transaction of its wallet spends one of its coins", async () => {
    let lost: LedgerTransaction | undefined;
    let spent = false;
    const context: { place?: PlaceContext } = {};
    const moved = new Set<string>();
    const { run, watch, harness, cost } = await runWith(hooks, () => [], {
      decide: (transaction, { harness }) => {
        if (lost !== undefined)
          return transaction.txHash === lost.txHash ? "withhold" : "accept";
        if (certificateOf(transaction, sitesOf(harness)) === undefined)
          return "accept";
        // Signed and journaled, and never seen by the ledger, however often
        // it is sent.
        lost = transaction;
        return "withhold";
      },
      during: (_, place) => {
        context.place = place;
      },
      // Between turns, once the lost transaction has been sent: another
      // transaction of the prover's wallet spends a coin it spends, so it
      // can never land.
      afterTurn: async () => {
        if (lost === undefined || spent) return;
        const { harness, watch } = context.place!;
        const [txHash, index] = lost.inputs[0]!.split("#");
        const utxo = harness.emulator.ledger[`${txHash!}${index!}`]?.utxo;
        if (utxo === undefined) return;
        spent = true;
        await removeOutput(
          watch,
          harness.proverLucid,
          utxo,
          harness.proverSigner.address,
        );
        moved.add(`${watch.accepted.at(-1)!.txHash}#0`);
        harness.emulator.awaitBlock();
      },
    });
    expect(spent).toBe(true);
    expect(run.convicted).toBe(true);
    expect(watch.ours().map(({ txHash }) => txHash)).not.toContain(
      lost!.txHash,
    );
    // One certificate, of the same field, in the lost one's place.
    const carriage = expectOwnFieldCarriage(watch, harness, moved);
    expect(carriage.certificates[0]!.output.datum).toBe(
      certificateOf(lost!, sitesOf(harness))!.datum,
    );
    // The same transactions as the baseline: only the certificate's is
    // built again.
    expect(cost.map(({ kind }) => kind)).toEqual(
      baseline.map(({ kind }) => kind),
    );
  }, 900_000);

  it.each([
    ["before its certify transaction is confirmed", "landed"],
    ["once its certify transaction is confirmed", "read"],
  ] as const)(
    "certifies the field once more when its own wallet retires the certificate %s",
    async (_, when) => {
      let retired: string | undefined;
      let held = 0;
      const withheld = new Set<string>();
      const moved = new Set<string>();
      const retire = async (
        watch: LedgerWatch,
        harness: Harness,
        outRef: string,
      ) => {
        retired = outRef;
        for (const output of await retireCertificate(watch, harness, outRef))
          moved.add(output);
      };
      const { run, watch, harness, cost } = await runWith(hooks, () => [], {
        // "read": once the certify transaction is confirmed the application
        // builds the step that reads the certificate. As that step is sent,
        // the wallet retires the certificate first, so the step never lands.
        decide: async (transaction, { harness, watch }) => {
          if (retired !== undefined)
            return withheld.has(transaction.txHash) ? "withhold" : "accept";
          const [certificate] = fieldCarriageOf(
            watch,
            sitesOf(harness),
          ).certificates;
          if (
            when !== "read" ||
            certificate === undefined ||
            !transaction.referenceInputs.includes(certificate.output.outRef)
          )
            return "accept";
          await retire(watch, harness, certificate.output.outRef);
          withheld.add(transaction.txHash);
          return "withhold";
        },
        during: ({ clock }, { harness, watch }) => {
          clock.state.onPoll = async () => {
            if (retired === undefined) {
              // "landed": the certify transaction is in the block this poll
              // added, and the application has not reconciled it yet.
              const [certificate] = fieldCarriageOf(
                watch,
                sitesOf(harness),
              ).certificates;
              if (when === "landed" && certificate !== undefined)
                await retire(watch, harness, certificate.output.outRef);
              return;
            }
            // The runner waits while the certify transaction is within the
            // recovery depth; then time passes it.
            held += 1;
            if (held === HOLD_POLLS)
              harness.emulator.awaitBlock(
                DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth,
              );
          };
          return () => {
            clock.state.onPoll = undefined;
          };
        },
      });
      expect(retired).toBeDefined();
      expect(withheld.size).toBe(when === "read" ? 1 : 0);
      expect(held).toBeGreaterThanOrEqual(HOLD_POLLS);
      expect(run.convicted).toBe(true);
      // The retired certificate and exactly one of the same field in its
      // place, which the proof reads.
      const carriage = expectOwnFieldCarriage(watch, harness, moved, 2);
      const [first, second] = carriage.certificates;
      expect(first!.output.outRef).toBe(retired);
      expect(second!.output.datum).toBe(first!.output.datum);
      expect(carriage.readersOf(retired!)).toEqual([]);
      // Only the certificate is built again.
      const certifies = (rows: readonly { kind: string }[]) =>
        rows.filter(({ kind }) => kind === "certify").length;
      const others = (rows: readonly { kind: string }[]) =>
        rows.map(({ kind }) => kind).filter((kind) => kind !== "certify");
      expect(others(cost)).toEqual(others(baseline));
      expect(certifies(cost)).toBe(certifies(baseline) + 1);
      // Waiting on the same pending reconciliation adds nothing to the
      // journal: over the HOLD_POLLS wait it gains only the few entries one
      // more certification and the step built again write, not one a poll.
      expect(run.polls).toBeGreaterThan(HOLD_POLLS);
      expect(run.journalEntries).toBeLessThan(baselineJournal + 20);
    },
    900_000,
  );
});
