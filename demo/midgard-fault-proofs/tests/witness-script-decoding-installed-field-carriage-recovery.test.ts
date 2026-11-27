/**
 * The witness field's carriage through the watcher's installed application
 * when its own carriage transactions or outputs do not stay as it sent them.
 *
 * A carriage transaction a stopped process signed but never sent is built
 * again by the next process from the chain alone, and one copy lands; a
 * publication transaction that can never land is served by another party's
 * exact plain copy of what it would have published; a confirmed publication
 * its own wallet spends while its transaction stays on chain is published
 * once more in its place; and a certificate transaction that can never land
 * is certified again, or served by another party's certificate of the same
 * field, as a certificate its own wallet retires while the certify
 * transaction stays on chain is certified again.
 *
 * The emulator has no chain rollback, so a carriage transaction rolled back
 * and then included again, or rolled back and then expired, is not run here.
 */
import { describe, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
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
  return {
    ...actual,
    createFraudProofFamilyL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyL1ObservationPort
      >[0],
    ) =>
      actual.createFraudProofFamilyRawL1ObservationPort({
        ...input,
        authority: hooks.authority as Parameters<
          typeof actual.createFraudProofFamilyRawL1ObservationPort
        >[0]["authority"],
      }),
  };
});

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { retireFieldPreimageCertificateRedeemer } from "@al-ft/midgard-sdk/state-queue";
import { Data } from "@lucid-evolution/lucid";

import {
  certificateOf,
  fieldCarriageOf,
  placeCertificateBefore,
  placeSameDatum,
  publicationsOf,
} from "./support/own-field-carriage-installed.js";
import {
  ledgerOutputs,
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
  runWith,
  sitesOf,
} from "./support/witness-field-carriage-installed-run.js";

/**
 * The prover's wallet retires the application's certificate at `outRef`
 * (spends it and burns its token) in a block of its own, once a block has
 * taken the certify transaction if it is still in the mempool. Returns the
 * outputs the retirement pays back to the wallet.
 */
const retireCertificate = async (
  watch: LedgerWatch,
  harness: Harness,
  outRef: string,
): Promise<readonly string[]> => {
  const [txHash, index] = outRef.split("#");
  const key = `${txHash!}${index!}`;
  if (harness.emulator.ledger[key] === undefined) harness.emulator.awaitBlock();
  const utxo = harness.emulator.ledger[key]!.utxo;
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

describe("witness field carriage recovered from the chain, in the installed watcher application", () => {
  let baseline: Awaited<ReturnType<typeof runWith>>["cost"];
  let baselineSent: number;

  it("publishes and certifies the field: the cost every other row matches", async () => {
    const { run, cost } = await runWith(hooks, () => []);
    expect(run.convicted).toBe(true);
    baseline = cost;
    baselineSent = run.sent.length;
  }, 900_000);

  it.each(["publication", "certificate"] as const)(
    "builds the %s transaction its stopped process signed but never sent again after the restart, landing one copy",
    async (kind) => {
      let withheld: string | undefined;
      const stopping = stopsOnceAndRestarts();
      const { run, watch, harness, cost } = await runWith(hooks, () => [], {
        decide: (transaction, { harness }) => {
          const sites = sitesOf(harness);
          const pays =
            kind === "publication"
              ? publicationsOf(transaction, sites).length > 0
              : certificateOf(transaction, sites) !== undefined;
          if (withheld !== undefined || !pays) return "accept";
          // Signed, and lost before the ledger saw it; the process stops at
          // its next observation, and the next one holds nothing of it.
          withheld = transaction.txHash;
          stopping.stopAtNextObservation();
          return "withhold";
        },
        during: stopping.during,
        onStopped: stopping.onStopped,
      });
      expect(stopping.stops()).toBe(1);
      expect(withheld).toBeDefined();
      expect(run.convicted).toBe(true);
      expectOwnFieldCarriage(watch, harness);
      // The same transactions as the baseline: the rebuilt one in the lost
      // one's place, and one copy of each output.
      expectBaselineCost(cost, baseline);
    },
    900_000,
  );

  it("publishes a confirmed publication once more when its own wallet spends it outside the workflow while its transaction stays on chain", async () => {
    let spent: string | undefined;
    const moved = new Set<string>();
    const { run, watch, harness, cost } = await runWith(hooks, () => [], {
      // At an observation once a later publication is sent and before the
      // certificate: a block takes what the mempool holds, so the first is
      // confirmed, and the prover's wallet spends the first publication's
      // output back to itself.
      during: ({ clock }, { harness, watch }) => {
        clock.state.onObservation = async () => {
          if (spent !== undefined) return;
          const carriage = fieldCarriageOf(watch, sitesOf(harness));
          if (
            carriage.publications.length < 2 ||
            carriage.certificates.length > 0
          )
            return;
          const [first] = carriage.publications[0]!.outputs;
          const [txHash, index] = first!.outRef.split("#");
          const key = `${txHash!}${index!}`;
          if (harness.emulator.ledger[key] === undefined)
            harness.emulator.awaitBlock();
          const utxo = harness.emulator.ledger[key]?.utxo;
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
          clock.state.onObservation = undefined;
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
    // Exactly one publication is added, the one in its place. The later
    // publication is the result for its chunk: it is never published again.
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

  it("reads another party's exact copies of what its publication transaction would have published, once that transaction is lost and another transaction of its wallet spends one of its coins", async () => {
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
        // Signed, and never seen by the ledger, however often it is sent.
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
    // Nothing of the lost publication is published again: the copies serve
    // it, and the certificate reads a copy of each of its chunks.
    const republished = carriage.publications
      .flatMap(({ outputs }) => outputs)
      .filter(({ datum }) => lostDatums.includes(datum));
    expect(republished).toEqual([]);
    const outputs = ledgerOutputs(watch);
    const planted = watch.plantedOutRefs();
    const certify = carriage.certificates[0]!.transaction;
    const read = certify.referenceInputs
      .filter((outRef) => planted.has(outRef))
      .map((outRef) => outputs.get(outRef)?.datum);
    expect([...read].sort()).toEqual([...lostDatums].sort());
  }, 900_000);

  it.each([
    ["certifies the field again", false],
    ["reads another party's certificate of the same field", true],
  ] as const)(
    "%s when its certificate transaction is lost and another transaction of its wallet spends one of its coins",
    async (_, copy) => {
      let lost: LedgerTransaction | undefined;
      let spent = false;
      const moved = new Set<string>();
      const { run, watch, harness, cost } = await runWith(hooks, () => [], {
        decide: (transaction, { harness }) => {
          if (lost !== undefined)
            return transaction.txHash === lost.txHash ? "withhold" : "accept";
          if (certificateOf(transaction, sitesOf(harness)) === undefined)
            return "accept";
          // Signed, and never seen by the ledger, however often it is sent.
          lost = transaction;
          return "withhold";
        },
        // At the observation after the lost transaction is first sent: another
        // transaction of the prover's wallet spends a coin it spends, so
        // neither it nor a rebuild of it on the same coin can land. With
        // `copy`, another party certifies the same field in the same block,
        // so its certificate is on chain before the application plans the
        // certificate again.
        during: ({ clock }, { harness, watch, other }) => {
          clock.state.onObservation = async () => {
            if (lost === undefined || spent) return;
            const [txHash, index] = lost.inputs[0]!.split("#");
            const utxo = harness.emulator.ledger[`${txHash!}${index!}`]?.utxo;
            if (utxo === undefined) return;
            spent = true;
            if (copy)
              await placeCertificateBefore(watch, other, {
                certify: lost,
                certificate: certificateOf(lost, sitesOf(harness))!,
                below: "f".repeat(64),
              });
            await removeOutput(
              watch,
              harness.proverLucid,
              utxo,
              harness.proverSigner.address,
            );
            moved.add(`${watch.accepted.at(-1)!.txHash}#0`);
            harness.emulator.awaitBlock();
          };
          return () => {
            clock.state.onObservation = undefined;
          };
        },
      });
      expect(spent).toBe(true);
      expect(run.convicted).toBe(true);
      expect(watch.ours().map(({ txHash }) => txHash)).not.toContain(
        lost!.txHash,
      );
      const kinds = (rows: readonly { kind: string }[]) =>
        rows.map(({ kind }) => kind);
      if (!copy) {
        // One certificate, of the same field, in the lost one's place.
        const carriage = expectOwnFieldCarriage(watch, harness, moved);
        expect(carriage.certificates[0]!.output.datum).toBe(
          certificateOf(lost!, sitesOf(harness))!.datum,
        );
        // The same transactions as the baseline: only the certificate's is
        // built again.
        expect(kinds(cost)).toEqual(kinds(baseline));
        return;
      }
      // The other party's certificate serves the field: the application
      // certifies nothing, and its proof reads that certificate.
      const carriage = fieldCarriageOf(watch, sitesOf(harness));
      expect(carriage.certificates).toEqual([]);
      const lostCertificate = certificateOf(lost!, sitesOf(harness))!;
      const outputs = ledgerOutputs(watch);
      const theirs = [...watch.plantedOutRefs()].filter(
        (outRef) =>
          outputs.get(outRef)?.address === lostCertificate.address &&
          outputs.get(outRef)?.datum === lostCertificate.datum,
      );
      expect(theirs).toHaveLength(1);
      expect(carriage.readersOf(theirs[0]!).length).toBeGreaterThan(0);
      // The baseline's transactions but the certificate's.
      const certify = kinds(baseline).indexOf("certify");
      expect(kinds(cost)).toEqual(
        kinds(baseline).filter((_, index) => index !== certify),
      );
    },
    900_000,
  );

  it.each([
    ["before its certify transaction is confirmed", "landed"],
    ["once its certify transaction is confirmed", "read"],
  ] as const)(
    "certifies the field once more when its own wallet retires the certificate %s",
    async (_, when) => {
      let retired: string | undefined;
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
          clock.state.onObservation = async () => {
            if (retired !== undefined) return;
            // "landed": the certify transaction is sent, and lands before
            // the application reconciles it.
            const [certificate] = fieldCarriageOf(
              watch,
              sitesOf(harness),
            ).certificates;
            if (when === "landed" && certificate !== undefined)
              await retire(watch, harness, certificate.output.outRef);
          };
          return () => {
            clock.state.onObservation = undefined;
          };
        },
      });
      expect(retired).toBeDefined();
      expect(withheld.size).toBe(when === "read" ? 1 : 0);
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
      // The run sends only one more certification and the step built again.
      expect(run.sent.length).toBeLessThanOrEqual(baselineSent + 2);
    },
    900_000,
  );
});
