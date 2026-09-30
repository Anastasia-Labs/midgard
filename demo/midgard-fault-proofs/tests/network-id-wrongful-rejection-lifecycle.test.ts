/**
 * networkId wrongful-rejection (forced) direction — complete Lucid lifecycle.
 *
 * Every thread starts from the generic computation-thread `Init` and walks the
 * real applied reference scripts: step 01 (forced dispatch) → forced door
 * (leaf binding) → forced scan (the resumable §10 walk over the outputs field)
 * → step 02 (permanent mint) → state-queue removal. Nothing starts from a
 * fabricated mid-thread datum. The suite covers the §5.3 items the family
 * owns: forced success, honest refusals at the exact on-chain predicate,
 * reason/subject and seam substitutions, cancel from every nonterminal
 * physical step (the self-looping scan in both of its states), restart by
 * out-ref, permanent mint + removal, the absent-network boundary, the scan's
 * own budget and checkpoint refusals, the maximum raw-carriage field, and the
 * adjacent certified field the grammar phase now makes convictable. Every
 * positive transaction is measured against the Van Rossem envelope with the
 * repository's reserves; no oversized route exists here.
 */
import "@aiken-lang/merkle-patricia-forestry";
import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/forced";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/field-opening.js";
import "../src/network-id/index.js";
import "../src/remove-fraudulent-block.js";
import "../src/transition-trace/phas.js";
import "./support/emulator/emulator-context.js";
import "./support/emulator/measurement.js";
import "./support/emulator/native-tx.js";
import "./support/emulator/proof-fit.js";
import "./support/emulator/reference-scripts.js";
import "./support/emulator/removal-deployment.js";
import "./support/emulator/setup-tx.js";
import "./support/measured-fit-ledger.js";
import "./support/network-id-emulator.js";
import "./support/network-id-shapes.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./network-id-wrongful-rejection-lifecycle.commit-forced-block.js";
import "./network-id-wrongful-rejection-lifecycle.make-stages.js";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import {
  ForcedInclusionTxV1Schema,
  NETWORK_ID_FORCED_SCAN_BATCH,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  NETWORK_ID_FORCED_CERTIFIED_SCAN_DRIVER_BATCH,
  NETWORK_ID_FORCED_GRAMMAR_DRIVER_BATCH,
  networkIdWrongfulRejectionCloses,
  planNetworkIdOutputsOpening,
} from "../src/network-id/index.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  commitForcedBlock,
  forcedTransactionAt,
  mutatedAdvance,
  network,
  NETWORK_ID_MISMATCH,
  preparedFor,
} from "./network-id-wrongful-rejection-lifecycle.commit-forced-block.js";
import {
  makeHarness,
  makeStages,
} from "./network-id-wrongful-rejection-lifecycle.make-stages.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { expectProofFit } from "./support/emulator/proof-fit.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import {
  MAXIMUM_CERTIFIED_OUTPUT_COUNT,
  MAXIMUM_SUPPORTED_OUTPUT_COUNT,
} from "./support/network-id-shapes.js";

describe("networkId wrongful-rejection real lifecycle", () => {
  it("runs Init through the forced door to a permanent mint and removal, cancels every nonterminal step, and restarts by out-ref", async () => {
    const { harness, refs } = await makeHarness();
    const transaction = forcedTransactionAt({ outputNetworkIds: [0, 0] });
    const block = await commitForcedBlock(harness, [
      {
        outputIndex: 0n,
        value: {
          tx_id: transaction.transactionId,
          submitted_source: transaction.source,
          verdict: NETWORK_ID_MISMATCH,
        },
      },
    ]);
    const prepared = preparedFor({
      transaction,
      headerHash: block.setup.headerHash,
      header: block.header,
      membership: block.memberships[0]!,
    });
    const stages = makeStages(harness, refs, block.setup);

    // Item 7: cancel from each nonterminal physical step.
    await stages.cancel(await stages.initialize(), "step01");
    await stages.cancel(
      await stages.dispatch(await stages.initialize(), prepared),
      "forcedStep",
    );
    const boundOnce = await stages.bind(
      await stages.dispatch(await stages.initialize(), prepared),
      prepared,
    );
    expect(boundOnce.state.Ready.bound.committed_tx_network_id).toBe(0n);
    expect(boundOnce.state.Ready.bound.forced_source_key).toBe(
      prepared.subject.source_key,
    );
    // The scan self-loops, so it is cancellable in both of its live states:
    // straight out of the door's `Ready`, and mid-walk from `Scanning`.
    await stages.cancel(boundOnce.nextThreadOutRef, "forcedScan");
    const { plan: baselinePlan } = stages.scanPlanFor(prepared);
    expect(baselinePlan.steps.map((step) => step.kind)).toStrictEqual([
      "open",
      "advance",
    ]);
    const midWalk = await stages.scanStep(
      (
        await stages.bind(
          await stages.dispatch(await stages.initialize(), prepared),
          prepared,
        )
      ).nextThreadOutRef,
      prepared,
      baselinePlan,
      baselinePlan.steps[0]!,
    );
    await stages.cancel(midWalk.nextThreadOutRef, "forcedScan");
    await stages.cancel(
      await stages.scan(
        (
          await stages.bind(
            await stages.dispatch(await stages.initialize(), prepared),
            prepared,
          )
        ).nextThreadOutRef,
        prepared,
        baselinePlan,
      ),
      "step02",
    );

    // Item 6: reference-script substitution at both forced seams is refused
    // before anything is signed.
    const dispatched = await stages.dispatch(
      await stages.initialize(),
      prepared,
    );
    await expect(stages.bind(dispatched, prepared, refs[1])).rejects.toThrow(
      /forced step validator/u,
    );
    await expect(
      stages.dispatch(await stages.initialize(), prepared, refs[2]),
    ).rejects.toThrow(/reference script/iu);
    // Item 5: a mutated subject coordinate is refused at the scan against the
    // forced-source key the door froze into the thread's bound.
    const bound = await stages.bind(dispatched, prepared);
    const mutatedSubject = {
      ...prepared,
      subject: {
        ...prepared.subject,
        source_key: Data.to(
          { ...prepared.forcedSource.membership.key, outputIndex: 9n },
          OutputReference,
        ),
      },
    };
    await expect(
      stages.scanStep(
        bound.nextThreadOutRef,
        mutatedSubject,
        baselinePlan,
        baselinePlan.steps[0]!,
      ),
    ).rejects.toThrow(/bound the forced door did not write/u);
    // Item 2 + 9: restart by out-ref from a fresh builder call, permanent
    // mint, then state-queue removal.
    const scanned = await stages.scan(
      bound.nextThreadOutRef,
      prepared,
      baselinePlan,
    );
    const final = await stages.finalize(scanned, prepared);
    expect(final.fraudProofUnit).toBeTruthy();
    await stages.remove();
    stages.assertFit("baseline");
  }, 600_000);

  it("treats the absent body network id as the boundary: 255 with expected outputs contradicts the rejection", async () => {
    const { harness, refs } = await makeHarness();
    const transaction = forcedTransactionAt({
      outputNetworkIds: [0],
      bodyNetworkId: 255n,
    });
    const block = await commitForcedBlock(harness, [
      {
        outputIndex: 0n,
        value: {
          tx_id: transaction.transactionId,
          submitted_source: transaction.source,
          verdict: NETWORK_ID_MISMATCH,
        },
      },
    ]);
    const membership = block.memberships[0]!;
    const prepared = preparedFor({
      transaction,
      headerHash: block.setup.headerHash,
      header: block.header,
      membership,
    });
    expect(networkIdWrongfulRejectionCloses(prepared.evidence)).toBe(true);
    const stages = makeStages(harness, refs, block.setup);
    const bound = await stages.bind(
      await stages.dispatch(await stages.initialize(), prepared),
      prepared,
    );
    expect(bound.state.Ready.bound.committed_tx_network_id).toBe(255n);
    const { plan } = stages.scanPlanFor(prepared);
    const final = await stages.finalize(
      await stages.scan(bound.nextThreadOutRef, prepared, plan),
      prepared,
    );
    expect(final.fraudProofUnit).toBeTruthy();
    await stages.remove();
    stages.assertFit("absent-network");
  }, 600_000);

  it.each<
    [
      string,
      {
        outputNetworkIds: readonly number[];
        bodyNetworkId?: bigint;
        refusedAt: "forcedStep" | "forcedScan";
      },
    ]
  >([
    [
      "a foreign output in the last batch of a multi-batch field",
      {
        // 70 outputs is two `Advance` batches (64 + 6); the foreign one is in
        // the second, so the first batch must fold cleanly and the refusal
        // must land on the batch that reaches the offending item.
        outputNetworkIds: Array.from({ length: 70 }, (_value, index) =>
          index === 69 ? 1 : 0,
        ),
        refusedAt: "forcedScan",
      },
    ],
    [
      "an explicit foreign body network",
      { outputNetworkIds: [0], bodyNetworkId: 1n, refusedAt: "forcedStep" },
    ],
  ])(
    "refuses the honest rejection with %s at the exact on-chain predicate",
    async (_label, shape) => {
      const { harness, refs } = await makeHarness();
      const transaction = forcedTransactionAt(shape);
      const block = await commitForcedBlock(harness, [
        {
          outputIndex: 0n,
          value: {
            tx_id: transaction.transactionId,
            submitted_source: transaction.source,
            verdict: NETWORK_ID_MISMATCH,
          },
        },
      ]);
      const membership = block.memberships[0]!;
      const honest = preparedFor({
        transaction,
        headerHash: block.setup.headerHash,
        header: block.header,
        membership,
      });
      // Off chain: the detector and the bind builder both refuse.
      expect(networkIdWrongfulRejectionCloses(honest.evidence)).toBe(false);
      const stages = makeStages(harness, refs, block.setup);
      const dispatched = await stages.dispatch(
        await stages.initialize(),
        honest,
      );
      await expect(stages.bind(dispatched, honest)).rejects.toThrow(
        /rejection was honest/u,
      );
      // On chain: a prover lying about the retained evidence is refused at the
      // step that authenticates the lie. The forced door binds the body
      // network id from the committed source, so a body-network lie dies
      // there; the outputs field is folded by the scan, so an output-network
      // lie binds, folds every clean batch, and dies on the batch that reaches
      // the foreign output.
      const lying = preparedFor({
        transaction,
        headerHash: block.setup.headerHash,
        header: block.header,
        membership,
        claimedOutputNetworkIds: transaction.outputs.map(() => 0n),
        claimedBodyNetworkId: 0n,
      });
      expect(networkIdWrongfulRejectionCloses(lying.evidence)).toBe(true);
      if (shape.refusedAt === "forcedStep") {
        await expect(stages.bind(dispatched, lying)).rejects.toThrow(
          /script|evaluat/iu,
        );
        await stages.cancel(dispatched, "forcedStep");
        return;
      }
      const bound = await stages.bind(dispatched, lying);
      expect(bound.state.Ready.bound.committed_tx_network_id).toBe(
        transaction.bodyNetworkId,
      );
      const { opening, plan } = stages.scanPlanFor(lying, true);
      expect(plan.steps.filter((step) => step.kind === "advance")).toHaveLength(
        2,
      );
      await publishFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned: opening,
        publisherAddress: harness.proverSigner.address,
        label: "network-id honest-refusal outputs",
      });
      let cursor = bound.nextThreadOutRef;
      const clean = plan.steps.slice(0, -1);
      for (const step of clean) {
        cursor = (
          await stages.scanStep(cursor, lying, plan, step, { publish: true })
        ).nextThreadOutRef;
      }
      await expect(
        stages.scanStep(cursor, lying, plan, plan.steps.at(-1)!, {
          publish: true,
        }),
      ).rejects.toThrow(/script|evaluat/iu);
      await stages.cancel(cursor, "forcedScan");
    },
    900_000,
  );

  it("refuses a mutated reason, a substituted root, a substituted header, and a wrong direction at the forced door", async () => {
    const { harness, refs } = await makeHarness();
    const wrongful = forcedTransactionAt({ outputNetworkIds: [0] });
    // The operator committed the rejection under a different typed reason.
    const block = await commitForcedBlock(harness, [
      {
        outputIndex: 0n,
        value: {
          tx_id: wrongful.transactionId,
          submitted_source: wrongful.source,
          verdict: { ForcedTxInvalid: { reason: "FeeBelowMinimum" } },
        },
      },
    ]);
    const committed = block.memberships[0]!;
    const claimed = preparedFor({
      transaction: wrongful,
      headerHash: block.setup.headerHash,
      header: block.header,
      membership: committed,
    });
    const stages = makeStages(harness, refs, block.setup);
    const thread = await stages.dispatch(await stages.initialize(), claimed);
    // Reason mutation: the authenticated leaf carries FeeBelowMinimum while the
    // door demands exactly NetworkIdMismatch.
    await expect(stages.bind(thread, claimed)).rejects.toThrow(
      /script|evaluat/iu,
    );
    // Root substitution: a proof against a root that does carry the wrongful
    // NetworkIdMismatch leaf, but which the header never committed.
    const foreign = await (async () => {
      const key = Buffer.from(Data.to(committed.key, OutputReference), "hex");
      const value = Buffer.from(
        Data.to(
          {
            tx_id: wrongful.transactionId,
            submitted_source: wrongful.source,
            verdict: NETWORK_ID_MISMATCH,
          } as never,
          ForcedInclusionTxV1Schema as never,
        ),
        "hex",
      );
      const root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
        { key, value },
      ]);
      const store = new Store(undefined);
      await store.ready();
      const trie = new Trie(store);
      await trie.insert(key, value);
      return {
        domain: root.domain,
        root: root.root,
        phas_root: root.phasRoot,
        count: root.count,
        key: committed.key,
        value: {
          tx_id: wrongful.transactionId,
          submitted_source: wrongful.source,
          verdict: NETWORK_ID_MISMATCH,
        },
        proof: Data.from(
          (await trie.prove(key)).toCBOR().toString("hex"),
          Proof,
        ),
      } as never as typeof committed;
    })();
    await expect(
      stages.bind(thread, {
        ...claimed,
        forcedSource: { ...claimed.forcedSource, membership: foreign },
      }),
    ).rejects.toThrow(/script|evaluat/iu);
    // Header substitution: a header that commits the foreign root hashes to a
    // value the thread does not dispute.
    await expect(
      stages.bind(thread, {
        ...claimed,
        forcedSource: {
          header: { ...block.header, forcedTransactionsRoot: foreign.root },
          membership: foreign,
          direction: 1n,
        },
      }),
    ).rejects.toThrow(/script|evaluat/iu);
    // Wrong direction is refused before submission.
    await expect(
      stages.bind(thread, {
        ...claimed,
        forcedSource: { ...claimed.forcedSource, direction: 0n } as never,
      }),
    ).rejects.toThrow(/direction 1/u);
    // The thread survives every refused attempt and is still cancellable.
    await stages.cancel(thread, "forcedStep");
  }, 600_000);

  it("refuses a leaf whose tx id is not the id of its committed source bytes", async () => {
    const { harness, refs } = await makeHarness();
    const wrongful = forcedTransactionAt({ outputNetworkIds: [0] });
    const other = forcedTransactionAt({
      outputNetworkIds: [0],
      inputByte: "88",
    });
    const block = await commitForcedBlock(harness, [
      {
        outputIndex: 0n,
        value: {
          tx_id: other.transactionId,
          submitted_source: wrongful.source,
          verdict: NETWORK_ID_MISMATCH,
        },
      },
    ]);
    const claimed = preparedFor({
      transaction: other,
      headerHash: block.setup.headerHash,
      header: block.header,
      membership: block.memberships[0]!,
    });
    const stages = makeStages(harness, refs, block.setup);
    const thread = await stages.dispatch(await stages.initialize(), claimed);
    await expect(stages.bind(thread, claimed)).rejects.toThrow(
      /script|evaluat/iu,
    );
    await stages.cancel(thread, "forcedStep");
  }, 600_000);

  /** Largest output count whose field-2 opening the planner places in `tier`. */
  const largestOutputCountIn = (
    owner: string,
    predicate: (tier: string) => boolean,
  ) => {
    const tierAt = (count: number) => {
      const transaction = forcedTransactionAt({
        outputNetworkIds: Array.from({ length: count }, () => 0),
      });
      return planNetworkIdOutputsOpening({
        prepared: {
          badTxId: transaction.transactionId,
          nativeTxCompactCbor: transaction.source.compact_cbor,
          outputsItemCbors: transaction.outputs.map((o) => o.toString("hex")),
          faultClaim: { kind: "forced-network-mismatch" },
        } as never,
        owner,
        publish: true,
      }).plan.tier;
    };
    let low = 1;
    let high = 1;
    while (predicate(tierAt(high))) high *= 2;
    while (high - low > 1) {
      const mid = Math.floor((low + high) / 2);
      if (predicate(tierAt(mid))) low = mid;
      else high = mid;
    }
    return low;
  };

  it("scans the maximum raw-carriage outputs field in batches, mints, and removes", async () => {
    const { harness, refs } = await makeHarness();
    // The scan folds the field in `scan_batch` batches, so the supported shape
    // is no longer bounded by what one transaction can decode: it is the
    // largest field raw-UTxO carriage can hold. The pinned count is that
    // planner bound, re-derived here so a carriage change cannot silently
    // shrink the family's coverage.
    const outputCount = MAXIMUM_SUPPORTED_OUTPUT_COUNT;
    expect(outputCount).toBe(
      largestOutputCountIn(
        harness.proverSigner.paymentKeyHash,
        (tier) => tier !== "Certified",
      ),
    );
    const transaction = forcedTransactionAt({
      outputNetworkIds: Array.from({ length: outputCount }, () => 0),
    });
    const fieldBytes = encodeMidgardFieldPreimage(transaction.outputs).length;
    console.info("[network-id-forced-maximum]", { outputCount, fieldBytes });
    const block = await commitForcedBlock(harness, [
      {
        outputIndex: 0n,
        value: {
          tx_id: transaction.transactionId,
          submitted_source: transaction.source,
          verdict: NETWORK_ID_MISMATCH,
        },
      },
    ]);
    const prepared = preparedFor({
      transaction,
      headerHash: block.setup.headerHash,
      header: block.header,
      membership: block.memberships[0]!,
    });
    const stages = makeStages(harness, refs, block.setup);
    const { opening, plan } = stages.scanPlanFor(prepared, true);
    expect(opening.plan.tier).toBe("RawUtxo");
    expect(plan.steps.map((step) => step.kind)).toStrictEqual([
      "open",
      ...Array.from(
        {
          length: Math.ceil(outputCount / Number(NETWORK_ID_FORCED_SCAN_BATCH)),
        },
        () => "advance",
      ),
    ]);
    const bound = await stages.bind(
      await stages.dispatch(await stages.initialize(), prepared),
      prepared,
    );
    const publication = await captureEmulatorSubmission(harness.emulator, () =>
      publishFaultProofFieldCarriage({
        lucid: harness.proverLucid,
        signer: harness.proverSigner,
        planned: opening,
        publisherAddress: harness.proverSigner.address,
        label: "network-id maximum outputs",
      }),
    );
    const final = await stages.finalize(
      await stages.scan(bound.nextThreadOutRef, prepared, plan, {
        publish: true,
      }),
      prepared,
    );
    expect(final.fraudProofUnit).toBeTruthy();
    await stages.remove();
    for (const [index, measurement] of publication.measurements.entries()) {
      expectProofFit({
        stage: `maximum:publication-${index.toString()}`,
        measurement,
        maxTxExMem: harness.emulator.protocolParameters.maxTxExMem,
        maxTxExSteps: harness.emulator.protocolParameters.maxTxExSteps,
      });
    }
    stages.assertFit(`maximum-${outputCount.toString()}-outputs`);
  }, 1_800_000);

  it("convicts the adjacent certified outputs field by certifying its envelope grammar first", async () => {
    const { harness, refs } = await makeHarness();
    // One output above the raw-carriage bound the planner switches to tier-3
    // certified carriage, whose §5.1 item count is provisional. Before the
    // scan existed this shape was unprovable — an operator could reject any
    // large forced transaction as NetworkIdMismatch and never be convicted.
    // The grammar phase certifies the envelope, and the walk then folds.
    const outputCount = MAXIMUM_CERTIFIED_OUTPUT_COUNT;
    expect(outputCount).toBe(
      largestOutputCountIn(
        harness.proverSigner.paymentKeyHash,
        (tier) => tier !== "Certified",
      ) + 1,
    );
    const transaction = forcedTransactionAt({
      outputNetworkIds: Array.from({ length: outputCount }, () => 0),
    });
    const fieldBytes = encodeMidgardFieldPreimage(transaction.outputs).length;
    console.info("[network-id-forced-certified]", { outputCount, fieldBytes });
    const block = await commitForcedBlock(harness, [
      {
        outputIndex: 0n,
        value: {
          tx_id: transaction.transactionId,
          submitted_source: transaction.source,
          verdict: NETWORK_ID_MISMATCH,
        },
      },
    ]);
    const prepared = preparedFor({
      transaction,
      headerHash: block.setup.headerHash,
      header: block.header,
      membership: block.memberships[0]!,
    });
    const stages = makeStages(harness, refs, block.setup);
    const { opening, plan } = stages.scanPlanFor(prepared, true);
    expect(opening.plan.tier).toBe("Certified");
    expect(plan.steps.map((step) => step.kind)).toStrictEqual([
      "startGrammar",
      ...Array.from(
        {
          length:
            Math.ceil(
              outputCount / Number(NETWORK_ID_FORCED_GRAMMAR_DRIVER_BATCH),
            ) - 1,
        },
        () => "resumeGrammar",
      ),
      "finishGrammar",
      ...Array.from(
        {
          length: Math.ceil(
            outputCount / Number(NETWORK_ID_FORCED_CERTIFIED_SCAN_DRIVER_BATCH),
          ),
        },
        () => "advance",
      ),
    ]);
    const bound = await stages.bind(
      await stages.dispatch(await stages.initialize(), prepared),
      prepared,
    );
    const chunks = await publishFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      planned: opening,
      publisherAddress: harness.proverSigner.address,
      label: "network-id certified outputs",
    });
    const certificateReference = await publishPlainReferenceScriptUtxo({
      lucid: harness.proverLucid,
      script: harness.contracts.fieldPreimageCertificate.mintingScript,
      label: "network-id field certificate mint",
    });
    const certificate = await certifyFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      network,
      signer: harness.proverSigner,
      planned: opening,
      certificatePolicyId: harness.contracts.fieldPreimageCertificate.policyId,
      certificateMintingScript:
        harness.contracts.fieldPreimageCertificate.mintingScript,
      certificateReferenceScriptUtxo: certificateReference.utxo,
      chunkUtxos: chunks,
      compactCbor: transaction.source.compact_cbor,
      witnessSetCompactCbor: transaction.source.witness_set_compact_cbor,
    });
    const certificateUtxos = [certificate.certificateUtxo];
    // A certified field's item count is provisional, so `Open` has nothing to
    // derive a semantic position from and the validator refuses it outright.
    await expect(
      stages.scanStep(
        bound.nextThreadOutRef,
        prepared,
        plan,
        { kind: "open" },
        { publish: true, certificateUtxos },
      ),
    ).rejects.toThrow(/certified carriage cannot be opened directly/u);
    const final = await stages.finalize(
      await stages.scan(bound.nextThreadOutRef, prepared, plan, {
        publish: true,
        certificateUtxos,
      }),
      prepared,
    );
    expect(final.fraudProofUnit).toBeTruthy();
    await stages.remove();
    stages.assertFit(`certified-${outputCount.toString()}-outputs`);
  }, 1_800_000);

  it("refuses every scan mutation at the exact check that owns it", async () => {
    const { harness, refs } = await makeHarness();
    // 70 outputs is the smallest shape with more than one `Advance`, so the
    // budget, checkpoint and successor mutations all have a real predecessor
    // batch to disagree with.
    const outputCount = 70;
    const transaction = forcedTransactionAt({
      outputNetworkIds: Array.from({ length: outputCount }, () => 0),
    });
    const block = await commitForcedBlock(harness, [
      {
        outputIndex: 0n,
        value: {
          tx_id: transaction.transactionId,
          submitted_source: transaction.source,
          verdict: NETWORK_ID_MISMATCH,
        },
      },
    ]);
    const prepared = preparedFor({
      transaction,
      headerHash: block.setup.headerHash,
      header: block.header,
      membership: block.memberships[0]!,
    });
    const stages = makeStages(harness, refs, block.setup);
    const { opening, plan } = stages.scanPlanFor(prepared, true);
    await publishFaultProofFieldCarriage({
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      planned: opening,
      publisherAddress: harness.proverSigner.address,
      label: "network-id scan-mutation outputs",
    });
    const options = { publish: true } as const;
    const bound = await stages.bind(
      await stages.dispatch(await stages.initialize(), prepared),
      prepared,
    );
    const ready = bound.nextThreadOutRef;
    const [open, firstAdvance, lastAdvance] = [
      plan.steps[0]!,
      plan.steps[1]!,
      plan.steps.at(-1)!,
    ];
    expect(plan.steps).toHaveLength(3);
    // Advancing out of `Ready`: the door writes no checkpoint, so there is
    // nothing to resume.
    await expect(
      stages.scanStep(ready, prepared, plan, firstAdvance, options),
    ).rejects.toThrow(/requires the Scanning state; the thread carries Ready/u);
    const scanning = (
      await stages.scanStep(ready, prepared, plan, open, options)
    ).nextThreadOutRef;
    // Opening a thread that is already walking.
    await expect(
      stages.scanStep(scanning, prepared, plan, open, options),
    ).rejects.toThrow(/requires the Ready state; the thread carries Scanning/u);
    // Substituted checkpoint bytes: the batch after next resumes a checkpoint
    // the thread never committed.
    await expect(
      stages.scanStep(scanning, prepared, plan, lastAdvance, options),
    ).rejects.toThrow(/but the thread committed/u);
    // Budget zero and one above `scan_batch` are refused on chain.
    await expect(
      stages.scanStep(
        scanning,
        prepared,
        plan,
        mutatedAdvance(firstAdvance, { itemBudget: 0n }),
        options,
      ),
    ).rejects.toThrow(/script|evaluat/iu);
    await expect(
      stages.scanStep(
        scanning,
        prepared,
        plan,
        mutatedAdvance(firstAdvance, { itemBudget: 65n }),
        options,
      ),
    ).rejects.toThrow(/script|evaluat/iu);
    // Wrong successor at the seam: a completing batch that self-loops, and a
    // nonterminal batch that finalises, are both refused on chain.
    await expect(
      stages.scanStep(
        scanning,
        prepared,
        plan,
        mutatedAdvance(firstAdvance, { completes: true }),
        options,
      ),
    ).rejects.toThrow(/script|evaluat/iu);
    const midWalk = (
      await stages.scanStep(scanning, prepared, plan, firstAdvance, options)
    ).nextThreadOutRef;
    await expect(
      stages.scanStep(
        midWalk,
        prepared,
        plan,
        mutatedAdvance(lastAdvance, { completes: false }),
        options,
      ),
    ).rejects.toThrow(/script|evaluat/iu);
    // The thread survives every refusal and still completes.
    const final = await stages.finalize(
      (await stages.scanStep(midWalk, prepared, plan, lastAdvance, options))
        .nextThreadOutRef,
      prepared,
    );
    expect(final.fraudProofUnit).toBeTruthy();
    await stages.remove();
  }, 1_800_000);
});

export {
  MAXIMUM_CERTIFIED_OUTPUT_COUNT,
  MAXIMUM_SUPPORTED_OUTPUT_COUNT,
} from "./support/network-id-shapes.js";
