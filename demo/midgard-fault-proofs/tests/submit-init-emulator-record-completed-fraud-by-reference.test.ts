/** Standalone record of an already-finalized fraud proof, citing the proof
 * token by reference. The queue spend is the applied production validator;
 * queue and proof issuance are fixture native policies (the proof policy is
 * the one the queue spend is parameterized with). The front-run scenario runs
 * the real double-spend proof chain, whose last step finalizes token-only,
 * and the real merge yield. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  CML,
  Data,
  fromText,
  generateEmulatorAccount,
  Lucid,
  scriptFromNative,
  toUnit,
  type UTxO,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { excludeUtxo } from "../src/spend-input-witness.js";
import { buildRecordCompletedFraudByReferenceTx } from "../src/submit/record-completed-fraud.js";
import { outputWithDatumAndUnitPredicate } from "../src/tx-layout.js";
import { setupCompletedFraudQueue } from "./support/completed-fraud-queue-harness.js";
import {
  buildOldestQueueHeaderMerge,
  publishOldestQueueHeaderDa,
} from "./support/cross-block-settlement-emulator.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import type { ReferenceScriptPublishingContracts } from "./support/emulator/reference-script-publisher.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { buildProvedDoubleSpendFixture } from "./support/submit-init-emulator-fixtures.build-proved-double-spend-fixture.js";

const records: unknown[] = [];
const OTHER_HEADER = "ee".repeat(28);
/** A key-locked native script nobody here signs for: proof tokens parked
 * there stay out of every wallet's coin selection. */
const tokenHome = validatorToAddress(
  "Custom",
  scriptFromNative({ type: "sig", keyHash: "cc".repeat(28) }),
);

type Queue = Awaited<ReturnType<typeof setupCompletedFraudQueue>>;

const setup = async (previouslyMarked = false) =>
  setupCompletedFraudQueue({ previouslyMarked, records });

const currentNode = async (h: Queue) => {
  const input = await h.lucid.utxoByUnit(h.unit);
  const view = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(input),
  );
  const node = Effect.runSync(SDK.getStateQueueNodeFromStateQueueDatum(view));
  return { input, view, node };
};

const deadlineOf = (h: Queue) =>
  Number(h.node.block_time + SDK.MATURITY_DURATION_MS);

/** Mint one proof-shaped token into its own output with the fraud-proof
 * datum. `issuer: "foreign"` mints the same name under the queue fixture
 * policy instead of the fraud-proof policy. */
const mintToken = async (
  h: Queue,
  {
    label,
    name = h.proofAsset,
    issuer = "proof",
    home = tokenHome,
  }: {
    readonly label: string;
    readonly name?: string;
    readonly issuer?: "proof" | "foreign";
    readonly home?: string;
  },
): Promise<UTxO> => {
  const [script, policy] =
    issuer === "proof"
      ? [h.proofIssuer, h.proofPolicy]
      : [h.queueIssuer, h.queuePolicy];
  const unit = toUnit(policy, name);
  const txHash = await h.submit(
    label,
    await h.lucid
      .newTx()
      .mintAssets({ [unit]: 1n })
      .attach.MintingPolicy(script)
      .pay.ToContract(
        home,
        {
          kind: "inline",
          value: Data.to({ fraud_prover: h.owner }, SDK.FraudProofTokenDatum),
        },
        { lovelace: 2_000_000n, [unit]: 1n },
      )
      .complete({ localUPLCEval: true }),
  );
  const [utxo] = await h.lucid.utxosByOutRef([{ txHash, outputIndex: 0 }]);
  expect(utxo?.assets[unit]).toBe(1n);
  return utxo!;
};

/** A hand-built record spend of the current node. `cite: null` is the
 * same-transaction path (`None`); otherwise the redeemer names `cite`'s
 * position among the final reference inputs. */
const buildRawRecord = async (
  h: Queue,
  {
    cite,
    read = [],
    proofName = h.proofAsset,
    mintHere = false,
    validFrom,
    validTo,
  }: {
    readonly cite: UTxO | null;
    readonly read?: readonly UTxO[];
    readonly proofName?: string;
    readonly mintHere?: boolean;
    readonly validFrom?: number;
    readonly validTo?: number;
  },
) => {
  const { input, view, node } = await currentNode(h);
  const datum = SDK.encodeLinkedListNodeView({
    ...view,
    data: SDK.castStateQueueNodeToData({
      ...node,
      proven_fraud: proofName,
    }) as SDK.LinkedListNodeView["data"],
  });
  const label = "raw completed fraud record";
  const matches = outputWithDatumAndUnitPredicate({
    address: h.address,
    datum,
    unit: h.unit,
  });
  const redeemer = ((ctx) =>
    Data.to(
      {
        RecordCompletedFraud: {
          state_queue_input_index: SDK.requireInputIndex(ctx, input, label),
          state_queue_output_index: SDK.requireUniqueOutputIndex(
            ctx.outputs,
            matches,
            label,
          ),
          fraud_proof_asset_name: proofName,
          m_fraud_proof_ref_input_index:
            cite === null
              ? null
              : SDK.requireReferenceInputIndex(ctx, cite, label),
        },
      },
      SDK.StateQueueSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const references = [
    ...(cite === null ? [] : [cite]),
    ...read,
    h.queueReferenceScript,
  ];
  const walletUtxos = references.reduce<readonly UTxO[]>(
    (candidates, excluded) => excludeUtxo(candidates, excluded),
    await h.lucid.wallet().getUtxos(),
  );
  let tx = h.lucid
    .newTx()
    .collectFrom([input], redeemer)
    .readFrom(references)
    .pay.ToContract(h.address, { kind: "inline", value: datum }, input.assets)
    .validFrom(validFrom ?? Number(node.block_time) + 1000)
    .validTo(validTo ?? h.emulator.now() + 120_000);
  if (mintHere)
    tx = tx
      .mintAssets({ [toUnit(h.proofPolicy, proofName)]: 1n })
      .attach.MintingPolicy(h.proofIssuer);
  return tx.complete({
    localUPLCEval: true,
    presetWalletInputs: [...walletUtxos],
  });
};

const buildByReference = (
  h: Queue,
  fraudProofTokenUtxo: UTxO,
  overrides: Partial<
    Parameters<typeof buildRecordCompletedFraudByReferenceTx>[0]
  > = {},
) =>
  buildRecordCompletedFraudByReferenceTx({
    lucid: h.lucid,
    hubOraclePolicyId: h.queuePolicy,
    stateQueuePolicyId: h.queuePolicy,
    fraudProofPolicyId: h.proofPolicy,
    headerHash: h.headerHash,
    headerEnd: h.header.endTime,
    proofAsset: h.proofAsset,
    fraudProofTokenUtxo,
    queueReferenceScript: h.queueReferenceScript,
    now: h.emulator.now(),
    ...overrides,
  });

const submitBuilt = async (
  h: Queue,
  label: string,
  outcome: Awaited<ReturnType<typeof buildRecordCompletedFraudByReferenceTx>>,
) => {
  if (outcome.kind !== "built")
    throw new Error(`expected a built record, got ${outcome.kind}`);
  return h.submit(label, outcome.tx);
};

describe("record a finalized fraud proof by reference", () => {
  it("lets any wallet record, leaving the cited token and the node value in place", async () => {
    const h = await setup();
    const token = await mintToken(h, { label: "finalized-proof-token" });
    const stranger = generateEmulatorAccount({ lovelace: 0n });
    await h.submit(
      "fund-unrelated-recorder",
      await h.lucid
        .newTx()
        .pay.ToAddress(stranger.address, { lovelace: 20_000_000n })
        .complete({ localUPLCEval: true }),
    );
    h.lucid.selectWallet.fromSeed(stranger.seedPhrase);
    const outcome = await buildByReference(h, token);
    if (outcome.kind !== "built") throw new Error(outcome.kind);
    const body = CML.Transaction.from_cbor_hex(outcome.tx.toCBOR()).body();
    expect(body.required_signers()).toBeUndefined();
    expect(body.mint()).toBeUndefined();
    await submitBuilt(h, "record-by-reference-unrelated-wallet", outcome);
    const after = await currentNode(h);
    expect(after.node).toEqual({ ...h.node, proven_fraud: h.proofAsset });
    expect(after.input.assets).toEqual(h.input.assets);
    expect(after.input.address).toBe(h.input.address);
    expect(await h.lucid.utxosByOutRef([token])).toHaveLength(1);
  });

  it("accepts an upper bound reaching the maturity deadline and refuses one past it", async () => {
    const h = await setup();
    const token = await mintToken(h, { label: "boundary-token" });
    const deadline = deadlineOf(h);
    h.emulator.awaitSlot((deadline - 2000 - h.emulator.now()) / 1000);
    expect(h.emulator.now()).toBe(deadline - 2000);
    const validFrom = h.emulator.now() - 60_000;
    // The ledger's exclusive TTL at the deadline is an inclusive upper bound
    // one millisecond below it; one slot later the inclusive bound is the
    // deadline itself.
    await expectOnchainRefusal(() =>
      buildRawRecord(h, { cite: token, validFrom, validTo: deadline + 1000 }),
    );
    await buildRawRecord(h, { cite: token, validFrom, validTo: deadline });
    const outcome = await buildByReference(h, token);
    if (outcome.kind !== "built") throw new Error(outcome.kind);
    const ttl = CML.Transaction.from_cbor_hex(outcome.tx.toCBOR()).body().ttl();
    // The builder's exclusive TTL lands exactly on the deadline's slot.
    expect(ttl).toBe(BigInt(h.emulator.slot + 2));
    await submitBuilt(h, "record-by-reference-at-deadline", outcome);
    expect((await currentNode(h)).node.proven_fraud).toBe(h.proofAsset);
  });

  it("returns a typed past-maturity outcome once no window remains", async () => {
    const h = await setup();
    const token = await mintToken(h, { label: "late-token" });
    const deadline = deadlineOf(h);
    h.emulator.awaitSlot((deadline - 1000 - h.emulator.now()) / 1000);
    const outcome = await buildByReference(h, token);
    expect(outcome).toEqual({
      kind: "past-maturity",
      headerHash: h.headerHash,
      nodeBlockTime: h.node.block_time,
      deadline: BigInt(deadline),
    });
    expect((await currentNode(h)).input.txHash).toBe(h.input.txHash);
  });

  it("refuses a cited token that names another header", async () => {
    const h = await setup();
    const own = await mintToken(h, { label: "own-header-token" });
    const otherAsset = "00000001" + OTHER_HEADER;
    const other = await mintToken(h, {
      label: "other-header-token",
      name: otherAsset,
    });
    await expectOnchainRefusal(() => buildRawRecord(h, { cite: other }));
    await expectOnchainRefusal(() =>
      buildRawRecord(h, { cite: other, proofName: otherAsset }),
    );
    await h.submit(
      "record-own-header-token",
      await buildRawRecord(h, { cite: own }),
    );
    expect((await currentNode(h)).node.proven_fraud).toBe(h.proofAsset);
  });

  it("refuses the right asset name under another policy", async () => {
    const h = await setup();
    const forged = await mintToken(h, {
      label: "foreign-policy-token",
      issuer: "foreign",
    });
    const genuine = await mintToken(h, { label: "genuine-token" });
    await expectOnchainRefusal(() => buildRawRecord(h, { cite: forged }));
    await h.submit(
      "record-genuine-token",
      await buildRawRecord(h, { cite: genuine }),
    );
    expect((await currentNode(h)).node.proven_fraud).toBe(h.proofAsset);
  });

  it("refuses a cited reference input that does not hold the token", async () => {
    const h = await setup();
    const token = await mintToken(h, { label: "held-token" });
    await expectOnchainRefusal(() =>
      buildRawRecord(h, { cite: h.queueReferenceScript, read: [token] }),
    );
    await h.submit(
      "record-citing-holder",
      await buildRawRecord(h, { cite: token }),
    );
    expect((await currentNode(h)).node.proven_fraud).toBe(h.proofAsset);
  });

  it("refuses a second record once the node is marked", async () => {
    const h = await setup();
    const first = await mintToken(h, { label: "first-category-token" });
    const secondAsset = "00000002" + h.headerHash;
    const second = await mintToken(h, {
      label: "second-category-token",
      name: secondAsset,
    });
    // Before the first record the second category's token records too.
    await buildRawRecord(h, { cite: second, proofName: secondAsset });
    await submitBuilt(h, "first-record", await buildByReference(h, first));
    await expectOnchainRefusal(() =>
      buildRawRecord(h, { cite: second, proofName: secondAsset }),
    );
    expect(
      await buildByReference(h, second, { proofAsset: secondAsset }),
    ).toEqual({ kind: "already-recorded" });
    expect((await currentNode(h)).node.proven_fraud).toBe(h.proofAsset);
  });

  it("keeps the same-transaction mint path, which ignores referenced tokens", async () => {
    const h = await setup();
    const token = await mintToken(h, { label: "referenced-token" });
    await expectOnchainRefusal(() =>
      buildRawRecord(h, { cite: null, read: [token] }),
    );
    await buildRawRecord(h, { cite: token });
    await h.submit(
      "record-minted-here",
      await buildRawRecord(h, { cite: null, mintHere: true }),
    );
    expect((await currentNode(h)).node.proven_fraud).toBe(h.proofAsset);
  });

  it("is unaffected by a second token of the same unit held elsewhere", async () => {
    const h = await setup();
    const elsewhere = validatorToAddress(
      "Custom",
      scriptFromNative({ type: "sig", keyHash: "dd".repeat(28) }),
    );
    const first = await mintToken(h, { label: "first-holder" });
    const second = await mintToken(h, {
      label: "second-holder",
      home: elsewhere,
    });
    const unit = toUnit(h.proofPolicy, h.proofAsset);
    expect(await h.lucid.utxosAtWithUnit(tokenHome, unit)).toHaveLength(1);
    expect(await h.lucid.utxosAtWithUnit(elsewhere, unit)).toHaveLength(1);
    // Either holder authenticates the record; the other changes nothing.
    await buildRawRecord(h, { cite: second, read: [first] });
    await submitBuilt(
      h,
      "record-first-holder",
      await buildByReference(h, first),
    );
    expect((await currentNode(h)).node.proven_fraud).toBe(h.proofAsset);
    expect(await h.lucid.utxosByOutRef([first, second])).toHaveLength(2);
  });
});

describe("token-only finalize front-run", () => {
  it.each([true, false])(
    "recorded by reference=%s: merge at maturity is refused only when recorded",
    async (recorded) => {
      const fixture = await buildProvedDoubleSpendFixture();
      const { contracts, emulator, funderLucid } = fixture;
      const unit = toUnit(
        contracts.stateQueue.policyId,
        fromText("MBLC") + fixture.headerHash,
      );
      const markerOf = async () =>
        Effect.runSync(
          SDK.getStateQueueNodeFromStateQueueDatum(
            await Effect.runPromise(
              SDK.getLinkedListNodeViewFromUTxO(
                await funderLucid.utxoByUnit(unit),
              ),
            ),
          ),
        ).proven_fraud;
      // The proof's last step minted the permanent token and left the node
      // unmarked: the same-transaction record is no longer possible.
      expect(await markerOf()).toBeNull();
      expect(
        fixture.fraudProofUtxo.assets[fixture.step04Result.fraudProofUnit],
      ).toBe(1n);
      if (recorded) {
        const queueReferenceScript = (
          await publishPlainReferenceScriptUtxo({
            lucid: funderLucid,
            script: contracts.stateQueue.spendingScript,
            label: "state-queue spend for the record",
          })
        ).utxo;
        const recorder = generateEmulatorAccount({ lovelace: 0n });
        await funderLucid.awaitTx(
          await (
            await (
              await funderLucid
                .newTx()
                .pay.ToAddress(recorder.address, { lovelace: 20_000_000n })
                .complete({ localUPLCEval: true })
            ).sign
              .withWallet()
              .complete()
          ).submit(),
        );
        const recorderLucid = await Lucid(emulator, "Custom", {
          slotConfig: funderLucid.config().slotConfig,
        });
        recorderLucid.selectWallet.fromSeed(recorder.seedPhrase);
        const outcome = await buildRecordCompletedFraudByReferenceTx({
          lucid: recorderLucid,
          hubOraclePolicyId: contracts.hubOracle.policyId,
          stateQueuePolicyId: contracts.stateQueue.policyId,
          fraudProofPolicyId: contracts.fraudProof.policyId,
          headerHash: fixture.headerHash,
          headerEnd: fixture.fraudulentHeader.endTime,
          proofAsset: fixture.step04Result.fraudProofAssetName,
          fraudProofTokenUtxo: fixture.fraudProofUtxo,
          queueReferenceScript,
          now: emulator.now(),
        });
        if (outcome.kind !== "built") throw new Error(outcome.kind);
        await recorderLucid.awaitTx(
          await (await outcome.tx.sign.withWallet().complete()).submit(),
        );
        expect(await markerOf()).toBe(fixture.step04Result.fraudProofAssetName);
      }
      await publishOldestQueueHeaderDa(fixture);
      // Fund the deployment's reference-script publisher for the merge yield.
      const publisher =
        (contracts as ReferenceScriptPublishingContracts)
          .referenceScriptPublisher?.lucid ?? funderLucid;
      await funderLucid.awaitTx(
        await (
          await (
            await funderLucid
              .newTx()
              .pay.ToAddress(await publisher.wallet().address(), {
                lovelace: 500_000_000n,
              })
              .complete({ localUPLCEval: true })
          ).sign
            .withWallet()
            .complete()
        ).submit(),
      );
      // Both arms present the node to the merge builder as unmarked, so the
      // transactions differ only in the spent node's real marker.
      const merge = () =>
        buildOldestQueueHeaderMerge(fixture, {
          presentedNode: (node) => ({ ...node, proven_fraud: null }),
        });
      if (recorded) {
        await expectOnchainRefusal(merge);
        expect(await markerOf()).toBe(fixture.step04Result.fraudProofAssetName);
        return;
      }
      const merged = await merge();
      await funderLucid.awaitTx(
        await (await merged.tx.sign.withWallet().complete()).submit(),
      );
      expect(
        await funderLucid.utxosAtWithUnit(
          contracts.stateQueue.spendingScriptAddress,
          unit,
        ),
      ).toHaveLength(0);
    },
  );
});
