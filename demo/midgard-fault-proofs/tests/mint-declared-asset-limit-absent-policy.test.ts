/**
 * A forced `MintDeclaredAssetLimit` rejection naming a policy field 5 lacks
 * is wrongful: canonical validation declares no asset count there. Step 02
 * closes a negative coordinate at once and compares any other against the
 * field's authenticated count (the directly opened field's, or the certified
 * walk's after the grammar steps), then writes a non-crossing decision to the
 * terminal, which convicts. A claim that a held policy is absent is refused at
 * step 02, and the route convicts at the mint field's byte cap.
 */
import {
  encodeMidgardFieldPreimage,
  encodeMidgardMintPolicyItem,
  MIDGARD_CONSENSUS_LIMITS,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk/fraud-proof";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { buildCommittedFieldShapeInit } from "../src/committed-field-shape/submit-committed-field-shape-init.js";
import {
  type MintDeclaredAssetLimitEvidence,
  prepareMintDeclaredAssetLimitEvidence,
} from "../src/mint-declared-asset-limit/family.advance-mint-declared-fold.js";
import {
  planMintDeclaredAbsentPolicy,
  planMintDeclaredAssetLimitField,
} from "../src/mint-declared-asset-limit/staged-plan.js";
import { buildMintDeclaredAssetLimitStep01Forced } from "../src/mint-declared-asset-limit/submit-step-01.js";
import { buildMintDeclaredAssetLimitStep02 } from "../src/mint-declared-asset-limit/submit-step-02.js";
import { buildMintDeclaredAssetLimitStep04 } from "../src/mint-declared-asset-limit/submit-step-04.js";
import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import { buildForcedTransactionLeafMembershipProof } from "../src/transition-trace/witnesses.build-source-membership-proof.js";
import {
  network,
  publishCarriage,
  publishFamilyReferences,
  registeredContracts,
} from "./mint-declared-asset-limit-lifecycle.registered-contracts.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init/forced-transitions.js";
import { buildAndSubmit } from "./support/submit-steps.js";

/** A one-asset policy item whose policy id encodes `index`. */
const policyItem = (index: number) => {
  const policyId = Buffer.alloc(28);
  policyId.writeUInt32BE(index, 24);
  return encodeMidgardMintPolicyItem({
    policyId,
    assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
  });
};

const policiesOf = (count: number) =>
  Array.from({ length: count }, (_, index) => policyItem(index));

/** The most one-asset policies field 5 holds within its byte cap. */
const maximumPolicyCount = (): number => {
  const cap = MIDGARD_CONSENSUS_LIMITS.maxMintPreimageBytes;
  let low = 1;
  let high = cap;
  while (low < high) {
    const middle = Math.ceil((low + high) / 2);
    if (encodeMidgardFieldPreimage(policiesOf(middle)).length <= cap)
      low = middle;
    else high = middle - 1;
  }
  return low;
};

/**
 * A block whose forced leaf rejects a transaction minting `policies` for a
 * declared-asset limit at `policyIndex`, with a thread bound through step 01.
 */
const boundForcedThread = async ({
  policies,
  policyIndex,
  claimAbsent = false,
}: {
  readonly policies: readonly Buffer[];
  readonly policyIndex: bigint;
  /** Binds a claim that the policy is absent, whatever field 5 holds. */
  readonly claimAbsent?: boolean;
}) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realMintDeclaredAssetLimit: true,
      alwaysFraudProofCatalogue: true,
      alwaysStateQueue: true,
    },
  });
  const registered = await registeredContracts(harness);
  const { contracts, category } = registered;
  const credential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("missing funder key");
  const field = encodeMidgardFieldPreimage(policies);
  const rejectionReason = {
    MintDeclaredAssetLimit: { policy_index: policyIndex },
  } as const;
  const forced = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    nativeTx: { mintPreimageCbor: field },
    rejectionReason,
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: registered.catalogue,
    header: forced.header,
  });
  const references = await publishFamilyReferences(harness, registered);
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: forced.reconstruction,
    eventKey: forced.eventKey,
  });
  const transactionId = forced.forcedTransaction.tx_id;
  const compactCbor = forced.forcedTransaction.submitted_source.compact_cbor;
  const finding = {
    subject: forcedVerdictSubject({
      transactionId,
      sourceKey: membership.key,
      rejectionReason,
    }),
    policyIndex,
  };
  const prepare = () =>
    prepareMintDeclaredAssetLimitEvidence({
      finding,
      fieldPreimage: field,
      committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
    });
  const evidence: MintDeclaredAssetLimitEvidence = claimAbsent
    ? Object.freeze({
        ...finding,
        fieldPreimageHex: field.toString("hex"),
        fieldCommitmentHex: midgardFieldCommitment(field).toString("hex"),
        carriage: selectMidgardFieldCarriageTier(field.length),
        crossing: false,
        accumulatedCount: 0,
        targetPolicyId: "",
        targetDeclaredCount: 0,
        targetItemHex: "",
        absent: true,
      })
    : prepare();
  const carriage = await publishCarriage(
    harness,
    registered,
    {
      id: transactionId,
      compactCbor,
      witnessSetCompactCbor:
        forced.forcedTransaction.submitted_source.witness_set_compact_cbor,
    },
    policies.map((item) => Buffer.from(item)),
    "mint declared absent policy",
    1n,
  );
  const step = {
    lucid: harness.proverLucid,
    contracts,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
  };
  const initialized = await buildAndSubmit(buildCommittedFieldShapeInit, {
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: contracts as never,
    category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: registered.catalogue.root,
    },
    signer: harness.proverSigner,
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    fraudulentHeaderHash: setup.headerHash,
    witnessReferenceScripts: harness.witnessReferenceScripts,
  });
  const bound = await buildAndSubmit(buildMintDeclaredAssetLimitStep01Forced, {
    ...step,
    threadOutRef: `${initialized.txHash}#${initialized.firstStepOutputIndex.toString()}`,
    finding: evidence,
    forcedSource: { header: setup.header, membership },
    referenceScriptUtxo: references[0],
  });
  const staged =
    policyIndex >= 0n && policyIndex < BigInt(policies.length)
      ? planMintDeclaredAssetLimitField({
          transactionId,
          fieldPreimageCbor: field.toString("hex"),
          policyIndex: Number(policyIndex),
        })
      : planMintDeclaredAbsentPolicy({
          transactionId,
          fieldPreimageCbor: field.toString("hex"),
          policyIndex,
        });
  /**
   * Step 02 to the decision: the grammar steps a certified field needs for a
   * coordinate past it, then the absent close. Returns the close's capture
   * and every step 02's measurement.
   */
  const authenticate = async () => {
    const fromGrammar =
      carriage.planned.plan.tier === "Certified" && policyIndex >= 0n;
    const step02Measurements: CompleteSignedTransactionMeasurement[] = [];
    const decode = async (
      threadOutRef: string,
      action: Parameters<typeof buildMintDeclaredAssetLimitStep02>[0]["action"],
    ) => {
      const captured = await captureEmulatorSubmission(harness.emulator, () =>
        buildAndSubmit(buildMintDeclaredAssetLimitStep02, {
          ...step,
          threadOutRef,
          evidence,
          nativeTxCompactCbor: compactCbor,
          staged,
          action,
          referenceScriptUtxo: references[1],
        }),
      );
      step02Measurements.push(captured.measurement);
      return captured;
    };
    let threadOutRef = bound.nextThreadOutRef;
    if (fromGrammar) {
      threadOutRef = (await decode(threadOutRef, { kind: "grammar_start" }))
        .result.nextThreadOutRef;
      for (let ordinal = 1; ordinal < staged.grammar.length; ordinal += 1)
        threadOutRef = (
          await decode(threadOutRef, {
            kind: "grammar_resume",
            nextOrdinal: ordinal,
          })
        ).result.nextThreadOutRef;
    }
    const closed = await decode(threadOutRef, {
      kind: "absent",
      fromGrammar,
    });
    return { ...closed, step02Measurements };
  };
  const convict = async (threadOutRef: string) =>
    await captureEmulatorSubmission(harness.emulator, () =>
      buildAndSubmit(buildMintDeclaredAssetLimitStep04, {
        ...step,
        threadOutRef,
        evidence,
        referenceScriptUtxo: references[3],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
  return { evidence, prepare, carriage, authenticate, convict };
};

describe("mintDeclaredAssetLimit forced rejection of an absent policy", () => {
  it("convicts a policy coordinate past field 5's last item", async () => {
    const thread = await boundForcedThread({
      policies: policiesOf(1),
      policyIndex: 1n,
    });
    expect(thread.evidence.absent).toBe(true);
    expect(thread.carriage.planned.plan.tier).not.toBe("Certified");
    const closed = await thread.authenticate();
    const minted = await thread.convict(closed.result.nextThreadOutRef);
    expect(minted.result.txHash).toMatch(/^[0-9a-f]{64}$/u);
  }, 900_000);

  it("convicts a negative policy coordinate", async () => {
    const thread = await boundForcedThread({
      policies: policiesOf(1),
      policyIndex: -1n,
    });
    expect(thread.evidence.absent).toBe(true);
    const closed = await thread.authenticate();
    expect(closed.step02Measurements).toHaveLength(1);
    const minted = await thread.convict(closed.result.nextThreadOutRef);
    expect(minted.result.txHash).toMatch(/^[0-9a-f]{64}$/u);
  }, 900_000);

  it.each([
    { label: "2^53 + 1", policyIndex: 2n ** 53n + 1n },
    { label: "the codec's largest, 2^64 - 1", policyIndex: 2n ** 64n - 1n },
    { label: "the codec's smallest, -2^64", policyIndex: -(2n ** 64n) },
  ])(
    "convicts a policy coordinate at $label",
    async ({ policyIndex }) => {
      const thread = await boundForcedThread({
        policies: policiesOf(1),
        policyIndex,
      });
      expect(thread.evidence.absent).toBe(true);
      const closed = await thread.authenticate();
      const minted = await thread.convict(closed.result.nextThreadOutRef);
      expect(minted.result.txHash).toMatch(/^[0-9a-f]{64}$/u);
    },
    900_000,
  );

  it("refuses on chain a claim that a held policy is absent", async () => {
    const thread = await boundForcedThread({
      policies: policiesOf(1),
      policyIndex: 0n,
      claimAbsent: true,
    });
    expect(thread.prepare().absent).toBe(false);
    await expectOnchainRefusal(() => thread.authenticate(), {
      refusedBy: "fraud_proofs/mint_declared_asset_limit/step_02",
      check: /^expect or \{ bound\.policy_index < 0, bound\.policy_index >= /u,
    });
  }, 900_000);

  it.each([
    { coordinate: "past the last", negative: false },
    { coordinate: "negative", negative: true },
  ])(
    "convicts a $coordinate coordinate over the most policies field 5 holds",
    async ({ negative }) => {
      const count = maximumPolicyCount();
      const policyIndex = negative ? -1n : BigInt(count);
      const thread = await boundForcedThread({
        policies: policiesOf(count),
        policyIndex,
      });
      expect(thread.carriage.planned.plan.tier).toBe("Certified");
      const closed = await thread.authenticate();
      // A negative coordinate closes in one step 02, before any grammar step.
      if (negative) expect(closed.step02Measurements).toHaveLength(1);
      const minted = await thread.convict(closed.result.nextThreadOutRef);
      for (const measurement of [
        ...closed.step02Measurements,
        minted.measurement,
      ]) {
        expect(measurement.executionMemory).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_MEMORY_UNITS,
        );
        expect(measurement.executionSteps).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_CPU_UNITS,
        );
      }
      console.info(
        `max-shape absent policy (${count.toString()} policies, coordinate ${policyIndex.toString()}): step-02 close mem=${closed.measurement.executionMemory.toString()} cpu=${closed.measurement.executionSteps.toString()}; step-04 mem=${minted.measurement.executionMemory.toString()} cpu=${minted.measurement.executionSteps.toString()}`,
      );
    },
    1_800_000,
  );
});
