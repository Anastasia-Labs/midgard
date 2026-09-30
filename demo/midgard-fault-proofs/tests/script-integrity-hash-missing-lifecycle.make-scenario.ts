import {
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import { ScriptIntegrityStepDatums } from "../src/script-integrity-hash-missing/schemas.js";
import {
  submitScriptIntegrityHashMissingStep01Accepted,
  submitScriptIntegrityHashMissingStep01Forced,
} from "../src/script-integrity-hash-missing/submit-direct.js";
import { submitScriptIntegrityHashMissingInit } from "../src/script-integrity-hash-missing/submit-init.js";
import {
  submitScriptIntegrityHashMissingCancel,
  submitScriptIntegrityHashMissingStep02,
  submitScriptIntegrityHashMissingStep03,
  submitScriptIntegrityHashMissingStep04,
} from "../src/script-integrity-hash-missing/submitters.js";
import {
  FORCED_ORDER_KEY,
  nativeTxOf,
  registeredFamily,
} from "./script-integrity-hash-missing-lifecycle.registered-family.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarness,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

/**
 * One committed block on the registered chain with the family's seven
 * reference scripts published, plus raw submitters that hand the caller's
 * datum and redeemer to the chain unchanged, so every negative below is a
 * validator refusal rather than an off-chain guard.
 */
export const makeScenario = async ({
  nativeTx,
  forcedReason,
}: {
  readonly nativeTx: ReturnType<typeof nativeTxOf>;
  readonly forcedReason?: SDK.RejectionReason;
}) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: { realScriptIntegrityHashMissing: true },
  });
  const { family, category } = await registeredFamily(harness);
  const block = await buildDecodingBlockFixture({
    operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
    startTime: BigInt(
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
    ),
    priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    subject:
      forcedReason === undefined
        ? { kind: "normal", nativeTx }
        : {
            kind: "forced",
            nativeTx,
            orderKey: FORCED_ORDER_KEY,
            verdict: { ForcedTxInvalid: { reason: forcedReason } },
          },
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header: block.header,
  });
  const refs: UTxO[] = [];
  for (const [index, step] of family.steps.entries())
    refs.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `integrity scenario step ${(index + 1).toString()}`,
        })
      ).utxo,
    );
  const compactCbor = block.nativeTxCompactCbor;
  const derived = deriveMidgardNativeTxWitnessSetCompact(nativeTx.witnessSet);
  const witnessSet: SDK.NativeTxWitnessSetCompact = {
    addr_tx_wits_hash: Buffer.from(derived.addrTxWitsHash).toString("hex"),
    script_tx_wits_hash: Buffer.from(derived.scriptTxWitsHash).toString("hex"),
    redeemer_tx_wits_hash: Buffer.from(derived.redeemerTxWitsHash).toString(
      "hex",
    ),
  };
  const witnessSetCbor = encodeMidgardNativeTxWitnessSetCompact({
    addrTxWitsHash: Buffer.from(derived.addrTxWitsHash),
    scriptTxWitsHash: Buffer.from(derived.scriptTxWitsHash),
    redeemerTxWitsHash: Buffer.from(derived.redeemerTxWitsHash),
  }).toString("hex");
  const witnessSetHash =
    nativeTx.compact.transactionWitnessSetHash.toString("hex");
  const owner = harness.proverSigner.paymentKeyHash;
  const common = (index: number) => ({
    lucid: harness.proverLucid,
    contracts: family,
    categoryId: category.categoryId,
    signer: harness.proverSigner,
    referenceScriptUtxo: refs[index]!,
  });
  const datum = (index: number, data: unknown) =>
    Data.to(
      { fraud_prover: owner, data } as never,
      ScriptIntegrityStepDatums[index] as never,
    );
  const init = async () =>
    (
      await submitScriptIntegrityHashMissingInit({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        network,
        contracts: family,
        category,
        catalogue: {
          policyId: harness.contracts.fraudProofCatalogue.policyId,
          spendingScriptAddress:
            harness.contracts.fraudProofCatalogue.spendingScriptAddress,
          root: harness.catalogue.root,
        },
        signer: harness.proverSigner,
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      })
    ).nextThreadOutRef;
  const accepted01 = async (
    threadOutRef: string,
    txInclusion = block.txInclusion!,
  ) =>
    (
      await submitScriptIntegrityHashMissingStep01Accepted({
        ...common(0),
        blueprint: harness.realBlueprint,
        network,
        threadOutRef,
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      })
    ).nextThreadOutRef;
  const forced01 = async (threadOutRef: string) =>
    (
      await submitScriptIntegrityHashMissingStep01Forced({
        ...common(0),
        threadOutRef,
        direction: 1n,
      })
    ).nextThreadOutRef;
  const step02 = async (
    threadOutRef: string,
    {
      subject,
      anchoredWitnessSetHash = witnessSetHash,
      forcedMembership = null,
    }: {
      readonly subject: SDK.VerdictSubject;
      readonly anchoredWitnessSetHash?: string;
      readonly forcedMembership?: SDK.RootMembershipProof<
        SDK.OutputReference,
        SDK.ForcedInclusionTxV1
      > | null;
    },
  ) =>
    (
      await submitScriptIntegrityHashMissingStep02({
        ...common(1),
        threadOutRef,
        nextDatum: datum(2, {
          subject,
          witness_set_hash: anchoredWitnessSetHash,
        }),
        buildArgs: ({ input_index, output_index }) => ({
          input_index,
          output_index,
          header: block.header,
          forced_membership: forcedMembership,
        }),
      })
    ).nextThreadOutRef;
  const direct03 = async (
    threadOutRef: string,
    {
      decision,
      compact = compactCbor,
      scriptPreimage,
      redeemerPreimage,
      staged = false,
    }: {
      readonly decision: {
        readonly subject: SDK.VerdictSubject;
        readonly script_integrity_hash: string;
        readonly contains_non_native_script: boolean;
        readonly has_redeemers: boolean;
      };
      readonly compact?: string;
      readonly scriptPreimage: Buffer;
      readonly redeemerPreimage: Buffer;
      readonly staged?: boolean;
    },
  ) =>
    (
      await submitScriptIntegrityHashMissingStep03({
        ...common(2),
        threadOutRef,
        staged,
        nextDatum: datum(6, decision),
        buildArgs: ({ input_index, output_index }) => ({
          Direct: {
            input_index,
            output_index,
            native_tx_compact_cbor: compact,
            witness_set: witnessSet,
            script_witnesses: {
              Inline: { preimage: scriptPreimage.toString("hex") },
            },
            redeemers: {
              Inline: { preimage: redeemerPreimage.toString("hex") },
            },
          },
        }),
      })
    ).nextThreadOutRef;
  const step04 = (threadOutRef: string) =>
    submitScriptIntegrityHashMissingStep04({
      ...common(6),
      threadOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  const cancel = (threadOutRef: string, index: number) =>
    submitScriptIntegrityHashMissingCancel({
      ...common(index),
      threadOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  return {
    harness,
    family,
    category,
    block,
    setup,
    refs,
    compactCbor,
    witnessSet,
    witnessSetCbor,
    witnessSetHash,
    owner,
    common,
    datum,
    init,
    accepted01,
    forced01,
    step02,
    direct03,
    step04,
    cancel,
  };
};
