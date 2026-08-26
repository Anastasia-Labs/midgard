/**
 * #606/§8.3 C53 "Resolver proof-fit sweep" — harness-based generation worker.
 *
 * This file exists only to make the pinned emulator-harness helpers
 * (`submit-init-emulator-shared.ts`, `submit-init-emulator-fixtures.ts`,
 * `legacy-submit-emulator.ts`, and `../src/index.js`'s
 * `submitValidationDispute*` family) importable *verbatim* by the sweep
 * generator. Those helpers are plain TypeScript sources with no compiled
 * `dist` entry point and mix type-only and value named imports without the
 * `type` keyword throughout their transitive closure, so plain `node` cannot
 * load them (confirmed: Node's built-in type-stripping only erases syntax it
 * recognises as type-only *in place* — it does not elide file-local
 * type-only usages the way `tsc`/`esbuild` do — and there is no tsx/ts-node
 * in this workspace to work around that without `pnpm install`). Running
 * this logic as a vitest test file, inside the same package the helpers
 * live in, is the sanctioned resolution (coordinator ruling, "Option A").
 *
 * Routine `vitest run`/CI passes over this package must not pay for this at
 * all: the whole suite is gated behind `MIDGARD_REGENERATE_RESOLVER_SWEEP`,
 * and `demo/midgard-validation/scripts/generate-resolver-proof-fit-sweep-v1.mjs`
 * is the only intended caller (it spawns `vitest run` against this file
 * with that variable set).
 *
 * Measurement contract (unchanged from the ruling that replaced the original
 * hand-built-`ScriptContext`/`Machine.eval` design):
 *   - a row is MEASURED only when its transaction genuinely completed
 *     through the harness: `tx.complete({ localUPLCEval: true })` + sign +
 *     `submit()` + `awaitTx()` all succeeding, exactly the way
 *     `submit-init-emulator-validation-dispute.test.ts` and
 *     `submit-init-emulator-soundness.test.ts` already drive these same
 *     resolver paths;
 *   - cpu/memory come from the evaluated ExUnits already present in the
 *     signed transaction's witness set, attributed to the target resolver's
 *     own redeemer (every captured transaction below is asserted to carry
 *     exactly one script redeemer — the target resolver's — so
 *     `measureCompleteSignedTransaction`'s redeemer-summed ExUnits are safe
 *     to attribute to it directly; a captured transaction that ever carries
 *     more than one script redeemer fails loudly instead of silently
 *     mis-attributing);
 *   - byte measurements come from the signed CBOR via the pinned
 *     `measureCompleteSignedTransaction` helper, imported verbatim;
 *   - `evalOutcome: "accepted"` is recorded only for rows measured this way.
 *
 * Coverage: only two existing, non-editable fixture builders in this
 * codebase drive a *genuine* one-step validation dispute end to end through
 * the harness:
 *   - `buildInvalidForcedValidationDisputeFixture` reaches resolverIndex 0
 *     (canonicalDecode) / semantic global index 1 (canonicalDecodeItem) plus
 *     its 4-stage canonical-decode-item chain
 *     (source/observe/proof/settlement). Whether the complete item this
 *     fixture disputes resolves via "direct" or "reference" carriage is
 *     decided honestly by the harness itself, from the item's real byte
 *     size (`selectValidationCompleteItemCarriageV1`) — this generator does
 *     not assume or force either;
 *   - `buildAcceptedClaimOverRejectingTransactionFixture` reaches
 *     resolverIndex 3 (InputSets) and whichever semantic global index that
 *     resolves to.
 * Both fixtures' `prepare-selected` transaction spends
 * `dispute.resolvers[resolverIndex]`, which is *literally* the same script
 * object as `dispute.prepareResolvers[resolverIndex]` for every
 * resolverIndex other than the two direct resolvers (contracts.ts splices
 * `prepareResolvers` straight into `resolvers`) — so the harness-measured
 * `prepare-selected` transaction is simultaneously the genuine, real
 * measurement for the corresponding `prepare` row; it is copied, not
 * re-derived. No other resolverIndex/semanticResolverIndex has an existing,
 * non-editable fixture reaching it, so the remaining rows are honestly
 * reported in `unfit[]` with a specific per-row reason — this generator does
 * not extend or edit the pinned fixture/support files to manufacture more
 * coverage.
 *
 * Determinism: `generateEmulatorAccount`'s seed phrases and `Emulator`'s
 * initial `time` are both wall-clock/randomness-derived by construction
 * (confirmed: `Emulator`'s constructor sets `this.time = Date.now()`, and
 * `generateEmulatorAccount` calls a fresh `generateSeedPhrase()` every
 * time). Both are pinned here to fixed literals — two real mnemonics minted
 * once via `generateSeedPhrase()` and hardcoded, and a fixed
 * `emulator.time` assignment immediately after construction, before any
 * `Lucid()` call reads it — so the two scenarios below are byte-for-byte
 * reproducible. Every other timestamp in the lifecycle (`headerStartTime`,
 * `validityRange()`, etc.) is derived from `emulator.now()`, so pinning that
 * one root value pins the rest transitively.
 */

import { mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { fileURLToPath } from "node:url";

import { MIDGARD_CONSENSUS_LIMITS_V1, outRefLabel } from "@al-ft/midgard-core";
import {
  buildValidationTraceDisputeFaultProofContracts,
  parseFaultProofBlueprint,
  VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES,
  VALIDATION_TRACE_RESOLVER_COUNT_V1,
  validationMachineStateDataFromCore,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  Emulator,
  getAddressDetails,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, it } from "vitest";

import {
  resolveProverSigner,
  submitValidationDisputeAward,
  submitValidationDisputeEnterResolution,
  submitValidationDisputeOpen,
  submitValidationDisputePrepareResolution,
  submitValidationDisputePrepareSelected,
  submitValidationDisputeReveal,
  submitValidationDisputeSemanticResolution,
  submitValidationDisputeVerifySource,
  validationDisputeValidityRange,
  validationResolverIndexV1,
  validationSemanticResolverGlobalIndexV1,
} from "../src/index.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import { buildInvalidForcedValidationDisputeFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  alwaysSucceedsBlueprintPath,
  buildAcceptedClaimOverRejectingTransactionFixture,
  buildCatalogueDeploymentInfo,
  buildMinimalFaultProofContracts,
  buildNonEmptyClaimedLedgerDeltaRootV1,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  cloneBlueprint,
  EMULATOR_PROTOCOL_PARAMETERS,
  expectSingleUtxoWithUnit,
  measureCompleteSignedTransaction,
  network,
  publishPlainReferenceScriptUtxo,
  publishValidationDisputeReferenceScript,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
  runEmulatorLifecycleStage,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";

const REGENERATE = process.env.MIDGARD_REGENERATE_RESOLVER_SWEEP === "1";

// --- fixed, deterministic constants (no timestamps, no randomness) --------

// Two real BIP-39 mnemonics, each minted once via lucid-evolution's own
// `generateSeedPhrase()` and hardcoded here — see the module doc comment on
// why account seeding must be pinned for byte-identical regeneration.
const OPERATOR_SEED_PHRASE =
  "echo version north meadow merit athlete real barrel beauty engine inside trade coral mix sock virus amateur trial nurse install public prison stool math";
const CHALLENGER_SEED_PHRASE =
  "crucial jaguar athlete mammal evoke game copy injury mimic captain practice rude mesh artist lumber shield maze anger nuclear defense excuse pudding motor harvest";
const FIXED_EMULATOR_TIME_MS = 1_700_000_000_000;

const HUB_ORACLE_POLICY_ID = "11".repeat(28);
const FRAUD_PROOF_CATALOGUE_POLICY_ID = "22".repeat(28);

const MAX_L1_PROOF_TX_BYTES =
  MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes;
const RESERVED_CPU_UNITS = BigInt(
  Math.floor(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxCpuUnits * 0.8),
);
const RESERVED_MEMORY_UNITS = BigInt(
  Math.floor(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits * 0.8),
);
// Real Cardano PlutusV3 script-execution price coefficients (the same
// constants `@lucid-evolution/lucid`'s `PROTOCOL_PARAMETERS_DEFAULT` uses).
const PRICE_STEP_NUMERATOR = 721n;
const PRICE_STEP_DENOMINATOR = 10_000_000n;
const PRICE_MEM_NUMERATOR = 577n;
const PRICE_MEM_DENOMINATOR = 10_000n;

const ceilDiv = (numerator: bigint, denominator: bigint) =>
  (numerator + denominator - 1n) / denominator;

const outputPath =
  process.env.MIDGARD_RESOLVER_SWEEP_OUTPUT_PATH ??
  fileURLToPath(
    new URL(
      "../../midgard-validation/tests/fixtures/resolver-proof-fit-sweep-v1.generated.json",
      import.meta.url,
    ),
  );

const deterministicEmulatorAccount = (
  seedPhrase: string,
  assets: Record<string, bigint>,
) => ({
  seedPhrase,
  address: walletFromSeed(seedPhrase, {
    addressType: "Base" as const,
    accountIndex: 0,
    network: "Custom" as const,
  }).address,
  assets,
  privateKey: "",
});

type ScenarioResult = {
  readonly resolverIndex: number;
  readonly semanticGlobalIndex: number;
  readonly prepareSelectedMeasurement: ReturnType<
    typeof measureCompleteSignedTransaction
  >;
  /**
   * The semantic-resolution capture's raw measurements, in submission order,
   * with any leading proof-item-publication measurement (present only when
   * `submitValidationDisputeSemanticResolution` genuinely resolves the
   * complete item via "reference" carriage instead of "direct" — a real,
   * measured, honest outcome of the harness, not something this generator
   * dictates) already stripped off, so index 0 is always the
   * semantic-resolver's own "authenticate" transaction and, for
   * canonicalDecodeItem, indices 1-4 are always the four canonical-decode
   * -item stage transactions in source/observe/proof/settlement order —
   * exactly matching `semanticResult.stageTransactions`.
   */
  readonly semanticMeasurements: readonly ReturnType<
    typeof measureCompleteSignedTransaction
  >[];
  readonly proofItemCarriage: "direct" | "reference" | undefined;
};

/**
 * Replicates, inline, exactly the lifecycle orchestration
 * `submit-init-emulator-validation-dispute.test.ts`'s `it.each` case (for
 * `needsItemSemanticPublication: true`) and `runForcedValidationDisputeScenario`
 * (for `needsItemSemanticPublication: false`) already perform — using only
 * their own exported building blocks, never re-derived — but with
 * `captureEmulatorSubmission` wrapped around the two stages this sweep
 * needs measurements from (`prepare-selected`, `semantic-resolution`),
 * which neither of those two call sites captures.
 */
const runResolverScenario = async ({
  buildFixture,
  needsItemSemanticPublication,
}: {
  readonly buildFixture: (input: {
    readonly operatorVkey: string;
    readonly now: number;
  }) => Promise<{
    readonly header: unknown;
    readonly claim: unknown;
    readonly operatorTrace: {
      readonly states: readonly unknown[];
      readonly tree: { readonly proofs: readonly unknown[] };
    };
    readonly challengerTrace: {
      readonly tree: { readonly proofs: readonly unknown[] };
    };
    readonly challengerDescriptor: unknown;
    readonly evidence: {
      readonly moves: readonly {
        readonly role: "operator" | "challenger";
        readonly proof: unknown;
      }[];
      readonly finalDispute: {
        readonly lowIndex: number;
        readonly highIndex: number;
      };
      readonly oneStepArgument: unknown;
    };
  }>;
  readonly needsItemSemanticPublication: boolean;
}): Promise<ScenarioResult> => {
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const feeUtxoCount = 12;
  const feeUtxoLovelace = 100_000_000n;
  const operator = deterministicEmulatorAccount(OPERATOR_SEED_PHRASE, {
    lovelace: 40_000_000_000n,
  });
  const challenger = deterministicEmulatorAccount(CHALLENGER_SEED_PHRASE, {
    lovelace: 20_000_000_000n,
  });
  const emulator = new Emulator(
    [
      {
        ...operator,
        assets: {
          lovelace:
            operator.assets.lovelace - BigInt(feeUtxoCount) * feeUtxoLovelace,
        },
      },
      ...Array.from({ length: feeUtxoCount }, () => ({
        ...operator,
        assets: { lovelace: feeUtxoLovelace },
      })),
      {
        ...challenger,
        assets: {
          lovelace:
            challenger.assets.lovelace - BigInt(feeUtxoCount) * feeUtxoLovelace,
        },
      },
      ...Array.from({ length: feeUtxoCount }, () => ({
        ...challenger,
        assets: { lovelace: feeUtxoLovelace },
      })),
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  // Must happen before any `Lucid()` call reads `emulator.now()` — see the
  // module doc comment.
  emulator.time = FIXED_EMULATOR_TIME_MS;

  const operatorLucid = await Lucid(emulator, "Custom");
  const challengerLucid = await Lucid(emulator, "Custom");
  operatorLucid.selectWallet.fromSeed(operator.seedPhrase);
  challengerLucid.selectWallet.fromSeed(challenger.seedPhrase);
  const operatorSigner = resolveProverSigner({
    network,
    walletSeedPhrase: operator.seedPhrase,
  });
  const challengerSigner = resolveProverSigner({
    network,
    walletSeedPhrase: challenger.seedPhrase,
  });
  const validityRange = () => validationDisputeValidityRange(emulator.now());

  await registerPhasMembershipRewardAccount(operatorLucid, realBlueprint);
  const nonceUtxo = (await operatorLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected operator wallet to expose a nonce UTxO");
  }
  const contracts = await buildMinimalFaultProofContracts(
    realBlueprint,
    alwaysBlueprint,
    nonceUtxo,
    { realValidationTraceDispute: true, alwaysFraudProofCatalogue: true },
  );

  let itemSemanticContract:
    | Effect.Effect.Success<
        ReturnType<typeof buildValidationTraceDisputeFaultProofContracts>
      >["validationTraceDispute"]["semanticResolvers"][number]
    | undefined;
  let itemObserveContract:
    | Effect.Effect.Success<
        ReturnType<typeof buildValidationTraceDisputeFaultProofContracts>
      >["validationTraceDispute"]["canonicalDecodeItemStages"]["observe"]
    | undefined;
  let canonicalDecodePrepareContract:
    | Effect.Effect.Success<
        ReturnType<typeof buildValidationTraceDisputeFaultProofContracts>
      >["validationTraceDispute"]["prepareResolvers"][number]
    | undefined;
  if (needsItemSemanticPublication) {
    const validationDisputeSdkContracts = await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
        network,
        hubOraclePolicyId: contracts.hubOracle.policyId,
        fraudProofCataloguePolicyId: contracts.fraudProofCatalogue.policyId,
      }),
    );
    itemSemanticContract =
      validationDisputeSdkContracts.validationTraceDispute.semanticResolvers[1];
    // #597 ruling a / #617: the observe stage sources its validator from a
    // published reference script too, so the complete-item chain needs the
    // observe publication beside the item-semantic one.
    itemObserveContract =
      validationDisputeSdkContracts.validationTraceDispute
        .canonicalDecodeItemStages.observe;
    // #617 follow-up: the prepare-selected step transaction sources the
    // canonical-decode prepare-resolver validator from a published
    // reference script as well.
    canonicalDecodePrepareContract =
      validationDisputeSdkContracts.validationTraceDispute.prepareResolvers[0];
  }

  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  const operatorPaymentCredential = getAddressDetails(
    await operatorLucid.wallet().address(),
  ).paymentCredential;
  if (
    operatorPaymentCredential === undefined ||
    operatorPaymentCredential.type !== "Key"
  ) {
    throw new Error("Expected operator wallet to expose a payment key hash");
  }
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(
      operatorLucid,
      emulator.now() + 120_000,
    ) - 1;
  const fixture = await buildFixture({
    operatorVkey: operatorPaymentCredential.hash,
    now: headerStartTime,
  });
  const setup = await runEmulatorLifecycleStage("setup", () =>
    submitSetupTx({
      lucid: operatorLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fixture.header as never,
    }),
  );
  const publicationSlotConfig = operatorLucid.config().slotConfig;
  if (publicationSlotConfig === undefined) {
    throw new Error(
      "Expected reference-script publisher Lucid to expose its Custom slot config",
    );
  }
  const setupProtocolParameters = emulator.protocolParameters;
  emulator.protocolParameters = {
    ...setupProtocolParameters,
    maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  };
  const referenceScriptPublisherLucid = await Lucid(emulator, "Custom", {
    slotConfig: publicationSlotConfig,
  });
  referenceScriptPublisherLucid.selectWallet.fromSeed(operator.seedPhrase);
  const validationDisputePublication = await runEmulatorLifecycleStage(
    "reference-script.publish-authenticated",
    async () => {
      try {
        return await publishValidationDisputeReferenceScript({
          lucid: referenceScriptPublisherLucid,
          contracts,
          now: emulator.now(),
        });
      } finally {
        emulator.protocolParameters = setupProtocolParameters;
      }
    },
  );

  const deploymentInfo = needsItemSemanticPublication
    ? await (async () => {
        if (itemSemanticContract === undefined) {
          throw new Error("Expected item-semantic contract to be resolved");
        }
        if (itemObserveContract === undefined) {
          throw new Error("Expected item-observe contract to be resolved");
        }
        if (canonicalDecodePrepareContract === undefined) {
          throw new Error(
            "Expected canonical-decode prepare contract to be resolved",
          );
        }
        const itemSemanticPublication = await runEmulatorLifecycleStage(
          "reference-script.publish-item-semantic",
          async () => {
            emulator.protocolParameters = {
              ...setupProtocolParameters,
              maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
            };
            try {
              return await publishPlainReferenceScriptUtxo({
                lucid: referenceScriptPublisherLucid,
                script: itemSemanticContract.spendingScript,
                label: "validation item-semantic",
              });
            } finally {
              emulator.protocolParameters = setupProtocolParameters;
            }
          },
        );
        const itemObservePublication = await runEmulatorLifecycleStage(
          "reference-script.publish-item-observe",
          async () => {
            emulator.protocolParameters = {
              ...setupProtocolParameters,
              maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
            };
            try {
              return await publishPlainReferenceScriptUtxo({
                lucid: referenceScriptPublisherLucid,
                script: itemObserveContract.spendingScript,
                label: "validation item-observe",
              });
            } finally {
              emulator.protocolParameters = setupProtocolParameters;
            }
          },
        );
        const canonicalDecodePreparePublication =
          await runEmulatorLifecycleStage(
            "reference-script.publish-canonical-decode-prepare",
            async () => {
              emulator.protocolParameters = {
                ...setupProtocolParameters,
                maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
              };
              try {
                return await publishPlainReferenceScriptUtxo({
                  lucid: referenceScriptPublisherLucid,
                  script: canonicalDecodePrepareContract.spendingScript,
                  label: "validation canonical-decode prepare",
                });
              } finally {
                emulator.protocolParameters = setupProtocolParameters;
              }
            },
          );
        return buildRemovalDeploymentInfo(contracts, catalogue, {
          validationDisputePublication,
          validationItemSemanticReference: {
            scriptHash: itemSemanticContract.spendingScriptHash,
            utxo: itemSemanticPublication.utxo,
          },
          validationItemObserveReference: {
            scriptHash: itemObserveContract.spendingScriptHash,
            utxo: itemObservePublication.utxo,
          },
          validationCanonicalDecodePrepareReference: {
            scriptHash: canonicalDecodePrepareContract.spendingScriptHash,
            utxo: canonicalDecodePreparePublication.utxo,
          },
        });
      })()
    : buildRemovalDeploymentInfo(contracts, catalogue, {
        validationDisputePublication,
      });

  const initResult = await runEmulatorLifecycleStage("init", () =>
    submitInit({
      lucid: challengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      fraudCategory: "validationTraceDispute",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    }),
  );
  const functionalProtocolParameters = emulator.protocolParameters;
  const functionalSlotConfig = challengerLucid.config().slotConfig;
  if (functionalSlotConfig === undefined) {
    throw new Error(
      "Expected functional emulator Lucid to expose its Custom slot config",
    );
  }
  emulator.protocolParameters = {
    ...functionalProtocolParameters,
    maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  };
  const targetOperatorLucid = await Lucid(emulator, "Custom", {
    slotConfig: functionalSlotConfig,
  });
  const targetChallengerLucid = await Lucid(emulator, "Custom", {
    slotConfig: functionalSlotConfig,
  });
  targetOperatorLucid.selectWallet.fromSeed(operator.seedPhrase);
  targetChallengerLucid.selectWallet.fromSeed(challenger.seedPhrase);
  const firstStepUtxo = await expectSingleUtxoWithUnit(
    targetChallengerLucid,
    initResult.firstStepAddress,
    initResult.computationThreadUnit,
  );
  const openResult = await runEmulatorLifecycleStage("open", () =>
    submitValidationDisputeOpen({
      lucid: targetChallengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      claim: fixture.claim as never,
      challengerDescriptor: fixture.challengerDescriptor as never,
      validityRange: validityRange(),
      awaitConfirmation: true,
    }),
  );
  const sourceResult = await runEmulatorLifecycleStage("source", () =>
    submitValidationDisputeVerifySource({
      lucid: targetChallengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      threadOutRef: openResult.nextThreadOutRef,
      validityRange: validityRange(),
      awaitConfirmation: true,
    }),
  );

  let threadOutRef = sourceResult.nextThreadOutRef;
  for (const move of fixture.evidence.moves) {
    const revealResult = await runEmulatorLifecycleStage(
      `reveal.${move.role}`,
      () =>
        submitValidationDisputeReveal({
          lucid:
            move.role === "operator"
              ? targetOperatorLucid
              : targetChallengerLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: move.role === "operator" ? operatorSigner : challengerSigner,
          threadOutRef,
          role: move.role,
          proof: move.proof as never,
          validityRange: validityRange(),
          awaitConfirmation: true,
        }),
    );
    threadOutRef = revealResult.nextThreadOutRef;
  }

  const resolutionResult = await runEmulatorLifecycleStage(
    "enter-resolution",
    () =>
      submitValidationDisputeEnterResolution({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef,
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
  );
  const { lowIndex, highIndex } = fixture.evidence.finalDispute;
  const prepareResult = await runEmulatorLifecycleStage(
    "prepare-resolution",
    () =>
      submitValidationDisputePrepareResolution({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef: resolutionResult.nextThreadOutRef,
        preState: validationMachineStateDataFromCore(
          fixture.operatorTrace.states[lowIndex] as never,
        ),
        operatorPost: validationTraceProofDataFromCore(
          fixture.operatorTrace.tree.proofs[highIndex] as never,
        ),
        challengerPost: validationTraceProofDataFromCore(
          fixture.challengerTrace.tree.proofs[highIndex] as never,
        ),
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
  );

  const prepareSelectedCapture = await runEmulatorLifecycleStage(
    "prepare-selected",
    () =>
      captureEmulatorSubmission(emulator, () =>
        submitValidationDisputePrepareSelected({
          lucid: targetChallengerLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: challengerSigner,
          threadOutRef: prepareResult.nextThreadOutRef,
          oneStepArgument: fixture.evidence.oneStepArgument as never,
          validityRange: validityRange(),
          awaitConfirmation: true,
        }),
      ),
  );
  const selectedResult = prepareSelectedCapture.result;

  const semanticCapture = await runEmulatorLifecycleStage(
    "semantic-resolution",
    () =>
      captureEmulatorSubmission(emulator, () =>
        submitValidationDisputeSemanticResolution({
          lucid: targetChallengerLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: challengerSigner,
          threadOutRef: selectedResult.nextThreadOutRef,
          oneStepArgument: fixture.evidence.oneStepArgument as never,
          validityRange: validityRange(),
          awaitConfirmation: true,
        }),
      ),
  );
  const semanticResult = semanticCapture.result;

  await runEmulatorLifecycleStage("award", () =>
    submitValidationDisputeAward({
      lucid: targetChallengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      threadOutRef: semanticResult.nextThreadOutRef,
      validityRange: validityRange(),
      awaitConfirmation: true,
    }),
  );

  const resolverIndex = (
    fixture.evidence.oneStepArgument as { resolverIndex: number }
  ).resolverIndex;
  const semanticResolverIndex = selectedResult.semanticResolverIndex as number;
  const semanticGlobalIndex = validationSemanticResolverGlobalIndexV1(
    resolverIndex,
    semanticResolverIndex,
  );

  // `submitValidationDisputeSemanticResolution` genuinely, honestly decides
  // between "direct" and "reference" carriage for the complete item it
  // resolves, purely as a function of that item's real byte size (see
  // `selectValidationCompleteItemCarriageV1`) — this generator does not
  // choose or assume a carriage; it reports whichever one the harness
  // actually reached. Only "reference" carriage prepends an extra
  // proof-item-publication transaction ahead of the semantic resolver's own
  // "authenticate" transaction (confirmed against
  // `submit-init-emulator-validation-dispute.test.ts`'s own
  // `referenceInputCount` assertions: `[0, 2, 0, 2, 0, 0]` for reference vs
  // `[1, 0, 1, 0, 0]` for direct — authenticate reads the item-semantic
  // reference script and observe reads the item-observe one, #597 ruling
  // a / #617); stripping it here keeps
  // `semanticMeasurements[0]` always the authenticate transaction and
  // `semanticMeasurements[1..4]` (when present) always the four
  // canonical-decode-item stage transactions, regardless of which carriage
  // was actually measured.
  const proofItemCarriage = (
    semanticResult as { proofItemCarriage?: "direct" | "reference" }
  ).proofItemCarriage;
  const semanticMeasurements =
    proofItemCarriage === "reference"
      ? semanticCapture.measurements.slice(1)
      : semanticCapture.measurements;

  return {
    resolverIndex,
    semanticGlobalIndex,
    prepareSelectedMeasurement: prepareSelectedCapture.measurement,
    semanticMeasurements,
    proofItemCarriage,
  };
};

describe.skipIf(!REGENERATE)(
  "resolver proof-fit sweep V1 generation (harness-based)",
  () => {
    it("drives every harness-reachable resolver row to genuine acceptance and writes the sweep artifact", async () => {
      // --- scriptHash/title map for all 105 rows, independent of the
      // harness scenarios below (fixed literal policy IDs, no account
      // dependency at all). ---
      const scriptHashBlueprint = readBlueprint(realBlueprintPath);
      const scriptHashContracts = await Effect.runPromise(
        buildValidationTraceDisputeFaultProofContracts({
          blueprint: parseFaultProofBlueprint(
            cloneBlueprint(scriptHashBlueprint),
          ),
          network,
          hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
          fraudProofCataloguePolicyId: FRAUD_PROOF_CATALOGUE_POLICY_ID,
        }),
      );
      const dispute = scriptHashContracts.validationTraceDispute;
      if (dispute.resolvers.length !== VALIDATION_TRACE_RESOLVER_COUNT_V1) {
        throw new Error(
          `resolvers length ${String(dispute.resolvers.length)} !== ${String(VALIDATION_TRACE_RESOLVER_COUNT_V1)}`,
        );
      }
      if (dispute.prepareResolvers.length !== 14) {
        throw new Error(
          `prepareResolvers length ${String(dispute.prepareResolvers.length)} !== 14`,
        );
      }
      if (dispute.semanticResolvers.length !== 91) {
        throw new Error(
          `semanticResolvers length ${String(dispute.semanticResolvers.length)} !== 91`,
        );
      }

      const prepareTitles = Object.values(
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.prepares,
      );
      const topLevelTitles = prepareTitles;
      const semanticTitles = Object.values(
        VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics,
      );
      const prepareResolverIndexOf = [
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
      ];

      // --- run the two harness-reachable scenarios ------------------------
      const canonicalDecodeItem = await runResolverScenario({
        buildFixture: ({ operatorVkey, now }) =>
          buildInvalidForcedValidationDisputeFixture({
            operatorVkey,
            now,
            inlineDatumPayloadBytes: 7_976,
            minimumCompleteItemBytes: 0,
          }),
        needsItemSemanticPublication: true,
      });
      if (canonicalDecodeItem.resolverIndex !== 0) {
        throw new Error(
          `expected buildInvalidForcedValidationDisputeFixture to reach resolverIndex 0 (canonicalDecode), got ${canonicalDecodeItem.resolverIndex.toString()}`,
        );
      }
      if (canonicalDecodeItem.semanticGlobalIndex !== 1) {
        throw new Error(
          `expected buildInvalidForcedValidationDisputeFixture to reach semantic global index 1 (canonicalDecodeItem), got ${canonicalDecodeItem.semanticGlobalIndex.toString()}`,
        );
      }
      if (
        canonicalDecodeItem.proofItemCarriage !== "direct" &&
        canonicalDecodeItem.proofItemCarriage !== "reference"
      ) {
        throw new Error(
          `expected the canonicalDecodeItem semantic-resolution result to expose a real proofItemCarriage, got ${String(canonicalDecodeItem.proofItemCarriage)}`,
        );
      }
      // Once the leading proof-item-publication measurement (present only
      // under "reference" carriage) is stripped, the capture always yields
      // exactly 5 measurements: the semantic resolver's own "authenticate"
      // transaction plus the four canonical-decode-item stage transactions
      // (source/observe/proof/settlement) — regardless of which carriage
      // the harness actually, honestly reached for this run's real item
      // byte size.
      if (canonicalDecodeItem.semanticMeasurements.length !== 5) {
        throw new Error(
          `expected the canonicalDecodeItem semantic-resolution capture to yield exactly 5 measurements (authenticate + source/observe/proof/settlement) once ${canonicalDecodeItem.proofItemCarriage} carriage's own publication measurement (if any) is stripped, got ${canonicalDecodeItem.semanticMeasurements.length.toString()}`,
        );
      }

      const inputSets = await runResolverScenario({
        buildFixture: async ({ operatorVkey, now }) =>
          buildAcceptedClaimOverRejectingTransactionFixture({
            operatorVkey,
            now,
            claimedLedgerDeltaRoot:
              await buildNonEmptyClaimedLedgerDeltaRootV1(),
          }),
        needsItemSemanticPublication: false,
      });
      if (inputSets.resolverIndex !== validationResolverIndexV1("InputSets")) {
        throw new Error(
          `expected buildAcceptedClaimOverRejectingTransactionFixture to reach the InputSets resolverIndex, got ${inputSets.resolverIndex.toString()}`,
        );
      }
      if (inputSets.semanticMeasurements.length !== 1) {
        throw new Error(
          `expected the InputSets semantic-resolution capture to yield exactly 1 measurement, got ${inputSets.semanticMeasurements.length.toString()}`,
        );
      }

      // --- row assembly ----------------------------------------------------
      const measuredRow = ({
        category,
        index,
        label,
        title,
        scriptHash,
        measurement,
      }: {
        readonly category: string;
        readonly index: number;
        readonly label: string;
        readonly title: string;
        readonly scriptHash: string;
        readonly measurement: ReturnType<
          typeof measureCompleteSignedTransaction
        >;
      }) => {
        if (measurement.redeemerCount !== 1) {
          throw new Error(
            `${category}[${String(index)}] ${label}: expected exactly 1 script redeemer in the harness-accepted transaction so summed ExUnits can be safely attributed to this resolver alone, found ${measurement.redeemerCount.toString()}`,
          );
        }
        const cpu = measurement.executionSteps;
        const memory = measurement.executionMemory;
        const executionFeeLovelace =
          ceilDiv(cpu * PRICE_STEP_NUMERATOR, PRICE_STEP_DENOMINATOR) +
          ceilDiv(memory * PRICE_MEM_NUMERATOR, PRICE_MEM_DENOMINATOR);
        const byteMargin =
          MAX_L1_PROOF_TX_BYTES - measurement.completeSignedBytes;
        const cpuMargin = RESERVED_CPU_UNITS - cpu;
        const memoryMargin = RESERVED_MEMORY_UNITS - memory;
        return {
          category,
          index,
          label,
          title,
          scriptHash,
          completeSignedBytes: measurement.completeSignedBytes,
          l1ByteMargin: measurement.l1ByteMargin,
          cpu: cpu.toString(),
          memory: memory.toString(),
          executionFeeLovelace: executionFeeLovelace.toString(),
          byteMargin,
          cpuMargin: cpuMargin.toString(),
          memoryMargin: memoryMargin.toString(),
          fitsByteMargin: byteMargin >= 0,
          fitsCpuMargin: cpuMargin >= 0n,
          fitsMemoryMargin: memoryMargin >= 0n,
          evalOutcome: "accepted" as const,
        };
      };

      const noFixtureReason = (what: string) =>
        `no harness-reachable fixture in this codebase drives a genuine one-step validation dispute to ${what}; the only two existing, non-editable fixture builders that drive a real transaction through the emulator harness to a genuine accept (buildInvalidForcedValidationDisputeFixture -> resolverIndex 0/canonicalDecode + semantic global index 1/canonicalDecodeItem + its 4-stage chain; buildAcceptedClaimOverRejectingTransactionFixture -> resolverIndex ${inputSets.resolverIndex.toString()}/InputSets + semantic global index ${inputSets.semanticGlobalIndex.toString()}) do not reach it. Measuring it would require either a new fixture builder reaching that phase (out of scope: pinned support/fixture files may only be imported, never extended or edited) or hand-constructing a transaction context outside the harness (explicitly disallowed by the ruling that produced this generator)`;

      const rows: Record<string, unknown>[] = [];

      // 14 top-level resolvers.
      const topLevelRows: (
        | ReturnType<typeof measuredRow>
        | {
            readonly unmeasuredReason: string;
            readonly category: string;
            readonly index: number;
            readonly label: string;
            readonly title: string;
          }
      )[] = [];
      for (
        let resolverIndex = 0;
        resolverIndex < VALIDATION_TRACE_RESOLVER_COUNT_V1;
        resolverIndex += 1
      ) {
        const label = `resolverIndex ${resolverIndex.toString()}`;
        const title = topLevelTitles[resolverIndex] as string;
        const scriptHash = dispute.resolvers[resolverIndex]!.spendingScriptHash;
        let row;
        if (resolverIndex === canonicalDecodeItem.resolverIndex) {
          row = measuredRow({
            category: "topLevel",
            index: resolverIndex,
            label,
            title,
            scriptHash,
            measurement: canonicalDecodeItem.prepareSelectedMeasurement,
          });
        } else if (resolverIndex === inputSets.resolverIndex) {
          row = measuredRow({
            category: "topLevel",
            index: resolverIndex,
            label,
            title,
            scriptHash,
            measurement: inputSets.prepareSelectedMeasurement,
          });
        } else {
          row = {
            category: "topLevel",
            index: resolverIndex,
            label,
            title,
            unmeasuredReason: noFixtureReason(
              `resolverIndex ${resolverIndex.toString()} (${title})`,
            ),
          };
        }
        topLevelRows.push(row as never);
        rows.push(row as never);
      }

      // 14 prepare resolvers — copied from the matching topLevel row where
      // the underlying compiled script is literally the same object
      // (`resolvers[i] === prepareResolvers[i]` for every resolverIndex;
      // confirmed in demo/midgard-sdk/src/fraud-proof/contracts.ts).
      for (let prepareIndex = 0; prepareIndex < 14; prepareIndex += 1) {
        const resolverIndex = prepareResolverIndexOf[prepareIndex]!;
        const source = topLevelRows[resolverIndex]!;
        const label = `prepare ${prepareTitles[prepareIndex]}`;
        if ("unmeasuredReason" in source) {
          rows.push({
            category: "prepare",
            index: prepareIndex,
            label,
            title: source.title,
            unmeasuredReason: noFixtureReason(
              `prepare resolver ${prepareIndex.toString()} (${label}), i.e. the same compiled script as topLevel[${resolverIndex.toString()}]`,
            ),
          });
        } else {
          rows.push({
            ...source,
            category: "prepare",
            index: prepareIndex,
            label,
            copiedFromCategory: "topLevel",
            copiedFromIndex: resolverIndex,
          });
        }
      }

      // 90 semantic resolvers.
      for (let globalIndex = 0; globalIndex < 90; globalIndex += 1) {
        const title = semanticTitles[globalIndex] as string;
        const scriptHash =
          dispute.semanticResolvers[globalIndex]!.spendingScriptHash;
        if (globalIndex === canonicalDecodeItem.semanticGlobalIndex) {
          rows.push(
            measuredRow({
              category: "semantic",
              index: globalIndex,
              label: title,
              title,
              scriptHash,
              measurement: canonicalDecodeItem.semanticMeasurements[0]!,
            }),
          );
        } else if (globalIndex === inputSets.semanticGlobalIndex) {
          rows.push(
            measuredRow({
              category: "semantic",
              index: globalIndex,
              label: title,
              title,
              scriptHash,
              measurement: inputSets.semanticMeasurements[0]!,
            }),
          );
        } else {
          rows.push({
            category: "semantic",
            index: globalIndex,
            label: title,
            title,
            unmeasuredReason: noFixtureReason(
              `semantic global index ${globalIndex.toString()} (${title})`,
            ),
          });
        }
      }

      // 4 canonical-decode-item stages — measurements[1..4] of the
      // canonicalDecodeItem semantic-resolution capture
      // (measurements[0] is the authentication/semantic-resolver spend
      // itself, already placed at semantic[1] above).
      const stageNames = ["source", "observe", "proof", "settlement"] as const;
      const stageScriptHashes = [
        dispute.canonicalDecodeItemStages.source.spendingScriptHash,
        dispute.canonicalDecodeItemStages.observe.spendingScriptHash,
        dispute.canonicalDecodeItemStages.proof.spendingScriptHash,
        dispute.canonicalDecodeItemStages.settlement.spendingScriptHash,
      ];
      for (let stageIndex = 0; stageIndex < 4; stageIndex += 1) {
        const stageName = stageNames[stageIndex];
        const title =
          VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.canonicalDecodeItemStages[
            stageName
          ];
        rows.push(
          measuredRow({
            category: "canonicalDecodeItemStage",
            index: stageIndex,
            label: stageName,
            title,
            scriptHash: stageScriptHashes[stageIndex]!,
            measurement:
              canonicalDecodeItem.semanticMeasurements[stageIndex + 1]!,
          }),
        );
      }

      if (rows.length !== 122) {
        throw new Error(`built ${rows.length.toString()} rows, expected 122`);
      }

      const unfit = rows
        .filter(
          (row) =>
            row.unmeasuredReason !== undefined ||
            row.fitsByteMargin === false ||
            row.fitsCpuMargin === false ||
            row.fitsMemoryMargin === false,
        )
        .map((row) => ({
          category: row.category,
          index: row.index,
          label: row.label,
          reason:
            row.unmeasuredReason ??
            [
              row.fitsByteMargin === false ? "byte margin exceeded" : null,
              row.fitsCpuMargin === false ? "cpu margin exceeded" : null,
              row.fitsMemoryMargin === false ? "memory margin exceeded" : null,
            ]
              .filter((entry) => entry !== null)
              .join("; "),
        }));

      const categoryCounts = rows.reduce<Record<string, number>>(
        (counts, row) => {
          const category = row.category as string;
          counts[category] = (counts[category] ?? 0) + 1;
          return counts;
        },
        {},
      );

      const measuredCount = rows.filter(
        (row) => row.unmeasuredReason === undefined,
      ).length;
      const unmeasuredCount = rows.length - measuredCount;

      const artifact = {
        schema: "midgard-validation-resolver-proof-fit-sweep-v1",
        version: 3,
        measurementMethod: "emulator-harness-v1",
        thresholds: {
          maxL1ProofTxBytes: MAX_L1_PROOF_TX_BYTES,
          reservedCpuUnits: RESERVED_CPU_UNITS.toString(),
          reservedMemoryUnits: RESERVED_MEMORY_UNITS.toString(),
        },
        rowCount: rows.length,
        categoryCounts,
        measuredCount,
        unmeasuredCount,
        rows,
        unfit,
      };

      const json = `${JSON.stringify(artifact, null, 2)}\n`;
      mkdirSync(dirname(outputPath), { recursive: true });
      writeFileSync(outputPath, json, "utf8");

      process.stdout.write(
        `${JSON.stringify(
          {
            outputPath,
            rowCount: rows.length,
            categoryCounts,
            measuredCount,
            unmeasuredCount,
            unfitCount: unfit.length,
          },
          null,
          2,
        )}\n`,
      );
    }, 900_000);
  },
);
